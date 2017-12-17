{-
Copyright (C) 2017 WATANABE Yuki <magicant@wonderwand.net>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : non-portable (flexible contexts)

This module defines types and functions for handling aliases in the syntax
parser.
-}
module Flesh.Language.Parser.Alias (
  module Flesh.Language.Alias,
  -- * Context
  ContextT,
  -- * ReparseT
  ReparseT, mapReparseT, runReparseT, evalReparseT, fromMaybeT,
  -- * Helper functions
  isAfterBlankEndingSubstitution, maybeAliasValue) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (MonadPlus, ap, guard)
import Control.Monad.Reader (MonadReader, ReaderT, ask, local, reader)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.Map.Strict (lookup)
import Data.Text (Text, unpack)
import Flesh.Data.Char
import Flesh.Language.Alias
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Source.Position
import Prelude hiding (lookup)

-- | Monad transformer that makes parse results depend on alias definitions.
type ContextT = ReaderT DefinitionSet

-- | Monad transformer that represents results of parse that may be
-- interrupted by alias substitution.
--
-- The result of a @ReparseT m a@ instance contains a value of type @a@ if
-- alias substitution did not occur, or nothing otherwise.
--
-- After alias substitution, the new input text must be parsed using the same
-- parser that caused the substitution. This process may include kind of
-- backtracking because the substitution may have been preceded by parsers
-- that consumed no text. Those parsers must be re-invoked because, after the
-- substitution, their result may be different from the previous parse. A
-- successful result of a @ReparseT m a@ parser may include another parser of
-- the same type, which must be used for re-parsing after alias substitution.
-- If the result does not contain a parser, the whole of the original parser
-- must be reused, in which case even the preceding parser is subject to
-- backtracking.
newtype ReparseT m a =
  ReparseT {getReparseT :: m (Maybe (Maybe (ReparseT m a), a))}

-- | Directly modifies the result of ReparseT.
mapReparseT :: (m (Maybe (Maybe (ReparseT m a), a))
             -> n (Maybe (Maybe (ReparseT n b), b)))
            -> ReparseT m a -> ReparseT n b
mapReparseT f = ReparseT . f . getReparseT

-- | Executes the ReparseT monad, only returning the optional final result.
-- The result is Nothing if alias substitution occurred.
runReparseT :: Functor m => ReparseT m a -> m (Maybe a)
runReparseT (ReparseT m) = fmap (fmap snd) m

-- | Executes the ReparseT monad, automatically re-parsing the input text
-- after alias substitution, if any.
evalReparseT :: Monad m => ReparseT m a -> m a
evalReparseT (ReparseT m) = m' where
  m' = do
       y <- m
       case y of
         Nothing -> m'
         Just (_, a) -> return a

-- | Converts MaybeT to ReparseT.
--
-- If the value of the MaybeT monad is Nothing, the result is @return ()@.
-- Otherwise, the result is nothing (as if alias substitution occurred).
fromMaybeT :: Functor m => MaybeT m a -> ReparseT m ()
fromMaybeT = ReparseT . fmap f . runMaybeT
  where f Nothing  = Just (Nothing, ())
        f (Just _) = Nothing

instance MonadTrans ReparseT where
  lift = ReparseT . fmap f
    where f a = Just (Nothing, a)

instance Functor m => Functor (ReparseT m) where
  fmap f = ff
    where ff = mapReparseT $ fmap $ fmap f'
          f' (y, a) = (fmap ff y, f a)

instance Monad m => Applicative (ReparseT m) where
  pure a = ReparseT $ pure $ Just (Nothing, a)
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (ReparseT m) where
  empty = ReparseT empty
  a <|> b = ReparseT $ getReparseT a <|> getReparseT b

instance Monad m => Monad (ReparseT m) where
  aa >>= fab = ReparseT $ do
    ya <- getReparseT aa
    case ya of
      Nothing -> return Nothing
      Just (yaa', a) -> do
        yb <- getReparseT $ fab a
        case yb of
          Nothing ->
            case yaa' of
              Nothing -> return Nothing
              Just aa' -> getReparseT $ aa' >>= fab
          Just (yab', b) -> return $
            case (yaa', yab') of
              (Just aa', Nothing) ->       Just (Just (aa' >>= fab), b)
              _                   -> yb -- Just (yab',               b)

instance MonadPlus m => MonadPlus (ReparseT m)

instance MonadInput m => MonadInput (ReparseT m) where
  popChar = ReparseT $ do
    c <- popChar
    let mc = return $ Just (Just (ReparseT mc), c)
    mc
  lookahead = mapReparseT $ fmap (fmap f) . lookahead
    where f (_, a) = (Nothing, a)
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  maybeReparse = mapReparseT $ maybeReparse . fmap f
    where f Nothing                         = (Nothing, Nothing)
          f (Just (_,  (mpcs@(Just _), _))) = (mpcs,    Nothing)
          f (Just (ma, (Nothing,       a))) = (Nothing, Just (ma', a))
            where ma' = fmap maybeReparse ma

instance MonadInputRecord m => MonadInputRecord (ReparseT m) where
  reverseConsumedChars = lift reverseConsumedChars

instance MonadError e m => MonadError e (ReparseT m) where
  throwError = lift . throwError
  catchError m f = ReparseT $ catchError (getReparseT m) (getReparseT . f)

instance MonadReader r m => MonadReader r (ReparseT m) where
  ask = lift ask
  local f = mapReparseT $ local f
  reader f = lift $ reader f

instance MonadParser m => MonadParser (ReparseT m)

-- | Tests if the current position is after an alias substitution whose value
-- ends with a blank.
isAfterBlankEndingSubstitution :: MonadInputRecord m => m Bool
isAfterBlankEndingSubstitution = do
  rcc <- reverseConsumedChars
  cp <- currentPosition
  return $ test rcc cp
    where test ((p, c) : cs) p' | isBlank c =
            isAlias p && differentSituations p p' || test cs p
          test _ _ = False
          isAlias p = isAlias' (s p)
          isAlias' (Alias _ _) = True
          isAlias' _ = False
          differentSituations p1 p2 = s p1 /= s p2
          s = situation . fragment

-- | Returns 'True' iff the given position is applicable for alias
-- substitution of the given name. The name is not applicable if the current
-- position is already a result of alias substitution of the name.
applicable :: Text -> Position -> Bool
applicable t (Position (Fragment _ (Alias pos def) _) _)
  | name def == t = False
  | otherwise     = applicable t pos
applicable _ _ = True

-- | Returns the alias value if the position and text match an alias in the
-- current context.
maybeAliasValue :: MonadReader DefinitionSet m
                => Position -> Text -> MaybeT m [Positioned Char]
maybeAliasValue pos' t = do
  defs <- ask
  def <- MaybeT $ return $ lookup t defs
  guard $ applicable t pos'
  let a = Alias pos' def
      v = unpack $ value def
      frag = Fragment v a 0
      pos = Position frag 0
  return $ unposition $ spread pos v

-- vim: set et sw=2 sts=2 tw=78:
