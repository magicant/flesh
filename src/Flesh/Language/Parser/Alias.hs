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
  -- * AliasT
  AliasT(..), mapAliasT, runAliasT, evalAliasT, fromMaybeT,
  -- * Helper functions
  isAfterBlankEndingSubstitution, substituteAlias) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (MonadPlus, ap, guard, void)
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
-- The result of an @AliasT m a@ instance contains a value of type @a@ if
-- alias substitution did not occur, or nothing otherwise.
--
-- After alias substitution, the new input text must be parsed using the same
-- parser that caused the substitution. This process may include kind of
-- backtracking because the substitution may have been preceded by parsers
-- that consumed no text. Those parsers must be re-invoked because, after the
-- substitution, their result may be different from the previous parse. A
-- successful result of an @AliasT m a@ parser may include another parser of
-- the same type, which must be used for re-parsing after alias substitution.
-- if the result does not contain a parser, the whole of the original parser
-- must be reused, in which case even the preceding parser is subject to
-- backtracking.
newtype AliasT m a = AliasT {getAliasT :: m (Maybe (Maybe (AliasT m a), a))}

-- | Directly modifies the result of AliasT.
mapAliasT :: (m (Maybe (Maybe (AliasT m a), a))
           -> n (Maybe (Maybe (AliasT n b), b)))
          -> AliasT m a -> AliasT n b
mapAliasT f = AliasT . f . getAliasT

-- | Executes the AliasT monad, only returning the optional final result. The
-- result is Nothing if alias substitution occurred.
runAliasT :: Functor m => AliasT m a -> m (Maybe a)
runAliasT (AliasT m) = fmap (fmap snd) m

-- | Executes the AliasT monad, automatically re-parsing the input text after
-- alias substitution, if any.
evalAliasT :: Monad m => AliasT m a -> m a
evalAliasT (AliasT m) = m' where
  m' = do
       y <- m
       case y of
         Nothing -> m'
         Just (_, a) -> return a

-- | Converts MaybeT to AliasT.
--
-- If the value of the MaybeT monad is Nothing, the result is @return ()@.
-- Otherwise, the result is nothing (as if alias substitution occurred).
fromMaybeT :: Functor m => MaybeT m a -> AliasT m ()
fromMaybeT = AliasT . fmap f . runMaybeT
  where f Nothing  = Just (Nothing, ())
        f (Just _) = Nothing

instance MonadTrans AliasT where
  lift = AliasT . fmap f
    where f a = Just (Nothing, a)

instance Functor m => Functor (AliasT m) where
  fmap f = ff
    where ff = mapAliasT $ fmap $ fmap f'
          f' (y, a) = (fmap ff y, f a)

instance Monad m => Applicative (AliasT m) where
  pure a = AliasT $ pure $ Just (Nothing, a)
  (<*>) = ap

instance (Monad m, Alternative m) => Alternative (AliasT m) where
  empty = AliasT empty
  a <|> b = AliasT $ getAliasT a <|> getAliasT b

instance Monad m => Monad (AliasT m) where
  aa >>= fab = AliasT $ do
    ya <- getAliasT aa
    case ya of
      Nothing -> return Nothing
      Just (yaa', a) -> do
        yb <- getAliasT $ fab a
        case yb of
          Nothing ->
            case yaa' of
              Nothing -> return Nothing
              Just aa' -> getAliasT $ aa' >>= fab
          Just (yab', b) -> return $
            case (yaa', yab') of
              (Just aa', Nothing) ->       Just (Just (aa' >>= fab), b)
              _                   -> yb -- Just (yab',               b)

instance MonadPlus m => MonadPlus (AliasT m)

instance MonadInput m => MonadInput (AliasT m) where
  popChar = AliasT $ do
    c <- popChar
    let mc = return $ Just (Just (AliasT mc), c)
    mc
  lookahead = reparsing . mapAliasT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  pushChars cs = AliasT $ Nothing <$ pushChars cs
  reparsing = mapAliasT $ fmap (fmap f)
    where f (_, a) = (Nothing, a)

instance MonadInputRecord m => MonadInputRecord (AliasT m) where
  reverseConsumedChars = lift reverseConsumedChars

instance MonadError e m => MonadError e (AliasT m) where
  throwError = lift . throwError
  catchError m f = AliasT $ catchError (getAliasT m) (getAliasT . f)

instance MonadReader r m => MonadReader r (AliasT m) where
  ask = lift ask
  local f = mapAliasT $ local f
  reader f = lift $ reader f

instance MonadParser m => MonadParser (AliasT m)

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

-- | Performs alias substitution if the text is an alias defined in the
-- context. The substitution is inserted into the input text by 'pushChars'.
--
-- This function substitutes a single alias only. It does not substitute
-- recursively nor substitute the next token (for an alias value ending with a
-- blank).
substituteAlias :: (MonadReader DefinitionSet m, MonadInput m)
                => Position -> Text -> m ()
substituteAlias pos' t = void $ runMaybeT $ do
  defs <- ask
  def <- MaybeT $ return $ lookup t defs
  guard $ applicable t pos'
  let a = Alias pos' def
      v = unpack $ value def
      frag = Fragment v a 0
      pos = Position frag 0
      cs = unposition $ spread pos v
  pushChars cs

-- vim: set et sw=2 sts=2 tw=78:
