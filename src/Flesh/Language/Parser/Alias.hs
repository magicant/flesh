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
  -- * MonadReparse
  MonadReparse(..),
  -- * PushBackT
  PushBackT, mapPushBackT, runPushBackT, evalPushBackT,
  -- * ReparseT
  ReparseT, mapReparseT, runReparseT, evalReparseT, fromMaybeT,
  -- * Helper functions
  isAfterBlankEndingSubstitution, maybeAliasValue) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (MonadPlus, ap, guard, when)
import Control.Monad.Except (
  ExceptT, MonadError, catchError, mapExceptT, throwError)
import Control.Monad.Reader (
  MonadReader, ReaderT, ask, local, mapReaderT, reader)
import Control.Monad.State.Strict (
  StateT, get, mapStateT, modify, modify', put, runStateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), mapMaybeT, runMaybeT)
import Control.Monad.Writer.Strict (WriterT, mapWriterT)
import Data.Foldable (for_)
import Data.Map.Strict (lookup)
import Data.Text (Text, unpack)
import Flesh.Data.Char
import Flesh.Language.Alias
import Flesh.Language.Parser.Input
import Flesh.Source.Position
import Prelude hiding (lookup)

-- | Monad transformer that makes parse results depend on alias definitions.
type ContextT = ReaderT DefinitionSet

-- | Extension of MonadBuffer that provides access to input characters that
-- | Monad that allows replacement of the input character sequence.
class Monad m => MonadReparse m where
  {-# MINIMAL maybeReparse | maybeReparse' #-}

  -- | Executes the given monad and examines the 'fst' part of the result. If
  -- it is Nothing, it is discarded and 'maybeReparse' has no other effect.
  -- Otherwise, the positioned character string replaces the input character
  -- sequence that was parsed by the argument parser. Parsing resumes with the
  -- current position at the beginning of the replacement.
  --
  -- 'maybeReparse' must not impose any additional side effect on the
  -- underlying input source.
  maybeReparse :: m (Maybe [Positioned Char], a) -> m a
  maybeReparse = fmap snd . maybeReparse'

  -- | Like 'maybeReparse', but returns the @Maybe [Positioned Char]@ as well.
  maybeReparse' :: m (Maybe [Positioned Char], a)
                -> m (Maybe [Positioned Char], a)
  maybeReparse' = maybeReparse . fmap f
    where f (mpcs, a) = (mpcs, (mpcs, a))

instance MonadReparse m => MonadReparse (ExceptT e m) where
  maybeReparse = mapExceptT $ maybeReparse . fmap f
    where f (Left e)          = (Nothing, Left e)
          f (Right (mpcs, a)) = (mpcs, Right a)

instance MonadReparse m => MonadReparse (MaybeT m) where
  maybeReparse = mapMaybeT $ maybeReparse . fmap f
    where f Nothing          = (Nothing, Nothing)
          f (Just (mpcs, a)) = (mpcs, Just a)

instance MonadReparse m => MonadReparse (ReaderT r m) where
  maybeReparse = mapReaderT maybeReparse
  maybeReparse' = mapReaderT maybeReparse'

instance MonadReparse m => MonadReparse (StateT s m) where
  maybeReparse = mapStateT $ maybeReparse . fmap f
    where f ((mpcs, a), s) = (mpcs, (a, s))

instance (MonadReparse m, Monoid w) => MonadReparse (WriterT w m) where
  maybeReparse = mapWriterT $ maybeReparse . fmap f
    where f ((mpcs, a), w) = (mpcs, (a, w))

instance Monad m => MonadReparse (PositionedStringT m) where
  maybeReparse' (PositionedStringT m) = PositionedStringT $ do
    r@(mpcs, _) <- m
    for_ mpcs $ modify' . push
    return r
      where push newcs oldcs = foldr (:~) oldcs newcs

instance MonadReparse m => MonadReparse (RecordT m) where
  maybeReparse' (RecordT m) = RecordT $ do
    s <- get
    r@(mpcs, _) <- maybeReparse' m
    when (mpcs /= Nothing) (put s)
    return r

-- | Monad transformer that allows insertion of input characters as a side
-- effect of parsing.
newtype PushBackT m a =
  PushBackT {getPushBackT :: StateT [Positioned Char] m a}

-- | Directly modifies the result of PushBackT.
mapPushBackT :: (m (a, [Positioned Char]) -> n (b, [Positioned Char]))
             -> PushBackT m a -> PushBackT n b
mapPushBackT f = PushBackT . mapStateT f . getPushBackT

-- | Executes the PushBackT monad, returning the main result and the
-- characters that have been inserted but not consumed.
runPushBackT :: PushBackT m a -> m (a, [Positioned Char])
runPushBackT (PushBackT m) = runStateT m []

-- | Executes the PushBackT monad, returning the main result only.
evalPushBackT :: Functor m => PushBackT m a -> m a
evalPushBackT = fmap fst . runPushBackT

instance MonadTrans PushBackT where
  lift = PushBackT . lift

instance Functor m => Functor (PushBackT m) where
  fmap f = PushBackT . fmap f . getPushBackT

instance Monad m => Applicative (PushBackT m) where
  pure = PushBackT . pure
  PushBackT a <*> PushBackT b = PushBackT (a <*> b)

instance MonadPlus m => Alternative (PushBackT m) where
  empty = PushBackT empty
  PushBackT a <|> PushBackT b = PushBackT (a <|> b)

instance Monad m => Monad (PushBackT m) where
  PushBackT a >>= f = PushBackT $ a >>= getPushBackT . f

instance MonadPlus m => MonadPlus (PushBackT m)

instance MonadError e m => MonadError e (PushBackT m) where
  throwError = PushBackT . throwError
  catchError m f = PushBackT $ catchError (getPushBackT m) (getPushBackT . f)

instance MonadBuffer m => MonadBuffer (PushBackT m) where
  popChar = PushBackT $ do
    pcs <- get
    case pcs of
      [] -> popChar
      (h:t) -> do { put t; return (Right h) }
  lookahead (PushBackT m) = PushBackT $ do
    oldpcs <- get
    a <- lookahead m
    put oldpcs
    return a
  peekChar = PushBackT $ do
    pcs <- get
    case pcs of
      [] -> peekChar
      (h:_) -> return (Right h)
  currentPosition = PushBackT $ do
    pcs <- get
    case pcs of
      [] -> currentPosition
      ((p, _) : _) -> return p

instance Monad m => MonadReparse (PushBackT m) where
  maybeReparse (PushBackT m) = PushBackT $ do
    (mpcs, a) <- m
    for_ mpcs $ modify . (++)
    return a

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

instance MonadBuffer m => MonadBuffer (ReparseT m) where
  popChar = ReparseT $ do
    c <- popChar
    let mc = return $ Just (Just (ReparseT mc), c)
    mc
  lookahead = mapReparseT $ fmap (fmap f) . lookahead
    where f (_, a) = (Nothing, a)
  peekChar = lift peekChar
  currentPosition = lift currentPosition

instance MonadReparse m => MonadReparse (ReparseT m) where
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
