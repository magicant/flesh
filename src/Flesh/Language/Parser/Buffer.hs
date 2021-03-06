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
Portability : non-portable (GHC language extensions)

This module defines types and functions for accessing input characters for the
syntax parser.
-}
module Flesh.Language.Parser.Buffer (
  -- * MonadBuffer
  MonadBuffer(..), followedBy,
  CursorT, mapCursorT, runCursorT, evalCursorT, execCursorT, withCursorT,
  PositionedStringT(..),
  -- * MonadRecord
  MonadRecord(..), RecordT(..), runRecordT, evalRecordT, mapRecordT)
  where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad (MonadPlus, mplus, mzero, void)
import Control.Monad.Except (
  ExceptT, MonadError, catchError, mapExceptT, throwError)
import Control.Monad.Reader (
  MonadReader, ReaderT, ask, local, mapReaderT, reader)
import Control.Monad.State.Strict (
  StateT, evalStateT, get, mapStateT, modify', put, runStateT, withStateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Writer.Strict (WriterT, mapWriterT)
import Flesh.Language.Parser.Input
import Flesh.Source.Position

-- | Monad that provides access to the stream of characters that are to be
-- parsed.
--
-- MonadBuffer is a stateful monad. It maintains a current cursor position to
-- the input characters. Characters can be read only from the current
-- position. The position can be advanced as the characters are read, but
-- cannot be rewound except when the 'lookahead' operation is used.
--
-- Reading a character may be an operation with a side effect on an underlying
-- (typically external) input source. Such side effects must be encoded in the
-- monad. When re-reading characters after the position was rewound, however,
-- characters must be read without side effects. In other words, reading
-- operations must be idempotent in terms of side effects.
class Monad m => MonadBuffer m where
  -- | Reads one character at the current position, advancing the position to
  -- the next character. If the current position is end-of-input, the position
  -- must not be changed and @Left position@ is returned.
  --
  -- 'popChar' may have a side effect of reading from the underlying input
  -- source.
  popChar :: m (Either Position (Positioned Char))

  -- | Returns the result of the given monad but cancels any position advance
  -- that may have occurred in the monad, i.e., the position is rewound to the
  -- original. Thereafter, the same character sequence must be read by
  -- following 'popChar' and 'peekChar' operations.
  --
  -- Note that any side effects that occur in the monad cannot be canceled by
  -- 'lookahead'.
  lookahead :: m a -> m a

  -- | Returns the character at the current position without advancing the
  -- position. The default implementation is @lookahead popChar@.
  --
  -- 'peekChar' may have a side effect of reading from the underlying input
  -- source.
  peekChar :: m (Either Position (Positioned Char))
  peekChar = lookahead popChar

  -- | Returns the current position.
  -- The default implementation is @either id fst '<$>' peekChar@.
  --
  -- 'currentPosition' must not have any side effect on the underlying input
  -- source, which means the default implementation is not applicable if
  -- 'peekChar' has one.
  currentPosition :: m Position
  currentPosition = either id fst <$> peekChar

-- | Like 'lookahead', but ignores the result.
followedBy :: MonadBuffer m => m a -> m ()
followedBy = void . lookahead

instance MonadBuffer m => MonadBuffer (ExceptT e m) where
  popChar = lift popChar
  lookahead = mapExceptT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition

instance MonadBuffer m => MonadBuffer (MaybeT m) where
  popChar = lift popChar
  lookahead = mapMaybeT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition

instance MonadBuffer m => MonadBuffer (ReaderT r m) where
  popChar = lift popChar
  lookahead = mapReaderT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition

-- | In the default implementation, 'lookahead' does not restore the state
-- after executing the inner monad. This behavior might not be what you want.
instance MonadBuffer m => MonadBuffer (StateT s m) where
  popChar = lift popChar
  lookahead = mapStateT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition

-- | In the default implementation, 'lookahead' does not ignore the output
-- from the inner monad. This behavior might not be what you want.
instance (MonadBuffer m, Monoid w) => MonadBuffer (WriterT w m) where
  popChar = lift popChar
  lookahead = mapWriterT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition

-- | Implementation of MonadBuffer based on StateT.
newtype CursorT c m a = CursorT {getCursorT :: StateT c m a}

-- | Maps both the main result and the cursor.
mapCursorT :: (m (a, c) -> n (b, c)) -> CursorT c m a -> CursorT c n b
mapCursorT f = CursorT . mapStateT f . getCursorT

-- | Executes the CursorT computation to get the final result and cursor in
-- the inner monad.
runCursorT :: CursorT c m a -> c -> m (a, c)
runCursorT = runStateT . getCursorT

-- | Like 'runCursorT', but returns the final result only.
evalCursorT :: Functor m => CursorT c m a -> c -> m a
evalCursorT = fmap (fmap fst) . runCursorT

-- | Like 'runCursorT', but returns the final cursor only.
execCursorT :: Functor m => CursorT c m a -> c -> m c
execCursorT = fmap (fmap snd) . runCursorT

-- | Executes the CursorT computation with the cursor modified by the given
-- function.
withCursorT :: (c -> c) -> CursorT c m a -> CursorT c m a
withCursorT f = CursorT . withStateT f . getCursorT

instance MonadTrans (CursorT c) where
  lift = CursorT . lift

instance Functor m => Functor (CursorT c m) where
  fmap f = CursorT . fmap f . getCursorT
  a <$ CursorT b = CursorT (a <$ b)

instance Monad m => Applicative (CursorT c m) where
  pure = CursorT . pure
  CursorT a <*> CursorT b = CursorT (a <*> b)
  CursorT a  *> CursorT b = CursorT (a  *> b)
  CursorT a <*  CursorT b = CursorT (a <*  b)

instance MonadPlus m => Alternative (CursorT c m) where
  empty = CursorT empty
  CursorT a <|> CursorT b = CursorT (a <|> b)
  some = CursorT . some . getCursorT
  many = CursorT . many . getCursorT

instance Monad m => Monad (CursorT c m) where
  CursorT a >>= f = CursorT (a >>= getCursorT . f)
  CursorT a >> CursorT b = CursorT (a >> b)

instance MonadPlus m => MonadPlus (CursorT c m) where
  mzero = CursorT mzero
  mplus (CursorT a) (CursorT b) = CursorT (mplus a b)

instance MonadError e m => MonadError e (CursorT c m) where
  throwError = CursorT . throwError
  catchError (CursorT a) f = CursorT (catchError a (getCursorT . f))

instance MonadInput c m => MonadBuffer (CursorT c m) where
  popChar = CursorT $ do
    c <- get
    r <- readAt c
    case r of
      Left p -> return (Left p)
      Right (c', pc) -> do
        put c'
        return (Right pc)
  lookahead (CursorT m) = CursorT $ do
    c <- get
    r <- m
    put c
    return r
  peekChar = CursorT $ do
    c <- get
    r <- readAt c
    return $ case r of
      Left p -> Left p
      Right (_, pc) -> Right pc
  currentPosition = CursorT $ get >>= positionAt

-- | State monad of PositionedString as a MonadBuffer instance.
newtype PositionedStringT m a =
  PositionedStringT {runPositionedStringT :: StateT PositionedString m a}

-- | Maps both the main result and the pending input.
mapPositionedStringT :: (m (a, PositionedString) -> n (b, PositionedString))
                     -> PositionedStringT m a -> PositionedStringT n b
mapPositionedStringT f =
  PositionedStringT . mapStateT f . runPositionedStringT

instance MonadTrans PositionedStringT where
  lift = PositionedStringT . lift

instance Functor m => Functor (PositionedStringT m) where
  fmap f = PositionedStringT . fmap f . runPositionedStringT
  a <$ PositionedStringT b = PositionedStringT (a <$ b)

instance Monad m => Applicative (PositionedStringT m) where
  pure = PositionedStringT . pure
  PositionedStringT a <*> PositionedStringT b = PositionedStringT (a <*> b)
  PositionedStringT a  *> PositionedStringT b = PositionedStringT (a  *> b)
  PositionedStringT a <*  PositionedStringT b = PositionedStringT (a <*  b)

instance MonadPlus m => Alternative (PositionedStringT m) where
  empty = PositionedStringT empty
  PositionedStringT a <|> PositionedStringT b = PositionedStringT (a <|> b)
  some = PositionedStringT . some . runPositionedStringT
  many = PositionedStringT . many . runPositionedStringT

instance Monad m => Monad (PositionedStringT m) where
  PositionedStringT a >>= f =
    PositionedStringT (a >>= runPositionedStringT . f)
  PositionedStringT a >> PositionedStringT b = PositionedStringT (a >> b)

instance MonadPlus m => MonadPlus (PositionedStringT m) where
  mzero = PositionedStringT mzero
  mplus (PositionedStringT a) (PositionedStringT b) =
    PositionedStringT (mplus a b)

instance Monad m => MonadBuffer (PositionedStringT m) where
  popChar = PositionedStringT $ do
    cs <- get
    case cs of
      Nil p -> return (Left p)
      c :~ cs' -> do
        put cs'
        return (Right c)

  lookahead (PositionedStringT m) = PositionedStringT $ do
    savedstate <- get
    result <- m
    put savedstate
    return result

  peekChar = PositionedStringT $ do
    cs <- get
    return $ case cs of
      Nil p -> Left p
      c :~ _ -> Right c

  currentPosition = PositionedStringT $ headPosition <$> get

instance MonadError e m => MonadError e (PositionedStringT m) where
  throwError = PositionedStringT . throwError
  catchError (PositionedStringT a) f =
    PositionedStringT (catchError a (runPositionedStringT . f))

instance MonadReader r m => MonadReader r (PositionedStringT m) where
  ask = lift ask
  local f = mapPositionedStringT $ local f
  reader = lift . reader

-- | Extension of MonadBuffer that provides access to input characters that
-- have been read.
class MonadBuffer m => MonadRecord m where
  -- | Reverse list of characters that have already been returned by 'popChar'
  -- so far. The list includes characters that have been inserted by
  -- 'maybeReparse' and then popped by 'popChar'.
  reverseConsumedChars :: m [Positioned Char]

instance MonadRecord m => MonadRecord (ExceptT e m) where
  reverseConsumedChars = lift reverseConsumedChars

instance MonadRecord m => MonadRecord (MaybeT m) where
  reverseConsumedChars = lift reverseConsumedChars

instance MonadRecord m => MonadRecord (ReaderT e m) where
  reverseConsumedChars = lift reverseConsumedChars

instance MonadRecord m => MonadRecord (StateT a m) where
  reverseConsumedChars = lift reverseConsumedChars

instance (MonadRecord m, Monoid w) => MonadRecord (WriterT w m) where
  reverseConsumedChars = lift reverseConsumedChars

-- | Implementation of MonadRecord based on the state monad.
newtype RecordT m a = RecordT {getRecordT :: StateT [Positioned Char] m a}

-- | Runs the record moand, returning the main result and the consumed input.
runRecordT :: RecordT m a -> m (a, [Positioned Char])
runRecordT = flip runStateT [] . getRecordT

-- | Runs the record monad, returning the main result only.
evalRecordT :: Functor m => RecordT m a -> m a
evalRecordT = fmap fst . runRecordT

-- | Maps both the main result and the consumed input.
mapRecordT :: (m (a, [Positioned Char]) -> n (b, [Positioned Char]))
           -> RecordT m a -> RecordT n b
mapRecordT f = RecordT . mapStateT f . getRecordT

instance (Monad m, Eq (m a)) => Eq (RecordT m a) where
  RecordT m == RecordT n = evalStateT m [] == evalStateT n []

instance (Monad m, Show (m a)) => Show (RecordT m a) where
  showsPrec n (RecordT m) = showsPrec n $ evalStateT m []

instance MonadTrans RecordT where
  lift = RecordT . lift

instance Functor m => Functor (RecordT m) where
  fmap f = RecordT . fmap f . getRecordT
  a <$ RecordT b = RecordT (a <$ b)

instance Monad m => Applicative (RecordT m) where
  pure = RecordT . pure
  RecordT a <*> RecordT b = RecordT (a <*> b)
  RecordT a  *> RecordT b = RecordT (a  *> b)
  RecordT a <*  RecordT b = RecordT (a <*  b)

instance MonadPlus m => Alternative (RecordT m) where
  empty = RecordT empty
  RecordT a <|> RecordT b = RecordT (a <|> b)
  some = RecordT . some . getRecordT
  many = RecordT . many . getRecordT

instance Monad m => Monad (RecordT m) where
  RecordT a >>= f = RecordT (a >>= getRecordT . f)
  RecordT a >> RecordT b = RecordT (a >> b)

instance MonadPlus m => MonadPlus (RecordT m) where
  mzero = RecordT mzero
  mplus (RecordT a) (RecordT b) = RecordT (mplus a b)

instance MonadBuffer m => MonadBuffer (RecordT m) where
  popChar = RecordT $ do
    eofOrChar <- popChar
    case eofOrChar of
      Left _ -> pure ()
      Right pc -> modify' (pc:)
    return eofOrChar
  lookahead (RecordT m) = RecordT $ do
    s <- get
    r <- lookahead m
    put s
    return r
  peekChar = lift peekChar
  currentPosition = lift currentPosition

instance MonadBuffer m => MonadRecord (RecordT m) where
  reverseConsumedChars = RecordT get

instance MonadError e m => MonadError e (RecordT m) where
  throwError = RecordT . throwError
  catchError (RecordT a) f = RecordT (catchError a (getRecordT . f))

instance MonadReader r m => MonadReader r (RecordT m) where
  ask = lift ask
  local f = mapRecordT $ local f
  reader = lift . reader

-- vim: set et sw=2 sts=2 tw=78:
