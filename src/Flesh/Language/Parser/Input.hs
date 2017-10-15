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

This module defines types and functions for reading input for the syntax
parser.
-}
module Flesh.Language.Parser.Input (
  -- * MonadInput
  MonadInput(..), followedBy,
  -- * MonadInputRecord
  MonadInputRecord(..), RecordT(..), runRecordT, evalRecordT, mapRecordT)
  where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad (MonadPlus, mplus, mzero, void)
import Control.Monad.Except (
  ExceptT, MonadError, catchError, mapExceptT, throwError)
import Control.Monad.Reader (
  MonadReader, ReaderT, ask, local, mapReaderT, reader)
import Control.Monad.State.Strict (
  StateT, evalStateT, get, mapStateT, modify', put, runStateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Writer.Strict (WriterT, mapWriterT)
import Flesh.Source.Position

-- | Monad for character input operations.
--
-- Input depends on an internal state of the monad. The state must contain
-- information that is necessary to track the current input position, which
-- determines the next character to be read. The state must be updated as
-- characters are read and the position is advanced.
--
-- The 'lookahead' function may be used to rewind the position after some
-- input is read. In this function, the state must be restored to indicate the
-- position before the characters were read. Thereafter the same characters
-- must be returned in subsequent read operations.
--
-- Reading a character may be an operation with a side effect on an underlying
-- (typically external) input source. Such side effects must be encoded in the
-- monad. When re-reading characters after the position was rewound, however,
-- characters must be read without side effects. In other words, reading
-- operations must be idempotent in terms of side effects.
class Monad m => MonadInput m where
  -- | Reads one character at the current position, advancing the position to
  -- the next character. If the current position is end-of-input, the position
  -- must not be changed and @Left position@ is returned.
  --
  -- 'popChar' may have a side effect of reading from an underlying input
  -- source.
  popChar :: m (Either Position (Positioned Char))

  -- | Returns the result of the given monad but cancels any position update
  -- that have occurred in the monad, i.e., the position is rewound to the
  -- original.
  --
  -- Note that any side effects that occur in the monad cannot be canceled by
  -- 'lookahead'.
  lookahead :: m a -> m a

  -- | Returns the character at the current position without advancing the
  -- position. The default implementation is @lookahead popChar@.
  --
  -- 'peekChar' may have a side effect of reading from an underlying input
  -- source.
  peekChar :: m (Either Position (Positioned Char))
  peekChar = lookahead popChar

  -- | Returns the current position.
  -- The default implementation is @either id fst <$> peekChar@.
  --
  -- 'currentPosition' must not have any side effect on an underlying input
  -- source, which means the default implementation is not applicable if
  -- 'peekChar' has a side effect on an underlying input source.
  currentPosition :: m Position
  currentPosition = either id fst <$> peekChar

  -- | Pushes the given characters into the current position. Subsequent reads
  -- must first return the inserted characters and then return to the original
  -- position, continuing to characters that would have been immediately read
  -- if 'pushChars' was not used.
  --
  -- 'pushChars' must not have any side effect on an underlying input source.
  pushChars :: [Positioned Char] -> m ()

-- | Like 'lookahead', but ignores the result.
followedBy :: MonadInput m => m a -> m ()
followedBy = void . lookahead

-- This would result in undecidable instance.
-- instance (Monad m, MonadState PositionedString m) => MonadInput m where
instance Monad m => MonadInput (StateT PositionedString m) where
  popChar = do
    cs <- get
    case cs of
      Nil p -> return (Left p)
      c :~ cs' -> do
        put cs'
        return (Right c)

  lookahead m = do
    savedstate <- get
    result <- m
    put savedstate
    return result

  peekChar = do
    cs <- get
    return $ case cs of
      Nil p -> Left p
      c :~ _ -> Right c

  currentPosition = headPosition <$> get

  pushChars [] = return ()
  pushChars (c:cs) = do
    pushChars cs
    modify' (c :~)

instance MonadInput m => MonadInput (ExceptT e m) where
  popChar = lift popChar
  lookahead = mapExceptT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  pushChars = lift . pushChars

instance MonadInput m => MonadInput (MaybeT m) where
  popChar = lift popChar
  lookahead = mapMaybeT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  pushChars = lift . pushChars

instance MonadInput m => MonadInput (ReaderT e m) where
  popChar = lift popChar
  lookahead = mapReaderT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  pushChars = lift . pushChars

-- instance MonadInput m => MonadInput (StateT s m) where
-- FIXME conflicts with: MonadInput (StateT PositionedString m)
instance MonadInput m => MonadInput (StateT [a] m) where
  popChar = lift popChar
  lookahead = mapStateT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  pushChars = lift . pushChars

instance (MonadInput m, Monoid w) => MonadInput (WriterT w m) where
  popChar = lift popChar
  lookahead = mapWriterT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  pushChars = lift . pushChars

-- | Extension of MonadInput that provides access to input characters that
-- have been read.
class MonadInput m => MonadInputRecord m where
  -- | Reverse list of characters that have already been returned by 'popChar'
  -- so far. The list includes characters that have been pushed by 'pushChars'
  -- and then popped by 'popChar'.
  reverseConsumedChars :: m [Positioned Char]

instance MonadInputRecord m => MonadInputRecord (ExceptT e m) where
  reverseConsumedChars = lift reverseConsumedChars

instance MonadInputRecord m => MonadInputRecord (MaybeT m) where
  reverseConsumedChars = lift reverseConsumedChars

instance MonadInputRecord m => MonadInputRecord (ReaderT e m) where
  reverseConsumedChars = lift reverseConsumedChars

-- FIXME: See MonadInput (StateT [a] m) above
instance MonadInputRecord m => MonadInputRecord (StateT [a] m) where
  reverseConsumedChars = lift reverseConsumedChars
instance MonadInputRecord m
    => MonadInputRecord (StateT PositionedString m) where
  reverseConsumedChars = lift reverseConsumedChars

instance (MonadInputRecord m, Monoid w)
    => MonadInputRecord (WriterT w m) where
  reverseConsumedChars = lift reverseConsumedChars

-- | Implementation of MonadInputRecord based on the state monad.
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

instance MonadInput m => MonadInput (RecordT m) where
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
  pushChars = lift . pushChars

instance MonadInput m => MonadInputRecord (RecordT m) where
  reverseConsumedChars = RecordT get

instance MonadError e m => MonadError e (RecordT m) where
  throwError = RecordT . throwError
  catchError (RecordT a) f = RecordT (catchError a (getRecordT . f))

instance MonadReader r m => MonadReader r (RecordT m) where
  ask = lift ask
  local f = mapRecordT $ local f
  reader = lift . reader

-- vim: set et sw=2 sts=2 tw=78:
