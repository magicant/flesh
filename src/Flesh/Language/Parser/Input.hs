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

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : non-portable (GHC language extensions)

This module defines types and functions for reading input for the syntax
parser.
-}
module Flesh.Language.Parser.Input (
  MonadInput(..), followedBy) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.State.Strict (StateT, get, modify', put)
import Control.Monad.Trans.Class (lift)
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

instance (MonadInput m, Monoid w) => MonadInput (WriterT w m) where
  popChar = lift popChar
  lookahead = mapWriterT lookahead
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  pushChars = lift . pushChars

-- vim: set et sw=2 sts=2 tw=78:
