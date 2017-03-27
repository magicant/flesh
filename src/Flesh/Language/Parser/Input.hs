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
  InputT(..), prepend,
  PositionT, MonadPosition(..)) where

import Flesh.Source.Position
import Control.Monad.Reader
import Control.Monad.State.Strict

-- | Monad for reading input.
--
-- The monad should simply return a single position (denoting the end of
-- input) or a pair of a positioned character and next input monad.
--
-- The monad may have side effects, but the same monad instance must yield the
-- same result for repeated reads. That is, the monad must be idempotent.
newtype InputT m = InputT (m (Either Position (Positioned Char, InputT m)))

-- | @prepend cs m@ returns an input monad that first yields the elements of
-- @cs@ and then @m@.
prepend :: Monad m => [Positioned Char] -> InputT m -> InputT m
prepend [] m = m
prepend (c:cs) m = InputT (return (Right (c, prepend cs m)))

-- | Monad transformer that manages input position as a state.
type PositionT m = StateT (InputT m)

-- | Extension of 'MonadState' that allows character input operations.
class MonadState (InputT n) m => MonadPosition n m where
  -- | Returns the character at the current position without advancing the
  -- position.
  peekChar :: m (Either Position (Positioned Char))
  -- | Reads one character at the current position and advancing the position
  -- to the next character.
  popChar :: m (Either Position (Positioned Char))
  -- | Returns the result of the given parser without advancing the input
  -- position.
  followedBy :: m a -> m a

instance Monad m => MonadPosition m (StateT (InputT m) m) where
  peekChar = do
    InputT i <- get
    lift (fmap (fmap fst) i)
  popChar = do
    InputT i1 <- get
    e <- lift i1
    case e of
      Left p -> return (Left p)
      Right (c, i2) -> do
        put i2
        return (Right c)
  followedBy m = do
    i <- get
    a <- m
    put i
    return a

instance MonadPosition n m => MonadPosition n (ReaderT r m) where
  peekChar = lift peekChar
  popChar = lift popChar
  followedBy = mapReaderT followedBy

-- vim: set et sw=2 sts=2 tw=78:
