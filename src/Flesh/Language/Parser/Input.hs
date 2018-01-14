{-
Copyright (C) 2018 WATANABE Yuki <magicant@wonderwand.net>

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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright   : (C) 2018 WATANABE Yuki
License     : GPL-2
Portability : non-portable (GHC language extensions)

This module defines types for reading input for the syntax parser.
-}
module Flesh.Language.Parser.Input (
  -- * MonadInput
  MonadInput(..)) where

import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Class (lift)
import Flesh.Source.Position

-- | Monad that provides access to the underlying input character source with
-- arbitrary backtracking. MonadInput is a low-level basis for implementing
-- MonadBuffer.
--
-- Operations on MonadInput depends on an abstract cursor of type @c@.
class Monad m => MonadInput c m | m -> c where
  -- | Reads a character at the given cursor position. If the position is an
  -- end-of-input, only the position is returned in Left. Otherwise, a pair of
  -- a cursor for the next character and the current character is returned in
  -- Right.
  --
  -- Reading a character may have a side effect on an underlying (typically
  -- external) input source. Such side effects must be encoded in the monad.
  -- The monad must remember the characters that have been read from the
  -- underlying input source. When a cursor is used more than once, the second
  -- and all succeeding read operations must return the same result without
  -- side effects.
  readAt :: c -> m (Either Position (c, Positioned Char))
  -- | Returns the position of the given cursor.
  --
  -- 'positionAt' should return the same position data as 'readAt', but must
  -- not have any side effect on the underlying input source.
  positionAt :: c -> m Position

instance MonadInput c m => MonadInput c (ExceptT e m) where
  readAt = lift . readAt
  positionAt = lift . positionAt

instance MonadInput c m => MonadInput c (StateT s m) where
  readAt = lift . readAt
  positionAt = lift . positionAt

-- vim: set et sw=2 sts=2 tw=78:
