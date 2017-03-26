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

{-# LANGUAGE Safe #-}

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : portable

This module defines types and functions for reading input for the syntax
parser.
-}
module Flesh.Language.Parser.Input (
  InputT(..), prepend,
  PositionT) where

import Flesh.Source.Position
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
prepend :: [Positioned Char] -> InputT m -> InputT m
prepend = undefined -- FIXME

-- | Monad transformer that manages input position as a state.
type PositionT n = StateT (InputT n)

-- vim: set et sw=2 sts=2 tw=78:
