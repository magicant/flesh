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

This module defines aliases.
-}
module Flesh.Language.Alias (
  Definition, DefinitionSet) where

import qualified Flesh.Language.Alias.Core as Core
import Flesh.Source.Position

-- | Alias definition.
type Definition = Core.Definition Position

-- | Abstract type of alias definition collections.
type DefinitionSet = [Definition] -- FIXME maybe better defined with newtype?

-- vim: set et sw=2 sts=2 tw=78:
