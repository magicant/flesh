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

This module defines fundamental types to handle aliases.
-}
module Flesh.Language.Alias.Core (
  Definition(..)) where

import qualified Data.Text as T

-- | Alias definition.
--
-- Type parameter @p@ should be assigned the position type. The type is not
-- directly named here in order to avoid recursive module definition.
data Definition p = Definition {
    name :: T.Text,
    value :: T.Text,
    -- | Position of the alias built-in that defined this alias.
    position :: p}
  deriving (Eq, Show)

-- vim: set et sw=2 sts=2 tw=78:
