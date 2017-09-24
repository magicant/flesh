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

This module defines warnings and monads to produce them.
-}
module Flesh.Language.Parser.Warning (
  -- * Warning
  Warning(..),
  -- * Monad for recording warnings
  ReportT) where

import Control.Monad.Writer.Strict (WriterT)

-- | Warning produced during source code parsing.
data Warning = Warning -- FIXME

-- | Writer monad specialized for recording warnings.
type ReportT = WriterT [Warning]

-- vim: set et sw=2 sts=2 tw=78:
