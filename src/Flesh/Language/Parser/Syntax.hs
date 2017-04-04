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
{-# LANGUAGE Safe #-}

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : non-portable (flexible contexts)

Collection of parser monads that take some input and return abstract syntax
tree, error, and warnings.
-}
module Flesh.Language.Parser.Syntax (
  module Flesh.Language.Syntax,
  lineContinuation, lc) where

import Control.Applicative
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Language.Syntax
import Flesh.Source.Position

-- | Parses a line continuation: a backslash followed by a newline.
lineContinuation :: (MonadInput m, MonadError (Severity, Error) m)
                 => m Position
lineContinuation = fst . head <$> string "\\\n"

-- | @lc m@ parses @m@ optionally preceded by any number of line
-- continuations.
lc :: (Alternative m, MonadInput m, MonadError (Severity, Error) m)
   => m a -> m a
lc m = many lineContinuation *> m

-- vim: set et sw=2 sts=2 tw=78:
