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
  -- * Syntactic primitives
  lineContinuation, lc,
  -- * Tokens
  backslashed, doubleQuoteUnit) where

import Control.Applicative
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Language.Syntax
import Flesh.Source.Position

-- | Parses a line continuation: a backslash followed by a newline.
lineContinuation :: (MonadInput m, MonadError (Severity, Error) m)
                 => m Position
lineContinuation = try $ fst . head <$> string "\\\n"

-- | @lc m@ parses @m@ optionally preceded by any number of line
-- continuations.
lc :: (Alternative m, MonadInput m, MonadError (Severity, Error) m)
   => m a -> m a
lc m = many lineContinuation *> m

-- | Parses a backslash-escaped character that is parsed by the given parser.
backslashed :: (MonadInput m, MonadError (Severity, Error) m)
            => m (Positioned Char) -> m (Positioned DoubleQuoteUnit)
backslashed m = try (char '\\') *> fmap (fmap Backslashed) m

-- | Parses a double-quote unit, possibly preceded by line continuations.
doubleQuoteUnit :: (Alternative m, MonadInput m,
                    MonadError (Severity, Error) m)
  => m (Positioned DoubleQuoteUnit)
doubleQuoteUnit = lc $ -- TODO parse expansions
  try (backslashed (oneOfChars "\\\"$`")) <|> fmap (fmap Char) anyChar

-- vim: set et sw=2 sts=2 tw=78:
