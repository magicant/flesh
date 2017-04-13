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
  backslashed, doubleQuoteUnit, doubleQuote, singleQuote, wordUnit, tokenTill)
  where

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

-- | Parses a backslash-escaped character that is parsed by the given parser.
backslashed :: (MonadInput m, MonadError (Severity, Error) m)
            => m (Positioned Char) -> m (Positioned DoubleQuoteUnit)
backslashed m = char '\\' *> fmap (fmap Backslashed) m

-- | Parses a double-quote unit, possibly preceded by line continuations.
--
-- The argument parser is used to parse a backslashed character.
doubleQuoteUnit' :: (Alternative m, MonadInput m,
                     MonadError (Severity, Error) m)
  => m (Positioned Char) -> m (Positioned DoubleQuoteUnit)
doubleQuoteUnit' c = lc $ -- TODO parse expansions
  backslashed c <|> fmap (fmap Char) anyChar

-- | Parses a double-quote unit, possibly preceded by line continuations.
doubleQuoteUnit :: (Alternative m, MonadInput m,
                    MonadError (Severity, Error) m)
  => m (Positioned DoubleQuoteUnit)
doubleQuoteUnit = doubleQuoteUnit' (oneOfChars "\\\"$`")

-- | Parses a pair of double quotes containing any number of double-quote
-- units.
doubleQuote :: (Alternative m, MonadInput m, MonadError (Severity, Error) m)
            => m (Positioned WordUnit)
doubleQuote = do
  let dq = lc (char '"')
  (p, _) <- dq
  let f units = (p, DoubleQuote units)
      closeQuote = setReason UnclosedDoubleQuote dq
  require $ f <$> doubleQuoteUnit `manyTill` closeQuote

-- | Parses a pair of single quotes containing any number of characters.
singleQuote :: (Alternative m, MonadInput m, MonadError (Severity, Error) m)
            => m (Positioned WordUnit)
singleQuote = do
  let sq = char '\''
  (p, _) <- lc sq
  let f chars = (p, SingleQuote chars)
      closeQuote = setReason UnclosedSingleQuote (char '\'')
  require $ f <$> anyChar `manyTill` closeQuote

-- | Parses a word unit.
wordUnit :: (Alternative m, MonadInput m, MonadError (Severity, Error) m)
         => m (Positioned WordUnit)
wordUnit = lc $
  doubleQuote <|> singleQuote <|>
    fmap (fmap Unquoted) (doubleQuoteUnit' anyChar)

-- | @tokenTill end@ parses a token, or non-empty word, until @end@ occurs.
--
-- Note that @end@ consumes the input. Use @'followedBy' end@ to keep @end@
-- unconsumed.
tokenTill :: (Alternative m, MonadInput m, MonadError (Severity, Error) m)
          => m a -> m Token
tokenTill a = notFollowedBy a >> (require $ Token <$> wordUnit `someTill` a)

-- vim: set et sw=2 sts=2 tw=78:
