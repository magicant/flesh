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

This module defines utilities for lexical parsing that are specific to the
shell language.
-}
module Flesh.Language.Parser.Lex (
  lineContinuation, lc, blank, digit, comment, whites, operatorStarter,
  endOfToken, anyOperator, operator, redirectOperator, ioNumber) where

import Control.Applicative
import Data.Char
import qualified Data.List.NonEmpty as NE
import Flesh.Source.Position
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Numeric.Natural

-- | Parses a line continuation: a backslash followed by a newline.
lineContinuation :: MonadParser m => m Position
lineContinuation = fst . head <$> string "\\\n"

-- | @lc m@ parses @m@ optionally preceded by any number of line
-- continuations.
lc :: MonadParser m => m a -> m a
lc m = many lineContinuation *> m

blank' :: MonadParser m => m (Positioned Char)
blank' = satisfy isBlank
  where isBlank c | ord c <= 0x7F = c == '\t' || c == ' '
                  | otherwise     = isSpace c

-- | Parses a blank character, possibly preceded by line continuations.
--
-- In this implementation, the definition of blank characters is
-- locale-independent: For ASCII characters, only the tab and space characters
-- are blank. For other characters, the categorization is solely based on the
-- Unicode general category of characters.
blank :: MonadParser m => m (Positioned Char)
blank = lc blank'

digit' :: MonadParser m => m (Positioned Char)
digit' = satisfy isDigit

-- | Parses a digit character, possibly preceded by line continuations.
digit :: MonadParser m => m (Positioned Char)
digit = lc digit'

-- | Parses a comment, possibly preceded by line continuations.
--
-- A comment starts with a @#@ sign and continues up to (but not including) a
-- newline. The returned string does not contain the initial @#@.
comment :: MonadParser m => m [Positioned Char]
comment = lc $ do
  _ <- char '#'
  anyChar `manyTill` (followedBy (char '\n') <|> followedBy eof)

-- | Parses any number of 'blank' characters possibly followed by a 'comment'.
-- Returns the result of 'comment' or 'Nothing' if no comment.
whites :: MonadParser m => m (Maybe [Positioned Char])
whites = many blank *> (Just <$> comment <|> return Nothing)

operatorStarters :: [Char]
operatorStarters = ";|&<>()"

-- | Parses a single-character operator, possibly preceded by line
-- continuations.
operatorStarter :: MonadParser m => m (Positioned Char)
operatorStarter = lc $ oneOfChars operatorStarters

-- | Succeeds before an end-of-token character. Consumes line continuations
-- but nothing else.
endOfToken :: MonadParser m => m ()
endOfToken = lc $ followedBy op <|> followedBy blank' <|> followedBy eof
  where op = oneOfChars ('\n' : operatorStarters)

-- | Parses a single operator, possibly including line continuations.
--
-- This function does /not/ parse the newline or end-of-input operator.
anyOperator :: MonadParser m => m (Positioned String)
anyOperator = do
  (p, c1) <- lc operatorStarter
  lc $ case c1 of
    ';' -> ((p, ";;") <$ char ';') <|> return (p, ";")
    '|' -> ((p, "||") <$ char '|') <|> return (p, "|")
    '&' -> ((p, "&&") <$ char '&') <|> return (p, "&")
    '<' -> do (_, c2) <- oneOfChars "<>&"
              case c2 of
                '<' -> lc $ ((p, "<<-") <$ char '-') <|> return (p, "<<")
                _ -> return (p, [c1, c2]) 
           <|> return (p, [c1])
    '>' -> do (_, c2) <- oneOfChars ">|&"
              return (p, [c1, c2])
           <|> return (p, ">")
    _ -> return (p, [c1])

-- | Parses the given single operator, possibly including line continuations.
-- The argument operator must be one of the operators parsed by 'anyOperator'.
operator :: MonadParser m => String -> m (Positioned String)
operator o = anyOperator `satisfying` (o ==)

-- | Parses a single direction operator, possibly including line
-- continuations.
redirectOperator :: MonadParser m => m (Positioned String)
redirectOperator = anyOperator `satisfying` isRedirect
  where isRedirect ('<':_) = True
        isRedirect ('>':_) = True
        isRedirect _ = False

-- | Parses an IO_NUMBER token.
ioNumber :: MonadParser m => m Natural
ioNumber = do
  ds <- some' digit
  case reads $ snd <$> NE.toList ds of
    [(n, "")] -> do
      followedBy $ lc $ oneOfChars "<>"
      return n
    _ -> failureOfPosition $ fst (NE.head ds)

-- vim: set et sw=2 sts=2 tw=78:
