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

This module defines utilities for lexical parsing that are specific to the
shell language.
-}
module Flesh.Language.Parser.Lex (
  lineContinuation, lc, blank, digit, comment, whites, operatorStarter,
  endOfToken, anyOperator, operator, operatorToken, redirectOperatorToken,
  ioNumber,
  -- * Reserved words
  reservedBang, reservedCase, reservedDo, reservedDone, reservedElif,
  reservedElse, reservedEsac, reservedFi, reservedFor, reservedFunction,
  reservedIf, reservedIn, reservedThen, reservedUntil, reservedWhile,
  reservedOpenBrace, reservedCloseBrace, isReserved,
  -- * Token identification
  IdentifiedToken(..), identify) where

import Control.Applicative (many, optional, (<|>))
import Control.Monad.Reader
import Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set, fromList, member)
import Data.Text (Text, pack)
import Flesh.Data.Char (isBlank, isDigit)
import Flesh.Source.Position
import qualified Flesh.Language.Alias as Alias
import Flesh.Language.Parser.Alias hiding (value)
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Language.Syntax hiding (Assignment)
import qualified Flesh.Language.Syntax as S
import Numeric.Natural (Natural)

-- | Parses a line continuation: a backslash followed by a newline.
lineContinuation :: MonadParser m => m Position
lineContinuation = fst . head <$> string "\\\n"

-- | @lc m@ parses @m@ optionally preceded by any number of line
-- continuations.
lc :: MonadParser m => m a -> m a
lc m = many lineContinuation *> m

blank' :: MonadParser m => m (Positioned Char)
blank' = satisfy isBlank

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
whites = many blank *> optional comment

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
  (p, c1) <- operatorStarter
  case c1 of
    ';' -> lc $ ((p, ";;") <$ char ';') <|> return (p, ";")
    '|' -> lc $ ((p, "||") <$ char '|') <|> return (p, "|")
    '&' -> lc $ ((p, "&&") <$ char '&') <|> return (p, "&")
    '<' -> do (_, c2) <- lc $ oneOfChars "<>&"
              case c2 of
                '<' -> lc $ ((p, "<<-") <$ char '-') <|> return (p, "<<")
                _ -> return (p, [c1, c2]) 
           <|> return (p, [c1])
    '>' -> do (_, c2) <- lc $ oneOfChars ">|&"
              return (p, [c1, c2])
           <|> return (p, ">")
    _ -> return (p, [c1])

-- | Parses the given single operator, possibly including line continuations.
-- The argument operator must be one of the operators parsed by 'anyOperator'.
operator :: MonadParser m => String -> m (Positioned String)
operator o = anyOperator `satisfyingP` (o ==)

-- | Parses the given single 'operator' and optional trailing 'whites'.
-- The argument operator must be one of the operators parsed by 'anyOperator'.
operatorToken :: MonadParser m => String -> m (Positioned String)
operatorToken o = operator o <* whites

-- | Parses a single direction 'operator', possibly including line
-- continuations, and optional trailing 'whites'.
redirectOperatorToken :: MonadParser m => m (Positioned String)
redirectOperatorToken = anyOperator `satisfyingP` isRedirect <* whites
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

-- | Reserved word text constant.
reservedBang, reservedCase, reservedDo, reservedDone, reservedElif,
  reservedElse, reservedEsac, reservedFi, reservedFor, reservedFunction,
  reservedIf, reservedIn, reservedThen, reservedUntil, reservedWhile,
  reservedOpenBrace, reservedCloseBrace :: Text
reservedBang       = pack "!"
reservedCase       = pack "case"
reservedDo         = pack "do"
reservedDone       = pack "done"
reservedElif       = pack "elif"
reservedElse       = pack "else"
reservedEsac       = pack "esac"
reservedFi         = pack "fi"
reservedFor        = pack "for"
reservedFunction   = pack "function"
reservedIf         = pack "if"
reservedIn         = pack "in"
reservedThen       = pack "then"
reservedUntil      = pack "until"
reservedWhile      = pack "while"
reservedOpenBrace  = pack "{"
reservedCloseBrace = pack "}"

-- | Set of all the reserved words.
reservedWords :: Set Text
reservedWords = fromList [reservedBang, reservedCase, reservedDo,
  reservedDone, reservedElif, reservedElse, reservedEsac, reservedFi,
  reservedFor, reservedFunction, reservedIf, reservedIn, reservedThen,
  reservedUntil, reservedWhile, reservedOpenBrace, reservedCloseBrace]

-- | Tests if the argument text is a reserved word token.
isReserved :: Text -> Bool
isReserved t = member t reservedWords

-- | Result of token identification.
data IdentifiedToken =
  Reserved Text -- ^ reserved word
  | Assignment S.Assignment -- ^ assignment word
  | Normal Token -- ^ normal word token
  deriving (Eq, Show)

assignmentOrNormal :: Token -> IdentifiedToken
assignmentOrNormal t@(Token us) =
  let isEqual = (== Unquoted (Char '=')) . snd
   in case break isEqual (NE.toList us) of
        (nameH : nameT, _ : value) -> Assignment $
          S.Assignment (Token (nameH NE.:| nameT)) (EWord value)
        _ -> Normal t

-- | @identify isReserved' isAliasable p t@ identifies a token.
--
-- First, if the token is a simple text and @isReserved'@ returns True for it,
-- then it is identified as Reserved.
--
-- Next, if @isAliasable@ is true, the token is tested for an alias. If it
-- matches a valid alias, its value is returned. (This is the only case where
-- 'fst' of the result is not Nothing. 'snd' has an identified token as if it
-- were not an alias.)
--
-- If @isAssignable@ is true, the token is tested for an assignment. If it
-- contains an equal sign that precedes a non-empty name, it is identified as
-- an assignment.
--
-- Otherwise, the token is identified as Normal.
identify :: (MonadParser m, MonadReader Alias.DefinitionSet m)
         => (Text -> Bool) -- ^ function that tests if a token is reserved
         -> Bool -- ^ whether the token should be checked for an alias
         -> Bool -- ^ whether the token should be checked for an assignment
         -> Position -- ^ position of the token to be identified
         -> Token -- ^ token to be identified
         -> m (Maybe [Positioned Char], IdentifiedToken)
identify isReserved' isAliasable isAssignable p t =
  case tokenText t of
    Nothing -> aon Nothing
    Just tt | isReserved' tt -> return (Nothing, Reserved tt)
            | otherwise -> do
                a <- runMaybeT $ do
                  guard isAliasable
                  maybeAliasValue p tt
                aon a
    where aon a = return (a, aon' t)
          aon' = if isAssignable then assignmentOrNormal else Normal
-- TODO support global aliases, possibly extending the @isAliasable@ argument

-- vim: set et sw=2 sts=2 tw=78:
