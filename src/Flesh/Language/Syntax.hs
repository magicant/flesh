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

This module defines the abstract syntax tree of the shell language.
-}
module Flesh.Language.Syntax (
  -- * Tokens
  DoubleQuoteUnit(..),
  WordUnit(..),
  EWord(..), wordUnits, wordText,
  Token(..), tokenUnits, tokenWord, tokenText,
  Assignment(..),
  -- * Redirections
  HereDocOp(..), Redirection(..), fd,
  -- * Syntax
  Command(..), Pipeline(..), AndOrCondition(..), ConditionalPipeline(..),
  AndOrList(..)) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Flesh.Source.Position as P
import Numeric.Natural

-- Utility for Show instances
showSpace :: ShowS
showSpace = showChar ' '

-- | Element of double quotes.
data DoubleQuoteUnit =
    -- | Single bear character.
    Char Char
    -- | Character escaped by a backslash.
    | Backslashed Char
    -- | Parameter expansion.
    | Parameter -- FIXME
    | CommandSubstitution -- FIXME of the $(...) form
    | Backquoted -- FIXME command substitution
    | Arithmetic -- FIXME
  deriving (Eq)

instance Show DoubleQuoteUnit where
  showsPrec _ (Char c) = showChar c
  showsPrec _ (Backslashed c) = \s -> '\\':c:s
  showsPrec _ Parameter = id
  showsPrec _ CommandSubstitution = id
  showsPrec _ Backquoted = id
  showsPrec _ Arithmetic = id
  -- | Just joins the given units, without enclosing double quotes.
  showList [] = id
  showList (u:us) = shows u . showList us

-- | Element of words.
data WordUnit =
    -- | Unquoted double-quote unit as a word unit.
    Unquoted DoubleQuoteUnit
    -- | Double-quote.
    | DoubleQuote [P.Positioned DoubleQuoteUnit]
    -- | Single-quote.
    | SingleQuote [P.Positioned Char]
  deriving (Eq)

instance Show WordUnit where
  showsPrec n (Unquoted unit) = showsPrec n unit
  showsPrec n (DoubleQuote units) =
    showChar '"' . showsPrec n (snd (unzip units)) . showChar '"'
  showsPrec _ (SingleQuote chars) =
    showChar '\'' . (\s -> foldr (showChar . snd) s chars) . showChar '\''
  showList [] = id
  showList (u:us) = shows u . showList us

-- | Expandable word, a possibly empty list of word units.
newtype EWord = EWord [P.Positioned WordUnit]
  deriving (Eq)

-- | Returns the content of a word.
wordUnits :: EWord -> [P.Positioned WordUnit]
wordUnits (EWord us) = us

-- | If the given word consists of constant unquoted characters only, then
-- returns the content as a text.
wordText :: EWord -> Maybe (T.Text)
wordText us = fmap T.pack $ sequenceA $ fmap (constChar . snd) $ wordUnits us
  where constChar (Unquoted (Char c)) = Just c
        constChar _ = Nothing

instance Show EWord where
  showsPrec n (EWord us) s = foldr (showsPrec n . snd) s us
  showList [] = id
  showList [w] = shows w
  showList (w:ws) = shows w . showSpace . showList ws

-- | Non-empty word, defined as a (lexical) token with the token identifier
-- @TOKEN@ in POSIX.
newtype Token = Token (NonEmpty (P.Positioned WordUnit))
  deriving (Eq)

-- | Returns the content of a token.
tokenUnits :: Token -> NonEmpty (P.Positioned WordUnit)
tokenUnits (Token us) = us

-- | Converts a token to a word.
tokenWord :: Token -> EWord
tokenWord = EWord . NE.toList . tokenUnits

-- | If the given token consists of constant unquoted characters only, then
-- returns the content as a text.
tokenText :: Token -> Maybe (T.Text)
tokenText = wordText . tokenWord

instance Show Token where
  showsPrec n t = showsPrec n (tokenWord t)
  showList ts = showList (fmap tokenWord ts)

-- | Assignment.
data Assignment = Assignment () -- FIXME
  deriving (Eq)

instance Show Assignment where
  show _ = "" -- FIXME

-- | Here document redirection operator.
data HereDocOp = HereDocOp {
  hereDocOpPos :: P.Position,
  hereDocFd :: Natural,
  isTabbed :: Bool,
  delimiter :: Token}
  deriving (Eq)

instance Show HereDocOp where
  showsPrec n o =
    showsPrec n (hereDocFd o) . showString s . showsPrec n (delimiter o)
    where s = if isTabbed o then "<<-" else "<<"

-- | Redirection.
data Redirection =
  FileRedirection {
    fileFd :: Natural} -- FIXME
  | HereDoc {
    hereDocOp :: HereDocOp,
    content :: EWord}
  deriving (Eq)

instance Show Redirection where
  showsPrec n (FileRedirection fd') = showsPrec n fd' -- FIXME
  showsPrec n (HereDoc o _) = showsPrec n o -- content is ignored
  showList [] = id
  showList [r] = shows r
  showList (r:rs) = shows r . showSpace . showList rs

-- | Returns the target file descriptor of the given redirection.
fd :: Redirection -> Natural
fd (FileRedirection fd') = fd'
fd (HereDoc (HereDocOp _ fd' _ _) _) = fd'

-- | Element of pipelines.
data Command =
  -- | Simple command.
  SimpleCommand [Token] [P.Positioned Assignment] [Redirection]
  -- FIXME Compound commands
  -- | Function definition.
  | FunctionDefinition -- FIXME
  deriving (Eq)

instance Show Command where
  showsPrec _ (SimpleCommand [] [] []) = id
  showsPrec _ (SimpleCommand ts [] []) = showList ts
  showsPrec _ (SimpleCommand [] as []) = showList as'
    where as' = snd <$> as
  showsPrec _ (SimpleCommand [] [] rs) = showList rs
  showsPrec _ (SimpleCommand ts [] rs) = showList ts . showSpace . showList rs
  showsPrec n (SimpleCommand ts as rs) =
    showList as' . showSpace . showsPrec n (SimpleCommand ts [] rs)
    where as' = snd <$> as
  showsPrec _ FunctionDefinition = id -- FIXME
  showList [] = id
  showList [c] = shows c
  showList (c:cs) = shows c . showString "; " . showList cs
  -- TOOD remove showList definition when no longer needed

-- | Element of and-or lists. Optionally negated sequence of one or more
-- commands.
data Pipeline = Pipeline {
  pipeCommands :: NonEmpty Command,
  isNegated :: Bool}
  deriving (Eq)

instance Show Pipeline where
  showsPrec n (Pipeline cs True) =
    showString "! " . showsPrec n (Pipeline cs False)
  showsPrec n (Pipeline (h :| t) False) = showsPrec n h . ft
      where ft s = foldr step s t
            step c = showString " | " . shows c
  showList [] = id
  showList [p] = shows p
  showList (p:ps) = shows p . showString "; " . showList ps

-- | Condition that determines if a pipeline should be executed in an and-or
-- list.
data AndOrCondition = AndThen | OrElse
  deriving (Eq)

instance Show AndOrCondition where
  show AndThen = "&&"
  show OrElse = "||"

-- | Pipeline executed conditionally in an and-or list.
newtype ConditionalPipeline = ConditionalPipeline (AndOrCondition, Pipeline)
  deriving (Eq)

instance Show ConditionalPipeline where
  showsPrec n (ConditionalPipeline (c, p)) =
    showsPrec n c . showSpace . showsPrec n p
  showList [] = id
  showList [p] = shows p
  showList (p:ps) = shows p . showSpace . showList ps

-- | One or more pipelines executed conditionally in sequence. The entire
-- sequence can be executed either synchronously or asynchronously.
data AndOrList = AndOrList {
  andOrHead :: Pipeline,
  andOrTail :: [ConditionalPipeline],
  isAsynchronous :: Bool}
  deriving (Eq)

showAndOrHeadTail :: Pipeline -> [ConditionalPipeline] -> ShowS
showAndOrHeadTail h [] = shows h
showAndOrHeadTail h t = shows h . showSpace . showList t

instance Show AndOrList where
  showsPrec n (AndOrList h t False) | n <= 0 = showAndOrHeadTail h t
  showsPrec _ (AndOrList h t isAsync) = showAndOrHeadTail h t . showChar c
    where c = if isAsync then '&' else ';'

-- vim: set et sw=2 sts=2 tw=78:
