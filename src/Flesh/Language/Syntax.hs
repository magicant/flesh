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
  DoubleQuoteUnit(..), unquoteDoubleQuoteUnit,
  WordUnit(..), unquoteWordUnit,
  EWord(..), wordUnits, wordText,
  Token(..), tokenUnits, tokenWord, tokenText, unquoteToken,
  Assignment(..),
  -- * Redirections
  HereDocOp(..), FileOp(..), Redirection(..), fd,
  -- * Syntax
  CompoundCommand(..), Command(..), Pipeline(..), AndOrCondition(..),
  ConditionalPipeline(..), AndOrList(..), showSeparatedList) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty (toList)
import Data.Text (Text, pack)
import Flesh.Source.Position (Position, Positioned)
import Numeric.Natural (Natural)

-- Utility for Show instances
showSpace :: ShowS
showSpace = showChar ' '

-- | Element of double quotes.
data DoubleQuoteUnit =
    -- | Single bear character.
    Char !Char
    -- | Character escaped by a backslash.
    | Backslashed !Char
    -- | Parameter expansion.
    | Parameter -- FIXME
    -- | @$(...)@
    | CommandSubstitution String
    | Backquoted -- FIXME command substitution
    | Arithmetic -- FIXME
  deriving (Eq)

instance Show DoubleQuoteUnit where
  showsPrec _ (Char c) = showChar c
  showsPrec _ (Backslashed c) = \s -> '\\':c:s
  showsPrec _ Parameter = id
  showsPrec _ (CommandSubstitution cs) =
    showString "$(" . showString cs . showChar ')'
  showsPrec _ Backquoted = id
  showsPrec _ Arithmetic = id
  -- | Just joins the given units, without enclosing double quotes.
  showList [] = id
  showList (u:us) = shows u . showList us

-- | Converts a backslashed character to a bare character. Other double-quote
-- units are returned intact. The Boolean is True iff conversion was
-- performed.
unquoteDoubleQuoteUnit :: DoubleQuoteUnit -> (Bool, DoubleQuoteUnit)
unquoteDoubleQuoteUnit (Backslashed c) = (True, Char c)
unquoteDoubleQuoteUnit u               = (False, u)

-- | Element of words.
data WordUnit =
    -- | Unquoted double-quote unit as a word unit.
    Unquoted !DoubleQuoteUnit
    -- | Double-quote.
    | DoubleQuote [Positioned DoubleQuoteUnit]
    -- | Single-quote.
    | SingleQuote [Positioned Char]
  deriving (Eq)

instance Show WordUnit where
  showsPrec n (Unquoted unit) = showsPrec n unit
  showsPrec n (DoubleQuote units) =
    showChar '"' . showsPrec n (snd (unzip units)) . showChar '"'
  showsPrec _ (SingleQuote chars) =
    showChar '\'' . (\s -> foldr (showChar . snd) s chars) . showChar '\''
  showList [] = id
  showList (u:us) = shows u . showList us

-- | Removes backslash escapes, double-quotes, and single-quotes without word
-- expansion. The Boolean is true iff quotation was removed.
unquoteWordUnit :: WordUnit -> (Bool, [DoubleQuoteUnit])
unquoteWordUnit (Unquoted u) = (b, [u'])
  where ~(b, u') = unquoteDoubleQuoteUnit u
unquoteWordUnit (DoubleQuote us) = (True, unq <$> us)
  where unq = snd . unquoteDoubleQuoteUnit . snd
unquoteWordUnit (SingleQuote cs) = (True, Char . snd <$> cs)

-- | Expandable word, a possibly empty list of word units.
newtype EWord = EWord [Positioned WordUnit]
  deriving (Eq)

-- | Returns the content of a word.
wordUnits :: EWord -> [Positioned WordUnit]
wordUnits (EWord us) = us

-- | If the given word consists of constant unquoted characters only, then
-- returns the content as a text.
wordText :: EWord -> Maybe (Text)
wordText us = fmap pack $ sequenceA $ fmap (constChar . snd) $ wordUnits us
  where constChar (Unquoted (Char c)) = Just c
        constChar _ = Nothing

instance Show EWord where
  showsPrec n (EWord us) s = foldr (showsPrec n . snd) s us
  showList [] = id
  showList [w] = shows w
  showList (w:ws) = shows w . showSpace . showList ws

-- | Non-empty word, defined as a (lexical) token with the token identifier
-- @TOKEN@ in POSIX.
newtype Token = Token (NonEmpty (Positioned WordUnit))
  deriving (Eq)

-- | Returns the content of a token.
tokenUnits :: Token -> NonEmpty (Positioned WordUnit)
tokenUnits (Token us) = us

-- | Converts a token to a word.
tokenWord :: Token -> EWord
tokenWord = EWord . toList . tokenUnits

-- | If the given token consists of constant unquoted characters only, then
-- returns the content as a text.
tokenText :: Token -> Maybe (Text)
tokenText = wordText . tokenWord

-- | Removes backslash escapes, double-quotes, and single-quotes without word
-- expansion. The Boolean is true iff quotation was removed.
unquoteToken :: Token -> (Bool, [DoubleQuoteUnit])
unquoteToken t = (or bs, concat uss)
  where ~(bs, uss) = unq t
        unq = unzip . fmap unquoteWordUnit . toList . fmap snd . tokenUnits

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
  hereDocOpPos :: Position,
  hereDocFd :: !Natural,
  isTabbed :: !Bool,
  delimiter :: Token}
  deriving (Eq)

instance Show HereDocOp where
  showsPrec n o =
    showsPrec n (hereDocFd o) . showString s . showsPrec n (delimiter o)
    where s = if isTabbed o then "<<-" else "<<"

-- | Types of file redirection operations.
data FileOp =
    In      -- ^ @<@
  | InOut   -- ^ @<>@
  | Out     -- ^ @>@
  | Append  -- ^ @>>@
  | Clobber -- ^ @>|@
  | DupIn   -- ^ @<&@
  | DupOut  -- ^ @>&@
  deriving (Eq)

instance Show FileOp where
  showsPrec _ In      = showString "<"
  showsPrec _ InOut   = showString "<>"
  showsPrec _ Out     = showString ">"
  showsPrec _ Append  = showString ">>"
  showsPrec _ Clobber = showString ">|"
  showsPrec _ DupIn   = showString "<&"
  showsPrec _ DupOut  = showString ">&"

-- | Redirection.
data Redirection =
  FileRedirection {
    fileOpPos :: Position,
    fileOpFd :: !Natural,
    fileOp :: !FileOp,
    fileOpTarget :: Token}
  | HereDoc {
    hereDocOp :: !HereDocOp,
    content :: [Positioned DoubleQuoteUnit]}
  deriving (Eq)

instance Show Redirection where
  showsPrec n (FileRedirection _ fd' op f) =
    showsPrec n fd' . showsPrec n op . showsPrec n f
  showsPrec n (HereDoc o _) = showsPrec n o -- content is ignored
  showList [] = id
  showList [r] = shows r
  showList (r:rs) = shows r . showSpace . showList rs

-- | Returns the target file descriptor of the given redirection.
fd :: Redirection -> Natural
fd (FileRedirection _ fd' _ _) = fd'
fd (HereDoc (HereDocOp _ fd' _ _) _) = fd'

-- | Commands that can contain other commands.
data CompoundCommand =
  -- | one or more and-or lists.
  Grouping (NonEmpty AndOrList)
  -- | one or more and-or lists.
  | Subshell (NonEmpty AndOrList)
  -- TODO for command
  -- TODO case command
  -- TODO if command
  -- | loop condition and body.
  | While (NonEmpty AndOrList) (NonEmpty AndOrList)
  -- | loop condition and body.
  | Until (NonEmpty AndOrList) (NonEmpty AndOrList)
  deriving (Eq)

showWhileUntilTail :: NonEmpty AndOrList -> NonEmpty AndOrList -> ShowS
showWhileUntilTail c b =
  showSeparatedList (toList c) . showString " do " .
    showSeparatedList (toList b) . showString " done"

instance Show CompoundCommand where
  showsPrec _ (Grouping ls) =
    showString "{ " . showSeparatedList (toList ls) . showString " }"
  showsPrec _ (Subshell ls) =
    showChar '(' . showList (toList ls) . showChar ')'
  showsPrec _ (While c b) =
    showString "while " . showWhileUntilTail c b
  showsPrec _ (Until c b) =
    showString "until " . showWhileUntilTail c b

-- | Element of pipelines.
data Command =
  -- | Simple command.
  SimpleCommand [Token] [Positioned Assignment] [Redirection]
  -- | Compound commands
  | CompoundCommand (Positioned CompoundCommand) [Redirection]
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
  showsPrec n (CompoundCommand (_, cc) []) = showsPrec n cc
  showsPrec n (CompoundCommand (_, cc) rs) =
    showsPrec n cc . showSpace . showList rs
  showsPrec _ FunctionDefinition = id -- FIXME

-- | Element of and-or lists. Optionally negated sequence of one or more
-- commands.
data Pipeline = Pipeline {
  pipeCommands :: NonEmpty Command,
  isNegated :: !Bool}
  deriving (Eq)

instance Show Pipeline where
  showsPrec n (Pipeline cs True) =
    showString "! " . showsPrec n (Pipeline cs False)
  showsPrec n (Pipeline (h :| t) False) = showsPrec n h . ft
      where ft s = foldr step s t
            step c = showString " | " . shows c

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
  isAsynchronous :: !Bool}
  deriving (Eq)

showAndOrHeadTail :: Pipeline -> [ConditionalPipeline] -> ShowS
showAndOrHeadTail h [] = shows h
showAndOrHeadTail h t = shows h . showSpace . showList t

instance Show AndOrList where
  showsPrec n (AndOrList h t False) | n <= 0 = showAndOrHeadTail h t
  showsPrec _ (AndOrList h t isAsync) = showAndOrHeadTail h t . showChar c
    where c = if isAsync then '&' else ';'
  showList [] = id
  showList [l] = shows l
  showList (l:ls) = showsPrec 1 l . showSpace . showList ls

-- | Shows a list of and-or lists. Unlike 'showList', the trailing separator
-- for the last and-or list is never omitted.
showSeparatedList :: [AndOrList] -> ShowS
showSeparatedList [] = id
showSeparatedList [l] = showsPrec 1 l
showSeparatedList (l:ls) = showsPrec 1 l . showSpace . showSeparatedList ls

-- vim: set et sw=2 sts=2 tw=78:
