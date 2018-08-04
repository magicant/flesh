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
  -- * Names
  isPosixNameChar, isPosixNameString, isSpecialParameter,
  -- * Tokens
  DoubleQuoteUnit(..), unquoteDoubleQuoteUnit,
  WordUnit(..), unquoteWordUnit,
  EWord(..), wordUnits, wordText,
  Token(..), tokenUnits, tokenWord, tokenText, unquoteToken, positionOfToken,
  posixNameFromToken,
  Assignment(..),
  -- * Redirections
  HereDocOp(..), FileOp(..), Redirection(..), fd,
  -- * Syntax
  IfThenList, CaseItem, CompoundCommand(..), Command(..), Pipeline(..),
  AndOrCondition(..), ConditionalPipeline(..), AndOrList(..),
  showSeparatedList, CommandList) where

import Control.Monad (guard)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty (NonEmpty((:|)), toList)
import Data.Monoid (Endo(Endo), appEndo)
import Data.Text (Text, pack, unpack)
import Flesh.Source.Position (Position, Positioned)
import Numeric.Natural (Natural)

-- Utility for Show instances
showSpace :: ShowS
showSpace = showChar ' '

-- | Returns true iff the argument character can be used in POSIX names.
isPosixNameChar :: Char -> Bool
isPosixNameChar c | isDigit c      = True
                  | isAsciiLower c = True
                  | isAsciiUpper c = True
                  | c == '_'       = True
                  | otherwise      = False

-- | Returns true iff the argument is an unquoted POSIX name.
isPosixNameString :: String -> Bool
isPosixNameString [] = False
isPosixNameString cs@(c:_) = not (isDigit c) && all isPosixNameChar cs

-- | Returns true iff the argument char is a special parameter name.
isSpecialParameter :: Char -> Bool
isSpecialParameter = flip elem "@*#?-$!0"

-- | Element of double quotes.
data DoubleQuoteUnit =
    -- | Single bear character.
    Char !Char
    -- | Character escaped by a backslash.
    | Backslashed !Char
    -- | Parameter expansion.
    | Parameter Text -- TODO prefix, TODO modifier
    -- | @$(...)@
    | CommandSubstitution String
    -- | @`...`@
    | Backquoted String
    -- | @$((...))@ where the inner EWord is of the form @(...)@
    | Arithmetic EWord
  deriving (Eq)

instance Show DoubleQuoteUnit where
  showsPrec _ (Char c) = showChar c
  showsPrec _ (Backslashed c) = \s -> '\\':c:s
  showsPrec _ (Parameter name) =
    showString "${" . showString (unpack name) . showChar '}'
  showsPrec _ (CommandSubstitution cs) =
    showString "$(" . showString cs . showChar ')'
  showsPrec n (Backquoted cs) = bq . f (n /= 0) cs . bq
    where bq = showChar '`'
          f _ "" = id
          f dq (c':cs') | c' == '`' || c' == '\\' || (dq && c' == '"') =
                          showChar '\\' . showChar c' . f dq cs'
                        | otherwise =
                          showChar c' . f dq cs'
  -- In (Arithmetic w), w always has an open and close parenthesis, so the
  -- number of parentheses is correct here.
  showsPrec _ (Arithmetic w) = showString "$(" . shows w . showChar ')'
  -- | Just joins the given units, without enclosing double quotes.
  {-
  showList [] = id
  showList (u:us) = shows u . showList us
  -}
  showList = foldr ((.) . shows) id

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
  showsPrec _ (DoubleQuote units) =
    showChar '"' . (\s -> foldr (showsPrec 1 . snd) s units) . showChar '"'
  showsPrec _ (SingleQuote chars) =
    showChar '\'' . (\s -> foldr (showChar . snd) s chars) . showChar '\''
  {-
  showList [] = id
  showList (u:us) = shows u . showList us
  -}
  showList = foldr ((.) . shows) id

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
wordText :: EWord -> Maybe Text
wordText us = fmap pack $ traverse (constChar . snd) $ wordUnits us
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
tokenText :: Token -> Maybe Text
tokenText = wordText . tokenWord

-- | Removes backslash escapes, double-quotes, and single-quotes without word
-- expansion. The Boolean is true iff quotation was removed.
unquoteToken :: Token -> (Bool, [DoubleQuoteUnit])
unquoteToken t = (or bs, concat uss)
  where ~(bs, uss) = unq t
        unq = unzip . fmap unquoteWordUnit . toList . fmap snd . tokenUnits

-- | Returns the position of the token
positionOfToken :: Token -> Position
positionOfToken (Token ((p, _) :| _)) = p

-- | Returns the token text only if the argument is an unquoted POSIX name.
posixNameFromToken :: Token -> Maybe Text
posixNameFromToken t = do
  let ttt = tokenText t
  tx <- ttt
  guard $ isPosixNameString $ unpack tx
  ttt

instance Show Token where
  showsPrec n t = showsPrec n (tokenWord t)
  showList ts = showList (fmap tokenWord ts)

-- | Assignment.
data Assignment = Assignment Token EWord
  deriving (Eq)

instance Show Assignment where
  showsPrec n (Assignment name value) =
    showsPrec n name . showChar '=' . showsPrec n value
  showList [] = id
  showList [a] = shows a
  showList (a:as) = shows a . showSpace . showList as

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

-- | List of (el)if-then clauses. Each pair represents a condition and
-- corresponding statement.
type IfThenList = NonEmpty (CommandList, CommandList)

-- | Body of the case command. One or more patterns and zero or more and-or
-- lists.
type CaseItem = (NonEmpty Token, [AndOrList])

-- | Commands that can contain other commands.
data CompoundCommand =
  -- | one or more and-or lists.
  Grouping CommandList
  -- | one or more and-or lists.
  | Subshell CommandList
  -- | name, optional words, and loop body.
  | For Token (Maybe [Token]) CommandList
  -- | word and case items.
  | Case Token [CaseItem]
  -- | list of (el)if-then clauses and optional else clause.
  | If IfThenList (Maybe CommandList)
  -- | loop condition and body.
  | While CommandList CommandList
  -- | loop condition and body.
  | Until CommandList CommandList
  -- TODO Ksh-style function definition.
  deriving (Eq)

showEach :: (a -> ShowS) -> [a] -> ShowS
showEach f = appEndo . mconcat . fmap (Endo . f)

showPattern :: Token -> ShowS
showPattern p = showString " | " . shows p

showCaseItem :: CaseItem -> ShowS
showCaseItem (p :| ps, ls) =
  showChar '(' . shows p . showEach showPattern ps . showString ") " .
    showList ls . showString ";; "

showDoGroup :: CommandList -> ShowS
showDoGroup c =
  showString " do " . showSeparatedList (toList c) . showString " done"

showWhileUntilTail :: CommandList -> CommandList -> ShowS
showWhileUntilTail c b = showSeparatedList (toList c) . showDoGroup b

instance Show CompoundCommand where
  showsPrec _ (Grouping ls) =
    showString "{ " . showSeparatedList (toList ls) . showString " }"
  showsPrec _ (Subshell ls) =
    showChar '(' . showList (toList ls) . showChar ')'
  showsPrec _ (For name optwords ls) =
    showString "for " . shows name . showForWords optwords . showDoGroup ls
      where showForWords Nothing = id
            showForWords (Just ws) =
              showString " in " . shows ws . showChar ';'
  showsPrec _ (Case w is) =
    showString "case " . shows w . showString " in " .
      showEach showCaseItem is . showString "esac"
  showsPrec _ (If its me) =
    showIfThenList its . maybeShowElse me . showString " fi"
      where showIfThenList (ifthen :| elifthens) =
              showIfThen ifthen . showElifThenList elifthens
            showElifThenList [] = id
            showElifThenList (h:t) =
              showString " el" . showIfThen h . showElifThenList t
            showIfThen (c, t) =
              showString "if " . showSeparatedList (toList c) .
                showString " then " . showSeparatedList (toList t)
            maybeShowElse Nothing = id
            maybeShowElse (Just e) =
              showString " else " . showSeparatedList (toList e)
  showsPrec _ (While c b) =
    showString "while " . showWhileUntilTail c b
  showsPrec _ (Until c b) =
    showString "until " . showWhileUntilTail c b

-- | Element of pipelines.
data Command =
  -- | Simple command.
  SimpleCommand [Token] [Assignment] [Redirection]
  -- | Compound commands
  | CompoundCommand (Positioned CompoundCommand) [Redirection]
  -- | Bourne-style function definition.
  | FunctionDefinition Text Command
  deriving (Eq)

instance Show Command where
  showsPrec _ (SimpleCommand [] [] []) = id
  showsPrec _ (SimpleCommand ts [] []) = showList ts
  showsPrec _ (SimpleCommand [] as []) = showList as
  showsPrec _ (SimpleCommand [] [] rs) = showList rs
  showsPrec _ (SimpleCommand ts [] rs) = showList ts . showSpace . showList rs
  showsPrec n (SimpleCommand ts as rs) =
    showList as . showSpace . showsPrec n (SimpleCommand ts [] rs)
  showsPrec n (CompoundCommand (_, cc) []) = showsPrec n cc
  showsPrec n (CompoundCommand (_, cc) rs) =
    showsPrec n cc . showSpace . showList rs
  showsPrec n (FunctionDefinition name cmd) =
    showString (unpack name) . showString "() " . showsPrec n cmd

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

-- | Sequence of one or more and-or lists. In POSIX, CommandList is simply
-- referred to as "list".
type CommandList = NonEmpty AndOrList

-- vim: set et sw=2 sts=2 tw=78:
