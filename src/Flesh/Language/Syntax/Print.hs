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

This module defines functions for printing parsed syntax. The functions in
this module print in multi-line format including here document contents,
unlike the Show instances implemented in Flesh.Language.Syntax, which print in
single-line format without here document contents.
-}
module Flesh.Language.Syntax.Print (
  PrintState(..), initPrintState,
  PrintS, runPrint,
  Printable(..), ListPrintable(..)) where

import Control.Monad (when)
import Control.Monad.State.Strict (
  MonadState, State, evalState, get, modify', put, state)
import Control.Monad.Writer.Lazy (Endo(Endo), MonadWriter, tell)
import Data.List.NonEmpty (NonEmpty((:|)), toList)
import Flesh.Language.Syntax
import Flesh.Text.Show

-- | Intermediate state used while constructing a printer function.
data PrintState = PrintState {
  -- | Number of spaces at the beginning of lines.
  indent :: !Int,
  -- | Function to print here document contents at the next newline.
  hereDoc :: ShowS}

-- | Initial state to initiate the PrintS monad.
initPrintState :: PrintState
initPrintState = PrintState 0 id

-- | Monad to construct ShowS functions.
--
-- The outer State monad carries PrintState, from which the ShowS function
-- results in the inner PrintT monad.
type PrintS = PrintT (State PrintState)

-- | Runs the PrintS state with 'initPrintState'.
runPrint :: PrintS a -> ShowS
runPrint = flip evalState initPrintState . execPrintT

-- | Shows the argument character.
printChar :: MonadWriter (Endo String) m => Char -> m ()
printChar = tell' . showChar

-- | Shows the argument string.
printString :: MonadWriter (Endo String) m => String -> m ()
printString = tell' . showString

-- | Appends the given here document content to the current 'hereDoc'.
appendHereDoc :: MonadState PrintState m => ShowS -> m ()
appendHereDoc s = modify' (\(PrintState i h) -> PrintState i (h . s))

-- | Shows the current hereDoc and clears it.
printHereDoc :: (MonadState PrintState m, MonadWriter (Endo String) m) => m ()
printHereDoc = state (\(PrintState i h) -> (Endo h, PrintState i id)) >>= tell

-- | Shows as many spaces as the indent of the current state.
printIndent :: (MonadState PrintState m, MonadWriter (Endo String) m) => m ()
printIndent = do
  s <- get
  printString $ replicate (indent s) ' '

-- | Combination of @showChar '\n'@ and printHereDoc and printIndent.
printNewline :: (MonadState PrintState m, MonadWriter (Endo String) m) => m ()
printNewline = do
  printChar '\n'
  printHereDoc
  printIndent

-- | Temporarily increments the 'indent' while performing the given monad.
indented :: MonadState PrintState m => m a -> m a
indented m = do
  before <- get
  put PrintState {indent = indent before + 2, hereDoc = hereDoc before}
  x <- m
  after <- get
  put PrintState {indent = indent before, hereDoc = hereDoc after}
  return x
-- TODO: Consider using Lens.

-- | Class of printable syntax.
class Printable s where
  -- | Prints the given syntax.
  prints :: (MonadState PrintState m, MonadWriter (Endo String) m) => s -> m ()

-- | Class of printable lists of syntax.
class ListPrintable s where
  -- | Prints the given list of syntax.
  printList :: (MonadState PrintState m, MonadWriter (Endo String) m)
            => [s] -> m ()

instance Printable Redirection where
  prints r@FileRedirection {} = tell' $ shows r
  prints r@(HereDoc op cntnt) = do
    appendHereDoc $ showContent . showDelimiter . showChar '\n'
    tell' $ shows r
      where showContent = showList $ map snd cntnt
            showDelimiter = showList $ snd $ unquoteToken $ delimiter op

instance ListPrintable Redirection where
  printList [] = return ()
  printList [r] = prints r
  printList (r:rs) = foldl printSpaceAnd (prints r) rs
    where printSpaceAnd mrs' r' = do 
            () <- mrs'
            showSpace'
            prints r'

printsIndentedLists :: (MonadState PrintState m, MonadWriter (Endo String) m)
                    => NonEmpty AndOrList -> m ()
printsIndentedLists ls = do
  indented $ do
    printNewline
    printList $ toList ls
  printIndent

printsWhileUntilTail :: (MonadState PrintState m, MonadWriter (Endo String) m)
                     => NonEmpty AndOrList -> NonEmpty AndOrList -> m ()
printsWhileUntilTail c b = do
  printsIndentedLists c
  printString "do"
  printsIndentedLists b
  printString "done"

instance Printable CompoundCommand where
  prints (Grouping ls) = do
    printString "{"
    printsIndentedLists ls
    printString "}"
  prints (Subshell ls) = do
    printChar '('
    printsIndentedLists ls
    printChar ')'
  prints (If its me) = do
    printsIfThenList its
    maybePrintsElse me
    printString "fi"
      where printsIfThenList (ifthen :| elifthens) = do
              printsIfThen ifthen
              printsElifThenList elifthens
            printsElifThenList [] = return ()
            printsElifThenList (h:t) = do
              printString "el"
              printsIfThen h
              printsElifThenList t
            printsIfThen (c, t) = do
              printString "if"
              printsIndentedLists c
              printString "then"
              printsIndentedLists t
            maybePrintsElse Nothing = return ()
            maybePrintsElse (Just e) = do
              printString "else"
              printsIndentedLists e
  prints (While c b) = do
    printString "while"
    printsWhileUntilTail c b
  prints (Until c b) = do
    printString "until"
    printsWhileUntilTail c b

instance Printable Command where
  prints (SimpleCommand [] [] []) = return ()
  prints c@(SimpleCommand _ _ []) = tell' $ shows c
  prints (SimpleCommand [] [] rs) = printList rs
  prints (SimpleCommand ts as rs) = do
    prints (SimpleCommand ts as [])
    showSpace'
    printList rs
  prints (CompoundCommand (_, cc) []) = prints cc
  prints (CompoundCommand (_, cc) rs) = do
    prints cc
    showSpace'
    printList rs
  prints FunctionDefinition = undefined -- TODO

instance Printable Pipeline where
  prints p = do
    when (isNegated p) (printString "! ")
    foldl printPipeAnd (prints c) cs
      where c :| cs = pipeCommands p
            printPipeAnd mcs' c' = do
              () <- mcs'
              printString " |"
              printNewline
              prints c'

instance Printable ConditionalPipeline where
  prints (ConditionalPipeline (c, p)) = do
    tell' $ shows c
    printNewline
    prints p

instance ListPrintable ConditionalPipeline where
  printList [] = return ()
  printList [p] = prints p
  printList (p:ps) = do
    prints p
    showSpace'
    printList ps

printAndOrHeadTail :: (MonadState PrintState m, MonadWriter (Endo String) m)
                   => Pipeline -> [ConditionalPipeline] -> m ()
printAndOrHeadTail h [] = prints h
printAndOrHeadTail h t = do
  prints h
  showSpace'
  printList t

instance Printable AndOrList where
  prints (AndOrList p ps asy) = do
    printAndOrHeadTail p ps
    when asy $ printChar '&'
    printChar '\n'
    printHereDoc

instance ListPrintable AndOrList where
  printList [] = return ()
  printList [a] = prints a
  printList (a:as) = do
    prints a
    printIndent
    printList as

-- vim: set et sw=2 sts=2 tw=78:
