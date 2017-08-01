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

import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy
import Data.List.NonEmpty (NonEmpty(..))
import Flesh.Language.Syntax

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
-- results in the inner WriterT monad.
type PrintS = WriterT (Endo String) (State PrintState)

-- | Runs the PrintS state with 'initPrintState'.
runPrint :: PrintS a -> ShowS
runPrint = appEndo . flip evalState initPrintState . execWriterT

-- | Utility for yielding a ShowS function
tell' :: MonadWriter (Endo String) m => ShowS -> m ()
tell' = tell . Endo

-- | Utility for Show instances
showSpace' :: MonadWriter (Endo String) m => m ()
showSpace' = tell' $ showChar ' '

-- | Appends the given here document content to the current 'hereDoc'.
appendHereDoc :: MonadState PrintState m => ShowS -> m ()
appendHereDoc s = modify' (\(PrintState i h) -> PrintState i (h . s))

-- | Shows the current 'hereDoc' and clears it.
printHereDoc :: (MonadState PrintState m, MonadWriter (Endo String) m) => m ()
printHereDoc = state (\(PrintState i h) -> (Endo h, PrintState i id)) >>= tell

-- | Shows as many spaces as 'indent' of the current state.
printIndent :: (MonadState PrintState m, MonadWriter (Endo String) m) => m ()
printIndent = do
  s <- get
  tell' $ showString $ replicate (indent s) ' '

-- | Combination of @showChar '\n'@ and 'printHereDoc' and 'printIndent'.
printNewline :: (MonadState PrintState m, MonadWriter (Endo String) m) => m ()
printNewline = do
  tell' $ showChar '\n'
  printHereDoc
  printIndent

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
  prints r@(FileRedirection _) = tell' $ shows r
  prints r@(HereDoc op cntnt) = do
    appendHereDoc $ showContent . showDelimiter . showChar '\n'
    tell' $ shows r
      where showContent = showList $ snd $ unzip cntnt
            showDelimiter = showList $ snd $ unquoteToken $ delimiter op

instance ListPrintable Redirection where
  printList [] = return ()
  printList [r] = prints r
  printList (r:rs) = foldl printSpaceAnd (prints r) rs
    where printSpaceAnd mrs' r' = do 
            () <- mrs'
            showSpace'
            prints r'

instance Printable Command where
  prints (SimpleCommand [] [] []) = return ()
  prints c@(SimpleCommand _ _ []) = tell' $ shows c
  prints (SimpleCommand [] [] rs) = printList rs
  prints (SimpleCommand ts as rs) = do
    prints (SimpleCommand ts as [])
    showSpace'
    printList rs
  prints FunctionDefinition = undefined -- TODO

instance Printable Pipeline where
  prints p = do
    when (isNegated p) (tell' $ showString "! ")
    foldl printPipeAnd (prints c) cs
      where c :| cs = pipeCommands p
            printPipeAnd mcs' c' = do
              () <- mcs'
              tell' $ showString " |"
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
    when asy $ tell' $ showChar '&'
    tell' $ showChar '\n'
    printHereDoc

instance ListPrintable AndOrList where
  printList [] = return ()
  printList [a] = prints a
  printList (a:as) = do
    prints a
    printIndent
    printList as

-- vim: set et sw=2 sts=2 tw=78: