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

This module defines conversion of position to human-readable presentation.
-}
module Flesh.Source.Position.Print (
  Kind(..), Block(..)) where

import Control.Monad.Writer.Lazy (Endo(Endo), Writer, appEndo, execWriter)
import Data.Text (unpack)
import qualified Flesh.Language.Alias as A
import qualified Flesh.Source.Position as P
import Flesh.Text.Show

-- | Kind of message blocks
data Kind = Error | Warning | Note
  deriving (Eq)

instance Show Kind where
  show Error = "error"
  show Warning = "warning"
  show Note = "note"

-- | A message block is a formatted message annotated with a position.
--
-- When printed, the main part of a block consists of three lines:
--
-- - colon-separated list of:
--
--     - filename
--     - line number
--     - column number
--     - (optional) message kind
--     - message text
--
-- - line of code that contains the position
-- - caret indented to indicate the position
--
-- Depending on the situation of the position, the main part may be followed
-- by another block that describes the situation.
data Block = Block {
  position :: P.Position,
  kind :: Maybe Kind,
  message :: String}
  deriving (Eq)

newline :: Writer (Endo String) ()
newline = tell' $ showChar '\n'

-- | Returns the filename part of the situation.
name :: P.Situation -> String
name P.StandardInput = "<stdin>"
name P.File {P.path = p, P.dotBuiltinPosition = _} = p
name (P.Eval _) = "<eval>"
name (P.CommandSubstitution _) = "<cmdsub>"
name (P.Alias _ _) = "<alias>"
name (P.ArithmeticExpansion _) = "<arith>"
name (P.FunctionCall _ _) = "<func>" -- TODO
-- FunctionCall should not be part of Situation because it does not describe
-- the origin of a source code fragment. Function call stacks should be
-- separated from Situation.

-- | Prints the first line of a block.
showsBlockHead :: Block -> Writer (Endo String) ()
showsBlockHead (Block {position = p, kind = k, message = m}) = do
  let f = P.fragment p
      colon = tell' $ showChar ':'
  tell' $ showString $ name $ P.situation f
  colon
  tell' $ shows $ P.lineNo f + 1
  colon
  tell' $ shows $ P.index p + 1
  colon
  showSpace'
  case k of
    Nothing -> return ()
    Just k' -> do
      tell' $ shows k'
      colon
      showSpace'
  tell' $ showString m
  newline

-- | Splits a string into lines, returning the line containing the indicated
-- index and the index in the line.
focusLine :: String -> Int -> (String, Int)
focusLine s n =
  if n <= l then (h, n) else focusLine t $! n - l - 1
    where (h, t) = break (== '\n') s
          l = length h

-- | Prints the second and third lines of a block, optionally followed by
-- another block that describes the situation.
showsBlockTail :: P.Position -> Writer (Endo String) ()
showsBlockTail p = do
  let f = P.fragment p
      (c, n) = focusLine (P.code f) (P.index p)
      space '\t' = '\t'
      space _    = ' '
      spaces = map space $ take n c
  tell' $ showString $ c
  newline
  tell' $ showString $ spaces ++ "^"
  newline
  showsSubblock $ P.situation f

-- | Prints a block.
showsBlock :: Block -> Writer (Endo String) ()
showsBlock blk = do
  showsBlockHead blk
  showsBlockTail (position blk)

-- | Optionally prints a sub-block for describing the situation.
showsSubblock :: P.Situation -> Writer (Endo String) ()
showsSubblock P.StandardInput = return ()
showsSubblock P.File {P.path = _, P.dotBuiltinPosition = Just p} =
  showsBlock Block {position = p, kind = Nothing, message = m}
    where m = "in the script file imported here"
showsSubblock (P.File _ _) = return ()
showsSubblock P.Eval {P.evalBuiltinPosition = p} =
  showsBlock Block {position = p, kind = Nothing, message = m}
    where m = "in the script evaluated here"
showsSubblock P.CommandSubstitution {P.position = p} =
  showsBlock Block {position = p, kind = Nothing, message = m}
    where m = "in the command substitution executed here"
showsSubblock P.Alias {P.position = p, P.aliasDefinition = a} =
  showsBlock Block {position = p, kind = Nothing, message = m}
    where m = "in alias \"" ++ n ++ "\" substituted here"
          n = unpack $ A.name a
showsSubblock P.ArithmeticExpansion {P.position = p} =
  showsBlock Block {position = p, kind = Nothing, message = m}
    where m = "in the arithmetic expansion executed here"
showsSubblock P.FunctionCall {P.position = p, P.functionDefinition = _} =
  showsBlock Block {position = p, kind = Nothing, message = m}
    where m = "in the function called here" -- TODO function name

instance Show Block where
  showsPrec _ = appEndo . execWriter . showsBlock
  showList = appEndo . mconcat . map (Endo . shows)

-- TODO support colored printing
-- TODO localization support

-- vim: set et sw=2 sts=2 tw=78:
