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

This module defines conversion of parser errors to human-readable
presentation.
-}
module Flesh.Language.Parser.Error.Print (showsError) where

import Data.Foldable (toList)
import Flesh.Language.Parser.Error as E
import Flesh.Language.Syntax
import Flesh.Source.Position (Position)
import Flesh.Source.Position.Print as P

note :: Position -> String -> P.Block
note p m = P.Block {P.position = p, P.kind = Just P.Note, P.message = m}

describe :: Reason -> (String, [Block])
describe UnknownReason = ("unknown error", [])
describe LineBeginningWithSemicolon =
  ("a line cannot start with a semicolon", [])
describe UnclosedDoubleQuote = ("the double quote is unclosed", [])
describe UnclosedSingleQuote = ("the single quote is unclosed", [])
describe UnclosedCommandSubstitution =
  ("the command substitution is unclosed", [])
describe MissingExpansionAfterDollar =
  ("a valid expansion is required after the \"$\"", [])
describe MissingRedirectionTarget =
  ("the redirection target is missing", [])
describe (UnclosedHereDocContent op) =
  ("the here-document content is unclosed", [b])
    where b = note p m
          p = hereDocOpPos op
          m = "the here-document content was requested by this redirection"
describe (MissingHereDocContents ops) =
  ("the here-document content is missing", bs)
    where bs = block <$> toList ops
          block op = note (hereDocOpPos op) m
          m = "the here-document content was requested by this redirection"
describe (MissingCommandAfter w) =
  ("a command is required after the \"" ++ w ++ "\"", [])
describe (UnclosedSubshell p) =
  ("a \")\" is required to close the subshell command", [b])
    where b = note p "the subshell command was introduced here"
describe (UnclosedGrouping p) =
  ("a \"}\" is required to close the grouping command", [b])
    where b = note p "the grouping command was introduced here"
describe (MissingThenForIf p) =
  ("a then clause is required after the if clause", [b])
    where b = note p "the \"if\" was here"
describe (MissingThenForElif p) =
  ("a then clause is required after the elif clause", [b])
    where b = note p "the \"elif\" was here"
describe (MissingFiForIf p) =
  ("a \"fi\" is required to close the if command", [b])
    where b = note p "the if command was introduced here"
describe MissingNameAfterFor =
  ("a variable name is required after the \"for\"", [])
describe SemicolonBeforeIn =
  ("no semicolon is allowed before \"in\"", [])
describe (MissingDoForFor p) =
  ("a do-done clause is required to complete the for loop", [b])
    where b = note p "the for loop was introduced here"
describe (MissingDoForWhile p) =
  ("a do-done clause is required to complete the while command", [b])
    where b = note p "the while command was introduced here"
describe (MissingDoForUntil p) =
  ("a do-done clause is required to complete the until command", [b])
    where b = note p "the until command was introduced here"
describe (MissingDoneForDo p) =
  ("a \"done\" is required to close the do-done clause", [b])
    where b = note p "the \"do\" was here"

-- | Converts an error to a human-readable format.
showsError :: Error -> ShowS
showsError E.Error {E.reason = r, E.position = p} = shows $ main : sub
  where main = Block {P.position = p, P.kind = Just P.Error, P.message = mm}
        (mm, sub) = describe r

-- TODO localization

-- vim: set et sw=2 sts=2 tw=78:
