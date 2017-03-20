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
Portability : POSIX

This module defines elements for positioning source code characters and
describing origin of them.
-}
module Flesh.Source.Position (
    Situation(..), Fragment(..), Position(..), dummyPosition,
    Positioned, next, spread) where

-- FIXME
type AliasDefinition = Position
type FunctionDefinition = Position

-- | Situation in which a code fragment is executed/evaluated.
data Situation =
  -- | Standard input.
    StandardInput
  -- | Script file.
  | File {
      -- | Path to the script file (for informative purposes only).
      path :: String,
      -- | Position of the dot built-in that sourced this file. (Nothing if
      -- not sourced by the dot built-in.)
      dotBuiltinPosition :: Maybe Position}
  -- | Command string evaluated by the eval built-in.
  | Eval {
      evalBuiltinPosition :: Position}
  -- | Command Substitution.
  | CommandSubstitution {
      -- | Position at which substitution/expansion/function call occurred.
      position :: Position}
  -- | Part of code that resulted from alias substitution.
  | Alias {
      position :: Position,
      -- | Definition of the alias substituted.
      aliasDefinition :: AliasDefinition}
  -- | Arithmetic expansion.
  | ArithmeticExpansion {
      position :: Position}
  -- | Function call.
  | FunctionCall {
      position :: Position,
      -- | Definition of the function called.
      functionDefinition :: FunctionDefinition}
  deriving (Eq, Show)

-- | Source code fragment, typically a single line of code.
data Fragment = Fragment {
    -- | Source code.
    code :: String,
    -- | Situation in which the source code occurred.
    situation :: Situation,
    -- | Line number (starts from 0).
    lineNo :: Int}
  deriving (Eq, Show)

-- | Position of a character that occurs in a source code fragment.
data Position = Position {
    -- | Fragment whose code the index is to.
    fragment :: Fragment,
    -- | Index to the character in the code of the fragment (starts from 0).
    index :: Int}
  deriving (Eq, Show)

-- | Unmeaningful position for testing.
dummyPosition :: String -> Position
dummyPosition c = Position {fragment = f, index = 0}
  where f = Fragment {code = c, situation = StandardInput, lineNo = 0}

-- | Something with a record of position from which it originated.
type Positioned a = (Position, a)

-- | Increments the index of a position.
next :: Position -> Position
next (Position fragment_ index_) = Position fragment_ (index_ + 1)

-- | Given a position for the first element of a list, returns a list of
-- elements positioned successively.
spread :: Position -> [a] -> [Positioned a]
spread p xs = zip (iterate next p) xs

-- vim: set et sw=2 sts=2 tw=78:
