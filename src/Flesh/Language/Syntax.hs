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
  DoubleQuoteUnit(..),
  WordUnit(..)) where

--import qualified Data.List.NonEmpty as NE
import qualified Flesh.Source.Position as P

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
  showsPrec _ (Char c) = (c:)
  showsPrec _ (Backslashed c) = \s -> '\\':c:s
  showsPrec _ Parameter = id
  showsPrec _ CommandSubstitution = id
  showsPrec _ Backquoted = id
  showsPrec _ Arithmetic = id
  -- | Just joins the given units, without enclosing double quotes.
  showList [] s = s
  showList (u:us) s = showsPrec 0 u $ showList us s

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
  showsPrec n (Unquoted unit) s = showsPrec n unit s
  showsPrec n (DoubleQuote units) s =
    '"' : (showsPrec n (snd (unzip units)) ('"' : s))
  showsPrec _ (SingleQuote chars) s =
    '\'' : (foldr f ('\'' : s) chars)
      where f (_, c) s' = c : s'
  showList [] s = s
  showList (u:us) s = showsPrec 0 u $ showList us s

-- vim: set et sw=2 sts=2 tw=78:
