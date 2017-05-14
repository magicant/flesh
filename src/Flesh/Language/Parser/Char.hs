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

This module defines character parsers.
-}
module Flesh.Language.Parser.Char where

import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Source.Position

-- | Parses any single character.
--
-- Returns 'UnknownReason' if there is no next character.
anyChar :: MonadParser m => m (Positioned Char)
anyChar = do
  e <- popChar
  case e of
    Left pos -> failureOfPosition pos
    Right pc -> return pc

-- | Parses a single character that satisfies the given predicate.
--
-- Returns 'UnknownReason' on dissatisfaction.
satisfy :: MonadParser m => (Char -> Bool) -> m (Positioned Char)
satisfy = satisfying anyChar

-- | Parses the given single character.
--
-- Returns 'UnknownReason' on failure.
char :: MonadParser m => Char -> m (Positioned Char)
char c = satisfy (c ==)

-- | Parses one of the given characters.
--
-- Returns 'UnknownReason' on failure.
oneOfChars :: MonadParser m => [Char] -> m (Positioned Char)
oneOfChars cs = satisfy (flip elem cs)

-- | Parses a sequence of characters.
--
-- Returns 'UnknownReason' on failure.
string :: MonadParser m => String -> m [Positioned Char]
string [] = return []
string (c:cs) = (:) <$> char c <*> string cs

-- | Parses the /end-of-file/.
eof :: MonadParser m => m Position
eof = do
  e <- peekChar
  case e of
    Left pos -> return pos
    Right (pos, _) -> failureOfPosition pos

-- vim: set et sw=2 sts=2 tw=78:
