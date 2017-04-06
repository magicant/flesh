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

module Flesh.Language.Parser.SyntaxSpec (spec) where

--import Flesh.Language.Parser.Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "doubleQuoteUnit" $ do
    it "parses backslashed backslash" $ pending

    it "parses single alphanumeric" $ pending

    it "skips line continuations" $ pending

-- vim: set et sw=2 sts=2 tw=78:
