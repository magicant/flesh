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

module Flesh.Language.Parser.LexSpec (spec) where

import Data.List
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Lex
import Flesh.Language.Parser.TestUtil
import Flesh.Source.Position
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "comment" $ do
    prop "fails if input does not start with #" $ \s ->
      not ("#" `isPrefixOf` s) && not ("\\\n" `isPrefixOf` s) ==>
        let p = dummyPosition s
            s' = spread p s
         in runTester comment s' === Left (Soft, Error UnknownReason p)

    prop "parses up to newline" $ \s s' ->
      not (elem '\n' s) ==>
        let input = '#' : s ++ '\n' : s'
            p = dummyPosition input
            input' = spread p input
            e = runTester comment input'
            out = unposition $ spread (next p) s
         in e === Right (out, dropP (length s + 1) input')

    prop "parses up to end-of-file" $ \s ->
      not (elem '\n' s) ==>
        let input = '#' : s
            p = dummyPosition input
            input' = spread p input
            e = runTester comment input'
            out = unposition $ spread (next p) s
         in e === Right (out, dropP (length s + 1) input')

    context "skips preceding line continuations" $ do
      expectSuccessEof "\\\n\\\n#" "" comment []

    context "ignores line continuations in comment body" $ do
      expectSuccessEof "#\\" "\n" (fmap (fmap snd) comment) "\\"

-- vim: set et sw=2 sts=2 tw=78:
