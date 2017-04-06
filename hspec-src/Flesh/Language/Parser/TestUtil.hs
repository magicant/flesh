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

{-# LANGUAGE Trustworthy #-}

module Flesh.Language.Parser.TestUtil where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Strict
import Flesh.Language.Parser.Error
--import Flesh.Language.Parser.Input
import Flesh.Source.Position
import Test.Hspec

type Tester = AttemptT
  (StateT PositionedString (ExceptT (Severity, Error) Identity))

expectSuccess :: (Eq a, Show a) =>
  String -> String -> Tester a -> a -> SpecWith ()
expectSuccess consumed lookahead parser result =
  let s = consumed ++ lookahead
      s' = spread (dummyPosition s) s
      e = runIdentity $ runExceptT $ runStateT (runAttemptT parser) s'
   in context s $ do
     it "returns expected result successfully" $
       fmap fst e `shouldBe` Right result

     it "consumes expected part of source code" $
       fmap snd e `shouldBe` Right (dropP (length consumed) s')

-- vim: set et sw=2 sts=2 tw=78:
