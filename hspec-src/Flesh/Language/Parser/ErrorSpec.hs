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

module Flesh.Language.Parser.ErrorSpec (spec) where

import Control.Applicative
import Control.Monad.Identity
import Flesh.Language.Parser.Error
import Flesh.Source.Position
import Test.Hspec

hardError :: AttemptT Identity Int
hardError = failure $ Error SomeReason (dummyPosition "")

softError :: AttemptT Identity Int
softError = try hardError

spec :: Spec
spec = do
  describe "Alternative (AttemptT m) (<|>)" $ do
    it "returns hard errors intact" $
      (hardError <|> return 0) `shouldBe` hardError

    it "returns success intact" $
      (return 42 <|> hardError) `shouldBe` return 42

    it "recovers soft errors" $
      (softError <|> return 123) `shouldBe` return 123

  describe "MonadAttempt (AttemptT m) setReason" $ do
    it "replaces UnknownReason, keeping hard severity" $
      let e = Error SomeReason (dummyPosition "abc") in
      setReason e empty `shouldBe` (failure e :: AttemptT Identity Int)

    it "replaces UnknownReason, keeping soft severity" $
      let e = Error SomeReason (dummyPosition "abc") in
      setReason e (try empty) `shouldBe`
        try (failure e :: AttemptT Identity Int)

-- vim: set et sw=2 sts=2 tw=78:
