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

{-# OPTIONS_GHC -Wno-orphans #-}

module Flesh.Language.Parser.InputSpec (spec) where

import Control.Monad.State.Strict (evalState)
import Flesh.Language.Parser.Input
import Flesh.Source.Position
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, arbitrary, (===))

instance Arbitrary a => Arbitrary (PositionedList a) where
  arbitrary = do
    s <- arbitrary
    xs <- arbitrary
    return $ spread (dummyPosition s) xs

spec :: Spec
spec = do
  describe "MonadInputRecord (RecordT m)" $ do
    describe "reverseConsumedChars" $ do
      prop "returns consumed characters" $ \s n ->
        let _ = s :: PositionedString
            r = flip evalState s $ runPositionedStringT $ evalRecordT $ do
              sequence_ $ replicate n popChar
              reverseConsumedChars
         in reverse r === take n (unposition s)

-- vim: set et sw=2 sts=2 tw=78:
