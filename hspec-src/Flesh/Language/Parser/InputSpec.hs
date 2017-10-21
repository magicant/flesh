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

import Control.Monad.State.Strict (evalState, execState)
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
  describe "MonadInput (RecordT m)" $ do
    describe "popChar" $ do
      prop "returns popped character" $ \s n ->
        let _ = s :: PositionedString
            r1 = flip evalState s $ runPositionedStringT $ evalRecordT $
              sequence $ replicate n popChar
            r2 = flip evalState s $ runPositionedStringT $
              sequence $ replicate n popChar
         in r1 === r2

    describe "lookahead" $ do
      prop "applies lookahead to inner monad" $ \s n ->
        let _ = s :: PositionedString
            r1 = flip execState s $ runPositionedStringT $ evalRecordT $
              lookahead $ sequence_ $ replicate n popChar
            r2 = flip execState s $ runPositionedStringT $ evalRecordT $
              return ()
         in r1 === r2

      prop "does not record consumed characters" $ \s n ->
        let _ = s :: PositionedString
            r1 = flip evalState s $ runPositionedStringT $ evalRecordT $ do
              lookahead $ sequence_ $ replicate n popChar
              reverseConsumedChars
            r2 = flip evalState s $ runPositionedStringT $ evalRecordT $
              reverseConsumedChars
         in r1 === r2

  describe "MonadInputRecord (RecordT m)" $ do
    describe "reverseConsumedChars" $ do
      prop "returns consumed characters" $ \s n ->
        let _ = s :: PositionedString
            r = flip evalState s $ runPositionedStringT $ evalRecordT $ do
              sequence_ $ replicate n popChar
              reverseConsumedChars
         in reverse r === take n (unposition s)

-- vim: set et sw=2 sts=2 tw=78:
