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

module Flesh.Language.Parser.BufferSpec (spec) where

import Control.Monad (replicateM, replicateM_)
import Control.Monad.State.Strict (evalState, execState)
import Flesh.Language.Parser.Buffer
import Flesh.Source.Position
import Flesh.Source.PositionTestUtil ()
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = parallel $ do
  describe "MonadBuffer (RecordT m)" $ do
    describe "popChar" $ do
      prop "returns popped character" $ \s n ->
        let _ = s :: PositionedString
            r1 = flip evalState s $ runPositionedStringT $ evalRecordT $
              replicateM n popChar
            r2 = flip evalState s $ runPositionedStringT $
              replicateM n popChar
         in r1 === r2

    describe "lookahead" $ do
      prop "applies lookahead to inner monad" $ \s n ->
        let _ = s :: PositionedString
            r1 = flip execState s $ runPositionedStringT $ evalRecordT $
              lookahead $ replicateM_ n popChar
            r2 = flip execState s $ runPositionedStringT $ evalRecordT $
              return ()
         in r1 === r2

      prop "does not record consumed characters" $ \s n ->
        let _ = s :: PositionedString
            r1 = flip evalState s $ runPositionedStringT $ evalRecordT $ do
              lookahead $ replicateM_ n popChar
              reverseConsumedChars
            r2 = flip evalState s $ runPositionedStringT $ evalRecordT $
              reverseConsumedChars
         in r1 === r2

  describe "MonadRecord (RecordT m)" $ do
    describe "reverseConsumedChars" $ do
      prop "returns consumed characters" $ \s n ->
        let _ = s :: PositionedString
            r = flip evalState s $ runPositionedStringT $ evalRecordT $ do
              replicateM_ n popChar
              reverseConsumedChars
         in reverse r === take n (unposition s)

-- vim: set et sw=2 sts=2 tw=78:
