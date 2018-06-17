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

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Flesh.Language.Parser.ClassSpec (spec) where

import Control.Applicative ((<|>))
import Flesh.Language.Parser.Class
import Flesh.Language.Parser.ClassTestUtil ()
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.ErrorTestUtil ()
import Flesh.Language.Parser.TestUtil
import Flesh.Source.Position
import Flesh.Source.PositionTestUtil ()
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

run :: FullInputTester a
    -> PositionedString -> Either Failure (a, PositionedString)
run = runFullInputTester

spec :: Spec
spec = parallel $ do
  describe "notFollowedBy" $ do
    prop "succeeds if argument fails" $ \e i ->
      let f = failureOfError e
       in run (notFollowedBy f) i === run (return ()) i

    prop "fails if argument succeeds" $ \v i ->
      let _ = v :: Int
       in run (notFollowedBy (return v)) i === run failure i

  describe "Alternative (ParserT m) (<|>)" $ do
    prop "returns hard errors intact" $ \e a i ->
      let _ = a :: FullInputTester Int
          f = require $ failureOfError e
       in run (f <|> a) i === run f i

    prop "returns success intact" $ \v a i ->
      let _ = a :: FullInputTester Int
          s = return v
       in run (s <|> a) i === run s i

    prop "recovers soft errors" $ \e a i ->
      let _ = a :: FullInputTester Int
          f = failureOfError e
       in run (f <|> a) i === run a i

-- vim: set et sw=2 sts=2 tw=78:
