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

module Flesh.Language.Parser.AliasSpec (spec) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Map.Strict (singleton)
import Data.Text (pack)
import Flesh.Language.Alias
import Flesh.Language.Parser.Alias
import Flesh.Source.Position
import Test.Hspec (Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===), (==>))

testMaybeAliasValue :: String -- ^ alias name in the definition set
                    -> String -- ^ alias value in the definition set
                    -> String -- ^ text passed to maybeAliasValue
                    -> Maybe [Positioned Char]
testMaybeAliasValue s v t =
  let [s', t', v'] = pack <$> [s, t, v]
      pos = dummyPosition ""
      def = definition s' v' pos
      defs = singleton s' def
   in runMaybeT (maybeAliasValue pos t') defs

spec :: Spec
spec = parallel $ do
  -- TODO Test PushBackT instances
  -- TODO Test ReparseT instances

  describe "maybeAliasValue" $ do
    prop "returns matching alias value" $ \s v ->
      fmap (fmap snd) (testMaybeAliasValue s v s) === Just v

    prop "returns nothing for unmatched name" $ \s v t ->
      s /= t ==> testMaybeAliasValue s v t === Nothing

-- vim: set et sw=2 sts=2 tw=78:
