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
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.TestUtil
import Flesh.Source.Position
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (===), (==>))

testSubstituteAlias :: String -- ^ expected remaining input
                    -> String -- ^ arbitrary lookahead of source code
                    -> String -- ^ arbitrary name of alias
                    -> String -- ^ arbitrary source code to be substituted
                    -> String -- ^ arbitrary value of alias
                    -> Property
testSubstituteAlias remainder l s t v =
  let pos = dummyPosition l
      l' = spread pos l
      [s', t', v'] = pack <$> [s, t, v]
      pos' = dummyPosition ""
      def = definition s' v' pos'
      defs = singleton s' def
      run m = fst <$> runFullInputTesterAlias m defs l'
      subst = ParserT $ substituteAlias pos' t'
   in run (subst >> readAll) === Right remainder

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
spec = do
  describe "substituteAlias" $ do
    prop "substitutes matching alias" $ \l s v ->
      testSubstituteAlias (v ++ l) l s s v

    prop "fails for unmatching alias" $ \l s t ->
      s /= t ==> testSubstituteAlias l l s t

    prop "prevents recursion" $ \l n v ->
      let lPos = dummyPosition l
          pl = spread lPos l
          [n', v'] = pack <$> [n, v]
          dPos = dummyPosition ""
          def = definition n' v' dPos
          defs = singleton n' def
          sSit = Alias lPos def
          sFrag = Fragment "" sSit 0
          sPos = Position sFrag 0
          run m = fmap fst (runFullInputTesterAlias m defs pl)
          subst = ParserT $ substituteAlias sPos n'
       in run (subst >> readAll) === Right l

  describe "maybeAliasValue" $ do
    prop "returns matching alias value" $ \s v ->
      fmap (fmap snd) (testMaybeAliasValue s v s) === Just v

    prop "returns nothing for unmatched name" $ \s v t ->
      s /= t ==> testMaybeAliasValue s v t === Nothing

-- vim: set et sw=2 sts=2 tw=78:
