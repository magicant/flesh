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

import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Flesh.Language.Alias
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.TestUtil
import Flesh.Source.Position
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

testSubstituteAlias :: Maybe () -- ^ expected result
                    -> String -- ^ expected remaining input
                    -> String -- ^ arbitrary lookahead of source code
                    -> String -- ^ arbitrary name of alias
                    -> String -- ^ arbitrary source code to be substituted
                    -> String -- ^ arbitrary value of alias
                    -> Property
testSubstituteAlias result remainder l s t v =
  let pos = dummyPosition l
      l' = spread pos l
      [s', t', v'] = T.pack <$> [s, t, v]
      pos' = dummyPosition ""
      def = definition s' v' pos'
      defs = M.singleton s' def
      run m = fmap fst (runTesterAlias m defs l')
      subst = ParserT $ runMaybeT $ substituteAlias pos' t'
   in run subst === Right result .&&.
        run (subst >> readAll) === Right remainder

spec :: Spec
spec = do
  describe "substituteAlias" $ do
    prop "substitutes matching alias" $ \l s v ->
      testSubstituteAlias (Just ()) (v ++ l) l s s v

    prop "fails for unmatching alias" $ \l s t ->
      s /= t ==> testSubstituteAlias Nothing l l s t

    prop "prevents recursion" $ \l n v ->
      let lPos = dummyPosition l
          pl = spread lPos l
          [n', v'] = T.pack <$> [n, v]
          dPos = dummyPosition ""
          def = definition n' v' dPos
          defs = M.singleton n' def
          sSit = Alias lPos def
          sFrag = Fragment "" sSit 0
          sPos = Position sFrag 0
          run m = fmap fst (runTesterAlias m defs pl)
          subst = ParserT $ runMaybeT $ substituteAlias sPos n'
       in run subst === Right Nothing .&&. run (subst >> readAll) === Right l

-- vim: set et sw=2 sts=2 tw=78:
