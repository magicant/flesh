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

module Flesh.Language.Parser.ErrorTestUtil () where

import Flesh.Language.Parser.Error
import Flesh.Source.Position
import Test.QuickCheck (Arbitrary, arbitrary, elements, oneof)

instance Arbitrary Reason where
  arbitrary = oneof [
    return UnknownReason,
    return UnclosedDoubleQuote,
    return UnclosedSingleQuote,
    return MissingExpansionAfterDollar,
    return MissingRedirectionTarget,
    -- return UnclosedHereDocContent ...
    -- return MissingHereDocContents ...
    MissingCommandAfter <$> arbitrary,
    UnclosedSubshell . dummyPosition <$> arbitrary,
    UnclosedGrouping . dummyPosition <$> arbitrary,
    MissingDoForWhile . dummyPosition <$> arbitrary,
    MissingDoForUntil . dummyPosition <$> arbitrary,
    MissingDoneForDo . dummyPosition <$> arbitrary]

instance Arbitrary Error where
  arbitrary = do
    r <- arbitrary
    s <- arbitrary
    return $ Error r $ dummyPosition s

instance Arbitrary Severity where
  arbitrary = elements [Hard, Soft]

-- vim: set et sw=2 sts=2 tw=78:
