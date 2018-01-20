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

module Flesh.Language.Parser.LexSpec (spec) where

import Data.List (isPrefixOf)
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Lex
import Flesh.Language.Parser.TestUtil
import Flesh.Source.Position
import Test.Hspec (Spec, context, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===), (==>))

spec :: Spec
spec = parallel $ do
  describe "blank" $ do
    context "does not accept newline" $ do
      expectFailure "\n" blank Soft UnknownReason 0

  describe "comment" $ do
    prop "fails if input does not start with #" $ \s ->
      not ("#" `isPrefixOf` s) && not ("\\\n" `isPrefixOf` s) ==>
        let p = dummyPosition s
            s' = spread p s
         in runFullInputTester comment s' ===
              Left (Soft, Error UnknownReason p)

    prop "parses up to newline" $ \s s' ->
      notElem '\n' s ==>
        let input = '#' : s ++ '\n' : s'
            p = dummyPosition input
            input' = spread p input
            e = runOverrunTester comment input'
            out = unposition $ spread (next p) s
         in e === Just (Right (out, dropP (length s + 1) input'))

    prop "parses up to end-of-file" $ \s ->
      notElem '\n' s ==>
        let input = '#' : s
            p = dummyPosition input
            input' = spread p input
            e = runFullInputTester comment input'
            out = unposition $ spread (next p) s
         in e === Right (out, dropP (length s + 1) input')

    context "skips preceding line continuations" $ do
      expectSuccessEof "\\\n\\\n#" "" comment []

    context "ignores line continuations in comment body" $ do
      expectSuccess "#\\" "\n" (fmap (fmap snd) comment) "\\"

  describe "whites" $ do
    context "does not skip newline" $ do
      expectSuccess "" "\n" ("" <$ whites) ""

  describe "anyOperator" $ do
    context "parses control operator" $ do
      expectSuccessEof ";"  ""  (snd <$> anyOperator) ";"
      expectSuccess    ";"  "&" (snd <$> anyOperator) ";"
      expectSuccess    ";;" ""  (snd <$> anyOperator) ";;"
      expectSuccessEof "|"  ""  (snd <$> anyOperator) "|"
      expectSuccessEof "|"  "&" (snd <$> anyOperator) "|"
      expectSuccess    "||" ""  (snd <$> anyOperator) "||"
      expectSuccessEof "&"  ""  (snd <$> anyOperator) "&"
      expectSuccess    "&"  "|" (snd <$> anyOperator) "&"
      expectSuccess    "&&" ""  (snd <$> anyOperator) "&&"
      expectSuccess    "("  ""  (snd <$> anyOperator) "("
      expectSuccess    ")"  ""  (snd <$> anyOperator) ")"

    context "parses redirection operator" $ do
      expectSuccessEof "<"   ""  (snd <$> anyOperator) "<"
      expectSuccess    "<"   "-" (snd <$> anyOperator) "<"
      expectSuccess    "<"   "|" (snd <$> anyOperator) "<"
      expectSuccessEof "<<"  ""  (snd <$> anyOperator) "<<"
      expectSuccess    "<<"  "|" (snd <$> anyOperator) "<<"
      expectSuccess    "<<-" ""  (snd <$> anyOperator) "<<-"
      expectSuccess    "<>"  ""  (snd <$> anyOperator) "<>"
      expectSuccess    "<&"  ""  (snd <$> anyOperator) "<&"
      expectSuccessEof ">"   ""  (snd <$> anyOperator) ">"
      expectSuccess    ">"   "-" (snd <$> anyOperator) ">"
      expectSuccess    ">>"  ""  (snd <$> anyOperator) ">>"
      expectSuccess    ">|"  ""  (snd <$> anyOperator) ">|"
      expectSuccess    ">&"  ""  (snd <$> anyOperator) ">&"

    context "skips line continuations" $ do
      expectSuccess "\\\n&\\\n\\\n&"  "\\\n" (snd <$> anyOperator) "&&"
      expectSuccess "\\\n<\\\n<\\\n-" "\\\n" (snd <$> anyOperator) "<<-"

  describe "operator" $ do
    context "parses argument control operator" $ do
      expectSuccessEof ";"  ""  (snd <$> operator ";")  ";"
      expectSuccess    ";"  "&" (snd <$> operator ";")  ";"
      expectSuccess    ";;" ""  (snd <$> operator ";;") ";;"
      expectSuccessEof "|"  ""  (snd <$> operator "|")  "|"
      expectSuccess    "|"  "&" (snd <$> operator "|")  "|"
      expectSuccess    "||" ""  (snd <$> operator "||") "||"
      expectSuccessEof "&"  ""  (snd <$> operator "&")  "&"
      expectSuccess    "&"  "|" (snd <$> operator "&")  "&"
      expectSuccess    "&&" ""  (snd <$> operator "&&") "&&"
      expectSuccess    "("  ""  (snd <$> operator "(")  "("
      expectSuccess    ")"  ""  (snd <$> operator ")")  ")"

    context "parses argument redirection operator" $ do
      expectSuccessEof "<"   ""  (snd <$> operator "<")   "<"
      expectSuccess    "<"   "-" (snd <$> operator "<")   "<"
      expectSuccess    "<"   "|" (snd <$> operator "<")   "<"
      expectSuccessEof "<<"  ""  (snd <$> operator "<<")  "<<"
      expectSuccess    "<<"  "|" (snd <$> operator "<<")  "<<"
      expectSuccess    "<<-" ""  (snd <$> operator "<<-") "<<-"
      expectSuccess    "<>"  ""  (snd <$> operator "<>")  "<>"
      expectSuccess    "<&"  ""  (snd <$> operator "<&")  "<&"
      expectSuccessEof ">"   ""  (snd <$> operator ">")   ">"
      expectSuccess    ">"   "-" (snd <$> operator ">")   ">"
      expectSuccess    ">>"  ""  (snd <$> operator ">>")  ">>"
      expectSuccess    ">|"  ""  (snd <$> operator ">|")  ">|"
      expectSuccess    ">&"  ""  (snd <$> operator ">&")  ">&"

    context "rejects operator other than argument" $ do
      expectFailure    ";;"  (operator ";")  Soft UnknownReason 0
      expectFailure    "&&"  (operator "&")  Soft UnknownReason 0
      expectFailure    "||"  (operator "|")  Soft UnknownReason 0
      expectFailureEof "<<"  (operator "<")  Soft UnknownReason 0
      expectFailure    "<<-" (operator "<")  Soft UnknownReason 0
      expectFailure    "<<-" (operator "<<") Soft UnknownReason 0
      expectFailure    "<>"  (operator "<<") Soft UnknownReason 0
      expectFailure    ">>"  (operator ">")  Soft UnknownReason 0
      expectFailure    ">|"  (operator ">")  Soft UnknownReason 0
      expectFailure    ">&"  (operator ">")  Soft UnknownReason 0
      expectFailure    "\\\n>&" (operator ">") Soft UnknownReason 2

  describe "ioNumber" $ do
    context "parses digits followed by < or >" $ do
      expectSuccess "1" "<" ioNumber 1
      expectSuccess "20" ">" ioNumber 20
      expectSuccess "123" ">" ioNumber 123

    context "rejects non-digits" $ do
      expectFailure "<" ioNumber Soft UnknownReason 0
      expectFailure "a" ioNumber Soft UnknownReason 0
      expectFailure " " ioNumber Soft UnknownReason 0

    context "rejects digits not followed by < or >" $ do
      expectFailureEof "0" ioNumber Soft UnknownReason 1
      expectFailure    "1-" ioNumber Soft UnknownReason 1
      expectFailure    "23 " ioNumber Soft UnknownReason 2

    context "skips line continuations" $ do
      expectSuccess "\\\n\\\n1" "<" ioNumber 1
      expectSuccess "1" "\\\n\\\n<" ioNumber 1
      expectFailure "1\\\n-" ioNumber Soft UnknownReason 3

-- vim: set et sw=2 sts=2 tw=78:
