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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : portable

This module implements pretty printing feature.
-}
module Flesh.Language.Pretty (main) where

import Data.Map.Lazy (empty)
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Class
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Error.Print
import Flesh.Language.Parser.Input
import Flesh.Language.Parser.Syntax
import Flesh.Language.Syntax.Print
import Flesh.Source.Position
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

readCompleteLine :: IO (Either Failure [AndOrList])
readCompleteLine = getLineStandardInputT $ runLineInputT li f
  where li = evalParserT s empty 0
        s = notFollowedBy eof *> completeLine
        f = Fragment {code = "", situation = StandardInput, lineNo = 0}

writeCompleteLine :: Either Failure [AndOrList] -> IO ()
writeCompleteLine (Left (s, e)) = do
  hPutStr stderr $ show s ++ " error\n" ++ showsError e ""
  exitFailure
writeCompleteLine (Right aols) = putStr (runPrint (printList aols) "")

-- | Repeatedly reads commands from the standard input and pretty-prints them
-- to the standard output.
main :: IO ()
main = readCompleteLine >>= writeCompleteLine >> main

-- vim: set et sw=2 sts=2 tw=78:
