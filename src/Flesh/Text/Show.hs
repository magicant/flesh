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
{-# LANGUAGE Safe #-}

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : non-portable (flexible contexts)

This module extends the Text.Show module, adding some useful functions.
-}
module Flesh.Text.Show (
  PrintT, Print, execPrintT, execPrint, tell', showSpace') where

import Control.Monad.Writer.Lazy (
  Endo(Endo), MonadWriter, WriterT, appEndo, runWriterT, tell)
import Data.Functor.Identity (Identity, runIdentity)

type PrintT = WriterT (Endo String)
type Print = PrintT Identity

execPrintT :: Functor m => PrintT m a -> m ShowS
execPrintT = fmap appEndo . fmap snd . runWriterT

execPrint :: Print a -> ShowS
execPrint = runIdentity . execPrintT

-- | Utility for yielding a ShowS function
tell' :: MonadWriter (Endo String) m => ShowS -> m ()
tell' = tell . Endo

-- | Utility for Show instances
showSpace' :: MonadWriter (Endo String) m => m ()
showSpace' = tell' $ showChar ' '

-- vim: set et sw=2 sts=2 tw=78:
