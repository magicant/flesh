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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright   : (C) 2017 WATANABE Yuki
License     : GPL-2
Portability : non-portable (GHC language extensions)

This module defines types and functions for capturing input strings consumed
by parsers.
-}
module Flesh.Language.Parser.Capture (
  CaptureT, runCaptureT, execCaptureT) where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad (MonadPlus, mplus, mzero)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Writer.Strict (WriterT, censor, pass, runWriterT, tell)
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Source.Position

-- | CaptureT modifies MonadInput to allow capturing the input string consumed
-- by the parser.
newtype CaptureT m a = CaptureT {getCaptureT :: WriterT [Positioned Char] m a}

-- | Performs capture, returning the main result and the input string
-- consumed.
runCaptureT :: CaptureT m a -> m (a, [Positioned Char])
runCaptureT = runWriterT . getCaptureT

-- | Performs capture, discarding the main result.
execCaptureT :: Functor m => CaptureT m a -> m [Positioned Char]
execCaptureT = fmap snd . runCaptureT

instance MonadTrans CaptureT where
  lift = CaptureT . lift

instance Functor m => Functor (CaptureT m) where
  fmap f = CaptureT . fmap f . getCaptureT
  a <$ CaptureT b = CaptureT (a <$ b)

instance Applicative m => Applicative (CaptureT m) where
  pure = CaptureT . pure
  CaptureT a <*> CaptureT b = CaptureT (a <*> b)
  CaptureT a  *> CaptureT b = CaptureT (a  *> b)
  CaptureT a <*  CaptureT b = CaptureT (a <*  b)

instance Alternative m => Alternative (CaptureT m) where
  empty = CaptureT empty
  CaptureT a <|> CaptureT b = CaptureT (a <|> b)
  some = CaptureT . some . getCaptureT
  many = CaptureT . many . getCaptureT

instance Monad m => Monad (CaptureT m) where
  CaptureT a >>= f = CaptureT (a >>= getCaptureT . f)
  CaptureT a >> CaptureT b = CaptureT (a >> b)

instance MonadPlus m => MonadPlus (CaptureT m) where
  mzero = CaptureT mzero
  mplus (CaptureT a) (CaptureT b) = CaptureT (mplus a b)

instance MonadInput m => MonadInput (CaptureT m) where
  popChar = CaptureT $ do
    eofOrChar <- popChar
    case eofOrChar of
      Left _ -> pure ()
      Right pc -> tell [pc]
    return eofOrChar
  lookahead = CaptureT . censor (const []) . lookahead . getCaptureT
  peekChar = lift peekChar
  currentPosition = lift currentPosition
  maybeReparse = CaptureT . pass . fmap f . maybeReparse' . getCaptureT
    where f (Nothing, a) = (a, id)
          f (Just _,  a) = (a, const [])

instance MonadInputRecord m => MonadInputRecord (CaptureT m) where
  reverseConsumedChars = lift reverseConsumedChars

instance MonadError e m => MonadError e (CaptureT m) where
  throwError = CaptureT . throwError
  catchError (CaptureT a) f = CaptureT (catchError a (getCaptureT . f))

instance MonadParser m => MonadParser (CaptureT m) where

-- vim: set et sw=2 sts=2 tw=78:
