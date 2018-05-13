{-
Copyright (C) 2018 WATANABE Yuki <magicant@wonderwand.net>

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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright   : (C) 2018 WATANABE Yuki
License     : GPL-2
Portability : non-portable (GHC language extensions)

This module defines types for reading input for the syntax parser.
-}
module Flesh.Language.Parser.Input (
  -- * MonadLineInput
  MonadLineInput(..),
  -- * LineStandardInputT
  LineStandardInputT(..), LineStandardInput, mapLineStandardInputT,
  -- * MonadInput
  MonadInput(..),
  -- * OneShotInputT
  OneShotInputT(..), OneShotInput, mapOneShotInputT) where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Functor.Identity (Identity)
import Flesh.Source.Position
import System.IO.Error (tryIOError)

-- | Monad that provides line-wise access to the input source based on side
-- effects.
class Monad m => MonadLineInput m where
  -- | Reads the next line from the underlying input source.
  --
  -- Input must be line-wise: Characters must be read exactly until the next
  -- newline is found. The resultant string must end with the newline. If
  -- there is no newline in the remaining input, then the resultant string
  -- must be the entire remaining input and 'nextLine' should not be used any
  -- more.
  nextLine :: m String

-- | IO Monad wrapper that simply reads the standard input.
newtype LineStandardInputT m a =
  LineStandardInputT {getLineStandardInputT :: m a}

type LineStandardInput = LineStandardInputT IO

mapLineStandardInputT :: (m a -> n b)
                      -> LineStandardInputT m a -> LineStandardInputT n b
mapLineStandardInputT f = LineStandardInputT . f . getLineStandardInputT

instance Functor f => Functor (LineStandardInputT f) where
  fmap f = LineStandardInputT . fmap f . getLineStandardInputT
  a <$ LineStandardInputT b = LineStandardInputT (a <$ b)

instance Applicative m => Applicative (LineStandardInputT m) where
  pure = LineStandardInputT . pure
  LineStandardInputT a <*> LineStandardInputT b = LineStandardInputT (a <*> b)
  LineStandardInputT a  *> LineStandardInputT b = LineStandardInputT (a  *> b)
  LineStandardInputT a <*  LineStandardInputT b = LineStandardInputT (a <*  b)

instance Alternative m => Alternative (LineStandardInputT m) where
  empty = LineStandardInputT empty
  LineStandardInputT a <|> LineStandardInputT b = LineStandardInputT (a <|> b)
  some = LineStandardInputT . some . getLineStandardInputT
  many = LineStandardInputT . many . getLineStandardInputT

instance Monad m => Monad (LineStandardInputT m) where
  LineStandardInputT a >>= f =
    LineStandardInputT (a >>= getLineStandardInputT . f)
  LineStandardInputT a >> LineStandardInputT b = LineStandardInputT (a >> b)

instance MonadIO m => MonadLineInput (LineStandardInputT m) where
  nextLine = LineStandardInputT $ liftIO $ m
    where m = do
            errorOrChar <- tryIOError getChar
            case errorOrChar of
              Left _ -> return ""
              Right '\n' -> return "\n"
              Right c -> do
                cs <- m
                return (c:cs)

-- | Monad that provides access to the underlying input character source with
-- arbitrary backtracking. MonadInput is a low-level basis for implementing
-- MonadBuffer.
--
-- Operations on MonadInput depends on an abstract cursor of type @c@.
class Monad m => MonadInput c m | m -> c where
  -- | Reads a character at the given cursor position. If the position is an
  -- end-of-input, only the position is returned in Left. Otherwise, a pair of
  -- a cursor for the next character and the current character is returned in
  -- Right.
  --
  -- Reading a character may have a side effect on an underlying (typically
  -- external) input source. Such side effects must be encoded in the monad.
  -- The monad must remember the characters that have been read from the
  -- underlying input source. When a cursor is used more than once, the second
  -- and all succeeding read operations must return the same result without
  -- side effects.
  readAt :: c -> m (Either Position (c, Positioned Char))
  -- | Returns the position of the given cursor.
  --
  -- 'positionAt' should return the same position data as 'readAt', but must
  -- not have any side effect on the underlying input source.
  positionAt :: c -> m Position

instance MonadInput c m => MonadInput c (ExceptT e m) where
  readAt = lift . readAt
  positionAt = lift . positionAt

instance MonadInput c m => MonadInput c (StateT s m) where
  readAt = lift . readAt
  positionAt = lift . positionAt

-- | Monad wrapper that instantiates MonadInput without an underlying input
-- source. OneShotInputT uses a PositionedString as a cursor.
newtype OneShotInputT m a = OneShotInputT {runOneShotInputT :: m a}

-- | Identity monad as a MonadInput instance.
type OneShotInput = OneShotInputT Identity

-- | Maps the value of OneShotInputT.
mapOneShotInputT :: (m a -> n b) -> OneShotInputT m a -> OneShotInputT n b
mapOneShotInputT f = OneShotInputT . f . runOneShotInputT

instance MonadTrans OneShotInputT where
  lift = OneShotInputT

instance Functor f => Functor (OneShotInputT f) where
  fmap = mapOneShotInputT . fmap
  a <$ OneShotInputT b = OneShotInputT (a <$ b)

instance Applicative m => Applicative (OneShotInputT m) where
  pure = OneShotInputT . pure
  OneShotInputT a <*> OneShotInputT b = OneShotInputT (a <*> b)
  OneShotInputT a  *> OneShotInputT b = OneShotInputT (a  *> b)
  OneShotInputT a <*  OneShotInputT b = OneShotInputT (a <*  b)

instance Monad m => Monad (OneShotInputT m) where
  OneShotInputT a >>= f = OneShotInputT (a >>= runOneShotInputT . f)
  OneShotInputT a >> OneShotInputT b = OneShotInputT (a >> b)

instance Monad m => MonadInput PositionedString (OneShotInputT m) where
  readAt (Nil p) = return $ Left p
  readAt (c :~ ps) = return $ Right (ps, c)
  positionAt (Nil p) = return p
  positionAt ((p, _) :~ _) = return p

-- vim: set et sw=2 sts=2 tw=78:
