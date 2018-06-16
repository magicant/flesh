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
Portability : non-portable (GHC language extensions)

This module defines types that describe parse errors for the shell language
and a monad transformer that injects parse error handling into another monad.
-}
module Flesh.Language.Parser.Class (
  -- * The 'MonadParser' class
  MonadParser, failure, failureOfReason, satisfying, satisfyingP,
  notFollowedBy, some',
  -- * The 'ParserT' monad transformer
  ParserT(..), evalParserT) where

import Control.Applicative (Alternative, empty, many, (<|>))
import Control.Monad (MonadPlus, join)
import Control.Monad.Except (
  ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.Reader (
  MonadReader, ReaderT, ask, local, reader, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.List.NonEmpty (NonEmpty((:|)))
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Buffer
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.Input
import Flesh.Source.Position

-- | Collection of properties required for basic parser implementation.
--
-- @MonadParser@ is a subclass of the input and error handling monads,
-- supporting the basic behavior of the parser. It is also an instance of
-- 'Alternative' and 'MonadPlus', where
--
--  * 'empty' and 'mzero' are equal to 'failure'; and
--  * '<|>' and 'mplus' behave like 'catchError' but they only catch 'Soft'
--    failures.
class (
  MonadReparse m, MonadRecord m, MonadReader DefinitionSet m,
  MonadError Failure m, MonadPlus m)
  => MonadParser m

-- | Failure of unknown reason at the current position.
failure :: MonadParser m => m a
failure = currentPosition >>= failureOfPosition

-- | Failure of the given reason at the current position.
failureOfReason :: MonadParser m => Reason -> m a
failureOfReason r = do
  p <- currentPosition
  failureOfError (Error r p)

-- | @satisfying m p@ behaves like @m@ but fails if the result of @m@ does not
-- satisfy predicate @p@. This is analogous to @'flip' 'mfilter'@.
satisfying :: MonadParser m => m a -> (a -> Bool) -> m a
satisfying m p = do
  pos <- currentPosition
  r <- m
  if p r then return r else failureOfPosition pos

-- | 'satisfyingP' is like 'satisfying' but applies the predicate to the
-- second item of the pair.
satisfyingP :: MonadParser m
            => m (Positioned a) -> (a -> Bool) -> m (Positioned a)
-- satisfyingP m p = satisfying m (p . snd)
-- This would return a better error position:
satisfyingP m p = do
  posr@(pos, r) <- m
  if p r then return posr else failureOfPosition pos

-- | @notFollowedBy m@ succeeds if @m@ fails. If @m@ succeeds, it is
-- equivalent to 'failure'.
notFollowedBy :: MonadParser m => m a -> m ()
notFollowedBy m = do
  pos <- currentPosition
  let m' = m >> return (failureOfPosition pos)
  join $ catchError m' (const $ return $ return ())

-- | @some' a@ is like @some a@, but returns a NonEmpty list.
some' :: Alternative m => m a -> m (NonEmpty a)
some' a = (:|) <$> a <*> many a

instance MonadParser m => MonadParser (ReparseT m)

-- | Combination of monads that constitute most part of a parser monad.
newtype ParserT c m a = ParserT {getParserT ::
  ReaderT DefinitionSet (RecordT (ReparseT
    (PushBackT (CursorT c (ExceptT Failure m))))) a}

-- | Returns the value of 'ParserT'.
evalParserT :: Monad m
            => ParserT c m a -> DefinitionSet -> c -> m (Either Failure a)
evalParserT (ParserT m) ds c = m3
  where m1 = runReaderT m ds
        m2 = evalPushBackT $ evalReparseT $ evalRecordT m1
        m3 = runExceptT $ evalCursorT m2 c

instance Functor m => Functor (ParserT c m) where
  fmap f = ParserT . fmap f . getParserT
  a <$ ParserT b = ParserT (a <$ b)

instance (Applicative m, Monad m) => Applicative (ParserT c m) where
  pure = ParserT . pure
  ParserT a <*> ParserT b = ParserT (a <*> b)
  ParserT a  *> ParserT b = ParserT (a  *> b)
  ParserT a <*  ParserT b = ParserT (a <*  b)

instance Monad m => Monad (ParserT c m) where
  return = ParserT . return
  ParserT a >>= f = ParserT (a >>= getParserT . f)
  ParserT a >> ParserT b = ParserT (a >> b)

instance MonadTrans (ParserT c) where
  lift = ParserT . lift . lift . lift . lift . lift . lift

instance MonadInput c m => MonadBuffer (ParserT c m) where
  popChar = ParserT popChar
  lookahead = ParserT . lookahead . getParserT
  peekChar = ParserT peekChar
  currentPosition = ParserT currentPosition

instance Monad m => MonadReparse (ParserT c m) where
  maybeReparse = ParserT . maybeReparse . getParserT
  maybeReparse' = ParserT . maybeReparse' . getParserT

instance MonadInput c m => MonadRecord (ParserT c m) where
  reverseConsumedChars = ParserT reverseConsumedChars

instance Monad m => MonadReader DefinitionSet (ParserT c m) where
  ask = ParserT ask
  local f = ParserT . local f . getParserT
  reader = ParserT . reader

instance Monad m => MonadError Failure (ParserT c m) where
  throwError = ParserT . throwError
  catchError (ParserT m) f = ParserT (catchError m (getParserT . f))

instance MonadInput c m => Alternative (ParserT c m) where
  empty = failure
  a <|> b =
    a `catchError` handle
      where handle (Soft, _) = b
            handle e = throwError e

instance MonadInput c m => MonadPlus (ParserT c m)

instance MonadInput c m => MonadParser (ParserT c m)

-- vim: set et sw=2 sts=2 tw=78:
