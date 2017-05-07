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
Portability : non-portable (flexible instances)

This module defines monads and functions dedicated for parsing here documents.

Although the basic syntax of the shell language is defined as a context free
grammar, here documents are an exception. The content of a here document is
separated from its corresponding redirection operator, making the grammar
context dependent. Hence the 'AccumT' and 'Filler' monads. 'AccumT'
accumulates here document operators as the parser scans main syntax of the
source code text and when a newline is reached the parser consumes the
accumulated operators to parse the content and record the results in 'AccumT'
again. 'Filler' is yielded as a temporary result of parsing the main syntax,
where here document contents are yet to be parsed. It receives a list of here
document contents after the entire source code was parsed and yields a final
syntax tree.
-}
module Flesh.Language.Parser.HereDoc (
  -- * Data types
  Operator, Content,
  -- * Accumulator
  AccumState, MonadAccum(..), AccumT, runAccumT, mapAccumT,
  -- * Filler
  Filler, popContent,
  -- * HereDocT
  HereDocT(..), runHereDocT, mapHereDocT, hereDocTAccumT, runHereDocTAccumT,
  fill,
  -- * HereDocAliasT
  HereDocAliasT(..), runHereDocAliasT, mapHereDocAliasT) where

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Flesh.Language.Syntax

-- | Here document redirection operator type.
type Operator = HereDocOp

-- | Here document content type.
type Content = EWord

-- | Monad for managing pending here document contents and parsed contents. It
-- works like a 'StateT' monad where computation interacts with the state of
-- type @(['Operator'], ['Content'])@.
--
-- Parsers accumulate here document operators in this monad, until the newline
-- parser consumes them to parse corresponding here document contents, which
-- are again accumulated in the monad.
class Monad m => MonadAccum m where
  -- | Adds a here document operator to the accumulation so that the
  -- corresponding here document content can be parsed later.
  yieldOperator :: Operator -> m ()
  -- | Returns accumulated here document operators in order, clearing the
  -- accumulation.
  drainOperators :: m [Operator]
  -- | Adds a here document content to the accumulation so that they can later
  -- be filled into corresponding operators.
  yieldContent :: Content -> m ()
  -- | Returns accumulated here document contents in order, clearing the
  -- accumulation.
  drainContents :: m [Content]

-- | Here document operators and contents accumulated in 'AccumT'.
type AccumState = ([Operator], [Content])

-- | Implementation of 'MonadAccum' which accumulates operators and contents
-- in lists using 'StateT'.
--
-- For performance reasons, operators and contents are accumulated in reverse
-- order in the lists.
newtype AccumT m a = AccumT (StateT AccumState m a)

-- | Returns the value of 'AccumT'.
runAccumT :: AccumT m a -> StateT AccumState m a
runAccumT (AccumT m) = m

-- | Directly modifies the value of 'AccumT'.
mapAccumT :: (m (a, AccumState) -> n (b, AccumState))
          -> AccumT m a -> AccumT n b
mapAccumT f = AccumT . mapStateT f . runAccumT

instance Functor m => Functor (AccumT m) where
  fmap f = AccumT . fmap f . runAccumT
  a <$ AccumT b = AccumT (a <$ b)

instance Monad m => Applicative (AccumT m) where
  pure = AccumT . pure
  AccumT a <*> AccumT b = AccumT (a <*> b)
  AccumT a  *> AccumT b = AccumT (a  *> b)
  AccumT a <*  AccumT b = AccumT (a <*  b)

instance Monad m => Monad (AccumT m) where
  return = AccumT . return
  AccumT a >>= f = AccumT (a >>= runAccumT . f)
  AccumT a >> AccumT b = AccumT (a >> b)

instance MonadTrans AccumT where
  lift = AccumT . lift

-- The context (Alternative m, Monad m) is not enough. StateT requires
-- (MonadPlus m) for it to be Alternative.
instance MonadPlus m => Alternative (AccumT m) where
  empty = AccumT empty
  AccumT a <|> AccumT b = AccumT (a <|> b)

instance MonadPlus m => MonadPlus (AccumT m)

instance Monad m => MonadAccum (AccumT m) where
  yieldOperator o =
    AccumT $ state $ \s -> let (os, cs) = s in ((), (o:os, cs))
  drainOperators =
    AccumT $ state $ \s -> let (os, cs) = s in (reverse os, ([], cs))
  yieldContent c =
    AccumT $ state $ \s -> let (os, cs) = s in ((), (os, c:cs))
  drainContents =
    AccumT $ state $ \s -> let (os, cs) = s in (reverse cs, (os, []))

instance MonadState s m => MonadState s (AccumT m) where
  get = lift get
  put = lift . put
  state = lift . state

-- | State monad that composes final parse results by filling an incomplete
-- syntax tree with here document contents.
--
-- This monad is defined as a state monad so that monads can be naturally
-- composed by the applicative and monadic bind operations. Given a list of
-- here document contents, the monad removes first several (possibly zero)
-- elements from the list which are filled to the resulting syntax tree. The
-- remaining elements are passed to a next monad so that they can be filled to
-- remaining part of the syntax tree.
type Filler = State [Content]

-- | Removes and returns one content from the current state.
popContent :: MonadState [Content] m => m Content
popContent = state $ \cs -> (head cs, tail cs)

-- | Combination of 'AccumT' and 'Filler'.
newtype HereDocT m a = HereDocT (AccumT m (Filler a))

-- | Reveals a 'HereDocT' monad.
runHereDocT :: HereDocT m a -> AccumT m (Filler a)
runHereDocT (HereDocT h) = h

-- | Directly modifies the value of 'HereDocT'.
mapHereDocT :: (m (Filler a, AccumState) -> n (Filler b, AccumState))
            -> HereDocT m a -> HereDocT n b
mapHereDocT f = HereDocT . mapAccumT f . runHereDocT

-- | Constructs 'HereDocT'.
hereDocTAccumT :: StateT AccumState m (Filler a) -> HereDocT m a
hereDocTAccumT = HereDocT . AccumT

-- | Reveals a 'HereDocT' monad.
runHereDocTAccumT :: HereDocT m a -> StateT AccumState m (Filler a)
runHereDocTAccumT = runAccumT . runHereDocT

instance Functor m => Functor (HereDocT m) where
  fmap f = HereDocT . fmap (fmap f) . runHereDocT
  a <$ HereDocT b = HereDocT (return a <$ b)

instance Monad m => Applicative (HereDocT m) where
  pure = HereDocT . pure . pure
  HereDocT a <*> HereDocT b = HereDocT ((<*>) <$> a <*> b)

-- The context (Alternative m, Monad m) is not enough. AccumT requires
-- (MonadPlus m) for it to be Alternative.
instance MonadPlus m => Alternative (HereDocT m) where
  empty = HereDocT empty
  HereDocT a <|> HereDocT b = HereDocT (a <|> b)

instance MonadTrans HereDocT where
  lift = HereDocT . lift . fmap return

-- | Fills the accumulated contents into the filler monad, producing the final
-- parse result.
fill :: Monad m => HereDocT m a -> m a
fill m = evalStateT (runAccumT fill') ([], [])
  where fill' = do
          f <- runHereDocT m
          os <- drainOperators
          case os of
            (_:_) -> error "unconsumed here document operators"
            [] -> do
              cs <- drainContents
              let (a, cs') = runState f cs
               in case cs' of
                    (_:_) -> error "unconsumed here document contents"
                    [] -> return a

-- | Combination of 'HereDocT' and 'MaybeT'.
--
-- The maybe monad is intended to represent a parse that may result in alias
-- substitution. If alias substitution occurs on the first token in the
-- parser, the result will be 'Nothing' and the parser must be applied again.
newtype HereDocAliasT m a = HereDocAliasT (HereDocT (MaybeT m) a)

-- | Reveals a 'HereDocAliasT' monad.
runHereDocAliasT :: HereDocAliasT m a -> HereDocT (MaybeT m) a
runHereDocAliasT (HereDocAliasT m) = m

-- | Directly modifies the value of 'HereDocAliasT'.
mapHereDocAliasT :: (m (Maybe (Filler a, AccumState))
                  -> n (Maybe (Filler b, AccumState)))
                 -> HereDocAliasT m a -> HereDocAliasT n b
mapHereDocAliasT f = HereDocAliasT . mapHereDocT g . runHereDocAliasT
  where g = mapMaybeT f

instance Functor m => Functor (HereDocAliasT m) where
  fmap f = HereDocAliasT . fmap f . runHereDocAliasT
  a <$ HereDocAliasT b = HereDocAliasT (a <$ b)

instance Monad m => Applicative (HereDocAliasT m) where
  pure = HereDocAliasT . pure
  HereDocAliasT a <*> HereDocAliasT b = HereDocAliasT (a <*> b)
  HereDocAliasT a  *> HereDocAliasT b = HereDocAliasT (a  *> b)
  HereDocAliasT a <*  HereDocAliasT b = HereDocAliasT (a <*  b)

-- The context (Alternative m, Monad m) is not enough. HereDocT requires
-- (MonadPlus m) for it to be Alternative.
instance MonadPlus m => Alternative (HereDocAliasT m) where
  empty = HereDocAliasT empty
  HereDocAliasT a <|> HereDocAliasT b = HereDocAliasT (a <|> b)

instance MonadTrans HereDocAliasT where
  lift = HereDocAliasT . lift . lift

-- vim: set et sw=2 sts=2 tw=78:
