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
  -- * Accumulator
  AccumT, yieldOperator, drainOperators, yieldContent, contents,
  -- * Filler
  Filler, popContent,
  -- * HereDocT
  HereDocT(..), runHereDocT
  ) where

import Control.Applicative
import Control.Monad.State.Strict

-- | Here document redirection operator type.
type Operator = () -- FIXME

-- | Here document content type.
type Content = () -- FIXME

-- | State monad transformer that is intended to be used as part of a parser
-- monad.
--
-- Parsers accumulate here document operators in this monad, until the newline
-- parser consumes them to parse corresponding here document contents, which
-- are again accumulated in the monad.
--
-- For performance reasons, operators and contents are accumulated in reverse
-- order in the lists.
type AccumT = StateT ([Operator], [Content])

-- | Adds a here document operator to the monad state.
yieldOperator :: MonadState ([Operator], [Content]) m => Operator -> m ()
yieldOperator o = state $ \s -> let (os, cs) = s in ((), (o:os, cs))

-- | Returns accumulated here document operators, erasing the accumulation.
--
-- The returned list of operators is in /normal/ order.
drainOperators :: MonadState ([Operator], [Content]) m => m [Operator]
drainOperators = state $ \s -> let (os, cs) = s in (reverse os, ([], cs))

-- | Adds a here document content to the monad state.
yieldContent :: MonadState ([Operator], [Content]) m => Content -> m ()
yieldContent c = state $ \s -> let (os, cs) = s in ((), (os, c:cs))

-- | Returns accumulated here document contents.
--
-- The returned list of contents is in /normal/ order.
contents :: MonadState ([Operator], [Content]) m => m [Content]
contents = (reverse . snd) <$> get

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

instance Functor m => Functor (HereDocT m) where
  fmap f = HereDocT . fmap (fmap f) . runHereDocT

instance Monad m => Applicative (HereDocT m) where
  pure = HereDocT . pure . pure
  HereDocT a <*> HereDocT b = HereDocT ((<*>) <$> a <*> b)

instance MonadPlus m => Alternative (HereDocT m) where
  empty = HereDocT empty
  HereDocT a <|> HereDocT b = HereDocT (a <|> b)

-- vim: set et sw=2 sts=2 tw=78:
