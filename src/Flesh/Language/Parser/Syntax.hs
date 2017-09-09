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

Collection of parser monads that take some input and return abstract syntax
tree, error, and warnings.
-}
module Flesh.Language.Parser.Syntax (
  module Flesh.Language.Syntax,
  HereDocAliasT,
  -- * Tokens
  backslashed, doubleQuoteUnit, doubleQuote, singleQuote, wordUnit, tokenTill,
  normalToken, reservedOrToken, aliasableToken, reservedOrAliasOrToken,
  literal,
  -- * Syntax
  -- ** Basic parts
  redirect, hereDocContent, newlineHD, whitesHD, linebreak,
  -- ** Commands
  simpleCommand, groupingTail, command,
  -- ** Lists
  pipeSequence, pipeline, conditionalPipeline, andOrList, compoundList,
  completeLine) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import qualified Flesh.Language.Alias as Alias
import Flesh.Language.Parser.Alias
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.HereDoc
import Flesh.Language.Parser.Input
import Flesh.Language.Parser.Lex
import Flesh.Language.Syntax
import Flesh.Source.Position
import Numeric.Natural

-- | Combination of 'HereDocT' and 'AliasT'.
type HereDocAliasT m a = HereDocT (AliasT m) a

-- | Parses a backslash-escaped character that is parsed by the given parser.
backslashed :: MonadParser m
            => m (Positioned Char) -> m (Positioned DoubleQuoteUnit)
backslashed m = char '\\' *> fmap (fmap Backslashed) m

-- | Parses a double-quote unit, possibly preceded by line continuations.
--
-- The argument parser is used to parse a backslashed character.
doubleQuoteUnit' :: MonadParser m
                 => m (Positioned Char) -> m (Positioned DoubleQuoteUnit)
doubleQuoteUnit' c = lc $ -- TODO parse expansions
  backslashed c <|> fmap (fmap Char) anyChar

-- | Parses a double-quote unit, possibly preceded by line continuations.
doubleQuoteUnit :: MonadParser m => m (Positioned DoubleQuoteUnit)
doubleQuoteUnit = doubleQuoteUnit' (oneOfChars "\\\"$`")

-- | Parses a pair of double quotes containing any number of double-quote
-- units.
doubleQuote :: MonadParser m => m (Positioned WordUnit)
doubleQuote = do
  let dq = lc (char '"')
  (p, _) <- dq
  let f units = (p, DoubleQuote units)
      closeQuote = setReason UnclosedDoubleQuote dq
  require $ f <$> doubleQuoteUnit `manyTill` closeQuote

-- | Parses a pair of single quotes containing any number of characters.
singleQuote :: MonadParser m => m (Positioned WordUnit)
singleQuote = do
  let sq = char '\''
  (p, _) <- lc sq
  let f chars = (p, SingleQuote chars)
      closeQuote = setReason UnclosedSingleQuote (char '\'')
  require $ f <$> anyChar `manyTill` closeQuote

-- | Parses a word unit.
wordUnit :: MonadParser m => m (Positioned WordUnit)
wordUnit = lc $
  doubleQuote <|> singleQuote <|>
    fmap (fmap Unquoted) (doubleQuoteUnit' anyChar)

-- | @tokenTill end@ parses a token, or non-empty word, until @end@ occurs.
--
-- Note that @end@ consumes the input. Use @'lookahead' end@ to keep @end@
-- unconsumed.
tokenTill :: MonadParser m => m a -> m Token
tokenTill a = notFollowedBy a >> (require $ Token <$> wordUnit `someTill` a)

-- | Parses a normal non-empty token, delimited by 'endOfToken'. Skips
-- whitespaces after the token.
normalToken :: MonadParser m => m Token
normalToken = tokenTill endOfToken <* whites

-- | Returns the token text in Left if the argument word 'isReserved',
-- otherwise the argument itself in Right.
reservedOrToken :: Token -> Either (Positioned T.Text) Token
reservedOrToken t = maybe (Right t) Left $ do
  t' <- tokenText t
  guard $ isReserved t'
  return (p, t')
    where Token ((p, _) :| _) = t -- position of the first word unit

-- | Like 'normalToken', but tries to perform alias substitution on the
-- result.
aliasableToken :: (MonadParser m, MonadReader Alias.DefinitionSet m)
               => AliasT m Token
aliasableToken = AliasT $ do
  t <- normalToken
  let inv Nothing = Just t
      inv (Just ()) = Nothing
      tt = MaybeT $ return $ tokenText t
      pos = fst $ NE.head $ tokenUnits t
   in fmap inv $ runMaybeT $ tt >>= substituteAlias pos
   -- TODO substitute the next token if the current substitute ends with a
   -- blank.

-- | Parses a normal non-empty token followed by optional whitespaces.
-- Reserved words are returned in Left as by 'reservedOrToken'.
-- Non-reserved words are subject to alias substitution.
reservedOrAliasOrToken :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                       => AliasT m (Either (Positioned T.Text) Token)
reservedOrAliasOrToken = AliasT $ do
  textOrToken <- reservedOrToken <$> normalToken
  case textOrToken of
    Left _ -> -- reserved words are not subject to alias substitution
      return $ Just textOrToken
    Right t -> do
      r <- runMaybeT $ do
        tt <- MaybeT $ return $ tokenText t
        let pos = fst $ NE.head $ tokenUnits t
        substituteAlias pos tt
      case r of
        Nothing -> -- no alias substitution performed
          return $ Just textOrToken
        Just () -> -- alias substitution performed!
          return $ Nothing
-- TODO substitute the next token if the current substitute ends with a blank.

-- | Parses an unquoted token that matches the given text.
literal :: MonadParser m => T.Text -> m Token
literal w = normalToken `satisfying` (\t -> tokenText t == Just w)

-- | Parses a redirection operator (@io_redirect@) and returns the raw result.
-- Skips trailing whitespaces.
redirectBody :: MonadParser m
             => m (Maybe Natural, Positioned String, Token)
redirectBody = liftA3 (,,) (optional ioNumber) redirectOperatorToken
  (require (setReason MissingRedirectionTarget normalToken))

yieldHereDoc :: Monad m => HereDocOp -> AccumT m (Filler Redirection)
yieldHereDoc op = do
  yieldOperator op
  return $ do
    c <- popContent
    return $ HereDoc op c

-- | Parses a redirection operator (@io_redirect@). Skips trailing
-- whitespaces.
redirect :: MonadParser m => HereDocT m Redirection
redirect = HereDocT $ do
  pos <- currentPosition
  (maybeFd, (_opPos, op), t) <- lift redirectBody
  -- TODO define 0 and 1 as constants elsewhere
  let defaultFd = if "<" `isPrefixOf` op then 0 else 1
      fd' = fromMaybe defaultFd maybeFd
  case op of
    "<"  -> return $ return $ FileRedirection fd' -- TODO redirection type
    "<>" -> return $ return $ FileRedirection fd' -- TODO redirection type
    "<&" -> return $ return $ FileRedirection fd' -- TODO redirection type
    ">"  -> return $ return $ FileRedirection fd' -- TODO redirection type
    ">>" -> return $ return $ FileRedirection fd' -- TODO redirection type
    ">|" -> return $ return $ FileRedirection fd' -- TODO redirection type
    ">&" -> return $ return $ FileRedirection fd' -- TODO redirection type
    "<<" -> yieldHereDoc $ HereDocOp pos fd' False t
    "<<-" -> yieldHereDoc $ HereDocOp pos fd' True t
    _ -> error $ "unexpected redirection operator " ++ op

hereDocTab :: MonadParser m => Bool -> m ()
hereDocTab tabbed = when tabbed $ void $ many $ char '\t'

hereDocLine :: MonadParser m => Bool -> Bool -> m [Positioned DoubleQuoteUnit]
hereDocLine tabbed isLiteral = do
  hereDocTab tabbed
  fmap NE.toList $ if isLiteral
     then fmap (fmap Char) anyChar `manyTo` nl
     else doubleQuoteUnit' (oneOfChars "\\$`") `manyTo` lc nl
       where nl = fmap (fmap Char) (char '\n')

hereDocDelimiter :: MonadParser m => Bool -> [DoubleQuoteUnit] -> m ()
hereDocDelimiter tabbed delim = do
  hereDocTab tabbed
  _ <- string (show delim)
  _ <- char '\n'
  return ()

hereDocContent :: (MonadParser m, MonadAccum m) => HereDocOp -> m ()
hereDocContent op = do
  ls <- line `manyTill` del
  yieldContent $ concat ls
    where line = hereDocLine tabbed quoted
          del = setReason (UnclosedHereDocContent op) $
            hereDocDelimiter tabbed delim
          tabbed = isTabbed op
          ~(quoted, delim) = unquoteToken (delimiter op)

pendingHereDocContents :: (MonadParser m, MonadAccum m) => m ()
pendingHereDocContents = do
  os <- drainOperators
  sequenceA_ $ hereDocContent <$> os

-- | Parses a newline character. Pending here document contents, if any, are
-- also parsed.
newlineHD :: MonadParser m => HereDocT m (Positioned Char)
newlineHD = lift (lc (char '\n')) <*
  HereDocT (return () <$ require pendingHereDocContents)

-- | 'whites' wrapped in 'HereDocT'.
whitesHD :: MonadParser m => HereDocT m (Maybe [Positioned Char])
whitesHD = lift whites

-- | Parses any number of 'newlineHD' optionally followed by 'whites'.
linebreak :: MonadParser m => HereDocT m ()
linebreak = void (many (newlineHD *> whitesHD))

-- | Parses a simple command. Skips whitespaces after the command.
simpleCommand :: (MonadParser m, MonadReader Alias.DefinitionSet m)
              => HereDocAliasT m Command
simpleCommand = f <$> nonEmptyBody
  where f (ts, as, rs) = SimpleCommand ts as rs
        nonEmptyBody = fRedir <$> redirect' <*> requireHD body <|>
          fToken <$> aliasableToken' <*> requireHD arguments
        body = nonEmptyBody <|> pure ([], [], [])
        arguments = fRedir <$> redirect' <*> requireHD arguments <|>
          fToken <$> normalToken' <*> requireHD arguments <|>
          pure ([], [], [])
        redirect' = mapHereDocT lift redirect
        aliasableToken' = lift aliasableToken
        normalToken' = lift normalToken
        fRedir r (ts, as, rs) = (ts, as, r:rs)
        fToken t (ts, as, rs) = (t:ts, as, rs)
-- TODO global aliases
-- TODO assignments

-- | Parses a grouping except the first open brace, which must have just been
-- parsed.
groupingTail :: (MonadParser m, MonadReader Alias.DefinitionSet m)
             => Position -- ^ Position of the open brace.
             -> HereDocT m (Positioned CompoundCommand)
groupingTail p = f <$> body <* closeBrace
  where f ls = (p, Grouping ls)
        body = setReasonHD (MissingCommandAfter openBraceString) $
          mapHereDocT reparse compoundList
        openBraceString = T.unpack reservedOpenBrace
        closeBrace = lift $ require $ setReason (UnclosedGrouping p) $
          literal reservedCloseBrace

-- | Parses a command.
command :: (MonadParser m, MonadReader Alias.DefinitionSet m)
        => HereDocAliasT m Command
command = simpleCommand -- FIXME support other types of commands

-- | Parses a @pipe_sequence@, a sequence of one or more commands.
pipeSequence :: (MonadParser m, MonadReader Alias.DefinitionSet m)
             => HereDocAliasT m (NonEmpty Command)
pipeSequence = (:|) <$> command <*> many trailer
  where trailer = lift (operatorToken "|") *> linebreak *> requireHD command

-- | Parses a @pipeline@, that is, a 'pipeSequence' optionally preceded by the
-- @!@ reserved word.
pipeline :: (MonadParser m, MonadReader Alias.DefinitionSet m)
         => HereDocAliasT m Pipeline
pipeline =
  lift (literal reservedBang) *> req (make True <$> pipeSequence) <|>
  make False <$> pipeSequence
    where req = setReasonHD (MissingCommandAfter "!") . requireHD
          make = flip Pipeline

-- | Parses an and-or condition token (@&&@ or @||@).
andOrCondition :: MonadParser m => m AndOrCondition
andOrCondition = op <* whites
  where op = do
          (p, o) <- anyOperator
          case o of
            "&&" -> return AndThen
            "||" -> return OrElse
            _ -> failureOfPosition p

-- | Parses a conditional pipeline.
conditionalPipeline :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                    => HereDocAliasT m ConditionalPipeline
conditionalPipeline =
{-
  make <$> lift andOrCondition <* linebreak <*> req pipeline
    where make c p = ConditionalPipeline (c, p)
          req = setReasonHD (MissingCommandAfter undefined) . requireHD
-}
  HereDocT $ do
    c <- andOrCondition
    let req = setReasonHD (MissingCommandAfter (show c)) . requireHD
        make p = ConditionalPipeline (c, p)
    runHereDocT $ linebreak *> (make <$> req pipeline)

-- | Parses a separator operator (@;@ or @&@). Returns True and False if the
-- separator is @&@ and @;@, respectively.
separatorOp :: MonadParser m => m Bool
separatorOp = op <* whites
  where op = do
          (p, o) <- anyOperator
          case o of
            ";" -> return False
            "&" -> return True
            _ -> failureOfPosition p

-- | Parses an and-or list (@and_or@) and 'separator'.
andOrList :: (MonadParser m, MonadReader Alias.DefinitionSet m)
          => HereDocAliasT m AndOrList
andOrList = AndOrList <$> pipeline <*> many conditionalPipeline <*> sep
  where sep = lift $ separatorOp <|> return False

-- | Parses a sequence of one or more and-or lists surrounded by optional
-- linebreaks.
compoundList :: (MonadParser m, MonadReader Alias.DefinitionSet m)
             => HereDocAliasT m (NonEmpty AndOrList)
compoundList = linebreak *> some' (andOrList <* linebreak)

completeLineBody :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                 => HereDocAliasT m [AndOrList]
completeLineBody =
  many andOrList <* requireHD (void newlineHD <|> lift (void eof))

-- | Parses a line.
--
-- 1. A line starts with 'optional' 'whites'.
-- 1. A line may contain any number of and-or lists. The lists must be
--    delimited by @;@ or @&@ except the last @;@ may be omitted.
-- 1. A line must be delimited by a 'newlineHD' or 'eof'.
completeLine :: (MonadParser m, MonadReader Alias.DefinitionSet m)
             => m [AndOrList]
completeLine = do
  _ <- whites
  reparse $ fill completeLineBody

-- vim: set et sw=2 sts=2 tw=78:
