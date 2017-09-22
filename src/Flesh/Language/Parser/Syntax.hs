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
  normalToken, reservedOrToken, reservedOrAliasOrToken, literal,
  -- * Syntax
  -- ** Basic parts
  redirect, hereDocContent, newlineHD, whitesHD, linebreak,
  -- ** Commands
  subshell, groupingTail, command,
  -- ** Lists
  pipeSequence, pipeline, conditionalPipeline, andOrList, compoundList,
  completeLine) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.List hiding (words)
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
import Prelude hiding (words)

-- | Combination of 'HereDocT' and 'AliasT'.
type HereDocAliasT m a = HereDocT (AliasT m) a

joinAliasHereDocAliasT :: MonadParser m
                       => AliasT m (HereDocAliasT m a) -> HereDocAliasT m a
joinAliasHereDocAliasT = HereDocT . join . lift . fmap runHereDocT

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

-- | Parses one or more 'newlineHD's optionally followed by 'whites'.
newlineList :: MonadParser m => HereDocT m ()
newlineList = void (some (newlineHD *> whitesHD))

-- | Parses any number of 'newlineHD' optionally followed by 'whites'.
linebreak :: MonadParser m => HereDocT m ()
linebreak = void (many (newlineHD *> whitesHD))

-- | Parses a sequence of (non-assignment) words and redirections.
--
-- Returns a triple of parsed tokens, empty list, and redirections.
simpleCommandArguments :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                       => HereDocAliasT m ([Token], [a], [Redirection])
simpleCommandArguments = arg <*> simpleCommandArguments <|> pure ([], [], [])
  where arg = consRedir <$> redirect' <|> consToken <$> normalToken'
        consRedir r (ts, as, rs) = (ts, as, r:rs)
        consToken t (ts, as, rs) = (t:ts, as, rs)
        redirect' = mapHereDocT lift redirect
        normalToken' = lift normalToken
-- TODO global aliases

-- | Parses a simple command but the first token.
--
-- The first token of the simple command is not parsed by this parser. It must
-- have been parsed by another parser and must be passed as the argument.
simpleCommandTail :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                  => Token -> HereDocAliasT m Command
simpleCommandTail t1 = toCommand . consToken t1 <$> simpleCommandArguments
  where toCommand (ts, as, rs) = SimpleCommand ts as rs
        consToken t (ts, as, rs) = (t:ts, as, rs)
-- TODO assignments

subshell :: (MonadParser m, MonadReader Alias.DefinitionSet m)
         => HereDocAliasT m (Positioned CompoundCommand)
subshell = HereDocT $ do
  (pos, _) <- operatorToken "("
  require $ do
    lsFiller <- setReason (MissingCommandAfter "(") $ runHereDocT compoundList
    _ <- setReason (UnclosedSubshell pos) $ operatorToken ")"
    return $ do
      ls <- lsFiller
      return (pos, Subshell ls)

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

-- | Parses a compound command except the first token that determines the type
-- of the compound command.
--
-- The first token is not parsed by this parser. It must have been parsed by
-- another parser and must be passed as the argument. This parser fails if the
-- first token does not start a compound command.
compoundCommandTail :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                    => Positioned T.Text
                    -> HereDocT m (Positioned CompoundCommand)
compoundCommandTail (p, t)
  | t == reservedOpenBrace = requireHD $ groupingTail p
  | otherwise = lift $ failureOfPosition p
  -- TODO if, while, until, for, case

-- | Parses a command.
command :: (MonadParser m, MonadReader Alias.DefinitionSet m)
        => HereDocAliasT m Command
command =
  -- First, check if this is a subshell.
  CompoundCommand <$> subshell <*> many redirect' <|>
  -- Next, if the command begins with a redirection, it is a simple command.
  toCommand <$> (consRedir <$> redirect' <*> simpleCommand') <|>
  -- Otherwise, the first token can be a reserved word or alias.
  joinAliasHereDocAliasT (compoundOrSimple <$> reservedOrAliasOrToken)
    where toCommand (ts, as, rs) = SimpleCommand ts as rs
          consRedir r (ts, as, rs) = (ts, as, r:rs)
          redirect' = mapHereDocT lift redirect
          simpleCommand' = simpleCommandArguments -- TODO parse assignments
          compoundOrSimple = either compoundCommandTail' simpleCommandTail
            -- :: Either (Positioned T.Text) Token -> HereDocAliasT m Command
          compoundCommandTail' t = CompoundCommand <$>
            mapHereDocT lift (compoundCommandTail t) <*> many redirect
            -- :: Positioned T.Text -> HereDocAliasT m Command
-- TODO parse function definitions

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

-- | Parses a separator operator (@;@ or @&@) and/or newlines. Returns True
-- and False if the separator is @&@ and @;@, respectively.
separator :: MonadParser m => HereDocT m Bool
separator = lift separatorOp <* linebreak <|> False <$ newlineList

-- | Parses an and-or list (@and_or@), not including a trailing separator.
andOrList :: (MonadParser m, MonadReader Alias.DefinitionSet m)
          => HereDocAliasT m (Bool -> AndOrList)
andOrList = AndOrList <$> pipeline <*> many conditionalPipeline
  --where sep = lift $ separatorOp <|> return False

-- | Given a separator parser, returns a pair of 'manyAndOrLists' and
-- 'someAndOrLists'.
manySomeAndOrLists :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                   => HereDocAliasT m Bool
                   -> (HereDocAliasT m [AndOrList],
                       HereDocAliasT m (NonEmpty AndOrList))
manySomeAndOrLists sep = (manyAols, someAols)
  where manyAols = toList <$> someAols <|> pure []
        someAols = cons <$> andOrList <*> maybeSepAndMany
        cons aol (a, aols) = aol a :| aols
        maybeSepAndMany = (,) <$> sep <*> manyAols <|> pure (False, [])

-- | Given a separator parser, parses a possibly empty sequence of and-or
-- lists. The lists must be each separated by the separator. The separator is
-- optional after the last list.
manyAndOrLists :: (MonadParser m, MonadReader Alias.DefinitionSet m)
               => HereDocAliasT m Bool -> HereDocAliasT m [AndOrList]
manyAndOrLists = fst . manySomeAndOrLists

-- | Given a separator parser, parses a non-empty sequence of and-or lists.
-- The lists must be each separated by the separator. The separator is
-- optional after the last list.
someAndOrLists :: (MonadParser m, MonadReader Alias.DefinitionSet m)
               => HereDocAliasT m Bool -> HereDocAliasT m (NonEmpty AndOrList)
someAndOrLists = snd . manySomeAndOrLists

-- | Parses a sequence of one or more and-or lists surrounded by optional
-- linebreaks.
compoundList :: (MonadParser m, MonadReader Alias.DefinitionSet m)
             => HereDocAliasT m (NonEmpty AndOrList)
compoundList = linebreak *> someAndOrLists separator

completeLineBody :: (MonadParser m, MonadReader Alias.DefinitionSet m)
                 => HereDocAliasT m [AndOrList]
completeLineBody =
  manyAndOrLists (lift separatorOp) <*
    requireHD (void newlineHD <|> lift (void eof))

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
