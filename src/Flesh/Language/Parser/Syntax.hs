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

{-# LANGUAGE ApplicativeDo #-}
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
  -- * Tokens
  backslashed, dollarExpansion, backquoteExpansion, doubleQuoteUnit,
  doubleQuote, singleQuote, wordUnit, tokenTill, neutralToken,
  identifiedToken, literal,
  -- * Redirections and here-documents
  redirect, hereDocContent, newlineHD, whitesHD, linebreak,
  -- * Syntax
  -- ** Commands
  subshell, braceGroupTail, doGroup, forClauseTail, ifClauseTail,
  whileClauseTail, untilClauseTail, command,
  -- ** Lists
  pipeSequence, pipeline, conditionalPipeline, andOrList, compoundList,
  completeLine, program) where

import Control.Applicative (liftA3, many, optional, some, (<|>))
import Control.Monad (join, void, when)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (sequenceA_, toList)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (empty)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Flesh.Language.Alias as Alias
import Flesh.Language.Parser.Alias hiding (name)
import Flesh.Language.Parser.Buffer
import Flesh.Language.Parser.Capture
import Flesh.Language.Parser.Char
import Flesh.Language.Parser.Class
import Flesh.Language.Parser.Error
import Flesh.Language.Parser.HereDoc
import Flesh.Language.Parser.Lex as L
import Flesh.Language.Syntax
import Flesh.Source.Position
import Numeric.Natural (Natural)

-- | Parses a backslash-escaped character that satisfies the given predicate.
backslashed :: MonadParser m
            => (Char -> Bool) -> m (Positioned DoubleQuoteUnit)
backslashed p = char '\\' *> fmap (fmap Backslashed) (satisfy p)

-- | Parses the body of an arithmetic expansion enclosed with a pair of
-- parentheses.
arithmeticParenthesis :: MonadParser m => m [Positioned DoubleQuoteUnit]
arithmeticParenthesis = do
  let singleChar c = fmap (fmap Char) $ lc $ char c
      unit = arithmeticParenthesis <|> fmap return doubleQuoteUnit
  h <- singleChar '('
  t <- require $ unit `manyTo` fmap return (singleChar ')')
  return (h : join (toList t))

-- | Parses an expansion that occurs after a dollar.
dollarExpansionTail :: MonadParser m => m DoubleQuoteUnit
dollarExpansionTail = do
  ~(p, c) <- lc $ setReason MissingExpansionAfterDollar anyChar
  case c of
    -- TODO braced parameter expansion
    -- TODO unbraced parameter expansion
    '(' -> require $ arithTail <|> cmdSubstTail
      where arithTail = arithContent <* lc closeParan
            arithContent = Arithmetic . EWord . fmap (fmap Unquoted) <$>
              try arithmeticParenthesis
            cmdSubstTail = cmdSubstContent <* closeParan
            cmdSubstContent = cs . map snd <$> execCaptureT cmdsubstBody
            cs = Flesh.Language.Syntax.CommandSubstitution
            cmdsubstBody = local (const empty) program
            closeParan = char ')' <|>
              failureOfError (Error UnclosedCommandSubstitution p)
    _ -> failureOfError (Error MissingExpansionAfterDollar p)

-- | Parses an expansion that starts with a dollar.
dollarExpansion :: MonadParser m => m (Positioned DoubleQuoteUnit)
dollarExpansion = do
  ~(p, _) <- char '$'
  dqu <- dollarExpansionTail
  return (p, dqu)

-- | Parses a character contained in a backquote command substitution.
--
-- The argument predicate defines if a character can be backslash-escaped.
backquoteExpansionUnit :: MonadParser m => (Char -> Bool) -> m Char
backquoteExpansionUnit canEscape = lc $ fmap snd $ escapedChar <|> anyChar
  where escapedChar = char '\\' *> satisfy canEscape

-- | Parses a command substitution surrounded with a pair of backquotes.
--
-- The argument predicate defines if a character can be backslash-escaped.
backquoteExpansion :: MonadParser m
                   => (Char -> Bool) -> m (Positioned DoubleQuoteUnit)
backquoteExpansion canEscape = do
  let bq = lc (char '`')
  (p, _) <- bq
  let e = failureOfError (Error UnclosedCommandSubstitution p)
  cs <- require $ backquoteExpansionUnit canEscape `manyTill` (bq <|> e)
  return (p, Backquoted cs)

-- | Parses a double-quote unit, possibly preceded by line continuations.
--
-- The argument predicates define if a character can be backslash-escaped.
doubleQuoteUnit' :: MonadParser m
                 => (Char -> Bool) -- ^ for outside double-quotes
                 -> (Char -> Bool) -- ^ for inside double-quotes
                 -> m (Positioned DoubleQuoteUnit)
doubleQuoteUnit' pOut pIn = lc $
  backslashed pOut <|> backquoteExpansion pIn <|>
    dollarExpansion <|> fmap (fmap Char) anyChar

-- | Parses a double-quote unit, possibly preceded by line continuations.
doubleQuoteUnit :: MonadParser m => m (Positioned DoubleQuoteUnit)
doubleQuoteUnit = doubleQuoteUnit' canEscape canEscape
  where canEscape c = elem c "\"\\$`"

-- | Parses a pair of double quotes containing any number of double-quote
-- units.
doubleQuote :: MonadParser m => m (Positioned WordUnit)
doubleQuote = do
  let dq = lc (char '"')
  (p, _) <- dq
  let e = failureOfError (Error UnclosedDoubleQuote p)
  us <- require $ doubleQuoteUnit `manyTill` (dq <|> e)
  return (p, DoubleQuote us)

-- | Parses a pair of single quotes containing any number of characters.
singleQuote :: MonadParser m => m (Positioned WordUnit)
singleQuote = do
  let sq = char '\''
  (p, _) <- lc sq
  let e = failureOfError (Error UnclosedSingleQuote p)
  cs <- require $ anyChar `manyTill` sq <|> e
  return (p, SingleQuote cs)

-- | Parses a word unit.
wordUnit :: MonadParser m => m (Positioned WordUnit)
wordUnit = lc $
  doubleQuote <|> singleQuote <|>
    fmap (fmap Unquoted) (doubleQuoteUnit' (const True) (`elem` "\\$`"))

-- | @tokenTill end@ parses a token, or non-empty word, until @end@ occurs.
--
-- Note that @end@ consumes the input. Use @'lookahead' end@ to keep @end@
-- unconsumed.
tokenTill :: MonadParser m => m a -> m Token
tokenTill a = notFollowedBy a >> require (Token <$> wordUnit `someTill` a)

-- | Parses a normal non-empty token, delimited by 'endOfToken', without
-- determining its token identifier. Skips whitespaces after the token.
neutralToken :: MonadParser m => m Token
neutralToken = tokenTill endOfToken <* whites

-- | Parses a normal non-empty token followed by optional whitespaces. The
-- token is identified by 'identify'.
identifiedToken :: MonadParser m
                => (Text -> Bool)
                -- ^ Function that tests if a token is reserved.
                -> Bool
                -- ^ Whether the token should be checked for an alias. If the
                -- current position 'isAfterBlankEndingSubstitution', this
                -- argument is ignored.
                -> Bool
                -- ^ Whether the token should be checked for an assignment.
                -> m (Positioned IdentifiedToken)
identifiedToken isReserved' isAliasable isAssignable = do
  iabes <- isAfterBlankEndingSubstitution
  pos <- currentPosition
  it <- maybeReparse $ do
    t <- neutralToken
    identify isReserved' (isAliasable || iabes) isAssignable pos t
  return (pos, it)

aliasableToken :: (MonadParser m, MonadReader Alias.DefinitionSet m)
               => m Token
aliasableToken = do
  t <- identifiedToken (const False) True False
  case snd t of
    Normal t' -> return t'
    _ -> error "unexpected abnormal token"

-- | Parses an unquoted token that matches the given text.
literal :: MonadParser m => Text -> m Token
literal w = neutralToken `satisfying` (\t -> tokenText t == Just w)

-- | Parses a redirection operator (@io_redirect@) and returns the raw result.
-- Skips trailing whitespaces.
redirectBody :: MonadParser m
             => m (Maybe Natural, Positioned String, Token)
redirectBody = liftA3 (,,) (optional ioNumber) redirectOperatorToken
  (require (setReason MissingRedirectionTarget neutralToken))

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
      fileRedir op' = return $ return $ FileRedirection pos fd' op' t
  case op of
    "<"  -> fileRedir In
    "<>" -> fileRedir InOut
    "<&" -> fileRedir DupIn
    ">"  -> fileRedir Out
    ">>" -> fileRedir Append
    ">|" -> fileRedir Clobber
    ">&" -> fileRedir DupOut
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
     else doubleQuoteUnit' (`elem` "\\$`") (`elem` "\"\\$`") `manyTo` lc nl
       where nl = fmap (fmap Char) (char '\n')

hereDocDelimiter :: MonadParser m => Bool -> [DoubleQuoteUnit] -> m ()
hereDocDelimiter tabbed delim = do
  hereDocTab tabbed
  _ <- string (show delim)
  _ <- char '\n'
  return ()

-- | Parses the content of a here-document, including the delimiter.
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

-- | Contents of a simple command.
type SimpleCommand = ([Token], [Assignment], [Redirection])

-- | Parses a sequence of assignment words, normal word tokens, and
-- redirections.
simpleCommandContents :: MonadParser m
                      => Bool -- ^ whether to parse assignments
                      -> HereDocT m SimpleCommand
simpleCommandContents assign = redir <|> token <|> pure ([], [], [])
  where
    redir = do
      r <- redirect
      (ts, as, rs) <- simpleCommandContents assign
      pure (ts, as, r:rs)
    token = HereDocT $ do
      tk <- lift $ identifiedToken (const False) False assign
      case snd tk of
        Reserved     _ -> error "unexpected reserved token"
        L.Assignment a -> runHereDocT $ simpleCommandContentsA a
        Normal       t -> runHereDocT $ simpleCommandContentsW t

-- | Parses a sequence of assignment words, normal word tokens, and
-- redirections and prepends the given assignment to the final result.
simpleCommandContentsA :: MonadParser m
                       => Assignment -> HereDocT m SimpleCommand
simpleCommandContentsA a = do
  (ts, as, rs) <- simpleCommandContents True
  pure (ts, a:as, rs)

-- | Parses a sequence of assignment words, normal word tokens, and
-- redirections and prepends the given word token to the final result.
simpleCommandContentsW :: MonadParser m => Token -> HereDocT m SimpleCommand
simpleCommandContentsW t = do
  (ts, as, rs) <- simpleCommandContents False
  pure (t:ts, as, rs)

-- | Parses a subshell command.
subshell :: MonadParser m => HereDocT m (Positioned CompoundCommand)
subshell = HereDocT $ do
  (pos, _) <- operatorToken "("
  require $ do
    lsFiller <- setReason (MissingCommandAfter "(") $ runHereDocT compoundList
    _ <- setReason (UnclosedSubshell pos) $ operatorToken ")"
    return $ do
      ls <- lsFiller
      return (pos, Subshell ls)

-- | Parses a brace-enclosed grouping except the first open brace, which must
-- have just been parsed.
braceGroupTail :: MonadParser m
               => Position -- ^ Position of the open brace.
               -> HereDocT m (Positioned CompoundCommand)
braceGroupTail p = do
  body <- setReasonHD (MissingCommandAfter openBraceString) compoundList
  _ <- closeBrace
  pure (p, Grouping body)
    where openBraceString = unpack reservedOpenBrace
          closeBrace = lift $ require $ setReason (UnclosedGrouping p) $
            literal reservedCloseBrace

-- | Parses an "in" clause of a for loop.
inClause :: MonadParser m => m [Token]
inClause = literal reservedIn *> many aliasableToken

-- | Parses a 'compoundList' surrounded with the "do" and "done" keywords.
doGroup :: MonadParser m
        => Reason -- ^ Error reason in case "do" is missing
        -> HereDocT m CommandList
doGroup r = HereDocT $ do
  p <- currentPosition
  _ <- setReason r $ literal reservedDo
  require $ do
    body <- setReason (MissingCommandAfter "do") $ runHereDocT compoundList
    _ <- setReason (MissingDoneForDo p) $ literal reservedDone
    return body

-- | Parses a for loop except the first "for" keyword, which must have just
-- been parsed.
forClauseTail :: MonadParser m
              => Position -- ^ Position of the first "for" keyword
              -> HereDocT m (Positioned CompoundCommand)
{-
  The for clause parser is more complex than you might think because
  1. We want to provide an intuitive reason on a syntax error.
  2. We want to avoid back-tracking on 'linebreak' which may require
     re-parsing long here-document contents.
-}
forClauseTail p = do
  name <- lift $ setReason MissingNameAfterFor aliasableToken
  (ws, ls) <- a <|> b
  pure (p, For name ws ls)
    where a = HereDocT $ do
            (p', _) <- lift semicolon
            runHereDocT $ do
              _ <- linebreak
              _ <- noInClause p'
              ls <- doGroup'
              pure (Nothing, ls)
          b = do
            _ <- optional $ newlineList <* noSemicolon
            ws <- lift $ optional $ inClause <* optional semicolon
            _ <- linebreak
            ls <- doGroup'
            pure (ws, ls)
          semicolon = operatorToken ";"
          doGroup' = requireHD $ doGroup $ MissingDoForFor p
          noInClause p' = lift $ optional $ do
            followedBy inClause
            require $ failureOfError $ Error SemicolonBeforeIn p'
          noSemicolon = lift $ require $
            setReason LineBeginningWithSemicolon $ notFollowedBy semicolon

-- | Parses an if command except the first "if" keyword, which must have just
-- been parsed.
ifClauseTail :: MonadParser m
             => Position -- ^ Position of the first "if" keyword
             -> HereDocT m (Positioned CompoundCommand)
ifClauseTail p = do
  c <- setReasonHD (MissingCommandAfter ifString) compoundList
  _ <- setReasonHD (MissingThenForIf p) $ lift $ literal reservedThen
  t <- setReasonHD (MissingCommandAfter thenString) compoundList
  elifthens <- many $ HereDocT $ do
    p' <- currentPosition
    _ <- literal reservedElif
    ct' <- require $ runHereDocT $ do
      c' <- setReasonHD (MissingCommandAfter elifString) compoundList
      _ <- setReasonHD (MissingThenForElif p') $ lift $ literal reservedThen
      t' <- setReasonHD (MissingCommandAfter thenString) compoundList
      pure (c', t')
    pure ct' -- GHC 8.0.2 wants this dummy 'pure'
  els <- optional $ do
    _ <- lift $ literal reservedElse
    e <- requireHD $ setReasonHD (MissingCommandAfter elseString) compoundList
    pure e -- GHC 8.0.2 wants this dummy 'pure'
  _ <- setReasonHD (MissingFiForIf p) $ lift $ literal reservedFi
  pure (p, If ((c, t) :| elifthens) els)
    where ifString = unpack reservedIf
          thenString = unpack reservedThen
          elifString = unpack reservedElif
          elseString = unpack reservedElse

whileUntilClauseTail :: MonadParser m
  => String -- ^ "while" or "until"
  -> (CommandList -> CommandList -> CompoundCommand)
  -> (Position -> Reason) -- ^ Error reason in case "do" is missing
  -> Position -- ^ Position of "while" or "until"
  -> HereDocT m (Positioned CompoundCommand)
whileUntilClauseTail s r e p = do
  cond <- setReasonHD (MissingCommandAfter s) compoundList
  body <- requireHD (doGroup (e p))
  return (p, r cond body)

-- | Parses a "while" command except the first "while" keyword, which must
-- have just been parsed.
whileClauseTail :: MonadParser m
                => Position -- ^ Position of the "while" keyword.
                -> HereDocT m (Positioned CompoundCommand)
whileClauseTail = whileUntilClauseTail "while" While MissingDoForWhile

-- | Parses a "until" command except the first "until" keyword, which must
-- have just been parsed.
untilClauseTail :: MonadParser m
                 => Position -- ^ Position of the "until" keyword.
                 -> HereDocT m (Positioned CompoundCommand)
untilClauseTail = whileUntilClauseTail "until" Until MissingDoForUntil

-- | Parses a compound command except the first token that determines the type
-- of the compound command.
--
-- The first token is not parsed by this parser. It must have been parsed by
-- another parser and must be passed as the argument. This parser fails if the
-- first token does not start a compound command.
compoundCommandTail :: MonadParser m
                    => Positioned Text
                    -> HereDocT m (Positioned CompoundCommand)
compoundCommandTail (p, t)
  | t == reservedOpenBrace = requireHD $ braceGroupTail p
  | t == reservedFor = requireHD $ forClauseTail p
  | t == reservedIf = requireHD $ ifClauseTail p
  | t == reservedWhile = requireHD $ whileClauseTail p
  | t == reservedUntil = requireHD $ untilClauseTail p
  | otherwise = lift $ failureOfPosition p
  -- TODO case

-- | Parses a command.
command :: MonadParser m => HereDocT m Command
command = subshell' <|> simpleCommandStartingWithRedirection <|> other
  where
    subshell' = do
      s <- subshell
      rs <- many redirect
      pure $ CompoundCommand s rs
    simpleCommandStartingWithRedirection = do
      r <- redirect
      (ts, as, rs) <- simpleCommandContents True
      pure $ SimpleCommand ts as (r:rs)
    other = joinHereDocT $ do
      t <- identifiedToken isReserved True True
      pure $ case t of
               (p, Reserved tx) -> do
                 cc <- compoundCommandTail (p, tx)
                 rs <- many redirect
                 pure $ CompoundCommand cc rs
               (_, L.Assignment a) -> sc <$> simpleCommandContentsA a
               (_, Normal tk) -> sc <$> simpleCommandContentsW tk
    sc (ts, as, rs) = SimpleCommand ts as rs
-- TODO parse function definitions

-- | Parses a @pipe_sequence@, a sequence of one or more commands.
pipeSequence :: MonadParser m => HereDocT m (NonEmpty Command)
pipeSequence = do
  h <- command
  t <- many $ lift (operatorToken "|") *> linebreak *> requireHD command
  pure $ h :| t

-- | Parses a @pipeline@, that is, a 'pipeSequence' optionally preceded by the
-- @!@ reserved word.
pipeline :: MonadParser m => HereDocT m Pipeline
pipeline = withBang <|> withoutBang
  where withBang = do
          _ <- lift $ literal reservedBang
          ps <- requireHD $ setReasonHD (MissingCommandAfter bangString)
            pipeSequence
          pure $ Pipeline ps True
        withoutBang = do
          ps <- pipeSequence
          pure $ Pipeline ps False
        bangString = unpack reservedBang

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
conditionalPipeline :: MonadParser m => HereDocT m ConditionalPipeline
conditionalPipeline = HereDocT $ do
  c <- andOrCondition
  runHereDocT $ do
    _ <- linebreak
    p <- requireHD $ setReasonHD (MissingCommandAfter (show c)) pipeline
    pure $ ConditionalPipeline (c, p)

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
andOrList :: MonadParser m => HereDocT m (Bool -> AndOrList)
andOrList = do
  h <- pipeline
  t <- many conditionalPipeline
  pure $ AndOrList h t

-- | Given a separator parser, returns a pair of manyAndOrLists and
-- someAndOrLists.
manySomeAndOrLists :: MonadParser m
                   => HereDocT m Bool
                   -> (HereDocT m [AndOrList],
                       HereDocT m (NonEmpty AndOrList))
manySomeAndOrLists sep = (manyAols, someAols)
  where manyAols = toList <$> someAols <|> pure []
        someAols = cons <$> andOrList <*> maybeSepAndMany
        cons aol (a, aols) = aol a :| aols
        maybeSepAndMany = (,) <$> sep <*> manyAols <|> pure (False, [])

-- | Given a separator parser, parses a possibly empty sequence of and-or
-- lists. The lists must be each separated by the separator. The separator is
-- optional after the last list.
manyAndOrLists :: MonadParser m => HereDocT m Bool -> HereDocT m [AndOrList]
manyAndOrLists = fst . manySomeAndOrLists

-- | Given a separator parser, parses a non-empty sequence of and-or lists.
-- The lists must be each separated by the separator. The separator is
-- optional after the last list.
someAndOrLists :: MonadParser m
               => HereDocT m Bool -> HereDocT m (NonEmpty AndOrList)
someAndOrLists = snd . manySomeAndOrLists

-- | Parses a sequence of one or more and-or lists surrounded by optional
-- linebreaks.
compoundList :: MonadParser m => HereDocT m CommandList
compoundList = linebreak *> someAndOrLists separator

completeLineBody :: MonadParser m => HereDocT m [AndOrList]
completeLineBody =
  manyAndOrLists (lift separatorOp) <*
    requireHD (void newlineHD <|> lift (void eof))

-- | Parses a line.
--
-- 1. A line starts with 'optional' 'whites'.
-- 1. A line may contain any number of and-or lists. The lists must be
--    delimited by @;@ or @&@ except the last @;@ may be omitted.
-- 1. A line must be delimited by a 'newlineHD' or 'eof'.
completeLine :: MonadParser m => m [AndOrList]
completeLine = whites >> fill completeLineBody

-- | Parses an entire program.
program :: MonadParser m => m [AndOrList]
program = fill $
  whitesHD *> linebreak *> manyAndOrLists separator <* lift endOfToken
-- TODO should not return UnknownReason

-- vim: set et sw=2 sts=2 tw=78:
