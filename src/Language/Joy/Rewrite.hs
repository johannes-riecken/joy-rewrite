{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Language.Joy.Rewrite
  ( rewrite
  , tokenize
  ) where
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Loops
import Data.Foldable (asum)
import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Test.Hspec
import Text.Parsec hiding ((<|>), many)

type Error = String

-- A rewrite rule matches a pattern and tries to rewrite it with the
-- replacement. If there is a condition attached, the rewriting will only happen
-- if the rewrite given in the condition can happen based on known rewrite
-- rules.
data Rule = Rule
  { pat       :: [RuleExpr String]
  , repl      :: [RuleExpr String]
  , condition :: Maybe Condition
  }
  deriving Show

data Condition = Condition
  { premise    :: [RuleExpr String]
  , conclusion :: [RuleExpr String]
  }
  deriving Show

convertError :: Either ParseError a -> Either Error a
convertError (Left x) = Left $ show x
convertError (Right x) = Right x

-- Construct a Rule by parsing a string like "a b swap => b a"
mkRule :: String -> Either Error Rule
mkRule xs = do
  xs' <- tokenizeRule xs
  if CondSep `elem` xs'
    then do
      (v,w) <- cut CondSep xs'
      (a,b) <- cut RuleSep v
      (c,d) <- cut RuleSep w
      pure $ Rule a b (Just $ Condition c d)
    else do
      (a, b) <- tokenizeFromTo xs
      pure $ Rule a b Nothing where

  tokenizeFromTo :: String -> Either Error ([RuleExpr String], [RuleExpr String])
  tokenizeFromTo xs = tokenizeRule xs >>= cut RuleSep

  cut :: Eq a => a -> [a] -> Either Error ([a], [a])
  cut _ [] = Left "tried to cut empty list"
  cut sep xs = Right . second tail . break (== sep) $ xs

  tokenizeRule :: String -> Either Error [RuleExpr String]
  tokenizeRule = convertError . parse parser ""   where
    parser =
      let metaVar     = MetaVar <$> fmap pure lower
          metaListVar = MetaListVar <$> fmap pure upper
          var = Var <$> (
                string "["
            <|> string "]"
            <|> do
                  x <- string "="
                  notFollowedBy (string ">")
                  pure x
            <|> many1 digit
            <|> ((:) <$> lower <*> many1 alphaNum)
            )
          ruleSep = RuleSep <$ string "=>"
          condSep = CondSep <$ string ":-"
      in  sepBy (try var <|> metaVar <|> metaListVar <|> ruleSep <|> condSep) spaces

-- An atom in a Rule
data RuleExpr a =
      Var a -- matches with one exact atom, e.g. "swap"
    | MetaVar a -- can match with any single atom and assign it to the variable name
    | MetaListVar a -- matches a list of atoms and assigns it to the variable name
    | RuleSep -- "=>"
    | CondSep -- ":-" (read as "if")
    deriving (Show, Eq, Ord)

-- Stores the associations between rule variables and their matched Joy code
type RuleMap a = Map (RuleExpr a) [a]

-- Given a list of rewrite rules and Joy code, apply the rules and get the resulting
-- list of tokens
rewrite :: [String] -> String -> Either Error [String]
rewrite ruleStrs code = do
  rs <- traverse mkRule ruleStrs
  rewrite' rs (tokenize code) where

  rewrite' :: [Rule] -> [String] -> Either Error [String]
  rewrite' rules = fmap concat . unfoldrM
    (\case
      [] -> Right Nothing
      xxs@(x : xs) ->
        Right $ asum (map (\r -> case matchPat r xxs of
                    Right (s, m) -> do
                      case condition r of
                        Nothing -> Just (concatMap (\k -> case k of
                            Var k' -> M.findWithDefault [k'] k m
                            _      -> m M.! k
                            )
                            (repl r)
                          , drop (length s) xxs
                          )
                        Just c -> case rewrite' rules (apply m $ premise c) of
                          Right x' -> case matchConclusion r x' of
                            Right (_, m') -> Just (replTokens, drop (length s) xxs)
                              where replTokens = apply (M.union m m') $ repl r
                            _ -> Nothing
                          _ -> Nothing
                    _ -> Nothing
                  ) rules
                )
          <|> Just ([x], xs)
    )

  -- Applies the stored rewrite associations to Joy code.
  apply :: RuleMap String -> [RuleExpr String] -> [String]
  apply m = (=<<) (\x -> M.findWithDefault [(\case (Var x') -> x') x] x m)

  -- Matches the pattern part of a rule. I didn't find anything like `runState`
  -- for Parsec, so I return both the value and the state in the same tuple
  -- format.
  matchPat :: Rule -> [String] -> Either Error ([String], RuleMap String)
  matchPat r = convertError . runParser
    (do
      x <- mkParser (pat r)
      y <- getState
      pure (x, y)
    )
    M.empty
    ""

  matchConclusion
    :: Rule -> [String] -> Either Error ([String], RuleMap String)
  matchConclusion r xs = do
    conc <- conclusion <$> maybeToParseResult (condition r)
    convertError $ runParser
      (do
        x <- mkParser conc
        y <- getState
        pure (x, y)
      )
      M.empty
      ""
      xs where
    -- I think this is also a clear case where I should use a more generic error
    -- type.
    maybeToParseResult :: Maybe a -> Either Error a
    maybeToParseResult = maybe (Left "") Right

  mkParser :: [RuleExpr String] -> Parsec [String] (RuleMap String) [String]
  mkParser = foldr
    (\x acc -> case x of
      Var x' -> do
        modifyState $ M.insert (Var x') [x']
        (:) <$> char' x' <*> acc
      MetaVar x' -> do
        -- `nonBracket'` and `nonTrueNonBracket'` are hacks because I couldn't
        -- figure out how to reluctantly match as little as possible. I think
        -- the parser-combinators package might contain what I need.
        x'' <- nonBracket'
        modifyState $ M.insert (MetaVar x') [x'']
        (x'' :) <$> acc
      MetaListVar x' -> do
        x'' <- many nonTrueNonBracket'
        modifyState $ M.insert (MetaListVar x') x''
        (x'' ++) <$> acc
      RuleSep -> error "assertion error: RuleSep found by mkParser"
      CondSep -> error "assertion error: CondSep found by mkParser"
    )
    (pure [])

tokenize :: String -> [String]
tokenize = filter (not . null) . tokenize' where

  tokenize' :: String -> [String]
  tokenize' = unfoldr
    (\case
      []         -> Nothing
      ('[' : xs) -> Just ("[", xs)
      (']' : xs) -> Just ("]", xs)
      xxs        -> Just (a, dropWhile (== ' ') b)
        where (a, b) = break (`elem` "[] ") xxs
    )

-- I ended these with an apostrophe, because they work on tokens, not Strings.
satisfy' :: (Eq a, Show a) => (a -> Bool) -> Parsec [a] u a
satisfy' p = tokenPrim showTok posFromTok testTok
 where
  showTok t = show t
  posFromTok pos _ _ = incSourceColumn pos 1
  testTok t = mfilter p (Just t)

char' :: (Eq a, Show a) => a -> Parsec [a] u a
char' x = satisfy' (== x) <?> show x

nonBracket' :: Parsec [String] u String
nonBracket' = satisfy' (`notElem` ["[", "]"])

nonTrueNonBracket' :: Parsec [String] u String
nonTrueNonBracket' = satisfy' (`notElem` ["[", "]", "true"])
