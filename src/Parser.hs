{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
  ( parseArguments
  ) where

import           Control.Applicative (Alternative (..))

-- | Map function to first pair element
mapFirst :: (a -> c) -> (a, b) -> (c, b)
mapFirst f (a, b) = (f a, b)

-- | Parser data structure
newtype Parser s a =
  Parser
    { runParser :: [s] -> Maybe (a, [s])
    }

-- | Functor instance implementation for Parser
instance Functor (Parser s) where
  fmap f (Parser parser) = Parser $ fmap (mapFirst f) . parser

-- | Applicative instance implementation for Parser
instance Applicative (Parser s) where
  pure v = Parser $ \s -> Just (v, s)
  Parser fp <*> Parser ap =
    Parser $ \s -> do
      (f, t) <- fp s
      fmap (mapFirst f) (ap t)

-- | Alternative instance implementation for Parser
instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  Parser a <|> Parser b = Parser $ \s -> a s <|> b s

-- | Monad instance implementation for Parser
instance Monad (Parser s) where
  return = pure
  Parser p >>= f =
    Parser $ \s -> do
      (a, t) <- p s
      runParser (f a) t

-- | Check if parser reached data end
eof :: Parser s ()
eof =
  Parser $ \case
    [] -> Just ((), [])
    _ -> Nothing

-- | Check if streams element satisfies predicate
satisfy :: (s -> Bool) -> Parser s s
satisfy predicate =
  Parser $ \case
    [] -> Nothing
    (a:as) ->
      if predicate a
        then Just (a, as)
        else Nothing

-- | Parse stream object
element :: Eq s => s -> Parser s s
element e = satisfy (e ==)

-- | Arguments Parser
argsParser :: Parser Char [String]
argsParser = (:) <$> argParser <*> tailParser <|> emptyStreamParser
  where
    tailParser :: Parser Char [String]
    tailParser =
      element ' ' *> ((:) <$> argParser <*> tailParser) <|> emptyStreamParser
    emptyStreamParser :: Parser Char [String]
    emptyStreamParser = [] <$ skipWS <* eof
    skipWS :: Parser Char String
    skipWS = many (element ' ')
    argParser :: Parser Char String
    argParser =
      element '"' *> many (satisfy (/= '"')) <* element '"' <|>
      some (satisfy (/= ' '))

-- | Parse arguments of input line
parseArguments :: String -> Maybe [String]
parseArguments str =
  case runParser argsParser str of
    Nothing        -> Nothing
    Just (args, _) -> Just args
