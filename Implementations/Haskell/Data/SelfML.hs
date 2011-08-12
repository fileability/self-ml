module Data.SelfML (
  Tree(Node, Terminal),
  Forest,
  readSML,
  showSML,
  prettyPrintSML) where

import Control.Applicative
import Data.Char (isSpace)
import Data.Traversable
import Text.Parsec hiding (many, (<|>), spaces)

-- | Like Data.Tree.Tree, but provides a singleton case. In essence, a conc list.
data Tree a = Node a (Forest a) | Terminal a
            deriving (Eq, Read, Show)

type Forest a = [Tree a]

-- | Reads SelfML out of a string.
readSML :: String -> Either ParseError (Forest String)
readSML = parse (smlForest <* eof) ""

-- | Serializes into a SelfML string.
showSML :: Forest String -> String
showSML = undefined

-- | Serializes into a SelfML string with indentation to make it more human-readable.
prettyPrintSML :: Forest String -> String
prettyPrintSML = undefined

--- parser ---

smlForest :: Parsec [Char] () (Forest String)
smlTree   :: Parsec [Char] () (Tree   String)

smlForest       = spaces (smlTree `sepEndBy` whitespace)
smlTree         = Terminal <$> spaces smlString <|>
                  parens (Node <$> spaces smlString <*> smlForest)
smlString       = smlBacktick <|> smlBracketed <|> smlVerbatim <?> "a string"
smlVerbatim     = many1 $ satisfyNone [flip elem "[](){}#", isSpace]
smlBracketed    = brackets $ concat <$> many (try smlBracketed <|> many1 (noneOf "[]"))
smlBacktick     = backticks $ many1 (try (string "``") *> pure '`' <|> noneOf "`")
smlLineComment  = char '#' *> many (noneOf "\n") *> (char '\n' *> pure () <|> eof)
smlComment      = braceHash $ many1 (notFollowedBy (string "#}") *>
                                     (try smlComment <|>
                                      anyChar *> pure ())) *> pure ()

parens    p = char   '('  *> p <* char   ')'
brackets  p = char   '['  *> p <* char   ']'
braceHash p = string "{#" *> p <* string "#}"
backticks p = char   '`'  *> p <* char   '`'

spaces   p = whitespace *> p <* whitespace
whitespace = many (smlLineComment <|> smlComment <|> satisfy isSpace *> pure ())

satisfyNone fs = satisfy $ \ c -> not $ any ($ c) fs

--- various utility functions for trees ---

instance Functor Tree where
  fmap f (Node x ts)  = Node (f x) (map (fmap f) ts)
  fmap f (Terminal x) = Terminal (f x)