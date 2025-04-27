module Org.Sexp
  (
  )
where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Data.Text              as Tx
import qualified Data.Text.IO           as TxIO
import           Data.Void              (Void)
import           Data.Monoid
import           Data.List              (isPrefixOf, lookup, intercalate)
import           Data.List.Split        (chunksOf)
  
data Sym = Sym String
  deriving (Show, Eq)

data Atom
data Cons

data Sexp = AS String
          | AI Int
          | ASY Sym
          | L [Sexp]
          deriving (Show, Eq)

type Text   = Tx.Text
type Parser = Parsec Void Text

toStr :: Sexp -> String
toStr (AS s)        = "\"" ++ s ++ "\"" -- String
toStr (AI i)        = show i            -- Int
toStr (ASY (Sym s)) = s                 -- Symbol
toStr (L s)         =                   -- List
  "(" ++ intercalate " " (map toStr s) ++ ")"

atomStr :: Parser Sexp
atomStr = between quote quote atomStr
  where
    quote   = single '"'
    atomStr = AS <$> many (noneOf ['"', '\n'])

atomInt :: Parser Sexp
atomInt = AI <$> digital
  where
    digital = do
      sign  <- 1 `option` (single '-' >> return (-1))
      digit <- read <$> some digitChar
      return $ digit * sign

atomSymbol :: Parser Sexp
atomSymbol = (ASY . Sym) <$> symbol
  where
    symbol = some (noneOf ['(', ')', '"', ' ', '\n'])

slist :: Parser Sexp
slist = do
  contents <- sep >> between (single '(') (single ')') inner
  return $ L contents
  where
    selector = [atomStr, atomInt, atomSymbol, slist]
    sep = many (oneOf [' ', '\n'])
    inner = sep >> sepEndBy (choice selector) sep

belongsP :: Sexp -> Sexp -> Maybe Sexp
belongsP query slist@(L (s:ss))
  | query == s = Just slist
  | otherwise = query `belongsP` s
                <|> getFirst (foldMap First $ map (belongsP query) ss)
belongsP query _ = Nothing

assoc :: Sexp -> Sexp -> Maybe Sexp
assoc k@(ASY (Sym kwd)) (L xs)
  | odd $ length xs            = Nothing
  | not (":" `isPrefixOf` kwd) = Nothing
  | otherwise =
    let assocList = [ (x,y) | [x, y] <- chunksOf 2 xs ] in
      k `lookup` assocList
assoc _ _ = Nothing

memq :: String -> Sexp -> [Sexp] -> Maybe Sexp
memq query val ss = getFirst $ foldMap f ss
  where
    q = ASY (Sym query)
    f sexp = case (== val) <$> assoc q sexp of
               Just True -> First $ Just sexp
               _         -> First Nothing

test :: IO Sexp
test = do
  content <- TxIO.readFile "c:/Users/jumpei/Documents/home/OrgFiles/shibu.lisp"
  case parse slist "" content of
    Right s -> return s
    Left _  -> error ""

