{-# LANGUAGE GADTs         #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
import qualified Data.List              as Dl
import qualified Data.Map.Strict        as M
import           Data.List.Split        (chunksOf)
import           Control.Monad.Trans.State.Strict
import           Control.Monad          (guard)

data Sym = Sym String
  deriving (Show, Eq)

data Atom
data Kons

data Sexp = AS String -- String
          | AI Int    -- Int
          | ASY Sym   -- Symbol
          | Nil
          | Cons Sexp Sexp
          | Quote Sexp
          deriving (Show, Eq)

-- deriving instance Show (SP a)

-- data SP a where
--   String :: String -> SP String
--   Int    :: Int -> SP Int
--   Symbol :: Sym -> SP Sym
--   Null   :: SP ()
--   Kons   :: (SP a) -> (SP b) -> SP Kons

type Text   = Tx.Text
type Parser = Parsec Void Text

type Global = M.Map String Sexp

-- tos :: SP a -> String
-- tos (String s) = s
-- tos (Int i) = show i
-- tos (Symbol (Sym s)) = s
-- tos _ = ""

-- aString :: Parser (SP String)
-- aString = between quote quote atomStr
--   where
--     quote   = single '"'
--     atomStr = String <$> many (noneOf ['"', '\n'])

-- aInt :: Parser (SP Int)
-- aInt = Int <$> digital
--   where
--     digital = do
--       sign  <- 1 `option` (single '-' >> return (-1))
--       digit <- read <$> some digitChar
--       return $ digit * sign

-- aSymbol :: Parser (SP Sym)
-- aSymbol = (Symbol . Sym) <$> symbol
--   where
--     symbol = do
--       symbolName <- some (noneOf ['(', ')', '"', ' ', '\n'])
--       guard $ symbolName /= "."
--       return symbolName

-- konsP :: Parser (SP Kons)
-- konsP = listsep >> between (single '(') (single ')') core
--   where
--     selector  = choice $ map try [ aString
--                                  , aInt
--                                  , aSymbol
--                                  , konsP]
--     coreBlank = return Null
--     coreCons  = Kons <$> (listsep >> selector)
--                      <*> (listsep >> single '.' >> listsep >> selector)
--     coreList  = Kons <$> (listsep >> selector <* listsep)
--                      <*> (try core <|> coreBlank)
--     core = try coreCons <|> try coreList

-- aisString, aisInt :: SP a -> Bool
-- aisString (String _) = True
-- aisString _          = False
-- aisInt (Int _)       = True
-- aisInt _             = False

-- global = M.fromList [("car")]

toStr :: Sexp -> String
toStr (AS s)        = "\"" ++ s ++ "\"" -- String
toStr (AI i)        = show i            -- Int
toStr (ASY (Sym s)) = s                 -- Symbol
-- toStr (L s)         =                   -- List
--   "(" ++ Dl.intercalate " " (map toStr s) ++ ")"

atomStr :: Parser Sexp
atomStr = between quote quote atomStr
  where
    quote   = single '"'
    atomStr = AS <$> many (noneOf ['"', '\n'])

isString, isInt :: Sexp -> Bool
isString (AS _) = True
isString _ = False
isInt (AI _) = True
isInt _ = False

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
    symbol = do
      symbolName <- some (noneOf ['(', ')', '"', ' ', '\n'])
      guard $ symbolName /= "."
      return symbolName

listsep :: Parser [Char]
listsep = many (oneOf [' ', '\n'])

-- listInner :: Parser [Sexp]
-- listInner = do
--   let selector = [atomStr, atomInt, atomSymbol, slist]
--   listsep >> sepEndBy (choice selector) listsep

consP :: Parser Sexp
consP = listsep >> between (single '(') (single ')') core
  where
    selector  = choice $ map try [atomStr
                                 , atomInt
                                 , atomSymbol
                                 , consP]
    coreBlank = return Nil
    coreCons  = Cons <$> (listsep >> selector)
                     <*> (listsep >> single '.' >> listsep >> selector)
    coreList  = Cons <$> (listsep >> selector <* listsep)
                     <*> (try core <|> coreBlank)
    core = try coreCons <|> try coreList

car, cdr :: Sexp -> Sexp
car (Cons a _) = a
cdr (Cons _ d) = d

-- slist :: Parser Sexp
-- slist = do
--   contents <- listsep >> between (single '(') (single ')') listInner
--   return $ L contents

-- globalMap :: M.Map Sexp Sexp
-- globalMap = M.empty

eval :: Sexp -> StateT (M.Map String Sexp) IO Sexp
eval Nil    = return Nil
eval (AS s) = return (AS s)
eval (AI i) = return (AI i)
eval (ASY (Sym s)) = do
  m <- get
  case s `M.lookup` m of
    Just v  -> return v
    Nothing -> error $ "Symbol " ++ s ++ " is not registered"
eval (Cons a d)
  | isString a || isInt a = error $ "Head is not symbol."
  | otherwise = apply <$> eval a <*> eval d

apply :: Sexp -> Sexp -> Sexp
apply = undefined

-- repl :: IO ()
-- repl = do
--   line <- getLine
--   (`runStateT` M.empty) $ do
--     evaled <- eval line
--     liftIO $ print evaled

