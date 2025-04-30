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
data Cons

data Sexp = AS String -- String
          | AI Int    -- Int
          | ASY Sym   -- Symbol
          | Nil
          | Cons Sexp Sexp
          deriving (Show, Eq)

type Text   = Tx.Text
type Parser = Parsec Void Text

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

-- eval :: Sexp -> StateT (M.Map Sexp Sexp) IO Sexp
-- eval (AS s) = return (AS s)
-- eval (AI i) = return (AI i)
-- eval (ASY (Sym s)) = do
--   m <- get
--   case m (M.!) s of
--     Just v  -> v
--     Nothing -> error $ "Symbol " ++ s ++ "is not registered"
-- eval (L x:xs)
--   | isString x || isInt x = error $ "Head is not symbol."
--   | otherwise = apply (eval x) (map eval xs)

-- apply :: Sexp -> Sexp -> Sexp
-- apply = undefined

-- repl :: IO ()
-- repl = do
--   line <- getLine
--   (`runStateT` M.empty) $ do
--     evaled <- eval line
--     liftIO $ print evaled

-- -- belongsP :: Sexp -> Sexp -> Maybe Sexp
-- -- belongsP query slist@(L (s:ss))
-- --   | query == s = Just slist
-- --   | otherwise = query `belongsP` s
-- --                 <|> getFirst (foldMap First $ map (belongsP query) ss)
-- -- belongsP query _ = Nothing

-- -- assoc :: Sexp -> Sexp -> Maybe Sexp
-- -- assoc k@(ASY (Sym kwd)) (L xs)
-- --   | odd $ length xs            = Nothing
-- --   | not (":" `Dl.isPrefixOf` kwd) = Nothing
-- --   | otherwise =
-- --     let assocList = [ (x,y) | [x, y] <- chunksOf 2 xs ] in
-- --       k `lookup` assocList
-- -- assoc _ _ = Nothing

-- -- memq :: String -> Sexp -> [Sexp] -> Maybe Sexp
-- -- memq query val ss = getFirst $ foldMap f ss
-- --   where
-- --     q = ASY (Sym query)
-- --     f sexp = case (== val) <$> assoc q sexp of
-- --                Just True -> First $ Just sexp
-- --                _         -> First Nothing

-- -- test :: IO Sexp
-- -- test = do
-- --   -- content <- TxIO.readFile "c:/Users/jumpei/Documents/home/OrgFiles/shibu.lisp"
-- --   content <- TxIO.readFile "e:/OrgFiles/shibu.lisp"
-- --   case parse slist "" content of
-- --     Right s -> return s
-- --     Left _  -> error ""

