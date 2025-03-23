{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}
module Org.Node
  (
    Node (..)
  , OrgTitle (..)
  , EQNode (..)
  --   timestamps
  -- , hasChildrenAliveTime
  -- , hasAliveTime
  -- , notTODO
  -- , notChildrenTODO
  -- , nodeToList
  -- , collectNodeList
  -- , addNode
  -- , showTitle
  -- , makeOrgNode
  -- , location
  -- , description
  -- , (~~>)
  -- , OrgNode (..)
  )
where

import Org.Parse
import Data.Maybe
import Data.Time
import Data.Either (rights)
-- import qualified Data.List as DL
import Control.Monad.State

data OrgTitle = OrgTitle { otitle      :: String
                         , olevel      :: Int
                         , otodo       :: Maybe String
                         , otags       :: [String]
                         , otimestamps :: [OrgTimeStamp]
                         , oparagraph  :: [String]
                         , oproperties :: [OrgProperty]
                         , opath       :: [String]
                         }
  deriving (Show, Eq)

data OrgTimeStamp = OrgTimeStamp { obegin    :: UTCTime
                                 , odatetype :: OrgTimeStampType
                                 , oactive   :: Bool
                                 , oend      :: Maybe UTCTime }
  deriving (Show, Eq)

data OrgProperty = OrgProperty (String, String)
  deriving (Show, Eq)

data OrgValue = ValueTimeStamp OrgTimeStamp
              | ValueTags [String]
              | ValueProperty OrgProperty
              | ValueLink String (Maybe String)
              | ValueOther String

data Node a = Node a (Node a) (Node a)
            | None
            deriving  (Show, Eq, Foldable)

-- newtype NodeManip a b = Mp { runManip :: Node a -> Node b }
-- newtype NodeState s a = NodeState { runNode :: s -> (a, s)}

-- testTree :: Node Int
-- testTree = Node 20 (Node 200 None None)
--                    (Node 300 (Node 12 None None)
--                              (Node 100 None None))

instance Functor Node where
  _ `fmap` None = None
  f `fmap` (Node a n c) = Node (f a) (f `fmap` n) (f `fmap` c)

instance Applicative Node where
  pure a = Node a None None
  None <*> _ = None
  _ <*> None = None
  (Node a n c) <*> (Node a' n' c') =
    Node (a a') (n <*> n') (c <*> c')

def = OrgTitle { otitle      = mempty
               , olevel      = 1
               , otodo       = Nothing
               , otags       = []
               , otimestamps = []
               , oparagraph  = []
               , oproperties = []
               , opath       = []
               }

-- instance Show (Node OrgTitle) where
--   show None = "N"
--   show (Node a n c) = "(O " ++ (show $ olevel a) ++ " " ++ show n ++ " " ++ show c ++ ")"


newtype EQNode = EQN { getEQN :: Node OrgTitle }
  deriving Show

instance Eq EQNode where
  EQN None == EQN None = True
  EQN None == EQN _    = False
  EQN _ == EQN None    = False
  EQN (Node n1 _ _) == EQN (Node n2 _ _)
    | olevel n1 `compare` olevel n2 == EQ = True
    | otherwise = False

instance Ord EQNode where
  EQN None `compare` EQN None  = EQ
  EQN None `compare` EQN _     = LT
  EQN _    `compare` EQN None  = GT
  (EQN n1) `compare` (EQN n2)  = nodeLevel n1 `compare` nodeLevel n2

-- nextNode :: Node a -> Node a
-- nextNode None = None
-- nextNode (Node _ n _) = n

-- childNode :: Node a -> Node a
-- childNode None = None
-- childNode (Node _ _ c) = c

nodeTitle :: Node OrgTitle -> String
nodeTitle (Node el _ _) = otitle el

nodeLevel :: Node OrgTitle -> Int
nodeLevel (Node el _ _) = olevel el

nodeTimeStamps :: Node OrgTitle -> [OrgTimeStamp]
nodeTimeStamps = foldMap ((`mappend` []) . otimestamps)

hasAliveTime :: Node OrgTitle -> Bool
hasAliveTime = not . null. filter notCloseOrInActive . nodeTimeStamps
  where
    notCloseOrInActive :: OrgTimeStamp -> Bool
    notCloseOrInActive timestamp =
      ((odatetype timestamp) /= Closed) && ((oactive timestamp) /= False)

hasChildrenAliveTime :: Node OrgTitle -> Bool
hasChildrenAliveTime o@(Node _ _ None)  = hasAliveTime o
hasChildrenAliveTime o@(Node _ _ child) =
  (hasAliveTime o) || (hasAliveTime child)

notTODO :: Node OrgTitle -> Bool
notTODO None = False
notTODO (Node title _ _) = not . isJust . otodo $ title

notChildrenTODO :: Node OrgTitle -> Bool
notChildrenTODO (Node name _ None) = not $ isJust $ otodo name
notChildrenTODO o@(Node _ _ child) = notTODO o || notChildrenTODO child

-- nodeLocation :: OrgNodeElement -> Maybe String
-- nodeLocation = loop . children . element
--   where
--     loop [] = Nothing
--     loop ((OrgProperty ("LOCATION", val)):_) = Just val
--     loop (_:xs) = loop xs

-- nodeDescription :: OrgNodeElement -> String
-- nodeDescription = loop mempty . children . element
--   where
--     loop s [] = s
--     loop s ((OrgOther o):xs) = loop (s <> o) xs
--     loop s (_:xs) = loop s xs
testNode1, testNode2 :: Node OrgTitle
testNode1 = pure $ def { olevel = 1, otitle = "node1" }
testNode2 = pure $ def { olevel = 2, otitle = "node2" }

testTitles = [def {olevel=1}, def {olevel=2}, def {olevel=2}, def {olevel=3}]
-- nextNode :: a -> OrgNode a
addNode :: (Node OrgTitle) -> (Node OrgTitle) -> (Node OrgTitle)
addNode title None = title
addNode None title = title
addNode title (Node a n@(Node _ _ _) c) = Node a (addNode title n) c
addNode title n@(Node a None c)
  | EQN title == EQN n = Node a title c
  | EQN title > EQN n  = Node a None (addNode title c)
  | otherwise  = error "must not happen"

-- addNodeWithPath :: (Node OrgTitle) -> (Node OrgTitle) -> (Node OrgTitle)
-- addNodeWithPath newnode oldnode = loop newnode oldnode []
--   where
--     addPath path title = undefined
--     loop n None _      = n
--     loop None n _      = n
--     loop n (Node a next@(Node _ _ _) c) path
--                        = Node a (loop n next path) c
--     loop n o@(Node a None c) path
--       | EQN n == EQN o = Node a (addPath path node) c
--       | EQN n > EQN o  = Node a None (loop n c (otitle a : path))
--       | otherwise  = error "must not happen"

element2Node :: Element -> Node OrgTitle -> Node OrgTitle
element2Node el node =
  case el of
    ParserTitle ti l to (ParserTags tags') timestamps' ->
      let ot = OrgTitle { otitle      = ti
                        , olevel      = l
                        , otodo       = to
                        , otags       = tags'
                        , otimestamps = map parser2TimeStamp timestamps'
                        , oparagraph  = mempty
                        , oproperties = mempty} in
        addNode (pure ot) node
    p@(ParserTimeStamp _ _ _ _)
                     -> setNodeValue (ValueTimeStamp (parser2TimeStamp p)) node
    ParserTags t     -> setNodeValue (ValueTags t) node
    ParserProperty p -> setNodeValue (ValueProperty (OrgProperty p)) node
    ParserLink d e   -> setNodeValue (ValueLink d e) node
    ParserOther s    -> setNodeValue (ValueOther s) node
  where
    parser2TimeStamp (ParserTimeStamp begin' datetype' active' end') =
      OrgTimeStamp { obegin    = begin'
                   , odatetype = datetype'
                   , oactive   = active'
                   , oend      = end'}

setNodeValue :: OrgValue -> Node OrgTitle -> Node OrgTitle
setNodeValue _ None = None
setNodeValue v (Node a None None) =
  let orgtitle =
        case v of
          ValueTimeStamp s -> let current = otimestamps a in a { otimestamps = s:current }
          ValueTags t      -> let current = otags a       in a { otags = t ++ current }
          ValueProperty op -> let current = oproperties a in a { oproperties = op:current }
       -- ValueLink s      -> let current = oparagraph a  in a { oparagraph = s:current }
          ValueOther s     -> let current = oparagraph a  in a { oparagraph = s:current }
          _                -> a
  in pure orgtitle
setNodeValue v (Node a n@(Node _ _ _) c) = Node a (setNodeValue v n) c
setNodeValue v (Node a n c@(Node _ _ _)) = Node a n (setNodeValue v c)

orgLineNode :: [String] -> Node OrgTitle
orgLineNode orglines =
  let (x:xs) = concat $ rights $ map orgLineParse orglines in
    case x of
      ParserTitle _ _ _ _ _ -> foldr element2Node (element2Node x None) (reverse xs)
      _ -> error "TopLine must be title line"

-- collectNodeList ::
--   (OrgElement -> Bool) -> OrgNodeElement -> [(String, OrgElement)]
-- collectNodeList f = fst . foldr core ([], "")
--   where
--     core el seed@(ret, path)
--       | f el = let newpath = path ++ "/" ++ title el in
--                  ((newpath, el):ret, newpath)
--       | otherwise = 

-- orgTitleOrder :: OrgElement -> OrgElement -> Maybe Ordering
-- orgTitleOrder t1 t2
--   | isTitle t1 && isTitle t2 = Just (level t1 `compare` level t2)
--   | otherwise = Nothing
