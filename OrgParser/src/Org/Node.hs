{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable, FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Org.Node
  (
    Node (..)
  , OrgTitle (..)
  , OrgTimeStamp (..)
  , EQNode (..)
  , OrgValue (..)
  , DebugPrint (..)
  , hasChildrenAliveTime
  , notChildrenTODO
  , addNode
  , orgLineNode
  , orgFileNode
  , nodeCollectList
  , normalFilter
  , nodeToCalendarEvents
  , orgFile
  )
where

import Org.Parse
import Org.GoogleCalendar.Event
import Data.List        (intercalate)
import Data.Maybe
import Data.Time
import Data.Either (rights)
import Control.Monad.State
import Control.Applicative ((<|>))
import Text.Blaze.Html.Renderer.String
import Text.Hamlet
import System.Environment      (getEnv)

data OrgTitle = OrgTitle { otitle      :: String
                         , olevel      :: Int
                         , otodo       :: Maybe String
                         , otags       :: [String]
                         , otimestamps :: [OrgTimeStamp]
                         , oparagraph  :: String
                         , oproperties :: [OrgProperty]
                         , opath       :: [String]
                         }
  deriving (Show, Eq)

newtype DebugPrint = Dp OrgTitle

instance Show DebugPrint where
  show (Dp ttl) =
    let level' = olevel ttl in
    (take level' $ repeat ' ')
    ++ show level'
    ++ " : "
    ++ otitle ttl
    ++ " -> "
    ++ show (otimestamps ttl)

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
            deriving  (Show, Eq)

instance Functor Node where
  _ `fmap` None = None
  f `fmap` (Node a n c) = Node (f a) (f `fmap` n) (f `fmap` c)

instance Applicative Node where
  pure a = Node a None None
  None <*> _ = None
  _ <*> None = None
  (Node a n c) <*> (Node a' n' c') =
    Node (a a') (n <*> n') (c <*> c')

instance Foldable Node where
  foldMap _ None = mempty
  foldMap f (Node a n c) = f a <> foldMap f c <> foldMap f n

def :: OrgTitle
def = OrgTitle { otitle      = mempty
               , olevel      = 1
               , otodo       = Nothing
               , otags       = []
               , otimestamps = []
               , oparagraph  = []
               , oproperties = []
               , opath       = []
               }

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

class FromParser a where
  fromParse :: Element -> a

instance FromParser OrgTitle where
  fromParse (ParserTitle ti l to tgs timestamp') =
    let ParserTags tag = tgs in
    OrgTitle { otitle      = ti
             , olevel      = l
             , otodo       = to
             , otags       = tag
             , otimestamps = map fromParse timestamp'
             , oparagraph  = mempty
             , oproperties = mempty
             , opath       = mempty }
  fromParse k = error $ show k ++ " : must not happen"

instance FromParser OrgTimeStamp where
  fromParse (ParserTimeStamp b d a e) =
      OrgTimeStamp { obegin    = b
                   , odatetype = d
                   , oactive   = a
                   , oend      = e }
  fromParse k = error $ show k ++ " : must not happen"

nodeTitle :: Node OrgTitle -> OrgTitle
nodeTitle (Node el _ _) = el

nodeTitleString :: Node OrgTitle -> String
nodeTitleString (Node el _ _) = otitle el

nodeLevel :: Node OrgTitle -> Maybe Int
nodeLevel (Node el _ _) = Just $ olevel el
nodeLevel None          = Nothing

nodeTimeStamps :: Node OrgTitle -> [OrgTimeStamp]
nodeTimeStamps None = []
nodeTimeStamps (Node title' _ _) = otimestamps title'

nodeLocation :: Node OrgTitle -> Maybe String
nodeLocation None = Nothing
nodeLocation node = titleLocation $ nodeTitle node

childrenTimeStamps :: Node OrgTitle -> [OrgTimeStamp]
childrenTimeStamps = foldMap ((`mappend` []) . otimestamps)

hasAliveTime :: Node OrgTitle -> Bool
hasAliveTime = not . null. filter notCloseOrInActive . nodeTimeStamps
  where
    notCloseOrInActive :: OrgTimeStamp -> Bool
    notCloseOrInActive timestamp =
      ((odatetype timestamp) /= Closed) && ((oactive timestamp) /= False)

hasChildrenAliveTime :: Node OrgTitle -> Bool
hasChildrenAliveTime None               = False
hasChildrenAliveTime o@(Node _ _ None)  = hasAliveTime o
hasChildrenAliveTime o@(Node _ _ child) =
  (hasAliveTime o) || (hasAliveTime child)

notTODO :: Node OrgTitle -> Bool
notTODO None = False
notTODO (Node title' _ _) = not . isJust . otodo $ title'

notChildrenTODO :: Node OrgTitle -> Bool
notChildrenTODO None               = False
notChildrenTODO (Node name _ None) = not $ isJust $ otodo name
notChildrenTODO o@(Node _ _ child) = notTODO o || notChildrenTODO child

titleLocation :: OrgTitle -> Maybe String
titleLocation = loop . oproperties
  where
    loop [] = Nothing
    loop ((OrgProperty ("LOCATION", val)):_) = Just val
    loop (_:xs) = loop xs

-- nodeDescription :: OrgNodeElement -> String
-- nodeDescription = loop mempty . children . element
--   where
--     loop s [] = s
--     loop s ((OrgOther o):xs) = loop (s <> o) xs
--     loop s (_:xs) = loop s xs
testNode1, testNode2 :: Node OrgTitle
testNode1 = pure $ def { olevel = 1, otitle = "node1" }
testNode2 = pure $ def { olevel = 2, otitle = "node2" }

testTitles :: [OrgTitle]
testTitles = [def {olevel=1}, def {olevel=2}, def {olevel=3}, def {olevel=3}]
-- nextNode :: a -> OrgNode a

addNode :: (Node OrgTitle) -> (Node OrgTitle) -> (Node OrgTitle)
addNode newn oldn = loop newn oldn `evalState` mempty
  where
    setPath :: Node OrgTitle -> State [String] (Node OrgTitle)
    setPath node = do
      path <- get
      let t1 = nodeTitle node
      return $ pure $ t1 { opath = (nodeTitleString node) : path }
    loop :: (Node OrgTitle) -> (Node OrgTitle) -> State [String] (Node OrgTitle)
    loop n None    = setPath n
    loop None n    = return n
    loop n (Node a next@(Node _ _ _) c)
                   = do { nex' <- loop n next; return $ Node a nex' c }
    loop n o@(Node a None c)
      | EQN n == EQN o = do { nex <- setPath n; return $ Node a nex c }
      | EQN n > EQN o  = do
          path <- get
          put (nodeTitleString o : path)
          Node a None <$> loop n c
      | otherwise = error $ nodeTitleString n ++ " : must not happen"


element2Node :: Element -> Node OrgTitle -> Node OrgTitle
element2Node el node =
  case el of
    ParserTitle _ _ _ _ _   -> (pure $ fromParse el) `addNode` node
    ParserTimeStamp _ _ _ _ -> ValueTimeStamp (fromParse el) `setNodeValue` node
    ParserTags t            -> ValueTags t                   `setNodeValue` node
    ParserProperty p        -> ValueProperty (OrgProperty p) `setNodeValue` node
    ParserLink d e          -> ValueLink d e                 `setNodeValue` node
    ParserOther s           -> ValueOther s                  `setNodeValue` node
    ParserLineBreak         -> ValueOther "\\r\\n\n"         `setNodeValue` node

setNodeValue :: OrgValue -> Node OrgTitle -> Node OrgTitle
setNodeValue _ None = None
setNodeValue v (Node a None None) =
  let orgtitle =
        case v of
          ValueTimeStamp s
            -> let current = otimestamps a in a { otimestamps = s:current }
          ValueTags t
            -> let current = otags a in a { otags = t ++ current }
          ValueProperty op
            -> let current = oproperties a in a { oproperties = op:current }
          ValueLink _ _
            -> let current = oparagraph a in
                 a { oparagraph = current <> htmlLink v }
          ValueOther s
            -> let current = oparagraph a  in a { oparagraph = current <> s }
          _ -> a
  in pure orgtitle
setNodeValue v (Node a n@(Node _ _ _) c) = Node a (setNodeValue v n) c
setNodeValue v (Node a n c@(Node _ _ _)) = Node a n (setNodeValue v c)

htmlLink :: OrgValue -> String
htmlLink (ValueLink url Nothing) =
  renderHtml [shamlet|<a href="#{url}">#{url}</a>|]
htmlLink (ValueLink url (Just expr)) =
  renderHtml [shamlet|<a href="#{url}">#{expr}</a>|]
htmlLink _ = mempty

orgLineNode :: [String] -> Node OrgTitle
orgLineNode orglines =
  let parsed = concat $ rights $ map orgLineParse orglines in
  foldr element2Node None (reverse parsed)

orgFileNode :: FilePath -> IO (Node OrgTitle)
orgFileNode fp = orgLineNode . lines <$> readFile fp

nodeCollectList :: (Node OrgTitle -> Bool) -> Node OrgTitle -> [OrgTitle]
nodeCollectList _ None = []
nodeCollectList f node@(Node a n c) =
  let second = nodeCollectList f c ++ nodeCollectList f n in
  case f node of
    True  -> a : second
    False -> second

normalFilter :: Node OrgTitle -> Bool
normalFilter node = (hasAliveTime node) && (notTODO node)

titleToCalendarEvent :: OrgTimeStamp -> OrgTitle -> CalendarEvent
titleToCalendarEvent stamp ttl' =
  eventDefault { eventDescription = Just desc
               , eventEnd         = oend stamp <|> Just (obegin stamp)
               , eventStart       = Just $ obegin stamp
               , eventSummary     = tailSpaceKill $ otitle ttl'
               , eventLocation    = titleLocation ttl' }
  where
    paths = intercalate "/" $ reverse $ opath ttl'
    desc = case oparagraph ttl' of
             "" -> paths
             pg -> paths ++ "\n" ++ pg

nodeToCalendarEvents :: Node OrgTitle -> [CalendarEvent]
nodeToCalendarEvents node =
  let titleList = nodeCollectList normalFilter node
      byTime    = foldMap timestampVtitle titleList
      timestampVtitle ttl' =
                  map (flip (,) ttl') $ otimestamps ttl'
  in
  map (uncurry titleToCalendarEvent) byTime

tailSpaceKill :: String -> String
tailSpaceKill = reverse . kloop . reverse
  where
    kloop [] = []
    kloop (x:xs)
      | x == ' ' = kloop xs
      | otherwise = x : kloop xs

orgFile :: IO FilePath
orgFile = flip (++) "/notes.org" <$> getEnv "ORG"
