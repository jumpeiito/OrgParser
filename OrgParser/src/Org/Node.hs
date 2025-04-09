{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Org.Node
  (
    Node (..)
  , OrgTitle (..)
  , OrgTimeStamp (..)
  , OrgNode
  , EQNode (..)
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
  , orgEvents
  )
where

import Org.Parse
import Org.GoogleCalendar.Event
import Data.List                  (intercalate, isSuffixOf)
import Data.Maybe
import Data.Time
import Data.Either                (rights)
import Control.Monad.State
import Control.Applicative        ((<|>))
-- import Text.Blaze.Html.Renderer.String
-- import Text.Hamlet
import System.Environment         (getEnv)
import System.Directory           (getDirectoryContents)

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
    let level' = ttl.olevel in
      replicate level' ' '
      ++ show level'
      ++ " : "
      ++ ttl.otitle
      ++ " -> "
      ++ show ttl.otimestamps

data OrgTimeStamp = OrgTimeStamp { obegin    :: UTCTime
                                 , odatetype :: OrgTimeStampType
                                 , oactive   :: Bool
                                 , oend      :: Maybe UTCTime }
  deriving (Show, Eq)

newtype OrgProperty = OrgProperty (String, String)
  deriving (Show, Eq)

data Node a = Node a (Node a) (Node a)
            | None
            deriving  (Show, Eq)

type OrgNode = Node OrgTitle
type OrgNodeState = State [String] OrgNode

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
    | olevel n1 == olevel n2 = True
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

nodeTitle :: OrgNode -> OrgTitle
nodeTitle (Node el _ _) = el

nodeTitleString :: OrgNode -> String
nodeTitleString (Node el _ _) = el.otitle

nodeLevel :: OrgNode -> Maybe Int
nodeLevel (Node el _ _) = Just $ el.olevel
nodeLevel None          = Nothing

nodeTimeStamps :: OrgNode -> [OrgTimeStamp]
nodeTimeStamps None = []
nodeTimeStamps (Node title' _ _) = title'.otimestamps

nodeLocation :: OrgNode -> Maybe String
nodeLocation None = Nothing
nodeLocation node = titleLocation $ nodeTitle node

childrenTimeStamps :: OrgNode -> [OrgTimeStamp]
childrenTimeStamps = foldMap ((`mappend` []) . otimestamps)

hasAliveTime :: OrgNode -> Bool
hasAliveTime = any notCloseOrInActive . nodeTimeStamps
  where
    notCloseOrInActive :: OrgTimeStamp -> Bool
    notCloseOrInActive timestamp =
      timestamp.odatetype /= Closed && timestamp.oactive

hasChildrenAliveTime :: OrgNode -> Bool
hasChildrenAliveTime None               = False
hasChildrenAliveTime o@(Node _ _ None)  = hasAliveTime o
hasChildrenAliveTime o@(Node _ _ child) =
  hasAliveTime o || hasAliveTime child

notTODO :: OrgNode -> Bool
notTODO None = False
notTODO (Node title' _ _) = isNothing . otodo $ title'

notChildrenTODO :: OrgNode -> Bool
notChildrenTODO None               = False
notChildrenTODO (Node name _ None) = isNothing $ otodo name
notChildrenTODO o@(Node _ _ child) = notTODO o || notChildrenTODO child

titleLocation :: OrgTitle -> Maybe String
titleLocation = loop . oproperties
  where
    loop [] = Nothing
    loop (OrgProperty ("LOCATION", val):_) = Just val
    loop (_:xs) = loop xs

-- testNode1, testNode2 :: OrgNode
-- testNode1 =
--   pure $ def { olevel = 1, otitle = "node1"
--              , otimestamps = [OrgTimeStamp { obegin = UTCTime (fromGregorian 2025 4 1)
--                                                               (secondsToDiffTime 0)
--                                           , odatetype = Normal
--                                           , oactive = True
--                                           , oend = Nothing }]}

-- testNode2 = pure $ def { olevel = 2, otitle = "node2" }

-- testTitles :: [OrgTitle]
-- testTitles = [def {olevel=1}, def {olevel=2}, def {olevel=3}, def {olevel=3}]

addNode :: OrgNode -> OrgNode -> OrgNode
addNode attach oldn = addNodeState attach oldn `evalState` mempty

addNodeSetPath :: OrgNode -> OrgNodeState
addNodeSetPath attach = do
  path <- get
  let t1 = nodeTitle attach
  return $ pure $ t1 { opath = nodeTitleString attach : path }

addNodeState :: OrgNode -> OrgNode -> OrgNodeState
addNodeState attach None = addNodeSetPath attach
addNodeState None o = return o
addNodeState attach o@(Node a Node{} c) = addNodeNextState attach o
addNodeState attach o@(Node a None c)
  | EQN attach == EQN o = addNodeNextEmptyState attach o
  | EQN attach > EQN o  = addNodeChildState attach o
  | otherwise = error $ nodeTitleString attach ++ " : must not happen"

addNodeNextState :: OrgNode -> OrgNode -> OrgNodeState
addNodeNextState attach (Node a n c) = do
  nex' <- addNodeState attach n
  return $ Node a nex' c

addNodeNextEmptyState :: OrgNode -> OrgNode -> OrgNodeState
addNodeNextEmptyState attach (Node a _ c) = do
  nex <- addNodeSetPath attach
  return $ Node a nex c

addNodeChildState :: OrgNode -> OrgNode -> OrgNodeState
addNodeChildState attach (Node a _ c) = do
  path <- get
  put (otitle a : path)
  Node a None <$> addNodeState attach c

element2Node :: Element -> OrgNode -> OrgNode
element2Node ptitle@ParserTitle{} node =
  pure (fromParse ptitle) `addNode` node
element2Node pelement node =
  pelement `setNodeValue` node

setNodeValue :: Element -> OrgNode -> OrgNode
setNodeValue _ None = None
setNodeValue pel (Node a None None) =
  let
    org =
      case pel of
        ParserTimeStamp{} ->
          a { otimestamps = fromParse pel : a.otimestamps }
        ParserTags t ->
          a { otags = t <> a.otags }
        ParserProperty p ->
          a { oproperties = OrgProperty p : a.oproperties }
        ParserLink{} ->
          a { oparagraph = a.oparagraph <> htmlLink pel }
        ParserOther s ->
          a { oparagraph = a.oparagraph <> s }
        ParserLineBreak ->
          a { oparagraph = a.oparagraph <> "\\r\\n\n" }
  in
    pure org
setNodeValue v (Node a n@Node{} c) = Node a (setNodeValue v n) c
setNodeValue v (Node a n c@Node{}) = Node a n (setNodeValue v c)

htmlLink :: Element -> String
htmlLink (ParserLink url expr) =
  concat [ "<a href=\"", url, "\">" , url `fromMaybe` expr, "</a>"]

nodeCollectList :: (OrgNode -> Bool) -> OrgNode -> [OrgTitle]
nodeCollectList _ None = []
nodeCollectList f node@(Node a n c) =
  let second = nodeCollectList f c ++ nodeCollectList f n in
  case f node of
    True  -> a : second
    False -> second

normalFilter :: OrgNode -> Bool
normalFilter node = hasAliveTime node && notTODO node

titleToCalendarEvent :: OrgTimeStamp -> OrgTitle -> CalendarEvent
titleToCalendarEvent stamp ttl' =
  eventDefault { eventDescription = Just desc
               , eventEnd         = stamp.oend <|> Just stamp.obegin
               , eventStart       = Just stamp.obegin
               , eventSummary     = tailSpaceKill ttl'.otitle
               , eventLocation    = titleLocation ttl' }
  where
    paths = intercalate "/" $ reverse ttl'.opath
    desc = case ttl'.oparagraph of
             "" -> paths
             pg -> paths ++ "\n" ++ pg

nodeToCalendarEvents :: OrgNode -> [CalendarEvent]
nodeToCalendarEvents node =
  let
    titleList = nodeCollectList normalFilter node
    byTime    = foldMap timestampVtitle titleList
    timestampVtitle ttl' =
                  map (flip (,) ttl') ttl'.otimestamps
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

orgLineNode :: [String] -> OrgNode
orgLineNode orglines =
  let parsed = concat $ rights $ map orgLineParse orglines in
  foldr element2Node None (reverse parsed)

orgFileNode :: FilePath -> IO OrgNode
orgFileNode fp = orgLineNode . lines <$> readFile fp

orgArchiveFiles :: IO [FilePath]
orgArchiveFiles = do
  orgDir   <- getEnv "ORG"
  contents <- getDirectoryContents orgDir
  let fullpaths = map (\n -> orgDir ++ "\\" ++ n) contents
  return $ filter ("org_archive" `isSuffixOf`) fullpaths

orgArchiveNode :: IO [OrgNode]
orgArchiveNode = orgArchiveFiles >>= mapM orgFileNode

orgEvents :: IO [CalendarEvent]
orgEvents = do
  notes    <- orgFile >>= orgFileNode
  archives <- orgArchiveNode
  return $ concatMap nodeToCalendarEvents (notes : archives)
