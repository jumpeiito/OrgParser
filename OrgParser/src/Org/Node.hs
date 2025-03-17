module Org.Node
  (
    timestamps
  , hasChildrenAliveTime
  , notChildrenTODO
  , nodeToList
  , addNode
  , showTitle
  , makeOrgNode
  )
where

import Org.Parse
import Control.Monad
import Data.Maybe
import Data.Either

type NextNode  = Maybe OrgNode
type ChildNode = Maybe OrgNode
data OrgNode   = OrgNode OrgElement NextNode ChildNode
  deriving (Show, Eq)

makeOrgNode :: OrgElement -> Maybe OrgNode
makeOrgNode el = Just $ OrgNode el Nothing Nothing

showTitle :: OrgNode -> Maybe String
showTitle (OrgNode el _ _)
  | isTitle el = Just $ title el
  | otherwise  = Nothing

(~~>) :: OrgNode -> (OrgElement -> a) -> a
(~~>) (OrgNode el _ _) f = f el

timestamps :: OrgNode -> [OrgElement]
timestamps node = filter isTimeStamp $ node ~~> children

hasAliveTime :: OrgNode -> Bool
hasAliveTime = not . null. filter notCloseOrInActive . timestamps
  where
    notCloseOrInActive timestamp =
      ((datetype timestamp) /= Closed) && ((active timestamp) /= False)

hasChildrenAliveTime :: OrgNode -> Bool
hasChildrenAliveTime o@(OrgNode _ _ Nothing)      = hasAliveTime o
hasChildrenAliveTime o@(OrgNode _ _ (Just child)) =
  (hasAliveTime o) || (hasAliveTime child)

notChildrenTODO :: OrgNode -> Bool
notChildrenTODO (OrgNode name _ Nothing) = not $ isJust $ todo name
notChildrenTODO (OrgNode name _ (Just child)) =
  (not $ isJust $ todo name) || notChildrenTODO child

addNode :: OrgElement -> Maybe OrgNode -> Maybe OrgNode
addNode el Nothing = makeOrgNode el
addNode el (Just (OrgNode name next child)) =
  case (isTitle el, isJust next, isJust child, el `orgTitleOrder` name) of
    (True, _, _, Just EQ) ->
      Just $ OrgNode name (addNode el next) child
    (True, True, _, Just GT) ->
      Just $ OrgNode name (addNode el next) child
    (True, False, _, Just GT) ->
      Just $ OrgNode name next (addNode el child)
    (True, _, _, Just LT) ->
      error "must not happen"
    (True, _, _, Nothing) -> Nothing
    (False, True, _, _) ->
      Just $ OrgNode name (addNode el next) child
    (False, False, True, _) ->
      Just $ OrgNode name next (addNode el child)
    (False, False, False, _) ->
      let c = children name ++ [el] in
      Just $ OrgNode (name { children = c }) next child

nodeToList :: Maybe OrgNode -> [(String, OrgNode)]
nodeToList Nothing = []
nodeToList node    = loop "" [] node
  where
    loop _ ret Nothing = ret
    loop path ret (Just o@(OrgNode name next child)) =
      let newpath = path ++ "/" ++ title name in
      [(newpath, o)] ++ (loop newpath ret child) ++ (loop path ret next)

isTitle :: OrgElement -> Bool
isTitle (OrgTitle _ _ _ _) = True
isTitle _ = False

isTimeStamp :: OrgElement -> Bool
isTimeStamp (OrgTimeStamp _ _ _ _) = True
isTimeStamp _ = False

orgTitleOrder :: OrgElement -> OrgElement -> Maybe Ordering
orgTitleOrder t1 t2
  | isTitle t1 && isTitle t2 = Just (level t1 `compare` level t2)
  | otherwise = Nothing

