{-# LANGUAGE InstanceSigs, DeriveFunctor, DeriveFoldable, FlexibleContexts, ScopedTypeVariables #-}
module Org.Node
  (
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

data OrgNode a = OrgNode a (Maybe (OrgNode a)) (Maybe (OrgNode a))
  deriving (Show, Eq, Functor, Foldable)

type OrgNodeElement = OrgNode OrgElement

testTree = OrgNode 20 (Just $ OrgNode 200 Nothing Nothing)
                      (Just $ OrgNode 300 (Just $ OrgNode 12 Nothing Nothing)
                                          (Just $ OrgNode 100 Nothing Nothing))
instance Traversable OrgNode where
  traverse f (OrgNode el (Just n) (Just c)) =
    OrgNode (f el) (Just $ traverse f n) (Just $ traverse f c)
  traverse f (OrgNode el (Just n) Nothing) =
    OrgNode (f el) (Just $ traverse f n) Nothing
  traverse f (OrgNode el Nothing (Just c)) =
    OrgNode (f el) Nothing (Just $ traverse f c)
  traverse f (OrgNode el Nothing Nothing) =
    OrgNode (f el) Nothing Nothing
-- instance Applicative OrgNode where
--   pure a = OrgNode a Nothing Nothing

--   OrgNode f f1 f2 <*> OrgNode el n c =
--     OrgNode (f el) (cus f1 n) (cus f2 c)
--     where
--       core p1 p2 = do
--         f <- 
--       cus Nothing _ = Nothing
--       cus _ Nothing = Nothing
--       cus (Just (OrgNode elf1 n1 c1)) (Just (OrgNode elf2 n2 c2)) =
--         Just (OrgNode (elf1 elf2) (n1 <*> n2) (c1 <*> c2))
      -- cus (Just (OrgNode elf n' c')) node =
      --   pure <$> (elf <$> (elements <$> node))
        -- toNode <$> (elf <$> (elements <$> node))
        -- toNode n' c' <$> (elf <$> (elements <$> node))

-- testFold = foldMap (*) testTree
elements :: OrgNode a -> a
elements (OrgNode el _ _) = el

makeOrgNode :: OrgElement -> Maybe OrgNodeElement
makeOrgNode el = Just $ OrgNode el Nothing Nothing

showTitle :: OrgNodeElement -> Maybe String
showTitle (OrgNode el _ _)
  | isTitle el = Just $ title el
  | otherwise  = Nothing

(~~>) :: OrgNodeElement -> (OrgElement -> a) -> a
(~~>) (OrgNode el _ _) f = f el

timestamps :: OrgNodeElement -> [OrgElement]
timestamps node = filter isTimeStamp $ node ~~> children

hasAliveTime :: OrgNodeElement -> Bool
hasAliveTime = not . null. filter notCloseOrInActive . timestamps
  where
    notCloseOrInActive timestamp =
      ((datetype timestamp) /= Closed) && ((active timestamp) /= False)

hasChildrenAliveTime :: OrgNodeElement -> Bool
hasChildrenAliveTime o@(OrgNode _ _ Nothing)      = hasAliveTime o
hasChildrenAliveTime o@(OrgNode _ _ (Just child)) =
  (hasAliveTime o) || (hasAliveTime child)

notTODO :: OrgNodeElement -> Bool
notTODO = not . isJust . (~~> todo)

notChildrenTODO :: OrgNodeElement -> Bool
notChildrenTODO (OrgNode name _ Nothing) = not $ isJust $ todo name
notChildrenTODO o@(OrgNode _ _ (Just child)) = notTODO o || notChildrenTODO child

location :: OrgNodeElement -> Maybe String
location node = loop $ node ~~> children
  where
    loop [] = Nothing
    loop ((OrgProperty ("LOCATION", val)):_) = Just val
    loop (_:xs) = loop xs

description :: OrgNodeElement -> String
description node = loop mempty $ node ~~> children
  where
    loop s [] = s
    loop s ((OrgOther o):xs) = loop (s <> o) xs
    loop s (_:xs) = loop s xs

-- addNode :: OrgElement -> Maybe OrgNode -> Maybe OrgNode
-- addNode el Nothing = makeOrgNode el
-- addNode el (Just (OrgNode name next child))
--   | isTitle name =
--     case (isTitle el, isJust next, isJust child, el `orgTitleOrder` name) of
--       (True, _, _, Just EQ) ->
--         Just $ OrgNode name (addNode el next) child
--       (True, True, _, Just GT) ->
--         Just $ OrgNode name (addNode el next) child
--       (True, False, _, Just GT) ->
--         Just $ OrgNode name next (addNode el child)
--       (True, _, _, Just LT) ->
--         error "must not happen"
--       (True, _, _, Nothing) -> Nothing
--       (False, True, _, _) ->
--         Just $ OrgNode name (addNode el next) child
--       (False, False, True, _) ->
--         Just $ OrgNode name next (addNode el child)
--       (False, False, False, _) ->
--         let c = children name ++ [el] in
--           Just $ OrgNode (name { children = c }) next child
-- addNode _ (Just _) = Nothing

node2List :: OrgNodeElement -> [(String, OrgElement)]
node2List = collectNodeList (const True)

collectNodeList ::
  (OrgElement -> Bool) -> OrgNodeElement -> [(String, OrgElement)]
collectNodeList f = fst . foldr core ([], "")
  where
    core el seed@(ret, path)
      | f el = let newpath = path ++ "/" ++ title el in
                 ((newpath, el):ret, newpath)
      | otherwise = seed

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
