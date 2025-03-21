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
import Control.Monad.State

data OrgNode a = OrgNode a (OrgNode a) (OrgNode a)
               | None
               deriving  (Show, Eq, Foldable)

type OrgNodeElement = OrgNode OrgElement

testTree :: OrgNode Int
testTree = OrgNode 20 (OrgNode 200 None None)
                      (OrgNode 300 (OrgNode 12 None None)
                                   (OrgNode 100 None None))

instance Functor OrgNode where
  _ `fmap` None = None
  f `fmap` (OrgNode a n c) = OrgNode (f a) (f `fmap` n) (f `fmap` c)

instance Applicative OrgNode where
  pure a = OrgNode a None None

  (OrgNode fa fn fc) <*> (OrgNode a n c) =
    OrgNode (fa a) (fn <*> n) (fc <*> c)
  _ <*> _ = None

instance Monad OrgNode where
  None >>= _ = None
  (OrgNode a _ _) >>= f = f a

instance Traversable OrgNode where
  traverse f None = pure None
  traverse f (OrgNode a n c) = OrgNode <$> (f a) <*> (traverse f n) <*> (traverse f c)

-- get :: OrgNode a
-- get = 
element :: OrgNode a -> a
element (OrgNode el _ _) = el

isTitle :: OrgElement -> Bool
isTitle (OrgTitle _ _ _ _) = True
isTitle _ = False

isTimeStamp :: OrgElement -> Bool
isTimeStamp (OrgTimeStamp _ _ _ _) = True
isTimeStamp _ = False

-- makeOrgNode :: OrgElement -> OrgNodeElement
-- makeOrgNode = pure

nodeTitle :: OrgNodeElement -> Maybe String
nodeTitle (OrgNode el _ _)
  | isTitle el = Just $ title el
  | otherwise  = Nothing

-- (~~>) :: OrgNodeElement -> (OrgElement -> a) -> a
-- (~~>) (OrgNode el _ _) f = f el

nodeTimeStamps :: OrgNodeElement -> [OrgElement]
nodeTimeStamps = foldMap ((`mappend` []) . filter isTimeStamp . children)

hasAliveTime :: OrgNodeElement -> Bool
hasAliveTime = not . null. filter notCloseOrInActive . nodeTimeStamps
  where
    notCloseOrInActive timestamp =
      ((datetype timestamp) /= Closed) && ((active timestamp) /= False)

hasChildrenAliveTime :: OrgNodeElement -> Bool
hasChildrenAliveTime o@(OrgNode _ _ None)  = hasAliveTime o
hasChildrenAliveTime o@(OrgNode _ _ child) =
  (hasAliveTime o) || (hasAliveTime child)

notTODO :: OrgNodeElement -> Bool
notTODO = not . isJust . todo . element

notChildrenTODO :: OrgNodeElement -> Bool
notChildrenTODO (OrgNode name _ None) = not $ isJust $ todo name
notChildrenTODO o@(OrgNode _ _ child) = notTODO o || notChildrenTODO child

nodeLocation :: OrgNodeElement -> Maybe String
nodeLocation = loop . children . element
  where
    loop [] = Nothing
    loop ((OrgProperty ("LOCATION", val)):_) = Just val
    loop (_:xs) = loop xs

nodeDescription :: OrgNodeElement -> String
nodeDescription = loop mempty . children . element
  where
    loop s [] = s
    loop s ((OrgOther o):xs) = loop (s <> o) xs
    loop s (_:xs) = loop s xs

nextNode :: a -> OrgNode a


-- -- addNode :: OrgElement -> Maybe OrgNode -> Maybe OrgNode
-- -- addNode el Nothing = makeOrgNode el
-- -- addNode el (Just (OrgNode name next child))
-- --   | isTitle name =
-- --     case (isTitle el, isJust next, isJust child, el `orgTitleOrder` name) of
-- --       (True, _, _, Just EQ) ->
-- --         Just $ OrgNode name (addNode el next) child
-- --       (True, True, _, Just GT) ->
-- --         Just $ OrgNode name (addNode el next) child
-- --       (True, False, _, Just GT) ->
-- --         Just $ OrgNode name next (addNode el child)
-- --       (True, _, _, Just LT) ->
-- --         error "must not happen"
-- --       (True, _, _, Nothing) -> Nothing
-- --       (False, True, _, _) ->
-- --         Just $ OrgNode name (addNode el next) child
-- --       (False, False, True, _) ->
-- --         Just $ OrgNode name next (addNode el child)
-- --       (False, False, False, _) ->
-- --         let c = children name ++ [el] in
-- --           Just $ OrgNode (name { children = c }) next child
-- -- addNode _ (Just _) = Nothing

-- node2List :: OrgNodeElement -> [(String, OrgElement)]
-- node2List = collectNodeList (const True)

-- collectNodeList ::
--   (OrgElement -> Bool) -> OrgNodeElement -> [(String, OrgElement)]
-- collectNodeList f = fst . foldr core ([], "")
--   where
--     core el seed@(ret, path)
--       | f el = let newpath = path ++ "/" ++ title el in
--                  ((newpath, el):ret, newpath)
--       | otherwise = seed

-- orgTitleOrder :: OrgElement -> OrgElement -> Maybe Ordering
-- orgTitleOrder t1 t2
--   | isTitle t1 && isTitle t2 = Just (level t1 `compare` level t2)
--   | otherwise = Nothing
