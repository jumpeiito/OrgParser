{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Org.Node
  (
    Node (..)
  , build
  , scrap
  , scrapAll
  , scrapWith
  , cut
  , pick
  , toEvent
  )
where

import Control.Applicative        ((<|>), Alternative (..))
import Control.Lens               hiding ((:>), noneOf)
import Control.Monad.State
import qualified Data.Text        as Tx
import Data.Maybe
import qualified Org.ParseText    as PTX
import Org.GoogleCalendar.Event

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

instance Alternative Node where
  empty = None
  None <|> None = None
  None <|> n    = n
  n    <|> _    = n

class Nodeable a where
  isNext         :: a -> a -> Bool
  final          :: [a] -> a -> a
  scrapFilter    :: a -> Bool
  --------------------------------------------------
  build          :: a -> Node a -> Node a
  buildPath      :: a -> Node a -> State [a] (Node a)
  buildChildPath :: a -> Node a -> State [a] (Node a)
  setPath        :: a -> State [a] (Node a)
  scrap          :: Node a -> [a]
  scrapAll       :: Node a -> [a]
  scrapWith      :: (a -> Bool) -> Node a -> [a]
  cut            :: (a -> Bool) -> Node a -> Node a
  pick           :: (a -> Bool) -> Node a -> Node a

  build newa oldn = buildPath newa oldn `evalState` mempty

  buildPath newa None = setPath newa
  buildPath newa (Node a next@Node{} c) = do
    nex' <- buildPath newa next
    return $ Node a nex' c
  buildPath newa oldn@(Node a None c)
    | newa `isNext` a == True = do
        next <- setPath newa
        return $ Node a next c
    | otherwise = buildChildPath newa oldn

  buildChildPath newa None = setPath newa
  buildChildPath newa (Node olda n c) = do
    path <- get
    put (path <> [olda])
    Node olda n <$> buildPath newa c

  setPath a = do
    path <- get
    return $ pure $ final path a

  scrapWith _ None = []
  scrapWith f (Node a n c) =
    let second = scrapWith f c ++ scrapWith f n in
      case f a of
        True  -> a : second
        False -> second

  scrap = scrapWith scrapFilter

  scrapAll = scrapWith (const True)

  cut _ None = None
  cut f (Node a n c)
    | f a = n -- cut
    | otherwise = Node a (cut f n) (cut f c)

  pick _ None = None
  pick f (Node a n c)
    | f a = Node a None c
    | otherwise = (pick f n) <|> (pick f c)

instance Nodeable PTX.Title where
  isNext t1 t2 = PTX.LEQ t1 == PTX.LEQ t2
  final paths ttl =
    let pathText = (Tx.intercalate "/" $ map (^. #label) paths) in
      ttl & #path .~ pathText
  scrapFilter ttl =
    let
      hasAliveTime = any notCloseAndActive timestamps
      timestamps = ttl ^. #timestamps
      notCloseAndActive timestamp =
        (timestamp ^. #datetype /= PTX.Closed) && (timestamp ^. #active)
      notTODO = isNothing $ ttl ^. #todo
    in
      hasAliveTime && notTODO


toEvent :: PTX.Timestamp -> PTX.Title -> CalendarEvent
toEvent stamp ttl =
  let
    ttlLocation = ttl ^. #location
    location = if Tx.null ttlLocation then Nothing else Just ttlLocation
  in
    eventDefault
  { eventDescription = desc
  , eventEnd         = stamp ^. #end <|> Just (stamp ^. #begin)
  , eventStart       = Just (stamp ^. #begin)
  , eventSummary     = Tx.dropWhileEnd (== ' ') (ttl ^. #label)
  , eventLocation    = location }
  where
    desc = case (ttl ^. #path, ttl ^. #paragraph) of
             ("", "") -> Nothing
             (p, "")  -> Just p
             ("", p)  -> Just p
             (p, pg)  -> Just (p <> "\n" <> pg)
