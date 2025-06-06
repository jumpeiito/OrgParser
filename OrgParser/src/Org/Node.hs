{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
module Org.Node (Node (..) , Nodeable (..)) where

import           Control.Applicative        ((<|>), Alternative (..))
import           Control.Monad.State

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
  select         :: (a -> Bool) -> Node a -> (Node a, Node a)

  build newa oldn = buildPath newa oldn `evalState` mempty

  buildPath newa None = setPath newa
  buildPath newa (Node a next@Node{} c) = do
    nex' <- buildPath newa next
    return $ Node a nex' c
  buildPath newa oldn@(Node a None c)
    | newa `isNext` a = do
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
      if f a
      then a : second
      else second

  scrap = scrapWith scrapFilter

  scrapAll = scrapWith (const True)

  cut _ None = None
  cut f (Node a n c)
    | f a = n -- cut
    | otherwise = Node a (cut f n) (cut f c)

  pick _ None = None
  pick f (Node a n c)
    | f a = Node a None c
    | otherwise = pick f n <|> pick f c

  select _ None = (None, None)
  select f n = (pick f n, cut f n)
