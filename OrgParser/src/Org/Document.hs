{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
module Org.Document
  (
    documents
  )
where

import qualified Data.Map.Strict        as M
import qualified Data.Text              as Tx
import qualified Data.Text.IO           as TxIO
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Control.Lens
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import           Org.Parse.Text
import           Org.Conduit            (documentProducer)

type ConduitMapState text final =
  StateT (M.Map Int Int) (ConduitT (Title text) final IO)

data YamlLine text = YL Int (Title text) deriving (Show)

jNum, jMaruNum :: M.Map Int Tx.Text
jNum     = M.fromList [(1, "１"), (2, "２"), (3, "３"), (4, "４"), (5, "５"),
                       (6, "６"), (7, "７"), (8, "８"), (9, "９"), (0, "０")]
jMaruNum = M.fromList [(1, "①"), (2, "②"), (3, "③"), (4, "④"), (5, "⑤"),
                       (6, "⑥"), (7, "⑦"), (8, "⑧"), (9, "⑨")]

textBuild :: TitleBuilder text => (YamlLine text) -> Tx.Text
textBuild (YL counter a)
  | a ^. #level == 1 = bracket 1 mempty                 mempty
  | a ^. #level == 2 = bracket 2 (jNum M.! counter)     "．"
  | a ^. #level == 3 = bracket 3 (jMaruNum M.! counter) "　"
  | a ^. #level == 4 = bracket 4 (jNum M.! counter)     "）"
  | a ^. #level == 5 = bracket 5 "・"                   mempty
  | otherwise = error "level must be 1 upto 5"
  where
    bracket :: Int -> Tx.Text -> Tx.Text -> Tx.Text
    bracket lv titlen after =
      let
        postTitle = Tx.unpack $ titlen <> after
        comma     = ", "
        quote     = "\""
        builderPre = map fromToken
                     ["- [", show lv, comma, quote, postTitle
                     , Tx.unpack (a ^. #label), quote, comma]
        builderPost = map fromToken [quote, "]"]
        builders = builderPre <> [a ^. #paragraph] <> builderPost
      in
        -- foldMap (<>) builders mempty
        toText $ builderConcat builders

addCounter :: Int -> State (M.Map Int Int) Int
addCounter level' = do
  counterMap <- get
  case level' `M.member` counterMap of
    False -> do
      modify (M.insert level' 1)
      return 1
    True  -> do
      let mapKeys = M.keys counterMap
      let deleted = foldr M.delete counterMap $ filter (> level') mapKeys
      let current = deleted M.! level'
      put $ M.insert level' (current + 1) deleted
      return $ current + 1

addCounterM :: Int -> ConduitMapState text a Int
addCounterM level' = do
  -- 例えば[(1, 2), (2, 1), (3, 3)]のようなMapをStateとして持っている。
  -- このMapは2番目のレベル1の中の最初のレベル2で,さらにその下の
  -- レベル3は3番目までカウントしたという意味。
  counterMap <- get
  case level' `M.member` counterMap of
    -- 上記でいうと,Mapにないレベル4のTitleが来た場合
    False -> do
      modify (M.insert level' 1)
      -- 元のモナドで活用するために, 章番号を返す
      return 1
    -- 上記でいうと,レベル1のTitleが来たような場合。
    True  -> do
      let mapKeys = M.keys counterMap
      -- レベル1より下にあるレベル2,レベル3などの番号を1から振りなおす
      -- ために,いったんMapから削除する。次にレベル2,レベル3が来たときに
      -- Falseとなって,1がMapにinsertされる。
      let deleted = foldr M.delete counterMap $ filter (> level') mapKeys
      let current = deleted M.! level'
      put $ M.insert level' (current + 1) deleted
      -- 元のモナドで活用するために, 新しい章番号を返す
      return $ current + 1

documentPrettify :: TitleBuilder text => [Title text] -> [Tx.Text]
documentPrettify titles = loop titles `evalState` M.empty
  where
    loop [] = return []
    loop (t:ts) = do
      counter <- addCounter (t ^. #level)
      let text = textBuild $ YL counter t
      (:) text <$> loop ts

documentPrettifyConduit :: ConduitT (Title BuilderType) Tx.Text IO ()
documentPrettifyConduit = do
  xs <- CL.consume
  CL.sourceList $ documentPrettify xs

documentSink :: ConduitT Tx.Text Void IO ()
documentSink = do
  liftIO $ TxIO.putStrLn "- [0, \"労働安全・労働条件改善、アスベスト問題解決をすすめる運動\", \"\"]"
  awaitForever (liftIO . TxIO.putStrLn)

documents :: FilePath -> IO ()
documents filepath =
  runConduit (documentProducer filepath
               .| documentPrettifyConduit
               .| documentSink)
