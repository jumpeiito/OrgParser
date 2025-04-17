{-# LANGUAGE OverloadedLabels, OverloadedStrings, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Org.Document
  (
    documents
  )
where

import qualified Data.Map.Strict  as M
import qualified Data.Text        as Tx
import qualified Data.Text.IO     as TxIO
import qualified Text.Builder     as TxLB
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Lens
import Data.Conduit
-- import Data.Extensible
import Org.ParseText
import Org.Conduit              (documentSource, documentConduit)

type ConduitMapState = StateT (M.Map Int Int) (ConduitT Title Void IO)

data YamlLine = YL Int Title deriving (Show)

jNum, jMaruNum :: M.Map Int Tx.Text
jNum     = M.fromList [(1, "１"), (2, "２"), (3, "３"), (4, "４"), (5, "５"),
                       (6, "６"), (7, "７"), (8, "８"), (9, "９"), (0, "０")]
jMaruNum = M.fromList [(1, "①"), (2, "②"), (3, "③"), (4, "④"), (5, "⑤"),
                       (6, "⑥"), (7, "⑦"), (8, "⑧"), (9, "⑨")]

textBuild :: YamlLine -> Tx.Text
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
        postTitle = TxLB.text (titlen <> after)
        comma     = TxLB.text (", ")
        quote     = TxLB.text ("\"")
        builders  =
          [ TxLB.text "- [", TxLB.decimal lv
          , comma, quote, postTitle
          , TxLB.text (a ^. #label), quote, comma
          , a ^. #paragraph , quote
          , TxLB.text "]"]
      in
        TxLB.run $ foldMap (<>) builders mempty

addCounter :: Int -> ConduitMapState Int
addCounter level' = do
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

documentSink :: ConduitT Title Void IO ()
documentSink = do
  liftIO $ putStrLn "- [0, \"労働安全・労働条件改善、アスベスト問題解決をすすめる運動\", \"\"]"
  _ <- loop `runStateT` M.empty
  return ()
    where
      loop :: ConduitMapState ()
      loop = do
        stream <- lift $ await
        case stream of
          Nothing -> return ()
          Just ttl -> do
            counter <- addCounter (ttl ^. #level)
            liftIO $ TxIO.putStrLn $ textBuild $ YL counter ttl
            loop

documents :: FilePath -> IO ()
documents filepath =
  runConduit (documentSource filepath
               .| documentConduit
               .| documentSink)
