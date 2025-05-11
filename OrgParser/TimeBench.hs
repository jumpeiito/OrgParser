{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty.Bench
import Org.Parse.Utility
import Org.Parse.Time
import Org.Parse.Text
import Text.Megaparsec

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

f1, f2 :: Parser (Other Text)
f1 = otherRefineP
f2 = otherExtremeP

test i  = [ parse f1 "" "hoge <2025-04-10 月> foo [[http://google.co.jp][Google]] buz! SCHEDULED: <2025-04-11 火 12:00-17:00> {{かみあり製麺}{島根県出雲市斐川町学頭1815-1}}" | _ <- [1..i]]
test4 i = [ parse f2 "" "hoge <2025-04-10 月> foo [[http://google.co.jp][Google]] buz! SCHEDULED: <2025-04-11 火 12:00-17:00> {{かみあり製麺}{島根県出雲市斐川町学頭1815-1}}" | _ <- [1..i]]

main :: IO ()
main = defaultMain
  [ bgroup "Fibonacci numbers"
    [ bench "fifth"     $ nf test 500000
    , bench "tenth"     $ nf test4 500000
    ]
  ]
