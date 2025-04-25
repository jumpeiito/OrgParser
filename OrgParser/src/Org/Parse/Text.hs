{-# LANGUAGE StrictData        #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Org.Parse.Text
  (
    Line (..)
  , Title
  , Other
  , Geocode (..)
  , LevelEQTitle (..)
  , defTitle
  , defOther
  , mplusOther
  , lineParse
  , searchQuery
  , getFirstTime
  ----------
  , GeocodeSearch (..)
  , todoP
  , tagsP
  , orgstarsP
  , indicateP
  , titleP
  , propertyP
  , linkP
  , geocodeP
  , otherRefineP
  )
where

import           Control.Monad          (guard)
import           Control.Lens           hiding ((:>), noneOf)
import           Data.List              (intercalate, sort)
import           Data.Time              (UTCTime)
import qualified Data.Text              as Tx
import           Data.Maybe             (fromMaybe, maybeToList, isJust
                                        , isNothing, listToMaybe)
import           Data.Extensible
import           Org.Parse.Utility
import           Org.Parse.Time
import           Org.Node
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Line = LL Title
          | LP (Text, Text)
          | LB
          | LO Other
          deriving (Show, Eq)

type Title = Record
  [ "label"      :> Text
  , "level"      :> Int
  , "todo"       :> Maybe Text
  , "tags"       :> [Text]
  , "timestamps" :> [Timestamp]
  , "paragraph"  :> Text
  , "properties" :> [(Text, Text)]
  , "location"   :> (Text, [GeocodeSearch])
  , "path"       :> Text ]

type Other = Record
  [ "timestamps" :> [Timestamp]
  , "others"     :> Text
  , "geocode"    :> [GeocodeSearch]]

data LineBreak       = LineBreak
data GeocodeSearch   = GeS Text (Maybe Text) deriving (Eq, Show)
newtype Link         = Link (Text, Maybe Text) deriving Show
newtype LevelEQTitle = LEQ Title
newtype Geocode      = Geo Title

instance Show Geocode where
  show (Geo t) =
    let
      normalActive stamp =
        (stamp ^. #datetype == Normal) && (stamp ^. #active == True)
      ts = filter normalActive (t ^. #timestamps)
    in
      (Tx.unpack (t ^. #label)) ++ "(" ++
      intercalate "・" (map (show . GU . (^. #begin)) ts) ++ ")"

instance Eq LevelEQTitle where
  (LEQ t1) == (LEQ t2) = t1 ^. #level == t2 ^. #level

defTitle :: Title
defTitle = #label         @= mempty
           <: #level      @= 0
           <: #todo       @= Nothing
           <: #tags       @= mempty
           <: #timestamps @= mempty
           <: #paragraph  @= mempty
           <: #properties @= mempty
           <: #location   @= mempty
           <: #path       @= mempty
           <: nil

defOther :: Other
defOther = #timestamps @= mempty
           <: #others  @= mempty
           <: #geocode @= mempty
           <: nil

mplusOther :: Other -> Title -> Title
mplusOther o t =
  let
    othersRefine = (^. #others)
    x1 = t  & #timestamps %~ (<> o ^. #timestamps)
    x2 = x1 & #paragraph  %~ (<> othersRefine o)
    (loc, geo) = x2 ^. #location
    x3 = x2 & #location .~ (loc, geo <> (o ^. #geocode))
  in
    x3

tagsP :: Parser [Text]
tagsP = do
  let tagToken = Tx.pack <$> (some $ noneOf [' ', ':', '\t', '\n'])
  let sep = single ':'
  tagname <- between sep (lookAhead sep) tagToken
  loop    <- mempty `option` try tagsP
  return $ tagname : loop

orgstarsP :: Parser Int
orgstarsP = length <$> someTill (single '*') (single ' ')

todoP :: Parser (Maybe Text)
todoP = Nothing `option` (kwd <* many (single ' '))
  where
    kwd = anyP $ [ Just <$> chunk k
                 | k <- [ "TODO" , "DONE" , "WAIT" , "PEND" ]]

indicateP :: Parser ()
indicateP = try indicate >> some (single ' ') >> return ()
  where
    numslash = some digitChar >> single '/' >> some digitChar
    indicate = between (single '[') (single ']') numslash

titleAttachment :: Parser (Maybe Timestamp, [Text])
titleAttachment = do
  ts <- Nothing `option` (Just <$> try timestampP <* many (single ' '))
  tg <- [] `option` try tagsP
  guard $ isJust ts || not (null tg)
  return (ts, tg)

titleP :: Parser Title
titleP = do
  stars  <- orgstarsP
  todo   <- todoP
  _      <- indicateP <|> return ()
  ttl    <- try (manyTill anySingle (lookAhead titleAttachment))
            <|> many anySingle
  (g, t) <- (Nothing, []) `option` titleAttachment
  return $
    #label         @= Tx.stripEnd (Tx.pack ttl)
    <: #level      @= stars
    <: #todo       @= todo
    <: #tags       @= t
    <: #timestamps @= maybeToList g
    <: #paragraph  @= mempty
    <: #properties @= mempty
    <: #location   @= mempty
    <: #path       @= mempty
    <: nil

propertyP :: Parser (Text, Text)
propertyP =
  try (chunk ":PROPERTIES:" >> return ("PROPERTIES", mempty))
  <|> try (chunk ":END:" >> return ("END", mempty))
  <|> (,) <$> pname <*> (some (single ' ') *> pval)
  where
    pname, pval, pnameCore :: Parser Text
    pnameCore = Tx.pack <$> some (noneOf [':', '\n'])
    pname = between (single ':') (single ':') pnameCore
    pval  = Tx.pack <$> ((:) <$> satisfy (/= ' ') <*> many anySingle)

linkP :: Parser Text
linkP = between (chunk "[[") (chunk "]]") linkCore
  where
    linkToken = Tx.pack <$> some (noneOf [']'])
    linkCore  = do
      url  <- linkToken
      guard $ "http" `Tx.isPrefixOf` url
      expr <- Nothing `option` (chunk "][" >> Just <$> linkToken)
      return $ Tx.concat ["<a href=\"", url, "\">"
                         , url `fromMaybe` expr, "</a>"]

geocodeP :: Parser GeocodeSearch
geocodeP = between (chunk "{{") (chunk "}}") geocodeCore
  where
    geocodeToken = Tx.pack <$> some (noneOf ['}'])
    geocodeCore  = do
      geoTarget <- geocodeToken
      expr      <- Nothing `option` (chunk "}{" >> Just <$> geocodeToken)
      return $ GeS geoTarget expr

linebreakP :: Parser LineBreak
linebreakP = chunk "# linebreak" >> return LineBreak

-- ((someTill (single ' ') (lookAhead (single ':')) >> (single ':')) :: Parser (Token Tx.Text)) `parseTest` Tx.pack "   :"
-- >>> ':'

otherRefineP :: Parser Other
otherRefineP = def `option` (loop def <|> literalOnly def)
  where
    def = defOther
    spaces = many (single ' ')
    loop :: Other -> Parser Other
    loop o = anyP [ eof' o
                  , timestamp' o
                  , link' o
                  , geocode' o
                  , withOther o]
    eof' o = eof >> return o
    timestamp' oth = do
      ts <- try (timestampP <* spaces)
      loop (oth & #timestamps %~ (<> [ts]))
    link' oth = do
      lk <- try (linkP <* spaces)
      loop (oth & #others %~ (<> lk))
    geocode' oth = do
      g  <- try (geocodeP <* spaces)
      loop (oth & #geocode %~ (<> [g]))
    withOther oth = do
      let end'  = lookAhead (timestamp' oth <|> link' oth <|> geocode' oth)
      let withP = manyTill anySingle end'
      other <- try withP <|> some anySingle
      loop (oth & #others %~ (<> Tx.pack other))
    literalOnly oth = do
      lo <- manyTill anySingle eof
      loop (oth & #others %~ (<> Tx.pack lo))
{-# INLINE otherRefineP #-}

lineParse :: Parser Line
lineParse = LO defOther `option` (ll <|> lp <|> lb <|> lo)
  where
    ll = LL <$> try titleP
    lp = LP <$> try propertyP
    lb = try linebreakP >> return LB
    lo = LO <$> try otherRefineP
{-# INLINE lineParse #-}

aliveTimes :: Title -> [Timestamp]
aliveTimes ttl = filter notCloseAndActive $ ttl ^. #timestamps
  where
    notCloseAndActive timestamp =
      (timestamp ^. #datetype /= Closed) && (timestamp ^. #active)

instance Nodeable Title where
  isNext t1 t2 = LEQ t1 == LEQ t2
  final paths ttl =
    let pathText = (Tx.intercalate "/" $ map (^. #label) paths) in
      ttl & #path .~ pathText
  scrapFilter ttl =
    let
      hasAliveTime = not $ null $ aliveTimes ttl
      notTODO = isNothing $ ttl ^. #todo
    in
      hasAliveTime && notTODO

searchQuery :: GeocodeSearch -> Text
searchQuery (GeS _ (Just y)) = y
searchQuery (GeS x _) = x

getFirstTime :: Title -> Maybe UTCTime
getFirstTime = fmap (^. #begin) . listToMaybe . sort . aliveTimes
