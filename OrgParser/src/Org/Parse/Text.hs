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
  , TitleBuilder (..)
  , BuilderType
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
  , otherExtremeP
  , lineParse2
  )
where

import           Control.Monad          (guard)
import           Control.Lens           hiding ((:>), noneOf)
import qualified Data.List              as Dl
import           Data.Time              (UTCTime)
import           Data.String            (IsString)
import           Data.Foldable          (fold)
import qualified Data.Text              as Tx
-- import qualified Data.Text.Lazy.Builder as TxLB
import           Data.Maybe             (fromMaybe, maybeToList, isJust
                                        , isNothing, listToMaybe)
import           Data.Extensible
import           Org.Parse.Utility
import           Org.Parse.Time
import           Org.Node
import qualified TextBuilder            as TXB
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.DeepSeq

data Line text = LL (Title text)
               | LP (Text, Text)
               | LB
               | LO (Other text)
               deriving (Show, Eq)
-- data Line text where
--   LL :: TitleBuilder text => Title text -> Line text
--   LP :: TitleBuilder text => (Text, Text) -> Line text
--   LB :: TitleBuilder text => Line text
--   LO :: TitleBuilder text => Other text -> Line text

type Title text = Record
  [ "label"      :> Text
  , "level"      :> Int
  , "todo"       :> Maybe Text
  , "tags"       :> [Text]
  , "timestamps" :> [Timestamp]
  , "paragraph"  :> text
  , "properties" :> [(Text, Text)]
  , "location"   :> (Text, [GeocodeSearch])
  , "path"       :> Text ]

type Other text = Record
  [ "timestamps" :> [Timestamp]
  , "others"     :> text
  , "geocode"    :> [GeocodeSearch]]

data LineBreak            = LineBreak
data GeocodeSearch        = GeS Text (Maybe Text) deriving (Eq, Show)
newtype Link              = Link (Text, Maybe Text) deriving Show
newtype LevelEQTitle text = LEQ (Title text)
newtype Geocode text      = Geo (Title text)

instance NFData GeocodeSearch where rnf = rwhnf
instance NFData (Line a) where rnf = rwhnf

instance Show (Geocode text) where
  show (Geo t) =
    let
      normalActive stamp =
        (stamp ^. #datetype == Normal) && (stamp ^. #active)
      ts = filter normalActive (t ^. #timestamps)
    in
      Tx.unpack (t ^. #label) ++ "(" ++
      Dl.intercalate "・" (map (show . GU . (^. #begin)) ts) ++ ")"

instance Eq (LevelEQTitle text) where
  (LEQ t1) == (LEQ t2) = t1 ^. #level == t2 ^. #level

defTitle :: Monoid text => (Title text)
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

defOther :: Monoid text => (Other text)
defOther = #timestamps @= mempty
           <: #others  @= mempty
           <: #geocode @= mempty
           <: nil

mplusOther :: Monoid text => Other text -> Title text -> Title text
mplusOther o t =
  let
    othersRefine = (^. #others)
    x1           = t  & #timestamps %~ (<> o ^. #timestamps)
    x2           = x1 & #paragraph  %~ (<> othersRefine o)
    (loc, geo)   = x2 ^. #location
    x3           = x2 & #location .~ (loc, geo <> (o ^. #geocode))
  in
    x3

tagsP :: Parser [Text]
tagsP = do
  let tagToken = Tx.pack <$> some (noneOf [' ', ':', '\t', '\n'])
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

titleP :: Monoid text => Parser (Title text)
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

linkP :: TitleBuilder text => Parser text
linkP = between (chunk "[[") (chunk "]]") linkCore
  where
    linkToken = fromToken <$> some (noneOf [']'])
    linkCore  = do
      url  <- linkToken
      guard $ "http" `builderIsPrefixOf` url
      expr <- Nothing `option` (chunk "][" >> Just <$> linkToken)
      return $ builderConcat ["<a href=\"", url, "\">"
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

otherRefineP :: TitleBuilder text => Parser (Other text)
-- otherRefineP = def `option` (loop def <|> literalOnly def)
otherRefineP = def `option` (loop def <|> literalOnly def)
  where
    def = defOther
    spaces = many (single ' ')
    loop :: TitleBuilder text => Other text -> Parser (Other text)
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
      loop (oth & #others %~ (<> fromToken other))
    literalOnly oth = do
      lo <- manyTill anySingle eof
      loop (oth & #others %~ (<> fromToken lo))
{-# INLINE otherRefineP #-}

otherExtremeP :: TitleBuilder text => Parser (Other text)
otherExtremeP = loop defOther mempty
  where
    spaces = many (single ' ')
    eofOperate :: TitleBuilder text => Other text -> text -> Parser (Other text)
    eofOperate o txt   = return (o & #others %~ (<> txt))
    tsOperate o txt = do
      ts <- try timestampP <* spaces
      loop (o & #timestamps %~ (<> [ts])) txt
    lkOperate :: TitleBuilder text =>
      Other text -> text -> Parser (Other text)
    lkOperate o txt = do
      lk <- try linkP <* spaces
      loop o (txt <> lk)
    gcOperate o txt = do
      gc <- try geocodeP <* spaces
      loop (o & #geocode %~ (<> [gc])) txt
    others o txt = do
      s <- anySingle
      loop o (txt <> fromToken [s])
    loop :: TitleBuilder text => Other text -> text -> Parser (Other text)
    loop o txt = do
      try eof >> eofOperate o txt
      <|> tsOperate o txt
      <|> lkOperate o txt
      <|> gcOperate o txt
      <|> others o txt
{-# INLINE otherExtremeP #-}

lineParse :: TitleBuilder text => Parser (Line text)
lineParse = LO defOther `option` (ll <|> lp <|> lb <|> lo)
  where
    ll = LL <$> try titleP
    lp = LP <$> try propertyP
    lb = try linebreakP >> return LB
    lo = LO <$> try otherRefineP
{-# INLINE lineParse #-}

lineParse2 :: TitleBuilder text => Parser (Line text)
lineParse2 = LO defOther `option` (ll <|> lp <|> lb <|> lo)
  where
    ll = LL <$> try titleP
    lp = LP <$> try propertyP
    lb = try linebreakP >> return LB
    lo = LO <$> try otherExtremeP
{-# INLINE lineParse2 #-}

aliveTimes :: Title text -> [Timestamp]
aliveTimes ttl = filter notCloseAndActive $ ttl ^. #timestamps
  where
    notCloseAndActive timestamp =
      (timestamp ^. #datetype /= Closed) && (timestamp ^. #active)

instance Nodeable (Title text) where
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

getFirstTime :: Title text -> Maybe UTCTime
getFirstTime = fmap (^. #begin) . listToMaybe . Dl.sort . aliveTimes

class (Semigroup a, Monoid a, IsString a) => TitleBuilder a where
  fromToken         :: String -> a
  toText            :: a -> Tx.Text
  builderIsPrefixOf :: a -> a -> Bool
  builderConcat     :: [a] -> a
  builderIsPrefixOf tb1 tb2 = toText tb1 `Tx.isPrefixOf` toText tb2
  builderConcat = fold

instance TitleBuilder Tx.Text where
  fromToken         = Tx.pack
  builderIsPrefixOf = Tx.isPrefixOf
  builderConcat     = Tx.concat
  toText            = id

instance TitleBuilder String where
  fromToken = id
  toText = Tx.pack

instance TitleBuilder TXB.TextBuilder where
  fromToken = TXB.string
  toText = TXB.toText

-- type TB = TXB.TextBuilder
type BuilderType = Tx.Text
-- type BuilderType = String
-- type BuilderType = TXB.TextBuilder
