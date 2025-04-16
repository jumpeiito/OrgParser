{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Prelude
import Org.Document
import Org.ICS            (updateGoogleCalendar, googleCalendar
                          , googleFamilyCalendar)
import Options.Applicative

main :: IO ()
main = execParser argumentParserInfo >>= runMain

runMain :: Argument -> IO ()
runMain Argument {..} = do
  case (orgDocument, calendarUpdate) of
    (Just doc, _)            -> documents doc
    (Nothing, Just "family") -> updateGoogleCalendar googleFamilyCalendar
    (Nothing, Just "f")      -> updateGoogleCalendar googleFamilyCalendar
    (Nothing, Just "office") -> updateGoogleCalendar googleCalendar
    (Nothing, Just "o")      -> updateGoogleCalendar googleCalendar
    (Nothing, Just "")       -> do
      updateGoogleCalendar googleCalendar
      updateGoogleCalendar googleFamilyCalendar
    (_, _) -> error "ArgumentError"

data Argument = Argument
  {
    orgDocument    :: Maybe FilePath
  , calendarUpdate :: Maybe String
  }

argumentParser :: Parser Argument
argumentParser = Argument
  <$> optional (strOption $ short 'f'
                          <> metavar "FILE_PATH"
                          <> help "translate orgFile to yaml file.")
  <*> optional (strOption $ short 'c'
                          <> metavar "Calendar Name"
                          <> help "Google Calendar update from orgfile")

withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (p <**> helper) . progDesc

argumentParserInfo :: ParserInfo Argument
argumentParserInfo = argumentParser `withInfo` "OrgParser"
