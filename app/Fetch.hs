{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Database.PostgreSQL.Simple ( Connection, Only(Only), query, execute )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT) )
import Data.Char (isSpace)
import Text.HTML.Scalpel
    ( scrapeURL, text, (@:), (@=), hasClass, Scraper, Selector )
import Result ( Result(Result) )
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (listToMaybe)

-- Dumb alias but useful for readability

{-
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-}

queryTemplate :: String
queryTemplate = "http://www.duckduckgo.com/html/?q=site%3Alosestudiantes.com"

replaceSpaces :: String -> String
replaceSpaces = map (\c -> if c == ' ' then '+' else c)

makeQuery :: String -> String
makeQuery professorName = replaceSpaces $ queryTemplate ++ " " ++ professorName

scrapeEntryLink :: String -> MaybeT IO String
scrapeEntryLink professorName = MaybeT $ scrapeURL (makeQuery professorName) entryLink
  where
    entryLink :: Scraper String String
    entryLink = text result

    result :: Selector
    result = "a" @: [hasClass "result__url"]

scrapeAverageGrade :: String -> MaybeT IO String
scrapeAverageGrade url = MaybeT $ scrapeURL url grade
  where
    grade = text $ "h5" @: ["id" @= "profesor_promedio"]

clean :: String -> String
clean = ("http://" ++) . filter (not . isSpace)

webScrape :: Connection -> String -> MaybeT IO Result
webScrape conn name =
  do
    link <- scrapeEntryLink name
    grade <- read <$> (scrapeAverageGrade . clean) link
    _ <- liftIO $ store conn name grade
    return $ Result grade

fetchFromDb :: Connection -> String -> MaybeT IO Result
fetchFromDb conn name = MaybeT $ do
    exists <- hasEntry conn name
    guard exists
    xs <- query conn "select (\"averageScore\") from public.scores where name = ?" (Only name)
    return $ listToMaybe xs

hasEntry :: Connection -> String -> IO Bool
hasEntry conn name = do
  [Only n] <- query conn "select count(*) from public.scores where name = ?" (Only name) :: IO [Only Int]
  return $ n > 0

store :: Connection -> String -> Float -> IO Bool
store conn name score = do
  n <- execute conn "insert into public.scores (\"name\", \"averageScore\") VALUES (?, ?)" (name, score)
  return $ n > 0
