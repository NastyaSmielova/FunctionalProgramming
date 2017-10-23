module Main where

import Database.HDBC
import Database.HDBC.ODBC
import Entity
import Author
import TermOfUse
import Data.Int
import System.IO
import System.Exit
import Control.Monad
import Text.Printf
import Usage
import Text
import Resource
import Statistics
import Data.Time
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import OtherQueries

main :: IO ()
-- do while user doesn't want to exit
main = forever (printMenu >> readChoice >>= menuAction)


--show all available operations
printMenu = do
    putStrLn "\nChoose action :"
    putStrLn "0 exit"
    putStrLn "1 add author (enter name)"
    putStrLn "2 delete author (enter id)"
    putStrLn "3 change author's name (enter id, name)"
    putStrLn "4 show all authors "
    putStrLn "5 find author by id (enter id)"


    putStrLn "6 add term of use (enter permission)"
    putStrLn "7 delete term of use (enter id)"
    putStrLn "8 change term of use's permission (enter id, permission)"
    putStrLn "9 show all term of use "
    putStrLn "10 find term of use by id (enter id)"

    putStrLn "11 add usage (enter id term of use, duration, date)"
    putStrLn "12 delete usage (enter id)"
    putStrLn "13 change usage's data (enter id,id term of use, duration, date )"
    putStrLn "14 show all usages "
    putStrLn "15 find usage by id (enter id)"


    putStrLn "16 add resource (enter title, author id, usage id)"
    putStrLn "17 delete resource (enter id)"
    putStrLn "18 change resource's data (enter id, title, author id, usage id)"
    putStrLn "19 show all resources "
    putStrLn "20 find resource by id (enter id)"

    putStrLn "21 add text (enter text, resource id)"
    putStrLn "22 delete text (enter id)"
    putStrLn "23 change text's data (enter id,text, resource id)"
    putStrLn "24 show all texts "
    putStrLn "25 find text by id (enter id)"

    putStrLn "26 add statisctics (enter total ammount, resource id)"
    putStrLn "27 delete statisctics (enter id)"
    putStrLn "28 change statisctics's data (enter id,total ammount, resource id)"
    putStrLn "29 show all statisctics "
    putStrLn "30 find statisctics by id (enter id)"

    putStrLn "31 find text by user id (enter id)"
    putStrLn "32 find resource by usage id (enter id)"
    putStrLn "33 find text by usage id (enter id)"
    putStr "\nYou want to do action no. : "
    hFlush stdout

readChoice = getLine


--parse year day and month int Day
parseDate :: (Int,Int,Int) -> Day
parseDate (y,d,m) =  fromGregorian  (toInteger y) d m

--read answer from user
readInt32 = do
    s <- getLine
    return (read s :: Int32)

--specify the answer by input data
menuAction "0" = exitSuccess

menuAction "1" = do
    nm <- getLine
    conn <- connectODBC "DSN=mysql_1_lab"
    create Author{authorName=nm} conn
    putStrLn "-- ok"


menuAction "2" = do
      id <- readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      delete Author{Author.authorId = id} conn

      putStrLn "-- ok"

menuAction "3" = do
      id <- readInt32
      nm <- getLine
      conn <- connectODBC "DSN=mysql_1_lab"
      update Author{Author.authorId = id , authorName = nm} conn

      putStrLn "-- ok"


menuAction "4" = do
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findAll conn :: IO[Author]
    display lst
    putStrLn "-- ok"


menuAction "5" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findByID id conn :: IO[Author]
    display lst
    putStrLn "-- ok"

menuAction "6" = do
    text <- getLine
    conn <- connectODBC "DSN=mysql_1_lab"
    create TermOfUse{termPermission = text} conn
    putStrLn "-- ok"


menuAction "7" = do
      id <- readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      delete TermOfUse{termId = id} conn

      putStrLn "-- ok"

menuAction "8" = do
      id <- readInt32
      text <- getLine
      conn <- connectODBC "DSN=mysql_1_lab"
      update TermOfUse{termId = id , termPermission = text} conn

      putStrLn "-- ok"


menuAction "9" = do
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findAll conn :: IO[TermOfUse]
    display lst
    putStrLn "-- ok"


menuAction "10" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findByID id conn :: IO[TermOfUse]
    display lst
    putStrLn "-- ok"

menuAction "11" = do
    id_term <- readInt32
    t <- readInt32
    date <- getLine
    y <-readInt32
    d <-readInt32
    m <-readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    create Usage{ idTerm = id_term, time = t, Usage.date = parseDate ( (fromIntegral y),(fromIntegral d),(fromIntegral m)) } conn
    putStrLn "-- ok"


menuAction "12" = do
      id <- readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      delete Usage{idTerm = id} conn

      putStrLn "-- ok"

menuAction "13" = do
      id <- readInt32
      idT <- readInt32
      t <- readInt32
    --  date <- getLine
      y <-readInt32
      d <-readInt32
      m <-readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      update Usage{Usage.usageId = id ,idTerm = idT , time = t,Usage.date = parseDate ( (fromIntegral y),(fromIntegral d),(fromIntegral m))} conn

      putStrLn "-- ok"


menuAction "14" = do
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findAll conn :: IO[Usage]
    display lst
    putStrLn "-- ok"


menuAction "15" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findByID id conn :: IO[Usage]
    display lst
    putStrLn "-- ok"


menuAction "16" = do
    nm <- getLine
    author <- readInt32
    usage<- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    create Resource{Resource.authorId = author, Resource.usageId =usage, resourceTitle=nm} conn
    putStrLn "-- ok"


menuAction "17" = do
      id <- readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      delete Resource{resourceId = id} conn

      putStrLn "-- ok"

menuAction "18" = do
      id <- readInt32
      nm <- getLine
      author <- readInt32
      usage<- readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      update Resource{Resource.resourceId = id , Resource.authorId = author, Resource.usageId =usage, resourceTitle=nm} conn

      putStrLn "-- ok"


menuAction "19" = do
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findAll conn :: IO[Resource]
    display lst
    putStrLn "-- ok"


menuAction "20" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findByID id conn :: IO[Resource]
    display lst
    putStrLn "-- ok"


menuAction "21" = do
    text <- getLine
    res <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    create Text{textT = text,Text.resource = res} conn
    putStrLn "-- ok"


menuAction "22" = do
      id <- readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      delete Text{textId = id} conn

      putStrLn "-- ok"

menuAction "23" = do
      id <- readInt32
      text <- getLine
      res <- readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      update Text{textId = id,textT = text,Text.resource = res} conn

      putStrLn "-- ok"


menuAction "24" = do
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findAll conn :: IO[Text]
    display lst
    putStrLn "-- ok"


menuAction "25" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findByID id conn :: IO[Text]
    display lst
    putStrLn "-- ok"


menuAction "26" = do
    total <- readInt32
    res <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    create Statistics{total = total,Statistics.resource = res} conn
    putStrLn "-- ok"


menuAction "27" = do
      id <- readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      delete Statistics{statId = id} conn

      putStrLn "-- ok"

menuAction "28" = do
      id <- readInt32
      res <- readInt32
      t <- readInt32
      conn <- connectODBC "DSN=mysql_1_lab"
      update Statistics{statId = id,total = t,Statistics.resource = res} conn

      putStrLn "-- ok"


menuAction "29" = do
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findAll conn :: IO[Statistics]
    display lst
    putStrLn "-- ok"


menuAction "30" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findByID id conn :: IO[Statistics]
    display lst
    putStrLn "-- ok"

menuAction "30" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- findByID id conn :: IO[Statistics]
    display lst
    putStrLn "-- ok"

menuAction "31" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- textFindTextByUserID id conn :: IO[Text]
    display lst
    putStrLn "-- ok"



menuAction "32" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- resourcesFindresByUsageID id conn :: IO[Resource]
    display lst
    putStrLn "-- ok"

menuAction "33" = do
    id <- readInt32
    conn <- connectODBC "DSN=mysql_1_lab"
    lst <- textFindTextByUsageID id conn :: IO[Text]
    display lst
    putStrLn "-- ok"


--ifuser has entered some wierd stuff
menuAction _ = hPutStrLn stderr "\nInvalid choice."