module Usage (
       create,
       update,
       display,
       delete,

       findAll,
       findByID,

       Usage(Usage),

       usageId,
       time,
       idTerm,
       date

) where


import qualified Data.ByteString.Char8 as BS

import Entity
import Database.HDBC.ODBC
import Database.HDBC
import Data.Int
import Data.Time.LocalTime
import Data.Time.Calendar
import Text.Printf

type UsageIdTerm = Int32
type UsageTime = Int32
type UsageID = Int32
type UsageDate = Day

data Usage = Usage {
   idTerm :: Int32,
   usageId :: Int32,
   time :: Int32,
   date :: Day
} deriving (Show)



-- determine all needed functions

instance Entity Usage where
    display lst = usageShow lst
    create usage conn = usageCreate (idTerm usage)  (time usage) (date usage)  conn
    delete usage conn = usageDelete (usageId usage) conn
    update usage conn = usageUpdate (usageId usage) (idTerm usage) (time usage) (date usage) conn
    findAll conn = usageFindAll conn
    findByID id conn = usageFindfByID id conn



--create new usage with  specific data

usageCreate :: IConnection a => UsageIdTerm->UsageTime->UsageDate-> a -> IO Bool

usageCreate id_term time date conn = withTransaction conn (usageCreate'  id_term time date )

usageCreate' id_term time date conn = do
    commited <- run conn query [SqlInt32 id_term,SqlInt32 time,SqlLocalDate date]
    return $ commited == 1
    where
     query = "INSERT INTO resUsage (id_termOfUse,timeforuse,dateOpen) VALUES (?,?,?)"


--find usage by its id

usageFindfByID :: IConnection a => UsageID -> a -> IO [Usage]

usageFindfByID id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM resUsage WHERE id_usage = %d" id
        unpuck [SqlInt32 id, SqlLocalDate d,SqlInt32 t, SqlInt32 idTerm ] =
          Usage{usageId=id,date = d,time = t,idTerm= idTerm }
        unpuck x = error $ "Unexpected result: " ++ show x



--delete usage by its id

usageDelete :: IConnection a => UsageID -> a -> IO Bool

usageDelete id conn = withTransaction conn (usageDelete' id)

usageDelete' id conn = do
    commited <- run conn query [SqlInt32 id]
    return $ commited == 1
    where
        query = "DELETE FROM resUsage WHERE id_usage = ?"


--update usage with specific id with new data on other fields

usageUpdate :: IConnection a => UsageID -> UsageTime ->UsageIdTerm->UsageDate-> a -> IO Bool

usageUpdate id time termId date conn =  withTransaction conn (usageUpdate' id time termId date)

usageUpdate' id time termId date conn = do
    commited <- run conn query [SqlInt32 time, SqlInt32 termId,SqlLocalDate date, SqlInt32 id]
    return $ commited == 1
    where
        query = "UPDATE resUsage SET timeForUse= ?, id_termOfUse = ?, dateOpen = ? WHERE id_usage = ?"


--find all usages from the table
usageFindAll :: IConnection a => a -> IO [Usage]
usageFindAll conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM resUsage"
        unpuck [SqlInt32 id, SqlLocalDate date,SqlInt32 time, SqlInt32 idTerm ] =
          Usage{usageId=id,date =date,time = time,idTerm= idTerm }
        unpuck x = error $ "Unexpected result: " ++ show x

--specify show

usageShow :: [Usage] -> IO[()]

usageShow lst = sequence (map usageShow' lst)

usageShow' usage =  putStrLn ((printf "%d. " (usageId usage))++ (printf "durtion %d " (time usage))++
                     (printf " id term of use %d. " (idTerm usage)) )