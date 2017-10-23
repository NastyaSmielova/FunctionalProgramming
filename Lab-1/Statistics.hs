module Statistics(
       create,
       update,
       display,
       delete,

       findAll,
       findByID,

       Statistics(Statistics),

       statId,
       total,
       resource

) where


import qualified Data.ByteString.Char8 as BS

import Entity
import Database.HDBC.ODBC
import Database.HDBC
import Data.Int
import Text.Printf
type StatisticsID = Int32
type Total = Int32
type Res = Int32

data Statistics = Statistics {
    total :: Int32,
    statId :: Int32,
    resource:: Int32
} deriving (Show)

-- determine all needed functions

instance Entity Statistics where
    display lst = statShow lst
    create stat conn = statCreate (total stat) (resource stat) conn
    delete stat conn = statDelete (statId stat) conn
    update stat conn = statUpdate (statId stat) (total stat) (resource stat)  conn
    findAll conn = statFindAll conn
    findByID id conn = statFindfByID id conn


--create new statistic with  specific data

statCreate :: IConnection a => Total ->Res-> a -> IO Bool

statCreate t res conn = withTransaction conn (statCreate' t res)

statCreate' t res conn = do
    commited <- run conn query [SqlInt32 t, SqlInt32 res]
    return $ commited == 1
    where
        query = "INSERT INTO statistics (total,id_resource) VALUES (?,?)"

--find statistic by its id
statFindfByID :: IConnection a => Int32 -> a -> IO [Statistics]

statFindfByID id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM statistics WHERE id_stat = %d" id
        unpuck [SqlInt32 id, SqlInt32 res, SqlInt32 total] = Statistics{statId=id,total=total,resource = res}
        unpuck x = error $ "Unexpected result: " ++ show x



--delete statistic by its id

statDelete :: IConnection a => StatisticsID -> a -> IO Bool

statDelete id conn = withTransaction conn (statDelete' id)

statDelete' id conn = do
    commited <- run conn query [SqlInt32 id]
    return $ commited == 1
    where
        query = "DELETE FROM statistics WHERE id_stat = ?"


--update statistic with specific id with new data on other fields

statUpdate :: IConnection a => StatisticsID -> Total ->Res -> a -> IO Bool

statUpdate id t res conn =  withTransaction conn (statUpdate' id t res)

statUpdate' id t res conn = do
    commited <- run conn query [SqlInt32 t,SqlInt32 res, SqlInt32 id]
    return $ commited == 1
    where
        query = "UPDATE statistics SET total = ?, id_resource = ? WHERE id_stat = ?"


--find all statistics from the table

statFindAll :: IConnection a => a -> IO [Statistics]
statFindAll conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM statistics"
        unpuck [SqlInt32 id, SqlInt32 res, SqlInt32 total] = Statistics{statId=id,total=total,resource = res}
        unpuck x = error $ "Unexpected result: " ++ show x

--specify show

statShow :: [Statistics] -> IO[()]

statShow lst = sequence (map statShow' lst)

statShow' stat =  putStrLn ((printf "%d. " (statId stat)) ++ (printf "total : %d. " (total stat))
                              ++(printf "id resource = %d. " (resource stat)))
