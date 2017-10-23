module Resource (
       create,
       update,
       display,
       delete,

       findAll,
       findByID,

       Resource(Resource),

       authorId,
       usageId,
       resourceId,
       resourceTitle

) where


import qualified Data.ByteString.Char8 as BS

import Entity
import Database.HDBC.ODBC
import Database.HDBC
import Data.Int
import Text.Printf
type ResourceID = Int32
type AuthorID = Int32
type UsageID = Int32
type ResourceTitle = String

data Resource = Resource {
   resourceTitle :: String,
    resourceId :: Int32,
    authorId :: Int32,
    usageId :: Int32
} deriving (Show)



-- determine all needed functions
instance Entity Resource where
    display lst = resourceShow lst
    create resource conn = resourceCreate (authorId resource) (usageId resource) (resourceTitle resource) conn
    delete resource conn = resourceDelete (resourceId resource) conn
    update resource conn = resourceUpdate (resourceId resource) (authorId resource) (usageId resource) (resourceTitle resource) conn
    findAll conn = resourceFindAll conn
    findByID id conn = resourceFindfByID id conn


--create new resource with  specific data

resourceCreate :: IConnection a =>AuthorID-> UsageID -> ResourceTitle ->a -> IO Bool

resourceCreate authID usID title conn = withTransaction conn (resourceCreate' authID usID title)

resourceCreate' authID usID title conn = do
    commited <- run conn query [SqlString title,SqlInt32 authID,SqlInt32 usID]
    return $ commited == 1
    where
        query = "INSERT INTO resource (title,id_author,id_usage) VALUES (?,?,?)"


--find resource by his id

resourceFindfByID :: IConnection a => Int32 -> a -> IO [Resource]

resourceFindfByID id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM resource WHERE id_resource = %d" id
        unpuck [SqlInt32 id, SqlByteString title,SqlInt32 authId,SqlInt32 usId] =
            Resource{resourceId=id,resourceTitle=(BS.unpack title),usageId = usId,authorId = authId}
        unpuck x = error $ "Unexpected result: " ++ show x


--delete resource by its id


resourceDelete :: IConnection a => ResourceID -> a -> IO Bool

resourceDelete id conn = withTransaction conn (resourceDelete' id)

resourceDelete' id conn = do
    commited <- run conn query [SqlInt32 id]
    return $ commited == 1
    where
        query = "DELETE FROM resource WHERE id_resource = ?"


--update resource with specific id with new data on other fields
resourceUpdate :: IConnection a => ResourceID -> AuthorID-> UsageID -> ResourceTitle -> a -> IO Bool

resourceUpdate id authID usID title conn =  withTransaction conn (resourceUpdate' id authID usID title)

resourceUpdate' id authID usID title conn = do
    commited <- run conn query [SqlString title,SqlInt32 authID,SqlInt32 usID,SqlInt32 id]
    return $ commited == 1
    where
        query = "UPDATE resource SET title=?, id_author=?,id_usage = ?  WHERE id_resource = ?"


--find all resources from the table

resourceFindAll :: IConnection a => a -> IO [Resource]
resourceFindAll conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM resource"
        unpuck [SqlInt32 id, SqlByteString title,SqlInt32 authId,SqlInt32 usId] =
                    Resource{resourceId=id,resourceTitle=(BS.unpack title),usageId = usId,authorId = authId}
        unpuck x = error $ "Unexpected result: " ++ show x

--specify show

resourceShow :: [Resource] -> IO[()]

resourceShow lst = sequence (map resourceShow' lst)

resourceShow' resource =  putStrLn ((printf "%d. " (resourceId resource)) ++
        (resourceTitle resource) ++ (printf " %d " (authorId resource)) ++ (printf "  %d " (usageId resource)))
