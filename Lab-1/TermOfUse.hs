module TermOfUse (

       create,
       update,
       display,
       delete,

       findAll,
       findByID,

       TermOfUse(TermOfUse),

       termId,
       termPermission

) where


import qualified Data.ByteString.Char8 as BS

import Entity
import Database.HDBC.ODBC
import Database.HDBC
import Data.Int
import Text.Printf
type TermID = Int32
type TermPermission = String

data TermOfUse = TermOfUse {
   termPermission :: String,
   termId  :: Int32
} deriving (Show)


-- determine all needed functions

instance Entity TermOfUse where
    display lst = termShow lst
    create termOfUse conn = termCreate (termPermission termOfUse) conn
    delete termOfUse conn = termDelete (termId termOfUse) conn
    update termOfUse conn = termUpdate (termId termOfUse) (termPermission termOfUse) conn
    findAll conn = termFindAll conn
    findByID id conn = termFindfByID id conn


--create new term of use with  specific data
termCreate :: IConnection a => TermPermission -> a -> IO Bool

termCreate permit conn = withTransaction conn (termCreate' permit)

termCreate' permit conn = do
    commited <- run conn query [SqlString permit]
    return $ commited == 1
    where
        query = "INSERT INTO termofuse (permission) VALUES (?)"


--find term of use by its id

termFindfByID :: IConnection a => TermID -> a -> IO [TermOfUse]

termFindfByID id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM termofuse WHERE id_term = %d" id
        unpuck [SqlInt32 id, SqlByteString name] = TermOfUse{termId=id,termPermission=(BS.unpack name)}
        unpuck x = error $ "Unexpected result: " ++ show x



--delete term of use by its id

termDelete :: IConnection a => TermID -> a -> IO Bool

termDelete id conn = withTransaction conn (termDelete' id)

termDelete' id conn = do
    commited <- run conn query [SqlInt32 id]
    return $ commited == 1
    where
        query = "DELETE FROM termofuse WHERE id_term = ?"


--update term of use with specific id with new data on other fields

termUpdate :: IConnection a => TermID -> TermPermission -> a -> IO Bool

termUpdate id name conn =  withTransaction conn (termUpdate' id name)

termUpdate' id name conn = do
    commited <- run conn query [SqlString name, SqlInt32 id]
    return $ commited == 1
    where
        query = "UPDATE termofuse SET permission = ? WHERE id_term = ?"


--find all terms of use from the table

termFindAll :: IConnection a => a -> IO [TermOfUse]
termFindAll conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM termofuse"
        unpuck [SqlInt32 id, SqlByteString name] = TermOfUse{termId=id,termPermission=(BS.unpack name)}
        unpuck x = error $ "Unexpected result: " ++ show x

--specify show

termShow :: [TermOfUse] -> IO[()]

termShow lst = sequence (map termShow' lst)

termShow' term =  putStrLn ((printf "%d. " (termId term)) ++ (termPermission term))

