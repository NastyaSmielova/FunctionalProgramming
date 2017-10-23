module Author(
       create,
       update,
       display,
       delete,

       findAll,
       findByID,

       Author(Author),

       authorId,
       authorName

) where


import qualified Data.ByteString.Char8 as BS

import Entity
import Database.HDBC.ODBC
import Database.HDBC
import Data.Int
import Text.Printf
type AuthorID = Int32
type AuthorName = String

data Author = Author {
   authorName :: String,
    authorId :: Int32
} deriving (Show)


-- determine all needed functions
instance Entity Author where
    display lst = authorShow lst
    create author conn = authorCreate (authorName author) conn
    delete author conn = authorDelete (authorId author) conn
    update author conn = authorUpdate (authorId author) (authorName author) conn
    findAll conn = authorFindAll conn
    findByID id conn = authorFindfByID id conn



--create new author with  specific data
authorCreate :: IConnection a => AuthorName -> a -> IO Bool
authorCreate name conn = withTransaction conn (authorCreate' name)
authorCreate' name conn = do
    commited <- run conn query [SqlString name]
    return $ commited == 1
    where
        query = "INSERT INTO authors (name) VALUES (?)"


--find author by his id
authorFindfByID :: IConnection a => Int32 -> a -> IO [Author]
authorFindfByID id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM authors WHERE id_author = %d" id
        unpuck [SqlInt32 id, SqlByteString name] = Author{authorId=id,authorName=(BS.unpack name)}
        unpuck x = error $ "Unexpected result: " ++ show x



--delete author by his id

authorDelete :: IConnection a => AuthorID -> a -> IO Bool
authorDelete id conn = withTransaction conn (authorDelete' id)

authorDelete' id conn = do
    commited <- run conn query [SqlInt32 id]
    return $ commited == 1
    where
        query = "DELETE FROM authors WHERE id_author = ?"


--update author with specific id with new data on other fields

authorUpdate :: IConnection a => AuthorID -> AuthorName -> a -> IO Bool

authorUpdate id name conn =  withTransaction conn (authorUpdate' id name)

authorUpdate' id name conn = do
    commited <- run conn query [SqlString name, SqlInt32 id]
    return $ commited == 1
    where
        query = "UPDATE authors SET name = ? WHERE id_author = ?"


--find all authors from the table
authorFindAll :: IConnection a => a -> IO [Author]
authorFindAll conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM authors"
        unpuck [SqlInt32 id, SqlByteString name] = Author{authorId=id,authorName=(BS.unpack name)}
        unpuck x = error $ "Unexpected result: " ++ show x



--specify show
authorShow :: [Author] -> IO[()]

authorShow lst = sequence (map authorShow' lst)

authorShow' author =  putStrLn ((printf "%d. " (authorId author)) ++(authorName author))