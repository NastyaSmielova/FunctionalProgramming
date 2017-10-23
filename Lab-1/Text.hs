module Text(
       create,
       update,
       display,
       delete,

       findAll,
       findByID,

       Text(Text),

       textId,
       textT,
       resource

) where


import qualified Data.ByteString.Char8 as BS

import Entity
import Database.HDBC.ODBC
import Database.HDBC
import Data.Int
import Text.Printf
type TextID = Int32
type TextT = String
type Res = Int32

data Text = Text {
   textT :: String,
    textId :: Int32,
    resource:: Int32
} deriving (Show)



-- determine all needed functions

instance Entity Text where
    display lst = textShow lst
    create text conn = textCreate (textT text) (resource text) conn
    delete text conn = textDelete (textId text) conn
    update text conn = textUpdate (textId text) (textT text) (resource text)  conn
    findAll conn = textFindAll conn
    findByID id conn = textFindfByID id conn


--create new text line with  specific data
textCreate :: IConnection a => TextT ->Res-> a -> IO Bool

textCreate t res conn = withTransaction conn (textCreate' t res)

textCreate' t res conn = do
    commited <- run conn query [SqlString t, SqlInt32 res]
    return $ commited == 1
    where
        query = "INSERT INTO text (text,id_resoure) VALUES (?,?)"



--find text by its id

textFindfByID :: IConnection a => TextID -> a -> IO [Text]

textFindfByID id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM text WHERE id_text = %d" id
        unpuck [SqlInt32 id, SqlInt32 res, SqlByteString text] = Text{textId=id,textT=(BS.unpack text),resource = res}
        unpuck x = error $ "Unexpected result: " ++ show x


--delete text line by its id


textDelete :: IConnection a => TextID -> a -> IO Bool

textDelete id conn = withTransaction conn (textDelete' id)

textDelete' id conn = do
    commited <- run conn query [SqlInt32 id]
    return $ commited == 1
    where
        query = "DELETE FROM text WHERE id_text = ?"


--update text info with specific id with new data on other fields

textUpdate :: IConnection a => TextID -> TextT ->Res -> a -> IO Bool

textUpdate id t res conn =  withTransaction conn (textUpdate' id t res)

textUpdate' id t res conn = do
    commited <- run conn query [SqlString t,SqlInt32 res, SqlInt32 id]
    return $ commited == 1
    where
        query = "UPDATE text SET text = ?, id_resoure = ? WHERE id_text = ?"


--find all text info from the table
textFindAll :: IConnection a => a -> IO [Text]
textFindAll conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = "SELECT * FROM text"
        unpuck [SqlInt32 id, SqlInt32 res, SqlByteString text] = Text{textId=id,textT=(BS.unpack text),resource = res}
        unpuck x = error $ "Unexpected result: " ++ show x

--specify show

textShow :: [Text] -> IO[()]

textShow lst = sequence (map textShow' lst)

textShow' text =  putStrLn ((printf "%d. " (textId text)) ++ (textT text) ++(printf " resource id : %d " (resource text)))