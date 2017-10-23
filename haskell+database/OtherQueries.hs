module OtherQueries where
--queries with sereval tables

import Text
import Database.HDBC.ODBC
import Database.HDBC
import Data.Int
import Resource
import qualified Data.ByteString.Char8 as BS
import Text.Printf


-- to find text line from table text
-- by entered author id
textFindTextByUserID :: IConnection a => Int32->a -> IO [Text]
textFindTextByUserID id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM text WHERE  id_text in (  Select id_resource from resource where id_author = %d )" id
        unpuck [SqlInt32 id, SqlInt32 res, SqlByteString text] = Text{textId=id,textT=(BS.unpack text),resource = res}
        unpuck x = error $ "Unexpected result: " ++ show x



-- to find resource accordingly from table called resource
-- by entered term of use id
resourcesFindresByUsageID:: IConnection a => Int32->a -> IO [Resource]
resourcesFindresByUsageID id conn = do
                             rslt <- quickQuery conn query []
                             return $ map unpuck rslt
                             where
                                 query = printf " Select * from resource where id_usage in(Select id_usage from resusage where id_termOfUse = %d ) " id
unpuck [SqlInt32 id, SqlByteString title,SqlInt32 authId,SqlInt32 usId] =
                    Resource{resourceId=id,resourceTitle=(BS.unpack title),usageId = usId,authorId = authId}
unpuck x = error $ "Unexpected result: " ++ show x

-- to text line from the table
-- by entered usage id
--(ussing additional table resource)
textFindTextByUsageID :: IConnection a => Int32->a -> IO [Text]
textFindTextByUsageID id conn = do
    rslt <- quickQuery conn query []
    return $ map unpuck rslt
    where
        query = printf "SELECT * FROM text WHERE  id_text in(  Select id_resource from resource where id_usage in(Select id_usage from resusage where id_termOfUse = %d) )" id
        unpuck [SqlInt32 id, SqlInt32 res, SqlByteString text] = Text{textId=id,textT=(BS.unpack text),resource = res}
        unpuck x = error $ "Unexpected result: " ++ show x