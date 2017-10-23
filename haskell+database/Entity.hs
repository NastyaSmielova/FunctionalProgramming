module Entity(

    Entity,
     create,
     update,
     display,
     delete,

     findAll,
     findByID,

) where

import Database.HDBC.ODBC
import Database.HDBC
import Data.Int


--basic class for all tables
class Entity entity where
    display :: [entity] -> IO[()]

    create :: IConnection a => entity -> a -> IO Bool
    update :: IConnection a => entity -> a -> IO Bool

    delete :: IConnection a => entity -> a -> IO Bool

    findAll :: IConnection a => a -> IO [entity]
    findByID :: IConnection a => Int32 -> a -> IO [entity]
