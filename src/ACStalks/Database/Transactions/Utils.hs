module ACStalks.Database.Transactions.Utils (
    Status(..),
    schema,
    sqlExec,
    sqlQuery
) where

import ACStalks.Database.DatabaseConnection
import qualified Data.Text as T
import Database.HDBC

data Status = Success | Failure String 
    deriving (Show)

schema x = (x ++ "_Schema1")

sqlQuery :: DatabaseConnection -> String -> [SqlValue] -> IO ([[SqlValue]])
sqlQuery dbc = withWConn (dbConnSqlConnection dbc) quickQuery'

sqlExec :: DatabaseConnection -> String -> [SqlValue] -> IO (Integer)
sqlExec dbc query vals = do
    n <- withWConn (dbConnSqlConnection dbc) run query vals
    commit (dbConnSqlConnection dbc)
    return n
