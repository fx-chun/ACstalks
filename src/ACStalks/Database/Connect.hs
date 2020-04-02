module ACStalks.Database.Connect (
    connectSql,
    disconnect
) where

import ACStalks.Database.DatabaseConnection
import ACStalks.Database.SqlSettings
import Database.HDBC.Sqlite3 (connectSqlite3)
import qualified Database.HDBC as H

connectSql :: SqlSettings -> IO (DatabaseConnection)

connectSql s@(SQLiteSettings filePath) = 
    do
        conn <- connectSqlite3 filePath 
        return (SqlConnection { dbConnSqlSettings = s
                              , dbConnSqlConnection = H.ConnWrapper conn })

disconnect :: DatabaseConnection -> IO ()
disconnect = H.disconnect . dbConnSqlConnection
