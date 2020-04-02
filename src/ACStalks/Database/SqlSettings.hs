module ACStalks.Database.SqlSettings (
    SqlSettings(..)
) where

data SqlSettings = SQLiteSettings    { sqlFilePath    :: String }
                 | PostgresSettings  { sqlHost        :: String
                                     , sqlDatabase    :: String
                                     , sqlUsername    :: String
                                     , sqlPassword    :: String }
