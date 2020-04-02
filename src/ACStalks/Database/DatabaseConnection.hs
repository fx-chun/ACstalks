module ACStalks.Database.DatabaseConnection (
    DatabaseConnection(..)
) where 

import ACStalks.Database.SqlSettings (SqlSettings)
import Database.HDBC (ConnWrapper)

data DatabaseConnection = SqlConnection { dbConnSqlSettings   :: SqlSettings
                                        , dbConnSqlConnection :: ConnWrapper }
