module ACStalks.Database.Transactions.Token (
    createToken,
    validateToken,
    invalidateToken
) where

import ACStalks.Schema.Token
import ACStalks.Schema.User
import ACStalks.Database.Transactions.Utils
import ACStalks.Database.Transactions.User
import ACStalks.Database.DatabaseConnection
import qualified Data.Text as T
import Data.UUID.V4
import Database.HDBC

table = schema "Tokens" 

validateToken :: DatabaseConnection -> T.Text -> IO (Maybe User)
validateToken dbc@(SqlConnection {}) t =
    do
        results   <- sqlQuery dbc
                  ( "SELECT UserID                             \
                   \  FROM " ++ table ++ " WHERE Token = ?;")
                  [ toSql t ] 

        if (length results > 0)
        then getUser dbc (fromSql $ (results !! 0) !! 0)
        else return Nothing
       

invalidateToken :: DatabaseConnection -> User -> IO (Status)
invalidateToken dbc@(SqlConnection {}) usr =
    do
        rows <- sqlExec dbc
                (  "DELETE FROM " ++ table ++ "   \
                 \ WHERE UserID = ?;   ")
                [ toSql $ userId usr ]
         
        if rows > 0  
        then return (Success)
        else return (Failure "")

createToken :: DatabaseConnection -> User -> IO (Token)
createToken dbc@(SqlConnection {}) usr =
    do
        uuid <- nextRandom

        let t = Token { token = T.pack $ show uuid
                      , tokenUserId = userId usr }

        rows <- sqlExec dbc
                (  "INSERT INTO " ++ table ++ "    \
                  \ (Token,                  \
                  \  UserID) VALUES (?,?);")
                [ toSql $ token t 
                , toSql $ tokenUserId t 
                ]

        return t
