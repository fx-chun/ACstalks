module ACStalks.Database.Transactions.HotItem (
    insertHotItem,
    getHotItemByUid,
    getNRandomHotItems,
    searchHotItems
) where

import ACStalks.Schema.HotItem
import ACStalks.Schema.User
import ACStalks.Database.Transactions.Utils
import ACStalks.Database.DatabaseConnection
import qualified Data.Text as T
import qualified Data.Time as Time
import Database.HDBC

table = schema "HotItems" 

hotItemConstructor :: [SqlValue] -> HotItem
hotItemConstructor p = HotItem { hotItemId             = fromSql $ (p !! 0)
                               , hotItem               = fromSql $ (p !! 1)
                               , hotItemTime           = fromSql $ (p !! 2)
                               , hotItemTimezone       = read $ fromSql $ (p !! 3)
                               , hotItemUserId         = fromSql $ (p !! 4)
                               }

getHotItemByUid :: DatabaseConnection -> Int -> IO (Maybe HotItem)
getHotItemByUid dbc@(SqlConnection {}) uid =
    do
        results <- sqlQuery dbc
                   ( "SELECT                                    \
                    \ HotItemID, HotItem, HotItemTime,          \
                    \ HotItemTimezone, UserID                   \
                    \ FROM " ++ table ++ " WHERE UserID = ?     \
                    \ ORDER BY HotItemTime DESC")
                   [ toSql $ uid ] 

        if (length results == 0) 
        then return Nothing
        else let h = results !! 0
             in return (Just (hotItemConstructor h))

searchHotItems :: DatabaseConnection -> T.Text -> IO ([HotItem])
searchHotItems dbc@(SqlConnection {}) str =
    do
        results <- sqlQuery dbc
                   ( "SELECT                                    \
                    \ HotItemID, HotItem, MAX(HotItemTime),     \
                    \ HotItemTimezone, UserID                   \
                    \ FROM " ++ table ++ "                      \
                    \ WHERE HotItem LIKE ?                      \
                    \ GROUP BY UserId                           \
                    \ ORDER BY HotItemTime                      \
                    \ LIMIT 10;") [ toSql $ T.pack . (\x -> "%" ++ x ++ "%") . T.unpack $ T.toLower str ]
        return (map hotItemConstructor results) 

getNRandomHotItems :: DatabaseConnection -> Int -> IO ([HotItem])
getNRandomHotItems dbc@(SqlConnection {}) n =
    do
        results <- sqlQuery dbc
                   ( "SELECT                                    \
                    \ HotItemID, HotItem, MAX(HotItemTime) as HotItemTime,     \
                    \ HotItemTimezone, UserID                   \
                    \ FROM " ++ table ++ "                      \
                    \ GROUP BY UserID                           \ 
                    \ ORDER BY HotItemTime LIMIT " ++ show n ++ ";") []
        return (map hotItemConstructor results) 
 
insertHotItem :: DatabaseConnection -> HotItem -> IO (Status)
insertHotItem dbc@(SqlConnection {}) h =
    do
        rows <- sqlExec dbc
                (  "INSERT INTO " ++ table ++ "    \
                  \ (HotItemID,                    \
                  \  HotItem,                      \
                  \  HotItemTime,                  \
                  \  HotItemTimezone,              \
                  \  UserID) VALUES (NULL,?,?,?,?);")
                [ toSql $ T.toLower $ hotItem h 
                , toSql $ hotItemTime h
                , toSql $ Time.timeZoneOffsetString $ hotItemTimezone h
                , toSql $ hotItemUserId h ]
        
        if rows > 0  
        then return (Success)
        else return (Failure "")
