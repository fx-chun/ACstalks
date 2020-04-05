{-# LANGUAGE OverloadedStrings #-}

module Main where

import ACStalks.API
import ACStalks.Database
import ACStalks.Schema
import qualified Data.Time as Time 

main :: IO ()
main = do
    dbc <- connectSql (SQLiteSettings "database.db")
    now <- Time.getCurrentTime 

    result <- insertUser dbc (User { userName = "admin" 
                             , userNickname = "Isabelle"
                             , userPasshash = "$2y$12$yldakotASgs1RpH0uNBeD.3Je8.lnicWXNbtPJH4pDw529/cDIywe"
                             , userSecurityAnswer = ""
                             , userSwitchFc = ""
                             , userDodoCode = "-----"
                             , userIslandOpen = IslandClosed
                             , userIslandOpenTime = now
                             , userBio = ""
                             , userFavVillager = ""
                             , userFavThing = ""
                             , userNativeFruit = "" })

    result2 <- getUser dbc 1
    
    putStrLn $ show result
    putStrLn $ show result2

    startServer dbc 8080 

    disconnect dbc
