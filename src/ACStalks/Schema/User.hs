{-# LANGUAGE OverloadedStrings #-}

module ACStalks.Schema.User (
    User(..),
    IslandOpen(..),

    userDefaults
) where

import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics

data IslandOpen = IslandOpen | IslandSeeBio | IslandFriends | IslandClosed
    deriving (Show, Read, Generic)

instance ToJSON IslandOpen
instance FromJSON IslandOpen

data User = User { userId :: Int
                 , userName :: T.Text
                 , userNickname :: T.Text
                 , userPasshash :: T.Text
                 , userSecurityAnswer :: T.Text
                 , userSwitchFc :: T.Text
                 , userDodoCode :: T.Text
                 , userIslandOpen :: IslandOpen 
                 , userIslandOpenTime :: Time.UTCTime
                 , userBio :: T.Text
                 , userFavVillager :: T.Text
                 , userFavThing :: T.Text
                 }
    deriving (Show)

userDefaults = User { userNickname = "User"
                    , userSecurityAnswer = ""
                    , userSwitchFc = "SW-"
                    , userDodoCode = "-----"
                    , userIslandOpen = IslandClosed
                    , userIslandOpenTime = Time.parseTimeOrError True Time.defaultTimeLocale "%s" "0"
                    , userBio = ""
                    , userFavVillager = ""
                    , userFavThing = "" }
