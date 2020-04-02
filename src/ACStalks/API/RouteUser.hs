module ACStalks.API.RouteUser (
    UserApi,
    userServer
) where

import ACStalks.Captcha
import ACStalks.Database
import ACStalks.Schema
import Control.Monad
import Control.Monad.IO.Class
import Crypto.BCrypt
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Maybe
import GHC.Generics
import Servant

type UserApi = "login"
                :> ReqBody '[JSON] RequestLogin
                :> Post '[JSON] ResponseLogin
          :<|> "register"
                :> ReqBody '[JSON] RequestRegister
                :> Post '[JSON] ResponseRegister
          :<|> "search"
                :> ReqBody '[JSON] [Int]
                :> Post '[JSON] [ResponseUserSearch]
          :<|> "update" 
                :> ReqBody '[JSON] RequestUpdate
                :> Post '[PlainText] String 
          :<|> "price"
                :> ReqBody '[JSON] RequestPrice 
                :> Post '[PlainText] String
          :<|> "hotitem"
                :> ReqBody '[JSON] RequestHotItem
                :> Post '[PlainText] String
validate :: DatabaseConnection -> T.Text -> IO (a) -> (User -> IO (a)) -> IO (a)
validate dbc t invalidAction validAction =
    do
        maybeUser <- validateToken dbc t
        case maybeUser of 
            Just usr -> validAction usr
            Nothing  -> invalidAction

hashPassword' :: T.Text -> IO T.Text
hashPassword' p = fmap (T.pack . B.unpack)
                $ fmap fromJust 
                $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (B.pack . T.unpack $ p)

userServer :: DatabaseConnection -> Server UserApi
userServer dbc = login :<|> register :<|> search
            :<|> update :<|> pricePost :<|> hotItemPost
    where
        validate' = validate dbc 

        login :: RequestLogin -> Handler ResponseLogin 
        login req = liftIO $ 
            do
                maybeUser <- getUserByUsername dbc (loginUsername req)

                case maybeUser of
                    Just user -> do 
                        let hash = T.unpack $ userPasshash user
                            attempt = T.unpack $ loginPassword req
                            isValid = validatePassword (B.pack hash) (B.pack attempt)
                        if isValid 
                        then do
                            t <- createToken dbc user
                            return (ResponseLogin (Just $ token t) (Just $ userId user))
                        else return (ResponseLogin Nothing Nothing)
                    Nothing   -> return (ResponseLogin Nothing Nothing)

        register :: RequestRegister -> Handler ResponseRegister
        register req = liftIO $ validateRegistration $
            do
                now <- Time.getCurrentTime
                hash <- hashPassword' (regPassword req)
                
                let newUser = User { userName           = regUsername req
                                   , userNickname       = regNickname req
                                   , userPasshash       = hash 
                                   , userSecurityAnswer = T.pack ""
                                   , userSwitchFc       = T.pack ""
                                   , userDodoCode       = T.pack ""
                                   , userIslandOpen     = IslandClosed
                                   , userIslandOpenTime = now
                                   , userBio            = T.pack ""
                                   , userFavVillager    = T.pack ""
                                   , userFavThing       = T.pack "" } -- TODO defaults?
                insertUser dbc newUser
                maybeUser <- getUserByUsername dbc (regUsername req)
 
                case maybeUser of
                   Just user -> do
                       t <- createToken dbc user
                       return (ResponseRegister (Just $ token t))
                   Nothing -> do return (ResponseRegister Nothing)
            where 
                validateRegistration success =
                    do
                        result <- validateCaptcha (regCaptcha req)
                        if result
                        then success
                        else return (ResponseRegister Nothing)

        search :: [Int] -> Handler [ResponseUserSearch]
        search uids = liftIO $
            do
                users <- mapM (getUser dbc) uids
                return (map stripUser $ catMaybes users) 
            where
                stripUser usr =
                    ResponseUserSearch { searchId = userId usr
                                       , searchUserName = userName usr
                                       , searchNickname = userNickname usr
                                       , searchSwitchFc = userSwitchFc usr
                                       , searchDodoCode = userDodoCode usr
                                       , searchIslandOpen = userIslandOpen usr
                                       , searchIslandOpenTime = userIslandOpenTime usr 
                                       , searchBio = userBio usr  
                                       , searchFavVillager = userFavVillager usr  
                                       , searchFavThing = userFavThing usr } 
 
        update :: RequestUpdate -> Handler String 
        update req = liftIO $ validate' (reqUpdateToken req) (return "false") update' 
            where
                updaters = [ nicknameUpdater
                           , passwordUpdater
                           , switchFcUpdater
                           , islandUpdater 
                           , bioUpdater
                           , favUpdater ]

                nicknameUpdater usr req = return $
                    case (reqUpdateNickname req) of
                        Just n  -> usr { userNickname = n }
                        Nothing -> usr

                passwordUpdater usr req = do
                    case (reqUpdatePassword req) of
                        Just p -> do
                            hash <- hashPassword' p 
                            return usr { userPasshash = hash }
                        Nothing -> return usr

                switchFcUpdater usr req = return $
                    case (reqUpdateSwitchFc req) of
                        Just fc -> usr { userSwitchFc = fc }
                        Nothing -> usr 

                islandUpdater usr req = do
                    case (reqUpdateIslandOpen req) of
                        Just status ->
                            case status of
                                IslandOpen -> 
                                    case (reqUpdateDodoCode req) of
                                        Just dodo -> 
                                            do
                                                now <- Time.getCurrentTime
                                                return usr { userIslandOpen     = IslandOpen
                                                           , userDodoCode       = dodo
                                                           , userIslandOpenTime = now }
                                        Nothing   -> return usr
                                IslandSeeBio ->  return usr { userIslandOpen = IslandSeeBio 
                                                            , userDodoCode   = T.pack "-----" }
                                IslandFriends -> return usr { userIslandOpen = IslandFriends 
                                                            , userDodoCode   = T.pack "-----" }
                                IslandClosed ->  return usr { userIslandOpen = IslandClosed
                                                            , userDodoCode   = T.pack "-----" }
                        Nothing -> return usr 

                bioUpdater usr req = return $
                    case (reqUpdateBio req) of
                        Just bio -> usr { userBio = bio }
                        Nothing  -> usr

                favUpdater usr req = return $
                    usr { userFavVillager = (fromMaybe . T.pack $ "Bob") 
                                            (reqUpdateFavVillager req)
                        , userFavThing    = (fromMaybe . T.pack $ "Nothing")    
                                            (reqUpdateFavThing req) }

                update' :: User -> IO (String)
                update' usr = 
                    do
                        updatedUsr <- foldM (\usr f -> f usr req) usr updaters
                        updateUser dbc updatedUsr
                        --putStrLn $ show updatedUsr
                        return "true"

        pricePost :: RequestPrice -> Handler String
        pricePost req = liftIO $ validate' (reqPriceToken req) (return "false") post' 
            where
                post' usr =
                    do
                        now <- Time.getCurrentTime 

                        insertPrice dbc Price { price           = reqPrice req
                                              , priceTime       = now
                                              , priceTimezone   = reqPriceTimezone req
                                              , priceUserId     = userId usr }

                        return "true"
 
        hotItemPost :: RequestHotItem -> Handler String
        hotItemPost req = liftIO $ validate' (reqHotItemToken req) (return "false") post'
            where
                post' usr =
                    do
                        now <- Time.getCurrentTime 

                        insertHotItem dbc HotItem { hotItem           = reqHotItem req
                                                  , hotItemTime       = now
                                                  , hotItemTimezone   = reqHotItemTimezone req
                                                  , hotItemUserId     = userId usr }

                        return "true"
                 

data RequestLogin = RequestLogin { loginUsername :: T.Text 
                                 , loginPassword :: T.Text }
    deriving Generic

data ResponseLogin = ResponseLogin { loginToken :: Maybe T.Text
                                   , loginUserId :: Maybe Int }
    deriving Generic

instance FromJSON RequestLogin 
instance ToJSON ResponseLogin

data RequestRegister = RequestRegister { regUsername :: T.Text 
                                       , regNickname :: T.Text 
                                       , regPassword :: T.Text 
                                       , regSwitchFc :: T.Text
                                       , regCaptcha :: T.Text }
    deriving Generic

data ResponseRegister = ResponseRegister { regToken :: Maybe T.Text }
    deriving Generic

instance FromJSON RequestRegister
instance ToJSON ResponseRegister

data RequestUpdate = RequestUpdate { reqUpdateToken    :: T.Text
                                   , reqUpdateNickname :: Maybe T.Text
                                   , reqUpdatePassword :: Maybe T.Text
                                   , reqUpdateSwitchFc :: Maybe T.Text
                                   , reqUpdateDodoCode :: Maybe T.Text
                                   , reqUpdateIslandOpen :: Maybe IslandOpen
                                   , reqUpdateBio      :: Maybe T.Text
                                   , reqUpdateFavVillager :: Maybe T.Text
                                   , reqUpdateFavThing :: Maybe T.Text }
    deriving Generic

instance FromJSON RequestUpdate

data RequestPrice = RequestPrice { reqPriceToken    :: T.Text
                                 , reqPrice         :: Int
                                 , reqPriceTimezone :: Time.TimeZone }
    deriving Generic

instance FromJSON RequestPrice

data RequestHotItem = ReqHotItem { reqHotItemToken    :: T.Text
                                 , reqHotItem         :: T.Text
                                 , reqHotItemTimezone :: Time.TimeZone }
    deriving Generic

instance FromJSON RequestHotItem


data ResponseUserSearch = ResponseUserSearch{ searchId       :: Int 
                                            , searchUserName :: T.Text
                                            , searchNickname :: T.Text
                                            , searchSwitchFc :: T.Text
                                            , searchDodoCode :: T.Text
                                            , searchIslandOpen :: IslandOpen 
                                            , searchIslandOpenTime :: Time.UTCTime
                                            , searchBio :: T.Text 
                                            , searchFavVillager :: T.Text
                                            , searchFavThing :: T.Text }
    deriving Generic

instance ToJSON ResponseUserSearch
