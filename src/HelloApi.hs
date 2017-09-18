{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module HelloApi (
    main
    , hello
    , user

    , HelloAPI
    ) where

import           Control.Monad.Logger         (NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson
import qualified Data.ByteString.Lazy         as B
import           Data.Maybe
import qualified Data.Text                    as T
import           Data.Text                    (Text)
import           Database.Persist
import           Database.Persist.Postgresql  (PostgresConf(..)
                                              , withPostgresqlConn
                                              )
import           Database.Persist.Sql         (SqlPersistT
                                               , ConnectionPool
                                               , runSqlConn
                                               , runMigration
                                               )
import           Database.Persist.TH          (mkMigrate, mkPersist,
                                               persistLowerCase, share,
                                               sqlSettings)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    age Int
    deriving Eq Show Generic
|]

pgConf :: IO PostgresConf
pgConf = B.readFile "conf/database-setting.json" >>= pure . fromJust . decode

runDB :: PostgresConf -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB conf = runNoLoggingT . runResourceT . withPostgresqlConn (pgConnStr conf) . runSqlConn

doMigration :: IO ()
doMigration = do
    conf <- pgConf
    runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration migrateAll

selectUsers :: IO User
selectUsers = do
    conf <- pgConf
    userList <- runDB conf $ selectList [] []
    let ul = map (\(Entity _ u) -> u) userList
    return (ul!!0)

type HelloAPI  = Get '[PlainText] Text
            :<|> "user" :> Capture "name" Text :> Capture "age" Int :> Get '[JSON] User

helloApi :: Proxy HelloAPI
helloApi = Proxy

server :: Server HelloAPI
server = hello :<|> user
    where
        -- user n a = return $ User n a
        -- user n a = lift selectUsers

user n a = return $ User n a
hello = return "Hello world"

app :: Application
app = serve helloApi server

main :: IO ()
main = do
    run 1234 app
