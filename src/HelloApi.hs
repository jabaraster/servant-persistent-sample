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
    , server
    , hello
    , user

    , HelloAPI
    ) where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (NoLoggingT
                                              , runNoLoggingT
                                              , LoggingT
                                              , runLoggingT
                                              )
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
Employee json
    name Text
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

selectUsers :: IO [Entity User]
selectUsers = do
    conf <- pgConf
    runDB conf $ selectList [] []

type HelloAPI  = Get '[JSON] [Entity User]
            :<|> "user" :> Capture "name" Text :> Capture "age" Int :> Get '[JSON] User

helloApi :: Proxy HelloAPI
helloApi = Proxy

server :: Server HelloAPI
server = liftIO selectUsers
         :<|> user

-- user :: Text -> Int -> IO User
user n a = return $ User n a

hello :: IO Text
hello = return "Hello world"

app :: Application
app = serve helloApi server

main :: IO ()
main = do
    putStrLn "{- ----------------------------"
    putStrLn " - start server!"
    putStrLn " ----------------------------- -}"
    doMigration
    run 1234 app
