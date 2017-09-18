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

module Main where

import           Control.Monad.Logger         (NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson
import qualified Data.ByteString.Lazy         as B
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
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
import           Servant.API
import           System.Environment           (getArgs)

import qualified HelloApi as H

main = H.main

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- User json
--     name Text
--     age Int
--     deriving Eq Show Generic
-- |]
-- 
-- type HelloAPI = "users" :> Get '[JSON] [User]
--                 :<|> "users" :> Capture "name" Text :> Capture "age" Int :> Post '[JSON] ()
-- 
-- helloApi :: Proxy HelloAPI
-- helloApi = Proxy
-- 
-- app :: Application
-- app = serve helloApi server
-- 
-- pgConf :: IO PostgresConf
-- pgConf = B.readFile "conf/database-setting.json" >>= pure . fromJust . decode
-- 
-- runDB :: PostgresConf -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
-- runDB conf = runNoLoggingT . runResourceT . withPostgresqlConn (pgConnStr conf) . runSqlConn
-- 
-- doMigration :: IO ()
-- doMigration = do
--     conf <- pgConf
--     runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration migrateAll
-- 
-- selectUsers :: IO [User]
-- selectUsers = do
--     conf <- pgConf
--     userList <- runDB conf $ selectList [] []
--     return $ map (\(Entity _ u) -> u) userList
-- 
-- insertUser :: User -> IO ()
-- insertUser user = do
--     conf <- pgConf
--     runDB conf $ insert_ user
-- 
-- server :: Server HelloAPI
-- server = getUsers :<|> postUser
--     where
--         getUsers = lift selectUsers
--         postUser n a = lift $ insertUser (User n a)
-- 
-- main :: IO ()
-- main = do
--     args <- getArgs
--     let arg1 = if (length args > 0) then Just (args !! 0) else Nothing
--     case arg1 of
--         Just "migrate" -> doMigration
--         _ -> run 8080 app
