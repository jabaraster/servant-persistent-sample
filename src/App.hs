{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module App where

import Control.Monad.IO.Class   (liftIO)
import Database.Persist.Sql
import DataStore
import DataStore.Internal       (pgPool)
import Network.Wai.Handler.Warp (run, Port)
import Servant

type ApiDef  = Get '[JSON] [Entity User]
            :<|> "users" :> Get '[JSON] [Entity User]

server :: ConnectionPool -> Server ApiDef
server pool = (liftIO $ getUsers pool)
         :<|> (liftIO $ getUsers pool)

api :: Proxy ApiDef
api = Proxy

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: IO Application
mkApp = do
  migrateDb
  pool <- pgPool
  return $ app pool

startServer :: Port -> IO ()
startServer port = do
    putStrLn "{- ----------------------------"
    putStrLn " - start server!"
    putStrLn " ----------------------------- -}"
    run port =<< mkApp
