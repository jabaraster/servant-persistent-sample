module Main where

import           DataStore
import           DataStore.Internal
import           Handler
import           Network.Wai.Handler.Warp (run)
import           Router

main :: IO ()
main = do
    let port = 8080
    migrateDb
    pool <- pgPool
    putStrLn ("start server on " ++ (show port))
    run port $ app $ State pool
