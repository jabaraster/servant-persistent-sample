module State (
    State(..)
) where

import           Database.Persist.Sql (ConnectionPool)

data State = State
  { connPool :: ConnectionPool
  }
