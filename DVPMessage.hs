module DVPMessage where

data DVPVec = DVPVec {dvPAddr :: Int, dvpCost :: Int} deriving Show
type DVPMsg = [DVPVec]
