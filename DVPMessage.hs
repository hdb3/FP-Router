module DVPMessage where
import NetAddress

data DVPVec = DVPVec {dvPAddr :: NetAddress, dvpCost :: Int} deriving Show
type DVPMsg = [DVPVec]
