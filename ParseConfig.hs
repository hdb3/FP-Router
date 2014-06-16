module ParseConfig where
import Data.Char(isSpace)
import Data.List(isPrefixOf)

data RType = DVP | Static deriving (Show,Eq)
data AdminState = AdminDown | AdminUp deriving (Show,Eq)
data ConfigItem =
                  LinkConfig {lIndex :: Int, lAdminState :: AdminState } |
                  RouterConfig {rIndex :: Int, rType :: RType, rLinks :: [Int], rConfig :: String, rAdminState :: AdminState } |
                  Unknown String |
                  FrameworkConfig deriving Show

{- first split the config file into sections (based on non-whitespace in column 1) -}
parseConfig :: String -> [ConfigItem]
parseConfig s = map parseSection sections where
    numberedLines = zip [1..] (lines s)
    sections = preSplit (\(_,s) -> not (null s || isSpace (head s))) numberedLines

parseSection lx | t1 ==  "router" = RouterConfig 0 DVP [] "" AdminUp
                | t1 ==  "link"   = LinkConfig 0 AdminUp
                | otherwise = Unknown t1 where
                  t1 = head . tokenise . head . (map snd) $ lx
-- parseSection (l:lx) | isPrefixOf "router" l = RouterConfig i t links config st where
    

tokenise :: String -> [String]
-- break the string on whitespace
-- this version does not know about escaped or quoted whitespace
tokenise = tokeniseOn isSpace
tokeniseOn _ [] = []
tokeniseOn p s = if null a then [] else a : tokeniseOn p b where
             (a,b) = break p (dropWhile p s)

{-
-- parseConfig :: String -> [String]
parseConfig s = sections where
    numberedLines = zip [1..] (lines s)
    sections = preSplit (\(_,s) -> not (null s || isSpace (head s) || '#' == (head s))) numberedLines
-}

preSplit :: (a -> Bool) -> [a] -> [[a]]
preSplit _ [] = []
preSplit p (a:b) | not (p a) = preSplit p b
                 | otherwise = (a:c) : preSplit p d where
                   (c,d) = break p b
