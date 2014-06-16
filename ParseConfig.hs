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

parseConfig :: String -> [ConfigItem]
parseConfig s = map parseSection sections where
    -- number source lines for helpful error messages
    numberedLines = zip [1..] (lines s)
    -- ignore config file comments (lines starting with #)
    nonCommentLines = filter (\(_,s) -> not (null s) && '#' /= (head s)) numberedLines
    -- split the config file into sections (based on non-whitespace in column 1)
    sections = preSplit (\(_,s) -> not (null s || isSpace (head s))) nonCommentLines
    -- presplit groups lines into sections where a section starts in column 1
    preSplit :: (a -> Bool) -> [a] -> [[a]]
    preSplit _ [] = []
    preSplit p (a:b) | not (p a) = preSplit p b
                     | otherwise = (a:c) : preSplit p d where
                       (c,d) = break p b

parseSection lx | l == 2 && t1 == "router" = RouterConfig t2Int DVP [] "" AdminUp
                | l == 2 && t1 == "link" = LinkConfig t2Int AdminUp
                | otherwise = Unknown t1 where
                  tokens = tokenise . head . (map snd) $ lx
                  l = length tokens
                  t1 = head tokens
                  t2Int = read (tokens !! 1) :: Int

tokenise :: String -> [String]
-- break the string on whitespace
-- this version does not know about escaped or quoted whitespace
tokenise = tokeniseOn isSpace
tokeniseOn _ [] = []
tokeniseOn p s = if null a then [] else a : tokeniseOn p b where
             (a,b) = break p (dropWhile p s)

