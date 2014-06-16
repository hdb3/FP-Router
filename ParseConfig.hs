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
    sections = preSplit sectionSplitP nonCommentLines

sectionSplitP = (\(_,s) -> not (null s || isSpace (head s)))

-- presplit groups lines into sections where a section starts in column 1
preSplit :: (a -> Bool) -> [a] -> [[a]]
preSplit _ [] = []
preSplit p (a:b) | not (p a) = preSplit p b
                 | otherwise = (a:c) : preSplit p d where
                   (c,d) = break p b

parseSection lx | l == 2 && t1 == "router" = RouterConfig t2Int rtype links config status
                | l == 2 && t1 == "link" = LinkConfig t2Int status
                | otherwise = Unknown t1 where
                  tokens = tokenise . snd . head $ lx
                  l = length tokens
                  t1 = head tokens
                  t2Int = read (tokens !! 1) :: Int
                  deIndent s = map (\(ln,l) -> (ln, drop indent l)) s where
                     indent = minimum . map (length . (takeWhile isSpace) . snd) $ s
                  subSections = map parseSubSection . preSplit sectionSplitP . deIndent . tail $ lx

                  status = maybe AdminUp f (lookup "status" subSections) where
                      f (_,"down":_,_) = AdminDown 
                      f (_,"up":_,_) = AdminUp 

                  links = maybe [] f (lookup "links" subSections) where
                      f (_,lks,_) = map read lks 

                  rtype = maybe DVP f (lookup "type" subSections) where
                      f (_,"dvp":_,_) = DVP 
                      f (_,"static":_,_) = Static

                  config = maybe "" f (lookup "config" subSections) where
                      f (_,_,s) = unlines . map (drop (minimum . map (length . (takeWhile isSpace)) $ s)) $ s

-- this function tokenises the first line of each sub-section and makes the first token a key for lookup
-- it retains the other tokens for sunsequent processing
-- also keeps the body of the sub-section which is needed for router config data
parseSubSection ((ln,l):lx) = (t,(ln,tx,s)) where
                    tokens = tokenise l
                    t = head tokens
                    tx = drop 1 tokens
                    s = map snd lx


tokenise :: String -> [String]
-- break the string on whitespace
-- this version does not know about escaped or quoted whitespace
tokenise = tokeniseOn isSpace
tokeniseOn _ [] = []
tokeniseOn p s = if null a then [] else a : tokeniseOn p b where
             (a,b) = break p (dropWhile p s)

