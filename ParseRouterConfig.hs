module ParseRouterConfig where
import Data.Char(isSpace)
import Data.List(isPrefixOf)
import Data.Maybe(fromJust)
-- import Control.Applicative

parseRouter :: String -> [(String,[(String,String)])]
{- a router configuration should be single line records formatted type val [type val]
   the first pair are the object and its name
   additional pairs are optional attributes
   the first (object) name is stored under the key name
-}
parseRouter = map parseRouterConfigLine.(map tokenise).removeComments.lines where
    -- ignore config file comments (lines starting with #)
    removeComments = filter (\l -> not (null l) && '#' /= (head l))
    parseRouterConfigLine (a:[]) = (a,[("name","")])
    parseRouterConfigLine (a:b:abs) = (a,("name",b):parseBody abs) where
                                parseBody (a:b:x) = (a,b):parseBody x
                                parseBody _ = []

-- lazily copied from ParseConfig !!!
tokenise :: String -> [String]
-- break the string on whitespace
-- this version does not know about escaped or quoted whitespace
tokenise = tokeniseOn isSpace
tokeniseOn _ [] = []
tokeniseOn p s = if null a then [] else a : tokeniseOn p b where
             (a,b) = break p (dropWhile p s)

nodeAttr attr = lookup' attr . lookup' "node" where
   lookup' k m = fromJust $ lookup k m

getObjects obj m = filter (\(k,v) -> k == obj ) m
getNamedObj obj name m = first (\(k,v) -> k == obj && maybe False (name==) (lookup "name" v )) m

first _ [] = Nothing
first p (a:ax) = if p a then Just a else first p ax
