module Console where
import System.IO
import Data.Maybe
import Data.Char
import Control.Monad
import Text.Read(readMaybe)


{- extensible console interface
   allow commands to be sent to any router instance
   commands are tokenised and sent on the control channel
   commands are routed by router number:
   <router no> < command list> sends to the specified router
   <router no> sets the default context for subsequent commands
   Framework commands are identified by ! prefix
   <!> sets the context to framework commands
-}

console local remote = do
     state <- return Nothing
     commandLoop Nothing where
         commandLoop currTarget = do
             putStr $ (maybe "!" show currTarget)  ++ " > "
             hFlush stdout
             command <- getLine
             let commandTokens = tokenise command
             let newTarget = readMaybe (head commandTokens) :: Maybe Int
             let override = not (null commandTokens) && (("!" == head commandTokens) || isJust newTarget )
             let (target,command) = if override then (newTarget, tail commandTokens) else (currTarget,commandTokens)
             when (null command)
                  (commandLoop target)
             response <- maybe (local command) (remote command) target
             putStrLn response
             commandLoop currTarget

-- lazily copied from ParseConfig !!!
tokenise :: String -> [String]
-- break the string on whitespace
-- this version does not know about escaped or quoted whitespace
tokenise = tokeniseOn isSpace
tokeniseOn _ [] = []
tokeniseOn p s = if null a then [] else a : tokeniseOn p b where
             (a,b) = break p (dropWhile p s)
