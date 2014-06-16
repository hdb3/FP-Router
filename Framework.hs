module Framework where

import Message
import ParseConfig

framework configData = do
    configItems = parseConfig configData
    linkMap = 
    routerMap = mapM
