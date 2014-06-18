module Config (module Config, module ParseConfig) where
import ParseConfig
import qualified Data.Map as M

fileConfig :: FilePath -> IO ([ConfigItem], M.Map Int (AdminState, [Int]))
fileConfig f = do
  configData <- readFile f
  putStr "\n  ----------- config data -----------\n"
  putStr configData

  let configItems = parseConfig configData
  putStr "\n  ----------- parsed data -----------\n"
  print configItems

  let links = filter isLink configItems
  let linkLinks = map (\linkItem -> (lIndex linkItem,lAdminState linkItem)) links
  let routers = filter isRouter configItems
  let getRouterLinks item = map (\lnk -> (lnk,rIndex item)) (rLinks item)
  let routerLinks = concat $ map getRouterLinks routers
  let linkMap1 = foldl addrouterLink M.empty routerLinks where
      addrouterLink m (li,ri) = M.alter f li m where
          f Nothing = Just (AdminUp,[ri])
          f (Just (_,rix)) = Just (AdminUp,ri:rix)
  let linkMap = foldl updateLink linkMap1 linkLinks where
      updateLink m (li,st) = M.alter f li m where
          f Nothing = Just (st,[])
          f (Just (_,rix)) = Just (st,rix)
  putStr "\n  ----------- link table -----------\n"
  print linkMap
  return (routers,linkMap)
