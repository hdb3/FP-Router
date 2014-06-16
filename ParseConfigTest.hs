
{-# LANGUAGE QuasiQuotes #-}
-- in package raw-strings-qq
import Text.RawString.QQ
import ParseConfig

configData = [r|
# Sample Configuration File
router 1
  type dvp
  links 1 2 3
  config
    node address 1
    if 0 address 100
    if 1 address 101
    if 2 address 102
router 2
  type dvp
  links 1 4 5
  config
    node address 1
    if 0 address 100
    if 1 address 101
    if 2 address 102
router 3
  type dvp
  links 2 4 6
  config
    node address 3
    if 0 address 300
    if 1 address 301
    if 2 address 302
router 4
  type dvp
  links 3 5 6
  config
    node address 4
    if 0 address 100
    if 1 address 101
    if 2 address 102
link 1
  status down|]

main = do
  putStr configData
  print $ parseConfig configData
