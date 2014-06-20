module NetAddress where

{-
  this is an abstraction of network address schemes to ensure that no unwarranted assumptions are made
  regarding the semantics of network addresses

  the additional consideration of multiple types of address is not considered, nor yet is explicit support for
  host addresses, multicast, variable length subnets, or even subnets at all.....

-}

newtype NetAddress = NetAddress Int deriving (Show,Eq,Ord)
