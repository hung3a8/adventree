module Adventree.Stores where

import Adventree.Types

-- Store data
stores :: [StoreType]
stores = [
  (PigeonClub, "A club for pigeons.", [(BirdSeed, 10), (EnergyDrink, 5)]),
  (DuckHouse, "A house for ducks.", [(BirdCage, 20), (EnergyDrink, 5)]),
  (EagleDC, "A DC for eagles.", [(BirdCage, 20), (EnergyDrink, 5)]),
  (FlamingoResort, "A resort for flamingos.", [(BirdSeed, 10), (BirdCage, 20), (EnergyDrink, 5)]),
  (PhoenixHall, "A hall for phoenixes.", [(BirdSeed, 10), (BirdCage, 20), (EnergyDrink, 5)])
  ]

-- Get store by name
getStore :: StoreName -> StoreType
getStore name = head $ filter (\(n, _, _) -> n == name) stores

getStoreByLevelDefault :: TreeLevel -> StoreType
getStoreByLevelDefault level
  | level < 3 = getStore PigeonClub
  | level < 6 = getStore DuckHouse
  | level < 8 = getStore EagleDC
  | level < 9 = getStore FlamingoResort
  | otherwise = getStore PhoenixHall

addKeyToStore :: StoreType -> TreeLevel -> StoreType
addKeyToStore (name, desc, items) level = (name, desc, items ++ [(PortalKey (level + 1), 100 * (level + 1))])

-- return store by level, and add a (PortalKey (level + 1)) item to the store
getStoreByLevel :: TreeLevel -> StoreType
getStoreByLevel level = addKeyToStore (getStoreByLevelDefault level) level
