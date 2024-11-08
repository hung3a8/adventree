module Adventree.Birds where

import System.Random
import Adventree.Types

birds :: [BirdType]
birds = [
  -- Tier 1: Very Common Birds
  (Pigeon, "A common bird found in cities worldwide.", 0.6, VeryCommon, 0),
  (Sparrow, "A small, brown bird commonly found near human habitation.", 0.5, VeryCommon, 5),
  (Crow, "A large black bird known for its intelligence and adaptability.", 0.5, VeryCommon, 10),
  (Seagull, "A common coastal bird often seen scavenging near water.", 0.45, VeryCommon, 10),

  -- Tier 2: Common Birds
  (Robin, "A bird with a red breast often seen in gardens.", 0.35, Common, 20),
  (BlueJay, "A vibrant blue bird often seen in North America.", 0.3, Common, 20),
  (Starling, "A small bird known for its iridescent feathers and noisy calls.", 0.3, Common, 20),
  (Finch, "A small bird with a variety of colorful plumage patterns.", 0.3, Common, 20),
  (Duck, "A waterfowl commonly found near ponds and rivers.", 0.3, Common, 25),
  (Goose, "A larger waterfowl known for its loud honking calls.", 0.25, Common, 25),

  -- Tier 3: Uncommon Birds
  (Cardinal, "A red bird with a crest that is common in North America.", 0.2, Uncommon, 25),
  (Swallow, "A bird with a distinctive forked tail often seen around water.", 0.2, Uncommon, 25),
  (Woodpecker, "A bird known for pecking at wood to find insects.", 0.15, Uncommon, 30),
  (Magpie, "A black and white bird often seen in urban areas.", 0.15, Uncommon, 30),
  (Mockingbird, "A bird known for mimicking the calls of other birds.", 0.15, Uncommon, 30),
  (Hummingbird, "A tiny bird known for its rapid wing beats.", 0.15, Uncommon, 30),
  (Swan, "A large, graceful waterbird with white feathers.", 0.1, Uncommon, 35),
  (Heron, "A long-legged bird often found wading in shallow water.", 0.1, Uncommon, 35),

  -- Tier 4: Rare Birds
  (Eagle, "A large bird of prey with keen eyesight.", 0.08, Rare, 40),
  (Owl, "A nocturnal bird known for its silent flight.", 0.08, Rare, 40),
  (Kingfisher, "A brightly colored bird often found near rivers.", 0.07, Rare, 40),
  (Pelican, "A large waterbird known for its distinctive throat pouch.", 0.06, Rare, 40),
  (Crane, "A tall bird known for its graceful movements.", 0.05, Rare, 45),
  (Toucan, "A tropical bird with a large, colorful beak.", 0.04, Rare, 45),
  (Cockatoo, "A colorful parrot known for its crest and social behavior.", 0.04, Rare, 45),

  -- Tier 5: Very Rare Birds
  (Albatross, "A large seabird known for its long wingspan.", 0.02, VeryRare, 50),
  (Flamingo, "A tall wading bird known for its pink feathers.", 0.02, VeryRare, 50),
  (Peacock, "A bird with a colorful tail often found in zoos.", 0.02, VeryRare, 50),
  (Penguin, "A flightless bird known for its tuxedo-like appearance.", 0.01, VeryRare, 50),
  (Kiwi, "A flightless bird native to New Zealand.", 0.005, VeryRare, 60),
  (HarpyEagle, "A powerful bird of prey found in South America.", 0.005, VeryRare, 65),
  (Kakapo, "A nocturnal, flightless parrot found in New Zealand.", 0.002, VeryRare, 70),

  -- Tier 6: Mythological Birds
  (Phoenix, "A mythical bird that is reborn from its ashes, symbolizing renewal.", 0.001, Mythological, 85),
  (Griffin, "A legendary creature with the body of a lion and the head and wings of an eagle.", 0.001, Mythological, 90),
  (Roc, "A giant mythical bird of prey known for carrying away large animals.", 0.001, Mythological, 95)
  ]

birdRarityByLevel :: TreeLevel -> [BirdRarity]
birdRarityByLevel level
  | level < 3 = [VeryCommon, Common]
  | level < 6 = [Common, Uncommon]
  | level < 8 = [Uncommon, Rare]
  | level < 9 = [Rare, VeryRare]
  | otherwise = [VeryRare, Mythological]

getRandomBird :: (RandomGen g) => [BirdRarity] -> g -> (BirdType, g)
getRandomBird rarities gen =
  let filteredBirds = filter (\(_, _, _, rarity, _) -> rarity `elem` rarities) birds
      totalWeight = sum (map (\(_, _, weight, _, _) -> weight) filteredBirds)
      (randVal, gen') = randomR (0, totalWeight) gen
      selectBird _ [] = error "Empty bird list"
      selectBird acc ((name, desc, weight, rarity, diff):xs)
        | acc + weight >= randVal = ((name, desc, weight, rarity, diff), gen')
        | otherwise = selectBird (acc + weight) xs
  in selectBird 0 filteredBirds

-- Bird name -> price -> (min deviation, max deviation)
birdPriceBoard :: [(BirdName, (Price, (Float, Float)))]
birdPriceBoard = [
  (Pigeon, (10, (-0.1, 0.1))),
  (Sparrow, (15, (-0.15, 0.15))),
  (Crow, (15, (-0.15, 0.15))),
  (Seagull, (20, (-0.15, 0.15))),

  (Robin, (25, (-0.2, 0.2))),
  (BlueJay, (30, (-0.1, 0.2))),
  (Starling, (40, (-0.2, 0.1))),
  (Finch, (35, (-0.1, 0.2))),
  (Duck, (40, (-0.2, 0.1))),

  (Cardinal, (50, (-0.2, 0.2))),
  (Swallow, (55, (-0.1, 0.2))),
  (Woodpecker, (60, (-0.2, 0.1))),
  (Magpie, (65, (-0.1, 0.2))),
  (Mockingbird, (70, (-0.2, 0.1))),
  (Hummingbird, (75, (-0.1, 0.2))),
  (Swan, (80, (-0.2, 0.1))),

  (Eagle, (100, (-0.2, 0.2))),
  (Owl, (110, (-0.1, 0.2))),
  (Kingfisher, (120, (-0.2, 0.1))),
  (Pelican, (130, (-0.1, 0.2))),
  (Crane, (140, (-0.2, 0.1))),
  (Toucan, (150, (-0.1, 0.2))),
  (Cockatoo, (160, (-0.2, 0.1))),

  (Albatross, (200, (-0.2, 0.2))),
  (Flamingo, (220, (-0.1, 0.2))),
  (Peacock, (240, (-0.2, 0.1))),
  (Penguin, (260, (-0.1, 0.2))),
  (Kiwi, (280, (-0.2, 0.1))),
  (HarpyEagle, (300, (-0.1, 0.2))),

  (Phoenix, (500, (-0.2, 0.2))),
  (Griffin, (600, (-0.1, 0.2))),
  (Roc, (700, (-0.2, 0.1)))
  ]

-- random the deviation of the bird price, then calculate the final price
getBirdPriceQuote :: (RandomGen g) => BirdType -> g -> (Price, g)
getBirdPriceQuote (birdName, _, _, _, _) g = case lookup birdName birdPriceBoard of
  Just (basePrice, (minDev, maxDev)) ->
    let (deviation, g') = randomR (minDev, maxDev) g
        price = round $ fromIntegral basePrice * (1 + deviation)
    in (price, g')
  Nothing -> error "Bird not found in bird price board"

-- Return the bird with diffulty - 10
feedBird :: BirdType -> BirdType
feedBird (name, desc, chance, rarity, difficulty) = (name, desc, chance, rarity, max 0 (difficulty - 10))
