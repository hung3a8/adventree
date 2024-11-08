module Adventree.Birds where

import System.Random
import Adventree.Types

birds :: [BirdType]
birds = [
  -- Tier 1: Very Common Birds
  (Pigeon, "A common bird found in cities worldwide.", 0.6, VeryCommon),
  (Sparrow, "A small, brown bird commonly found near human habitation.", 0.5, VeryCommon),
  (Crow, "A large black bird known for its intelligence and adaptability.", 0.5, VeryCommon),
  (Seagull, "A common coastal bird often seen scavenging near water.", 0.45, VeryCommon),

  -- Tier 2: Common Birds
  (Robin, "A bird with a red breast often seen in gardens.", 0.35, Common),
  (BlueJay, "A vibrant blue bird often seen in North America.", 0.3, Common),
  (Starling, "A small bird known for its iridescent feathers and noisy calls.", 0.3, Common),
  (Finch, "A small bird with a variety of colorful plumage patterns.", 0.3, Common),
  (Duck, "A waterfowl commonly found near ponds and rivers.", 0.3, Common),
  (Goose, "A larger waterfowl known for its loud honking calls.", 0.25, Common),

  -- Tier 3: Uncommon Birds
  (Cardinal, "A red bird with a crest that is common in North America.", 0.2, Uncommon),
  (Swallow, "A bird with a distinctive forked tail often seen around water.", 0.2, Uncommon),
  (Woodpecker, "A bird known for pecking at wood to find insects.", 0.15, Uncommon),
  (Magpie, "A black and white bird often seen in urban areas.", 0.15, Uncommon),
  (Mockingbird, "A bird known for mimicking the calls of other birds.", 0.15, Uncommon),
  (Hummingbird, "A tiny bird known for its rapid wing beats.", 0.15, Uncommon),
  (Swan, "A large, graceful waterbird with white feathers.", 0.1, Uncommon),
  (Heron, "A long-legged bird often found wading in shallow water.", 0.1, Uncommon),

  -- Tier 4: Rare Birds
  (Eagle, "A large bird of prey with keen eyesight.", 0.08, Rare),
  (Owl, "A nocturnal bird known for its silent flight.", 0.08, Rare),
  (Kingfisher, "A brightly colored bird often found near rivers.", 0.07, Rare),
  (Pelican, "A large waterbird known for its distinctive throat pouch.", 0.06, Rare),
  (Crane, "A tall bird known for its graceful movements.", 0.05, Rare),
  (Toucan, "A tropical bird with a large, colorful beak.", 0.04, Rare),
  (Cockatoo, "A colorful parrot known for its crest and social behavior.", 0.04, Rare),

  -- Tier 5: Very Rare Birds
  (Albatross, "A large seabird known for its long wingspan.", 0.02, VeryRare),
  (Flamingo, "A tall wading bird known for its pink feathers.", 0.02, VeryRare),
  (Peacock, "A bird with a colorful tail often found in zoos.", 0.02, VeryRare),
  (Penguin, "A flightless bird known for its tuxedo-like appearance.", 0.01, VeryRare),
  (Kiwi, "A flightless bird native to New Zealand.", 0.005, VeryRare),
  (HarpyEagle, "A powerful bird of prey found in South America.", 0.005, VeryRare),
  (Kakapo, "A nocturnal, flightless parrot found in New Zealand.", 0.002, VeryRare),

  -- Tier 6: Mythological Birds
  (Phoenix, "A mythical bird that is reborn from its ashes, symbolizing renewal.", 0.001, Mythological),
  (Griffin, "A legendary creature with the body of a lion and the head and wings of an eagle.", 0.001, Mythological),
  (Roc, "A giant mythical bird of prey known for carrying away large animals.", 0.001, Mythological)
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
  let filteredBirds = filter (\(_, _, _, rarity) -> rarity `elem` rarities) birds
      totalWeight = sum (map (\(_, _, weight, _) -> weight) filteredBirds)
      (randVal, gen') = randomR (0, totalWeight) gen
      selectBird _ [] = error "Empty bird list"
      selectBird acc ((name, desc, weight, rarity):xs)
        | acc + weight >= randVal = ((name, desc, weight, rarity), gen')
        | otherwise = selectBird (acc + weight) xs
  in selectBird 0 filteredBirds
