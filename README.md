# Binary Tree World

## Game Description
Binary Tree World is an interactive text-based adventure game where you explore a world represented by binary trees. Each node in the tree can contain various elements such as birds, stores, portals, and items. Your goal is to navigate through the trees, capture birds, buy and sell items, and use portals to jump between different levels.

## Game Features
- **Birds**: Encounter and capture various birds with different rarities and difficulties.
- **Stores**: Buy and sell items to aid your journey.
- **Portals**: Use portals to jump between different levels of the tree.
- **Items**: Collect and use various items to help you on your adventure.
- **Multiple Trees**: Explore multiple trees generated with different levels and structures.

## Game Mechanics

### Capture Birds
- **Encounter Birds**: As you explore the tree, you will encounter various birds. Each bird has a rarity and a difficulty level associated with it.
- **Capture Difficulty**: The difficulty of capturing a bird depends on its rarity. Common birds are easier to capture, while rare and mythological birds are more challenging.
- **Capture Command**: Use the `capture` command to attempt to capture a bird. The success of the capture depends on the bird's difficulty and your current stamina.

### Stores
- **Buy Items**: Visit stores to buy items that can help you on your journey. Items can include capture tools, stamina potions, and more.
- **Sell Birds**: Sell captured birds at stores to earn gold. The amount of gold you receive depends on the rarity of the bird.
- **Store Locations**: Stores are randomly placed within the tree, and you can find them as you explore.

### Portals
- **Use Portals**: Portals allow you to jump between different levels of the tree. This can help you quickly navigate the tree and reach new areas.
- **Portal Locations**: Portals are randomly placed within the tree, and you can find them as you explore.

### Items
- **Collect Items**: Find and collect various items as you explore the tree. Items can help you capture birds, recover stamina, and more.
- **Use Items**: Use items from your pouch to aid your journey. The `use` command allows you to use an item.

### Stamina
- **Stamina Management**: Your stamina decreases as you perform actions such as moving, capturing birds, and using items. Manage your stamina carefully to avoid running out.
- **Recover Stamina**: Use the `sleep` command to recover stamina. You can also find and use stamina potions to recover stamina quickly.

## Game Commands and Actions

### Idle Commands
- **Climb/Go/Move (g)**: Move within the tree.
  - `climb down` or `go down` or `move down` or `g down`: Move down the tree.
  - `climb left` or `go left` or `move left` or `g left`: Move left in the tree.
  - `climb right` or `go right` or `move right` or `g right`: Move right in the tree.
- **Action (a)**: Prepare for actions.
- **Sleep (s)**: Sleep to recover stamina.
- **Display (d)**: Display the current subtree.
- **Display Cheat (dc)**: Reveal the entire subtree.
- **Show State**: Display the contents of your pouches.
  - `display capture` or `d capture`: Show captured birds.
  - `display gold` or `d gold`: Show gold pouch.
  - `display item` or `d item`: Show item pouch.
- **Quit (q)**: Quit the game.

### Action Commands
- **Capture (c)**: Capture a bird.
- **Buy (b)**: Buy items from a store.
- **Sell (s)**: Sell captured birds.
- **Use (u)**: Use an item from your pouch.
- **Display (d)**: Display information.
  - `display bird` or `d bird`: Display bird information.
  - `display store` or `d store`: Display store information.
  - `display tree` or `d tree`: Display tree information.
- **Jump (j)**: Use a portal to jump to a different level.
- **Quit Action (q)**: End the current action.

## Dependencies
- **pretty-tree**: Used for rendering the binary trees in a visually appealing format. This is important as our tree is relatively large and complex when fully explored and printed.

Enjoy exploring the Binary Tree World and uncovering its secrets!
