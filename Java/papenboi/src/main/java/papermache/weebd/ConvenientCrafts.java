package papermache.weebd;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.ShapedRecipe;
import org.bukkit.inventory.ShapelessRecipe;

/**
 *  Adds recipes, mainly for convince, such as slabs => blocks.
 */
final class ConvenientCrafts {
    static void startup() {
        ShapedRecipe[] shapedRecipes = new ShapedRecipe[80];
        ShapelessRecipe[] shapelessRecipes = new ShapelessRecipe[15];

        // Shaped indexes up for grabs:
        // Shapeless indexes up for grabs:

        // Cobblestone Slab => Cobblestone
        ShapedRecipe cobbleSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "cobbleSlab2Block_key"), new ItemStack(Material.COBBLESTONE));
        cobbleSlab2Block.shape("S",
                               "S");
        cobbleSlab2Block.setIngredient('S', Material.COBBLESTONE_SLAB);
        shapedRecipes[0] = cobbleSlab2Block;

        // Oak Slab => Oak Planks
        ShapedRecipe oakSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "oakSlab2Block_key"), new ItemStack(Material.OAK_PLANKS));
        oakSlab2Block.shape("S",
                            "S");
        oakSlab2Block.setIngredient('S', Material.OAK_SLAB);
        shapedRecipes[1] = oakSlab2Block;

        // Birch Slab => Birch Planks
        ShapedRecipe birchSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "birchSlab2Block_key"), new ItemStack(Material.BIRCH_PLANKS));
        birchSlab2Block.shape("S",
                              "S");
        birchSlab2Block.setIngredient('S', Material.BIRCH_SLAB);
        shapedRecipes[2] = birchSlab2Block;

        // Spruce Slab => Spruce Planks
        ShapedRecipe spruceSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "spruceSlab2Block_key"), new ItemStack(Material.SPRUCE_PLANKS));
        spruceSlab2Block.shape("S",
                               "S");
        spruceSlab2Block.setIngredient('S', Material.SPRUCE_SLAB);
        shapedRecipes[3] = spruceSlab2Block;

        // Jungle Slab => Jungle Planks
        ShapedRecipe jungleSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "jungleSlab2Block_key"), new ItemStack(Material.JUNGLE_PLANKS));
        jungleSlab2Block.shape("S",
                               "S");
        jungleSlab2Block.setIngredient('S', Material.JUNGLE_SLAB);
        shapedRecipes[4] = jungleSlab2Block;

        // Acacia Slab => Acacia Planks
        ShapedRecipe acaciaSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "acaciaSlab2Block_key"), new ItemStack(Material.ACACIA_PLANKS));
        acaciaSlab2Block.shape("S",
                               "S");
        acaciaSlab2Block.setIngredient('S', Material.ACACIA_SLAB);
        shapedRecipes[5] = acaciaSlab2Block;

        // Dark Oak Slab => Dark Oak Planks
        ShapedRecipe darkOakSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "darkOakSlab2Block_key"), new ItemStack(Material.DARK_OAK_PLANKS));
        darkOakSlab2Block.shape("S",
                                "S");
        darkOakSlab2Block.setIngredient('S', Material.DARK_OAK_SLAB);
        shapedRecipes[6] = darkOakSlab2Block;

        // Stone Slab => Stone
        ShapedRecipe stoneSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "stoneSlab2Block_key"), new ItemStack(Material.STONE));
        stoneSlab2Block.shape("S",
                              "S");
        stoneSlab2Block.setIngredient('S', Material.STONE_SLAB);
        shapedRecipes[19] = stoneSlab2Block;

        // Brick Slab => Bricks
        ShapedRecipe brickSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "brickSlab2Block_key"), new ItemStack(Material.BRICKS));
        brickSlab2Block.shape("S",
                              "S");
        brickSlab2Block.setIngredient('S', Material.BRICK_SLAB);
        shapedRecipes[20] = brickSlab2Block;

        // Nether Brick Slab => Bricks
        ShapedRecipe netherBrickSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "netherBrickSlab2Block_key"), new ItemStack(Material.NETHER_BRICKS));
        netherBrickSlab2Block.shape("S",
                                    "S");
        netherBrickSlab2Block.setIngredient('S', Material.NETHER_BRICK_SLAB);
        shapedRecipes[21] = netherBrickSlab2Block;

        // Prismarine Slab => Prismarine
        ShapedRecipe prismarineSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "prismarineSlab2Block_key"), new ItemStack(Material.PRISMARINE));
        prismarineSlab2Block.shape("S",
                                   "S");
        prismarineSlab2Block.setIngredient('S', Material.PRISMARINE_SLAB);
        shapedRecipes[22] = prismarineSlab2Block;

        // Prismarine Brick Slab => Prismarine Bricks
        ShapedRecipe prismarineBrickSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "prismarineBrickSlab2Block_key"), new ItemStack(Material.PRISMARINE_BRICKS));
        prismarineBrickSlab2Block.shape("S",
                                        "S");
        prismarineBrickSlab2Block.setIngredient('S', Material.PRISMARINE_BRICK_SLAB);
        shapedRecipes[23] = prismarineBrickSlab2Block;

        // Dark Prismarine Slab => Dark Prismarine
        ShapedRecipe darkPrismarineSlab2Block = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "darkPrismarineSlab2Block_key"), new ItemStack(Material.DARK_PRISMARINE));
        darkPrismarineSlab2Block.shape("S",
                                       "S");
        darkPrismarineSlab2Block.setIngredient('S', Material.DARK_PRISMARINE_SLAB);
        shapedRecipes[24] = darkPrismarineSlab2Block;



        // Oak Log => Sticks
        ShapedRecipe oakLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "oakLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        oakLog2Sticks.shape("L",
                            "L");
        oakLog2Sticks.setIngredient('L', Material.OAK_LOG);
        shapedRecipes[7] = oakLog2Sticks;

        // Birch Log => Sticks
        ShapedRecipe birchLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "birchLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        birchLog2Sticks.shape("L",
                              "L");
        birchLog2Sticks.setIngredient('L', Material.BIRCH_LOG);
        shapedRecipes[8] = birchLog2Sticks;

        // Spruce Log => Sticks
        ShapedRecipe spruceLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "spruceLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        spruceLog2Sticks.shape("L",
                               "L");
        spruceLog2Sticks.setIngredient('L', Material.SPRUCE_LOG);
        shapedRecipes[9] = spruceLog2Sticks;

        // Jungle Log => Sticks
        ShapedRecipe jungleLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "jungleLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        jungleLog2Sticks.shape("L",
                               "L");
        jungleLog2Sticks.setIngredient('L', Material.JUNGLE_LOG);
        shapedRecipes[10] = jungleLog2Sticks;

        // Acacia Log => Sticks
        ShapedRecipe acaciaLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "acaciaLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        acaciaLog2Sticks.shape("L",
                               "L");
        acaciaLog2Sticks.setIngredient('L', Material.ACACIA_LOG);
        shapedRecipes[11] = acaciaLog2Sticks;

        // Dark Oak Log => Sticks
        ShapedRecipe darkOakLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "darkOakLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        darkOakLog2Sticks.shape("L",
                                "L");
        darkOakLog2Sticks.setIngredient('L', Material.DARK_OAK_LOG);
        shapedRecipes[12] = darkOakLog2Sticks;

        // Stripped Oak Log => Sticks
        ShapedRecipe strippedOakLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedOakLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        strippedOakLog2Sticks.shape("L",
                                    "L");
        strippedOakLog2Sticks.setIngredient('L', Material.STRIPPED_OAK_LOG);
        shapedRecipes[28] = strippedOakLog2Sticks;

        // Stripped Birch Log => Sticks
        ShapedRecipe strippedBirchLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedBirchLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        strippedBirchLog2Sticks.shape("L",
                                      "L");
        strippedBirchLog2Sticks.setIngredient('L', Material.STRIPPED_BIRCH_LOG);
        shapedRecipes[29] = strippedBirchLog2Sticks;

        // Stripped Spruce Log => Sticks
        ShapedRecipe strippedSpruceLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedSpruceLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        strippedSpruceLog2Sticks.shape("L",
                                       "L");
        strippedSpruceLog2Sticks.setIngredient('L', Material.STRIPPED_SPRUCE_LOG);
        shapedRecipes[30] = strippedSpruceLog2Sticks;

        // Stripped Jungle Log => Sticks
        ShapedRecipe strippedJungleLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedJungleLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        strippedJungleLog2Sticks.shape("L",
                                       "L");
        strippedJungleLog2Sticks.setIngredient('L', Material.STRIPPED_JUNGLE_LOG);
        shapedRecipes[31] = strippedJungleLog2Sticks;

        // Stripped Acacia Log => Sticks
        ShapedRecipe strippedAcaciaLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedAcaciaLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        strippedAcaciaLog2Sticks.shape("L",
                                       "L");
        strippedAcaciaLog2Sticks.setIngredient('L', Material.STRIPPED_ACACIA_LOG);
        shapedRecipes[32] = strippedAcaciaLog2Sticks;

        // Stripped Dark Oak Log => Sticks
        ShapedRecipe strippedDarkOakLog2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedDarkOakLog2Sticks_key"), new ItemStack(Material.STICK, 16));
        strippedDarkOakLog2Sticks.shape("L",
                                        "L");
        strippedDarkOakLog2Sticks.setIngredient('L', Material.STRIPPED_DARK_OAK_LOG);
        shapedRecipes[33] = strippedDarkOakLog2Sticks;

        // Oak Wood => Sticks
        ShapedRecipe oakWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "oakWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        oakWood2Sticks.shape("L",
                             "L");
        oakWood2Sticks.setIngredient('L', Material.OAK_WOOD);
        shapedRecipes[56] = oakWood2Sticks;

        // Birch Wood => Sticks
        ShapedRecipe birchWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "birchWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        birchWood2Sticks.shape("L",
                              "L");
        birchWood2Sticks.setIngredient('L', Material.BIRCH_WOOD);
        shapedRecipes[57] = birchWood2Sticks;

        // Spruce Wood => Sticks
        ShapedRecipe spruceWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "spruceWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        spruceWood2Sticks.shape("L",
                                "L");
        spruceWood2Sticks.setIngredient('L', Material.SPRUCE_WOOD);
        shapedRecipes[58] = spruceWood2Sticks;

        // Jungle Wood => Sticks
        ShapedRecipe jungleWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "jungleWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        jungleWood2Sticks.shape("L",
                                "L");
        jungleWood2Sticks.setIngredient('L', Material.JUNGLE_WOOD);
        shapedRecipes[59] = jungleWood2Sticks;

        // Acacia Wood => Sticks
        ShapedRecipe acaciaWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "acaciaWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        acaciaWood2Sticks.shape("L",
                                "L");
        acaciaWood2Sticks.setIngredient('L', Material.ACACIA_WOOD);
        shapedRecipes[60] = acaciaWood2Sticks;

        // Dark Oak Wood => Sticks
        ShapedRecipe darkOakWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "darkOakWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        darkOakWood2Sticks.shape("L",
                                 "L");
        darkOakWood2Sticks.setIngredient('L', Material.DARK_OAK_WOOD);
        shapedRecipes[61] = darkOakWood2Sticks;

        // Stripped Oak Wood => Sticks
        ShapedRecipe strippedOakWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedOakWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        strippedOakWood2Sticks.shape("L",
                                     "L");
        strippedOakWood2Sticks.setIngredient('L', Material.STRIPPED_OAK_WOOD);
        shapedRecipes[62] = strippedOakWood2Sticks;

        // Stripped Birch Wood => Sticks
        ShapedRecipe strippedBirchWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedBirchWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        strippedBirchWood2Sticks.shape("L",
                                      "L");
        strippedBirchWood2Sticks.setIngredient('L', Material.STRIPPED_BIRCH_WOOD);
        shapedRecipes[63] = strippedBirchWood2Sticks;

        // Stripped Spruce Wood => Sticks
        ShapedRecipe strippedSpruceWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedSpruceWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        strippedSpruceWood2Sticks.shape("L",
                                        "L");
        strippedSpruceWood2Sticks.setIngredient('L', Material.STRIPPED_SPRUCE_WOOD);
        shapedRecipes[64] = strippedSpruceWood2Sticks;

        // Stripped Jungle Wood => Sticks
        ShapedRecipe strippedJungleWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedJungleWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        strippedJungleWood2Sticks.shape("L",
                                        "L");
        strippedJungleWood2Sticks.setIngredient('L', Material.STRIPPED_JUNGLE_WOOD);
        shapedRecipes[65] = strippedJungleWood2Sticks;

        // Stripped Acacia Wood => Sticks
        ShapedRecipe strippedAcaciaWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedAcaciaWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        strippedAcaciaWood2Sticks.shape("L",
                                        "L");
        strippedAcaciaWood2Sticks.setIngredient('L', Material.STRIPPED_ACACIA_WOOD);
        shapedRecipes[66] = strippedAcaciaWood2Sticks;

        // Stripped Dark Oak Wood => Sticks
        ShapedRecipe strippedDarkOakWood2Sticks = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedDarkOakWood2Sticks_key"), new ItemStack(Material.STICK, 20));
        strippedDarkOakWood2Sticks.shape("L",
                                         "L");
        strippedDarkOakWood2Sticks.setIngredient('L', Material.STRIPPED_DARK_OAK_WOOD);
        shapedRecipes[67] = strippedDarkOakWood2Sticks;



        // Oak Log => Chest
        ShapedRecipe oakLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "oakLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        oakLog2Chest.shape("LLL",
                           "L L",
                           "LLL");
        oakLog2Chest.setIngredient('L', Material.OAK_LOG);
        shapedRecipes[13] = oakLog2Chest;

        // Birch Log => Chest
        ShapedRecipe birchLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "birchLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        birchLog2Chest.shape("LLL",
                             "L L",
                             "LLL");
        birchLog2Chest.setIngredient('L', Material.BIRCH_LOG);
        shapedRecipes[14] = birchLog2Chest;

        // Spruce Log => Chest
        ShapedRecipe spruceLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "spruceLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        spruceLog2Chest.shape("LLL",
                              "L L",
                              "LLL");
        spruceLog2Chest.setIngredient('L', Material.SPRUCE_LOG);
        shapedRecipes[15] = spruceLog2Chest;

        // Jungle Log => Chest
        ShapedRecipe jungleLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "jungleLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        jungleLog2Chest.shape("LLL",
                              "L L",
                              "LLL");
        jungleLog2Chest.setIngredient('L', Material.JUNGLE_LOG);
        shapedRecipes[16] = jungleLog2Chest;

        // Acacia Log => Chest
        ShapedRecipe acaciaLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "acaciaLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        acaciaLog2Chest.shape("LLL",
                              "L L",
                              "LLL");
        acaciaLog2Chest.setIngredient('L', Material.ACACIA_LOG);
        shapedRecipes[17] = acaciaLog2Chest;

        // Dark Oak Log => Chest
        ShapedRecipe darkOakLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "darkOakLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        darkOakLog2Chest.shape("LLL",
                               "L L",
                               "LLL");
        darkOakLog2Chest.setIngredient('L', Material.DARK_OAK_LOG);
        shapedRecipes[18] = darkOakLog2Chest;

        // Stripped Oak Log => Chest
        ShapedRecipe strippedOakLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedOakLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        strippedOakLog2Chest.shape("LLL",
                                   "L L",
                                   "LLL");
        strippedOakLog2Chest.setIngredient('L', Material.STRIPPED_OAK_LOG);
        shapedRecipes[34] = strippedOakLog2Chest;

        // Stripped Birch Log => Chest
        ShapedRecipe strippedBirchLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedBirchLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        strippedBirchLog2Chest.shape("LLL",
                                     "L L",
                                     "LLL");
        strippedBirchLog2Chest.setIngredient('L', Material.STRIPPED_BIRCH_LOG);
        shapedRecipes[35] = strippedBirchLog2Chest;

        // Stripped Spruce Log => Chest
        ShapedRecipe strippedSpruceLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedSpruceLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        strippedSpruceLog2Chest.shape("LLL",
                                      "L L",
                                      "LLL");
        strippedSpruceLog2Chest.setIngredient('L', Material.STRIPPED_SPRUCE_LOG);
        shapedRecipes[36] = strippedSpruceLog2Chest;

        // Stripped Jungle Log => Chest
        ShapedRecipe strippedJungleLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedJungleLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        strippedJungleLog2Chest.shape("LLL",
                                      "L L",
                                      "LLL");
        strippedJungleLog2Chest.setIngredient('L', Material.STRIPPED_JUNGLE_LOG);
        shapedRecipes[37] = strippedJungleLog2Chest;

        // Stripped Acacia Log => Chest
        ShapedRecipe strippedAcaciaLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedAcaciaLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        strippedAcaciaLog2Chest.shape("LLL",
                                      "L L",
                                      "LLL");
        strippedAcaciaLog2Chest.setIngredient('L', Material.STRIPPED_ACACIA_LOG);
        shapedRecipes[38] = strippedAcaciaLog2Chest;

        // Stripped Dark Oak Log => Chest
        ShapedRecipe strippedDarkOakLog2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedDarkOakLog2Chest_key"), new ItemStack(Material.CHEST, 4));
        strippedDarkOakLog2Chest.shape("LLL",
                                       "L L",
                                       "LLL");
        strippedDarkOakLog2Chest.setIngredient('L', Material.STRIPPED_DARK_OAK_LOG);
        shapedRecipes[39] = strippedDarkOakLog2Chest;

        // Oak Wood => Chest
        ShapedRecipe oakWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "oakWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        oakWood2Chest.shape("LLL",
                            "L L",
                            "LLL");
        oakWood2Chest.setIngredient('L', Material.OAK_WOOD);
        shapedRecipes[68] = oakWood2Chest;

        // Birch Wood => Chest
        ShapedRecipe birchWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "birchWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        birchWood2Chest.shape("LLL",
                              "L L",
                              "LLL");
        birchWood2Chest.setIngredient('L', Material.BIRCH_WOOD);
        shapedRecipes[69] = birchWood2Chest;

        // Spruce Wood => Chest
        ShapedRecipe spruceWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "spruceWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        spruceWood2Chest.shape("LLL",
                               "L L",
                               "LLL");
        spruceWood2Chest.setIngredient('L', Material.SPRUCE_WOOD);
        shapedRecipes[70] = spruceWood2Chest;

        // Jungle Wood => Chest
        ShapedRecipe jungleWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "jungleWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        jungleWood2Chest.shape("LLL",
                               "L L",
                               "LLL");
        jungleWood2Chest.setIngredient('L', Material.JUNGLE_WOOD);
        shapedRecipes[71] = jungleWood2Chest;

        // Acacia Wood => Chest
        ShapedRecipe acaciaWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "acaciaWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        acaciaWood2Chest.shape("LLL",
                               "L L",
                               "LLL");
        acaciaWood2Chest.setIngredient('L', Material.ACACIA_WOOD);
        shapedRecipes[72] = acaciaWood2Chest;

        // Dark Oak Wood => Chest
        ShapedRecipe darkOakWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "darkOakWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        darkOakWood2Chest.shape("LLL",
                                "L L",
                                "LLL");
        darkOakWood2Chest.setIngredient('L', Material.DARK_OAK_WOOD);
        shapedRecipes[73] = darkOakWood2Chest;

        // Stripped Oak Wood => Chest
        ShapedRecipe strippedOakWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedOakWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        strippedOakWood2Chest.shape("LLL",
                                    "L L",
                                    "LLL");
        strippedOakWood2Chest.setIngredient('L', Material.STRIPPED_OAK_WOOD);
        shapedRecipes[74] = strippedOakWood2Chest;

        // Stripped Birch Wood => Chest
        ShapedRecipe strippedBirchWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedBirchWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        strippedBirchWood2Chest.shape("LLL",
                                      "L L",
                                      "LLL");
        strippedBirchWood2Chest.setIngredient('L', Material.STRIPPED_BIRCH_WOOD);
        shapedRecipes[75] = strippedBirchWood2Chest;

        // Stripped Spruce Wood => Chest
        ShapedRecipe strippedSpruceWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedSpruceWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        strippedSpruceWood2Chest.shape("LLL",
                                       "L L",
                                       "LLL");
        strippedSpruceWood2Chest.setIngredient('L', Material.STRIPPED_SPRUCE_WOOD);
        shapedRecipes[76] = strippedSpruceWood2Chest;

        // Stripped Jungle Wood => Chest
        ShapedRecipe strippedJungleWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedJungleWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        strippedJungleWood2Chest.shape("LLL",
                                       "L L",
                                       "LLL");
        strippedJungleWood2Chest.setIngredient('L', Material.STRIPPED_JUNGLE_WOOD);
        shapedRecipes[77] = strippedJungleWood2Chest;

        // Stripped Acacia Wood => Chest
        ShapedRecipe strippedAcaciaWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedAcaciaWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        strippedAcaciaWood2Chest.shape("LLL",
                                       "L L",
                                       "LLL");
        strippedAcaciaWood2Chest.setIngredient('L', Material.STRIPPED_ACACIA_WOOD);
        shapedRecipes[78] = strippedAcaciaWood2Chest;

        // Stripped Dark Oak Wood => Chest
        ShapedRecipe strippedDarkOakWood2Chest = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "strippedDarkOakWood2Chest_key"), new ItemStack(Material.CHEST, 5));
        strippedDarkOakWood2Chest.shape("LLL",
                                        "L L",
                                        "LLL");
        strippedDarkOakWood2Chest.setIngredient('L', Material.STRIPPED_DARK_OAK_WOOD);
        shapedRecipes[79] = strippedDarkOakWood2Chest;



        // Oak Wood => Oak Planks
        ShapelessRecipe oakWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "oakWood2Planks_key"), new ItemStack(Material.OAK_PLANKS, 5));
        oakWood2Planks.addIngredient(1, Material.OAK_WOOD);
        shapelessRecipes[3] = oakWood2Planks;

        // Birch Wood => Birch Planks
        ShapelessRecipe birchWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "birchWood2Planks_key"), new ItemStack(Material.BIRCH_PLANKS, 5));
        birchWood2Planks.addIngredient(1, Material.BIRCH_WOOD);
        shapelessRecipes[4] = birchWood2Planks;

        // Spruce Wood => Spruce Planks
        ShapelessRecipe spruceWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "spruceWood2Planks_key"), new ItemStack(Material.SPRUCE_PLANKS, 5));
        spruceWood2Planks.addIngredient(1, Material.SPRUCE_WOOD);
        shapelessRecipes[5] = spruceWood2Planks;

        // Jungle Wood => Jungle Planks
        ShapelessRecipe jungleWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "jungleWood2Planks_key"), new ItemStack(Material.JUNGLE_PLANKS, 5));
        jungleWood2Planks.addIngredient(1, Material.JUNGLE_WOOD);
        shapelessRecipes[6] = jungleWood2Planks;

        // Acacia Wood => Acacia Planks
        ShapelessRecipe acaciaWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "acaciaWood2Planks_key"), new ItemStack(Material.ACACIA_PLANKS, 5));
        acaciaWood2Planks.addIngredient(1, Material.ACACIA_WOOD);
        shapelessRecipes[7] = acaciaWood2Planks;

        // Dark Oak Wood => Dark Oak Planks
        ShapelessRecipe darkOakWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "darkOakWood2Planks_key"), new ItemStack(Material.DARK_OAK_PLANKS, 5));
        darkOakWood2Planks.addIngredient(1, Material.DARK_OAK_WOOD);
        shapelessRecipes[8] = darkOakWood2Planks;

        // Stripped Oak Wood => Oak Planks
        ShapelessRecipe strippedOakWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "strippedOakWood2Planks_key"), new ItemStack(Material.OAK_PLANKS, 5));
        strippedOakWood2Planks.addIngredient(1, Material.STRIPPED_OAK_WOOD);
        shapelessRecipes[9] = strippedOakWood2Planks;

        // Stripped Birch Wood => Birch Planks
        ShapelessRecipe strippedBirchWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "strippedBirchWood2Planks_key"), new ItemStack(Material.BIRCH_PLANKS, 5));
        strippedBirchWood2Planks.addIngredient(1, Material.STRIPPED_BIRCH_WOOD);
        shapelessRecipes[10] = strippedBirchWood2Planks;

        // Stripped Spruce Wood => Spruce Planks
        ShapelessRecipe strippedSpruceWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "strippedSpruceWood2Planks_key"), new ItemStack(Material.SPRUCE_PLANKS, 5));
        strippedSpruceWood2Planks.addIngredient(1, Material.STRIPPED_SPRUCE_WOOD);
        shapelessRecipes[11] = strippedSpruceWood2Planks;

        // Stripped Jungle Wood => Jungle Planks
        ShapelessRecipe strippedJungleWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "strippedJungleWood2Planks_key"), new ItemStack(Material.JUNGLE_PLANKS, 5));
        strippedJungleWood2Planks.addIngredient(1, Material.STRIPPED_JUNGLE_WOOD);
        shapelessRecipes[12] = strippedJungleWood2Planks;

        // Stripped Acacia Wood => Acacia Planks
        ShapelessRecipe strippedAcaciaWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "strippedAcaciaWood2Planks_key"), new ItemStack(Material.ACACIA_PLANKS, 5));
        strippedAcaciaWood2Planks.addIngredient(1, Material.STRIPPED_ACACIA_WOOD);
        shapelessRecipes[13] = strippedAcaciaWood2Planks;

        // Stripped Dark Oak Wood => Dark Oak Planks
        ShapelessRecipe strippedDarkOakWood2Planks = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "strippedDarkOakWood2Planks_key"), new ItemStack(Material.DARK_OAK_PLANKS, 5));
        strippedDarkOakWood2Planks.addIngredient(1, Material.STRIPPED_DARK_OAK_WOOD);
        shapelessRecipes[14] = strippedDarkOakWood2Planks;



        // Nether Quartz Block => Nether Quartz
        ShapelessRecipe quartzBlock2Crystal = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "quartzBlock2Crystal_key"), new ItemStack(Material.QUARTZ, 4));
        quartzBlock2Crystal.addIngredient(1, Material.QUARTZ_BLOCK);
        shapelessRecipes[0] = quartzBlock2Crystal;

        // Nether Wart Block => Nether Warts
        ShapelessRecipe wartBlock2warts = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "wartBlock2warts_key"), new ItemStack(Material.NETHER_WART, 9));
        wartBlock2warts.addIngredient(1, Material.NETHER_WART_BLOCK);
        shapelessRecipes[1] = wartBlock2warts;

        // Glowstone Block => Glowstone Block
        ShapelessRecipe glowstoneBlock2dust = new ShapelessRecipe(new NamespacedKey(Weebd.currInstance, "glowstoneBlock2dust_key"), new ItemStack(Material.GLOWSTONE_DUST, 4));
        glowstoneBlock2dust.addIngredient(1, Material.GLOWSTONE);
        shapelessRecipes[2] = glowstoneBlock2dust;



        // Polished Granite => Granite
        ShapedRecipe unposlishGranite = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "unposlishGranite_key"), new ItemStack(Material.GRANITE, 4));
        unposlishGranite.shape("SS",
                               "SS");
        unposlishGranite.setIngredient('S', Material.POLISHED_GRANITE);
        shapedRecipes[25] = unposlishGranite;

        // Polished Diorite => Diorite
        ShapedRecipe unposlishDiorite = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "unposlishDiorite_key"), new ItemStack(Material.DIORITE, 4));
        unposlishDiorite.shape("SS",
                               "SS");
        unposlishDiorite.setIngredient('S', Material.POLISHED_DIORITE);
        shapedRecipes[26] = unposlishDiorite;

        // Polished Andesite => Andesite
        ShapedRecipe unposlishAndesite = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "unposlishAndesite_key"), new ItemStack(Material.ANDESITE, 4));
        unposlishAndesite.shape("SS",
                                "SS");
        unposlishAndesite.setIngredient('S', Material.POLISHED_ANDESITE);
        shapedRecipes[27] = unposlishAndesite;



        // Magenta Carpet => Magenta Wool
        ShapedRecipe magentaCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "magentaCarpet2Wool_key"), new ItemStack(Material.MAGENTA_WOOL, 2));
        magentaCarpet2Wool.shape("C",
                                 "C",
                                 "C");
        magentaCarpet2Wool.setIngredient('C', Material.MAGENTA_CARPET);
        shapedRecipes[40] = magentaCarpet2Wool;

        // Orange Carpet => Orange Wool
        ShapedRecipe orangeCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "orangeCarpet2Wool_key"), new ItemStack(Material.ORANGE_WOOL, 2));
        orangeCarpet2Wool.shape("C",
                                "C",
                                "C");
        orangeCarpet2Wool.setIngredient('C', Material.ORANGE_CARPET);
        shapedRecipes[41] = orangeCarpet2Wool;

        // White Carpet => White Wool
        ShapedRecipe whiteCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "whiteCarpet2Wool_key"), new ItemStack(Material.WHITE_WOOL, 2));
        whiteCarpet2Wool.shape("C",
                               "C",
                               "C");
        whiteCarpet2Wool.setIngredient('C', Material.WHITE_CARPET);
        shapedRecipes[42] = whiteCarpet2Wool;

        // Light Blue Carpet => Light Blue Wool
        ShapedRecipe lightBlueCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "lightBlueCarpet2Wool_key"), new ItemStack(Material.LIGHT_BLUE_WOOL, 2));
        lightBlueCarpet2Wool.shape("C",
                                   "C",
                                   "C");
        lightBlueCarpet2Wool.setIngredient('C', Material.LIGHT_BLUE_CARPET);
        shapedRecipes[43] = lightBlueCarpet2Wool;

        // Yellow Carpet => Yellow Wool
        ShapedRecipe yellowCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "yellowCarpet2Wool_key"), new ItemStack(Material.YELLOW_WOOL, 2));
        yellowCarpet2Wool.shape("C",
                                "C",
                                "C");
        yellowCarpet2Wool.setIngredient('C', Material.YELLOW_CARPET);
        shapedRecipes[44] = yellowCarpet2Wool;

        // Lime Carpet => Lime Wool
        ShapedRecipe limeCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "limeCarpet2Wool_key"), new ItemStack(Material.LIME_WOOL, 2));
        limeCarpet2Wool.shape("C",
                              "C",
                              "C");
        limeCarpet2Wool.setIngredient('C', Material.LIME_CARPET);
        shapedRecipes[45] = limeCarpet2Wool;

        // Pink Carpet => Pink Wool
        ShapedRecipe pinkCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "pinkCarpet2Wool_key"), new ItemStack(Material.PINK_WOOL, 2));
        pinkCarpet2Wool.shape("C",
                              "C",
                              "C");
        pinkCarpet2Wool.setIngredient('C', Material.PINK_CARPET);
        shapedRecipes[46] = pinkCarpet2Wool;

        // Gray Carpet => Gray Wool
        ShapedRecipe grayCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "grayCarpet2Wool_key"), new ItemStack(Material.GRAY_WOOL, 2));
        grayCarpet2Wool.shape("C",
                              "C",
                              "C");
        grayCarpet2Wool.setIngredient('C', Material.GRAY_CARPET);
        shapedRecipes[47] = grayCarpet2Wool;

        // Light Gray Carpet => Light Gray Wool
        ShapedRecipe lightGrayCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "lightGrayCarpet2Wool_key"), new ItemStack(Material.LIGHT_GRAY_WOOL, 2));
        lightGrayCarpet2Wool.shape("C",
                                   "C",
                                   "C");
        lightGrayCarpet2Wool.setIngredient('C', Material.LIGHT_GRAY_CARPET);
        shapedRecipes[48] = lightGrayCarpet2Wool;

        // Cyan Carpet => Cyan Wool
        ShapedRecipe cyanCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "cyanCarpet2Wool_key"), new ItemStack(Material.CYAN_WOOL, 2));
        cyanCarpet2Wool.shape("C",
                              "C",
                              "C");
        cyanCarpet2Wool.setIngredient('C', Material.CYAN_CARPET);
        shapedRecipes[49] = cyanCarpet2Wool;

        // Purple Carpet => Purple Wool
        ShapedRecipe purpleCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "purpleCarpet2Wool_key"), new ItemStack(Material.PURPLE_WOOL, 2));
        purpleCarpet2Wool.shape("C",
                                "C",
                                "C");
        purpleCarpet2Wool.setIngredient('C', Material.PURPLE_CARPET);
        shapedRecipes[50] = purpleCarpet2Wool;

        // Blue Carpet => Blue Wool
        ShapedRecipe blueCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "blueCarpet2Wool_key"), new ItemStack(Material.BLUE_WOOL, 2));
        blueCarpet2Wool.shape("C",
                              "C",
                              "C");
        blueCarpet2Wool.setIngredient('C', Material.BLUE_CARPET);
        shapedRecipes[51] = blueCarpet2Wool;

        // Brown Carpet => Brown Wool
        ShapedRecipe brownCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "brownCarpet2Wool_key"), new ItemStack(Material.BROWN_WOOL, 2));
        brownCarpet2Wool.shape("C",
                               "C",
                               "C");
        brownCarpet2Wool.setIngredient('C', Material.BROWN_CARPET);
        shapedRecipes[52] = brownCarpet2Wool;

        // Green Carpet => Green Wool
        ShapedRecipe greenCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "greenCarpet2Wool_key"), new ItemStack(Material.GREEN_WOOL, 2));
        greenCarpet2Wool.shape("C",
                               "C",
                               "C");
        greenCarpet2Wool.setIngredient('C', Material.GREEN_CARPET);
        shapedRecipes[53] = greenCarpet2Wool;

        // Red Carpet => Red Wool
        ShapedRecipe redCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "redCarpet2Wool_key"), new ItemStack(Material.RED_WOOL, 2));
        redCarpet2Wool.shape("C",
                             "C",
                             "C");
        redCarpet2Wool.setIngredient('C', Material.RED_CARPET);
        shapedRecipes[54] = redCarpet2Wool;

        // Black Carpet => Black Wool
        ShapedRecipe blackCarpet2Wool = new ShapedRecipe(new NamespacedKey(Weebd.currInstance, "blackCarpet2Wool_key"), new ItemStack(Material.BLACK_WOOL, 2));
        blackCarpet2Wool.shape("C",
                               "C",
                               "C");
        blackCarpet2Wool.setIngredient('C', Material.BLACK_CARPET);
        shapedRecipes[55] = blackCarpet2Wool;

        for (ShapedRecipe sr : shapedRecipes)
            if (sr != null)
                Bukkit.addRecipe(sr);
        for (ShapelessRecipe sr : shapelessRecipes)
            if (sr != null)
                Bukkit.addRecipe(sr);
    }
}
