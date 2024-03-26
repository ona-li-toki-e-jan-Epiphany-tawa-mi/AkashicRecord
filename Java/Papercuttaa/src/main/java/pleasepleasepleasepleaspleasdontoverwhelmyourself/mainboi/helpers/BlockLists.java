package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.helpers;

import org.bukkit.Material;

import java.util.Arrays;
import java.util.List;

/**
 * A series of lists used to clean up the identification of blocks.
 */
public final class BlockLists {
    // List of blocks that can be replaced with air without much consideration of context.
    public final static List<Material> airReplaceables = Arrays.asList(
            Material.AIR, Material.CAVE_AIR, Material.VOID_AIR, Material.GRASS, Material.FERN, Material.DEAD_BUSH, Material.SNOW, Material.VINE,
            Material.TALL_GRASS, Material.LARGE_FERN, Material.FIRE
    );

    // List of blocks that can be replaced with air. For use with very hard difficulty.
    public final static List<Material> VH_airReplaceables = Arrays.asList(
            Material.AIR, Material.CAVE_AIR, Material.VOID_AIR,Material.OAK_SAPLING, Material.SPRUCE_SAPLING, Material.BIRCH_SAPLING,
            Material.JUNGLE_SAPLING, Material.ACACIA_SAPLING, Material.GRASS, Material.FERN, Material.DEAD_BUSH, Material.DARK_OAK_SAPLING,
            Material.DANDELION, Material.POPPY, Material.BLUE_ORCHID, Material.ALLIUM, Material.AZURE_BLUET, Material.RED_TULIP,
            Material.ORANGE_TULIP, Material.WHITE_TULIP, Material.PINK_TULIP, Material.OXEYE_DAISY, Material.CORNFLOWER, Material.LILY_OF_THE_VALLEY,
            Material.BROWN_MUSHROOM, Material.RED_MUSHROOM, Material.SNOW, Material.VINE, Material.LILY_PAD, Material.SUNFLOWER, Material.LILAC,
            Material.ROSE_BUSH, Material.PEONY, Material.TALL_GRASS, Material.LARGE_FERN, Material.WHEAT, Material.SUGAR_CANE, Material.BAMBOO_SAPLING,
            Material.MELON_STEM, Material.PUMPKIN_STEM, Material.BEETROOTS, Material.NETHER_WART, Material.CARROTS, Material.POTATOES,
            Material.SWEET_BERRY_BUSH, Material.FIRE
    );
}
