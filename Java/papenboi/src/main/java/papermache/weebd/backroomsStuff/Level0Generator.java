package papermache.weebd.backroomsStuff;

import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.data.BlockData;
import org.bukkit.block.data.type.Switch;
import org.bukkit.generator.ChunkGenerator;
import org.jetbrains.annotations.NotNull;

import java.util.Random;


/**
 *  Creates the first level of the backrooms
 */
public final class Level0Generator extends ChunkGenerator {
    private int[][] lightLocations = {
                              {13, 3}, {13, 4},   {13, 7}, {13, 8}, {13, 9}, {13, 12}, {13, 13},
                              {8, 3}, {8, 4},   {8, 7}, {8, 8}, {8, 9},   {8, 12}, {8, 13},
                              {3, 3}, {3, 4},   {3, 7}, {3, 8}, {3, 9},   {3, 12}, {3, 13}
                             };

    @Override
    public @NotNull ChunkData generateChunkData(@NotNull World world, @NotNull Random random, int x, int z, @NotNull BiomeGrid biome) {
        ChunkData chunkData = createChunkData(world);

        chunkData.setRegion(0, 0, 0, 16, 66, 16, Material.SMOOTH_SANDSTONE);
        chunkData.setRegion(0, 65, 0, 16, 66, 16, Material.STRIPPED_OAK_WOOD);
        chunkData.setRegion(0, 69, 0, 16, 256, 16, Material.SMOOTH_SANDSTONE);

        chunkData.setRegion(0, 66, 0, 1, 69, 7, Material.SMOOTH_SANDSTONE);
        chunkData.setRegion(0, 66, 10, 1, 69, 16, Material.SMOOTH_SANDSTONE);
        chunkData.setRegion(0, 66, 0, 7, 69, 1, Material.SMOOTH_SANDSTONE);
        chunkData.setRegion(10, 66, 0, 16, 69, 1, Material.SMOOTH_SANDSTONE);

        for (int[] coord : lightLocations) {
            chunkData.setBlock(coord[0], 69, coord[1], Material.GLASS);
            chunkData.setBlock(coord[0], 70, coord[1], Material.SEA_LANTERN);
            chunkData.setBlock(coord[0], 71, coord[1], Material.AIR);
            chunkData.setBlock(coord[0], 72, coord[1], Material.WATER);
        }

        return chunkData;
    }

    @Override
    public boolean canSpawn(@NotNull World world, int x, int z) {
        return false;
    }
}
