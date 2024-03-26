package mannyboidpaperrus.paperthinger.worldlayers.voidd;

import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Biome;
import org.bukkit.generator.ChunkGenerator;

import java.util.Random;

/**
 * Generates the VOID. Bwah hah ha!
 */
@SuppressWarnings("NullableProblems")
public class VoidGenerator extends ChunkGenerator {
    @Override
    public ChunkData generateChunkData(World world, Random random, int x, int z, BiomeGrid biome) {
        ChunkData chunkData = createChunkData(world);

        chunkData.setRegion(0, 0, 0, 16, 256, 16, Material.VOID_AIR);

        for (int xx = 0; xx < 16; xx++)
            for (int zz = 0; zz < 16; zz++)
                biome.setBiome(xx, zz, Biome.THE_VOID);

        return chunkData;
    }

    @Override
    public boolean canSpawn(World world, int x, int z) {
        return false;
    }
}
