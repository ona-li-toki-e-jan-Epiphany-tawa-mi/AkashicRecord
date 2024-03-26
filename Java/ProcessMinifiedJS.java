import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.StandardOpenOption;

/**
 * A build script that removes the original bytecode transformers and puts the minified ones in their place.
 *
 * @author ona li toki e jan Epiphany tawa mi.
 */
public class ProcessMinifiedJS {
    private static final String buildTransformersDirectory = "build/resources/main/transformers";

    public static void main(String[] args) {
        File transformersFolder = new File(buildTransformersDirectory);
        File[] transformers = transformersFolder.listFiles(
                (File pathName) -> pathName.getName().endsWith(".js"));

        if (transformers != null) {
            // Removes original transformers.
            for (File transformer : transformers)
                if (!transformer.getName().endsWith("min.js")) {
                    boolean result = transformer.delete();
                    if (!result) System.err.println("WARNING: Unable to delete file " + transformer.getName());
                }

            // Puts the minified versions in their place.
            for (File transformer : transformers)
                if (transformer.getName().endsWith("min.js")) {
                    String newFilePath = buildTransformersDirectory + "/" + transformer.getName().replaceFirst("\\.min", "");

                    boolean result = transformer.renameTo(new File(newFilePath));
                    if (!result) System.err.println("WARNING: Unable to rename file " + transformer.getName());
                }

        } else
            System.err.println("WARNING: Unable to find any transformer files in " + buildTransformersDirectory);

        // \x3c -> <, \x3e -> >.
        transformers = transformersFolder.listFiles(
                (File pathName) -> pathName.getName().endsWith(".js"));

        if (transformers != null) {
            for (File transformer : transformers) {
                try {
                    Files.write(
                            transformer.toPath(),
                            new String(Files.readAllBytes(transformer.toPath()), StandardCharsets.UTF_8).replace("'use strict';", "")
                                    .replace("\\x3c", "<").replace("\\x3e", ">").getBytes(StandardCharsets.UTF_8),
                            StandardOpenOption.WRITE
                    );
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}