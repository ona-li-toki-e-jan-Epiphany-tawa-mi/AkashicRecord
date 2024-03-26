package com.tracy.epiphany.packets;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Vector;

/**
 * A packet that stores the data inside a file to be sent.
 */
public class FilePacket implements IPacket {
    private Vector<String> message = new Vector<>();

    /**
     * Stores the data inside file to be sent.
     *
     * @param file The file to get the data from.
     *
     * @throws IOException If the file doesn't exist, isn't a file, or other garbage. Also throws an exception when
     * something goes wrong when reading the file.
     */
    public FilePacket(File file) throws IOException {
        FileReader fileReader = new FileReader(file);

        message.add("HTTP/1.0 200 OK");
        message.add("Content-Length: ");
        message.add("");

        int character = fileReader.read();
        long characterCount = 0;
        StringBuilder lineBuffer = new StringBuilder();

        while (character != -1) {
            if (character == 10) {
                message.add(lineBuffer.toString());
                lineBuffer.delete(0, lineBuffer.length() - 1);

            } else
                lineBuffer.append((char) character);

            characterCount++;
            character = fileReader.read();
        }

        message.set(1, message.elementAt(1) + characterCount);
    }

    @Override
    public String[] getMessage() {
        return message.toArray(new String[0]);
    }
}
