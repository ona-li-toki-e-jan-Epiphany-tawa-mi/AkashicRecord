package com.tracy.epiphany.packets;

import java.io.OutputStream;
import java.io.PrintWriter;

/**
 * Sends packets over an output stream.
 */
public class PacketSender {
    private PrintWriter printWriter;

    /**
     * Creates an instance of the sender without an output stream.
     */
    public PacketSender() {
        printWriter = null;
    }

    /**
     * Creates an instance of the sender with the output stream it is sending packets over.
     *
     * @param outputStream The output stream to send packets over.
     */
    public PacketSender(OutputStream outputStream) {
        printWriter = new PrintWriter(outputStream);
    }

    /**
     * Sets the output stream to send packets over.
     *
     * @param outputStream The output stream to send packets over.
     */
    public void setOutputStream(OutputStream outputStream) {
        printWriter = new PrintWriter(outputStream);
    }

    /**
     * Sends the packet over the output stream;
     *
     * @param packet The packet to be sent.
     */
    public void sendPacket(IPacket packet) {
        if (printWriter == null)
            throw new NullPointerException("PacketSender needs an output stream!");

        String[] packetMessage = packet.getMessage();

        for (String s : packetMessage) {
            printWriter.println(s);
        }

        printWriter.flush();
    }
}
