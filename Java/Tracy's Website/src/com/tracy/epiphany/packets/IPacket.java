package com.tracy.epiphany.packets;

/**
 * An interface for the normalization of packet methods.
 */
public interface IPacket {
    /**
     * Gets the message the packet is going to send.
     *
     * @return The message of the packet.
     */
    String[] getMessage();
}
