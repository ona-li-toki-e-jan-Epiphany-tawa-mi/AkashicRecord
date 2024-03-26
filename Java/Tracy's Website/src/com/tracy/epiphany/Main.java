package com.tracy.epiphany;

import com.tracy.epiphany.packets.FilePacket;
import com.tracy.epiphany.packets.PacketSender;

import java.io.File;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class Main {
    private static ServerSocket httpServerSocket;

    public static void main(String[] args) throws IOException {
        httpServerSocket = new ServerSocket(8080);
        PacketSender packetSender = new PacketSender();

        while (true) {
            Socket client = httpServerSocket.accept();
            System.out.println("Inbound connection from " + client.getInetAddress());

            FilePacket filePacket = new FilePacket(new File("src\\com\\tracy\\epiphany\\epic.html"));

            packetSender.setOutputStream(client.getOutputStream());
            packetSender.sendPacket(filePacket);

            client.close();
        }
    }
}
