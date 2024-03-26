package com.guitarthang.epiphany.lamestuff;

import java.util.Timer;

public final class SystemStuff {
    private static OSs operatingSystem;
    private static final int tps = 20;
    private static final Timer timer = new Timer();

    private static boolean prepDone = false;
    public static void doSystemPrep() {
        if (prepDone)
            return;

        // Determines OS.
        String os = System.getProperty("sun.desktop");
        if (os.equals("windows"))
            operatingSystem = OSs.Windows;
        else {
            System.err.println("Incompatible Operating System!");
            System.exit(-1);
        }

        prepDone = true;
    }

    public static OSs getOperatingSystem() {
        return operatingSystem;
    }

    public static int getTPS() {
        return tps;
    }

    public static Timer getTimer() {
        return timer;
    }

    enum OSs {
        Windows
    }
}
