package com.guitarthang.epiphany;
import com.guitarthang.epiphany.lamestuff.ScheduledRunnable;
import com.guitarthang.epiphany.lamestuff.SystemStuff;
import com.guitarthang.epiphany.lamestuff.nativecode.windows.WinUSB;

public final class GuitarThingMain {
    public static void main(String[] args) {
        SystemStuff.doSystemPrep();

        WinUSB usbInterface = new WinUSB();
        usbInterface.print("Wow!, some Java code just called some C/C++ code!");
        usbInterface.print("This is so EPIC!!!11!");

        // Runs general program at whatever speed is set in SystemStuff.
        new ScheduledRunnable() {
            @Override
            public void run() {
                timeHandler();
            }
        }.runTaskTimerSynchronously(0, (long)(1.0 / SystemStuff.getTPS() * 1000.0));
    }

    private static long lastTime = System.nanoTime();
    private static void timeHandler() {
        long currTime = System.nanoTime();
        // Gets the time passed since last execution in seconds.
        double deltaTime = (currTime - lastTime) / 1_000_000_000.0;

        run(deltaTime);

        lastTime = System.nanoTime();
    }

    private static void run(double deltaTime) {
        System.out.println(deltaTime);
    }
}
