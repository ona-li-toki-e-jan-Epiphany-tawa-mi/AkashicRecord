package com.guitarthang.epiphany.lamestuff.nativecode.windows;

public class WinUSB {
    static {
        System.loadLibrary("winusb");
    }

    public native void printfff(String string);

    public void print(String string) {
        this.printfff(string);
    }
}
