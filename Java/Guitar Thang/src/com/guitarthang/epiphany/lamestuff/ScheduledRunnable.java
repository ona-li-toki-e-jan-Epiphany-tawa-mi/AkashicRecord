package com.guitarthang.epiphany.lamestuff;

import java.util.TimerTask;

public class ScheduledRunnable extends TimerTask implements Runnable {
    public ScheduledRunnable() {}

    @Override
    public void run() {

    }

    public void runTaskTimerSynchronously(long delay_ms, long period_ms) {
        SystemStuff.getTimer().schedule(this, delay_ms, period_ms);
    }

    public void runTaskLaterSynchronously(long delay_ms) {
        SystemStuff.getTimer().schedule(this, delay_ms);
    }
}
