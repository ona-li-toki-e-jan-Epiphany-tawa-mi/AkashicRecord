import java.awt.MouseInfo;
import java.awt.Point;
import java.util.Timer;
import java.util.TimerTask;

public class CursorPos {
	static Point mouseCoords;
	public static void main(String[] args) {
		class SayHello extends TimerTask {
		    public void run() {
		    	mouseCoords = MouseInfo.getPointerInfo().getLocation();
		    	System.out.println(mouseCoords);
		    }
		}
		Timer timer = new Timer();
		timer.schedule(new SayHello(), 0, 50);
	}
}




