import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.SourceDataLine;

import com.studiohartman.jamepad.ControllerManager;
import com.studiohartman.jamepad.ControllerState;

// Just as a warning for the sound: I have no idea what I'm talking about

class ControllerInput implements Runnable {
	
	static int volume = 100; // Yeah
	static double multi = 2.0; // Some multiplier
	static int freq = 44100; // Frequency to play sound with
	static int pitch = 440; // Something that divides the frequency 
	static int time = 1000; // How long the sound should be "cleaned"
	static int waveT = 0; // Used to select type of wave
	double b = 0; // Used to generate PWM wave
	static boolean yes = true; // Dictates whether or not the program is running
	
	byte[] buf = new byte[1]; // Stores sound
	static AudioFormat af = new AudioFormat((float)freq, 8, 1, true, false ); // Format for the sound 
	
	public static void main(String args[]) { // Used to manipulate input from controller
		
		ControllerManager lvl35boss = new ControllerManager(); // Creates a new controller manager
		lvl35boss.initSDLGamepad(); // Initiates the program to sense controller input
    	
    	
        ControllerInput obj = new ControllerInput(); // Build the class as a object for multi-threading
        Thread tobj =new Thread(obj); // Makes a thread off of the class object 
        tobj.start(); // Begins secondary thread
             
        while(yes) { // Constantly runs controller part of program
			
        	ControllerState currState = lvl35boss.getState(0); // Gets the current input of controller 1
			  
        	if(!currState.isConnected) { // If the controller is disconnected the program stops
        		break; // Stops program    
       		}
       		
       		if (currState.rightStickY == 0) { // Allows controller to control volume
       			
    			volume = 80; // Sets volume to it's middle
    			
    		} else {
    			
    			volume = (int) Math.ceil(80 + (currState.rightStickY * 40)); // Makes volume greater or less than 80 based on the right stick's y value
    			
    		} 
        		
       		if (currState.leftStickY == 0) { // Allows controller to control pitch
       			
    			pitch = 440; // Sets pitch to it's middle
    			
    		} else { 
    			
    			pitch = (int) Math.ceil(440 + (currState.leftStickY * 60)); // Makes volume greater or less than 80 based on the right stick's y value
    			
    		}
        		        
       		if (currState.startJustPressed && waveT < 2) { // Tests if start button is pressed; holds upper limit for mode select; Used to change waveform
       			
       			waveT++; // Increments state
       			
        	} else if (currState.backJustPressed && waveT > 0) { // Tests if back button is pressed; holds lower limit for mode select; Used to change waveform
        		
        		waveT--; // Decrements state
        		
        	} 
       		
       		if (currState.rbJustPressed) { // Tests if rb is pressed; Used to change fixed pitch 
       			
       			multi++; // Increments state
       			
        	} else if (currState.lbJustPressed) { // Tests if lb is pressed; Used to change fixed pitch 
        		
        		multi--; // Decrements state
        		
        	} 
       		
       		if (currState.dpadLeft) { // Tests if rb is pressed; Used to erode waveform
       			
       			time--; // Increments state
       			try {
       				Thread.sleep(10); // Makes change slower
       			} catch (Exception e) {}
       			
        	} else if (currState.dpadRight) { // Tests if lb is pressed; Used to smooth waveform
        		
        		time++; // Decrements state
        		try {
       				Thread.sleep(10); // Makes change slower
       			} catch (Exception e) {}
        		
        	} 
        		
		}
        			 
		lvl35boss.quitSDLGamepad(); // Stops controller input for full stop
		yes = false; // Used to stop program
		
		}
	public void CntrAd() {
		
	}
	
	@Override
	public void run() { // Used to manipulate generated sound
		try { // Try/catch block for SDL
			
    		SourceDataLine sdl = AudioSystem.getSourceDataLine( af ); // Formats sdl audio
    		
    		sdl.open(); // Opens audio stream
        	sdl.start(); // Starts audio stream
        	
        	while (yes) { // Constantly runs sound assembly and delivery for program
        		
        		if (waveT == 0) {
        			
        			for( int i = 0; i < time * (float)freq / 1000; i++ ) { // Constantly generates and outputs sine waves
        				
        				buf[0] = (byte)(Math.sin(i / ((float) freq/pitch) * multi * Math.PI) * volume); // Calculates sound
        				System.out.println(buf[0]);
        				sdl.write(buf,0,1); // Writes computed sound to buffer
        				
        			}
        			
        		} else if (waveT == 1) {
        			
        			for( int i = 0; i < time * (float)freq / 100; i++ ) { // Constantly generates and outputs triangular waves
        				
        				buf[0] = (byte) ((Math.tan(i / ((float) freq/pitch) * multi * Math.PI) * volume)* 0.1); // Calculates sound
        				System.out.println(buf[0]);
        				sdl.write(buf,0,1); // Writes computed sound to buffer
        				
        			}
        			
        		} else if (waveT == 2) {
        			
        			for( int i = 0; i < time * (float)freq / 1000; i++ ) { // Constantly generates and outputs PWM waves
        				if (b < 11 && b >= 0) {
        					
        					buf[0] = (byte) (((i/((float) freq/440) * multi * Math.PI)) * (volume/2)* 0.7); // Calculates sound easy peasy lemon squeezey
        					
        				} else if (b >= 0) {
        					
        					b = pitch*(-23/24); // Resets incrementer
        					
        				} else {
        					
        					buf[0] = 0; // Easy peasy lemon squeezey
        					
        				}
        				
        				b++; // Incremental guy
        				System.out.println(buf[0]);
        				sdl.write(buf,0,1); // Writes computed sound to buffer
        				
        			}
        		}
        	}
        	
        	sdl.drain(); // Clears audio stream
        	sdl.stop(); // Stops audio stream
        	
        } catch (Exception e) {
       		System.out.println("get dunked on, kiddo"); // Outputs error text
       	}
	}	
}
	

