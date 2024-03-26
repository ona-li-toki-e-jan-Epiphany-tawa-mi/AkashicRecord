import java.awt.BorderLayout;
import java.awt.GridLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;

public class MineSweeper extends JFrame {
	static JPanel pan = new JPanel(); // Creates a new JPanel
	static Baton pos[] = new Baton[646]; // An array to create/store buttons
	
	public static void main(String args[]) {	
		new MineSweeper(); // Runs MineSweeper out of main so it dosen't need static values
	}
	
	public MineSweeper() {
		super("MineSweeper"); // Super class of the constructor, Has to be at top
		
		// TODO Timer, Glasses Dude, Scorekeeper, Configuration Options
		
		setSize(800,300); // TODO JFrame#pack
		setResizable(false); // TODO true
		setDefaultCloseOperation(EXIT_ON_CLOSE); // Allows players to actually stop playing the game
		pan.setLayout(new GridLayout(15,15)); // Sets up a grid for various buttons 
		
		int b = pos.length - 1; // Stores the highest index of b for future scalability
		for (int r = 0; r < b; r++) {  // Standard 0 - n for loop
			pos[r] = new Baton(); // Makes a new button for every button place
			pan.add(pos[r], BorderLayout.CENTER); // Places button onto JPanel
		}
		add(pan); // Finally adds JPanel to JFrame
		
		setVisible(true); // Allows the player to see true art
	}
}
