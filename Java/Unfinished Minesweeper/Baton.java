import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import MineSweeper;

public class Baton extends JButton implements MouseListener {
	// ImageIcons filled with PNGs for graphics
	ImageIcon Default, Bomb, Flag, Empty, One, Two, Three, Four, Five, Six, Seven, Eight, BombD;
	
	private static String[] wowzers = {"hi"}; 
	
	int is_bomb; // Determines whether a tile is a bomb, unique from tile to tile 
	public static int cntr = 0; // A counter for mapping coordinates and neighbors[], global variable
	int cn; // A version of cntr which is unique from tile to tile for logic operations
	int bombN = 0; // A counter for how many bombs surround a select tile, unique from tile to tile 
	public static int neighbors[] = new int[646]; // Basically a number line mapping the state of the tiles, global variable
	int checker[] = new int[8]; // Used to optimize for loop, determines which tile to pull data from, unique from tile to tile 
	boolean clicked; // Used to mark if tiles have been clicked, interferes with flagged
	boolean flagged; // Used to mark if tiles have been flagged, interferes with clicked
	public static boolean gameover; // Determines if the game is in a gameover state
	
	public Baton() {
		// Massive block loads images onto variables
		Default = new ImageIcon(getClass().getResource("/default.png"));
		Bomb = new ImageIcon(this.getClass().getResource("/bomb_select.png"));
		Flag = new ImageIcon(this.getClass().getResource("/flag.png"));
		Empty = new ImageIcon(this.getClass().getResource("/empty.png"));
		One = new ImageIcon(getClass().getResource("/one.png"));
		Two = new ImageIcon(this.getClass().getResource("/two.png"));
		Three = new ImageIcon(this.getClass().getResource("/three.png"));
		Four = new ImageIcon(this.getClass().getResource("/four.png"));
		Five = new ImageIcon(this.getClass().getResource("/five.png"));
		Six = new ImageIcon(this.getClass().getResource("/six.png"));
		Seven = new ImageIcon(this.getClass().getResource("/seven.png"));
		Eight = new ImageIcon(this.getClass().getResource("/eight.png"));
		BombD = new ImageIcon(this.getClass().getResource("/bomb_defused.png"));
		
		is_bomb = (int)(Math.random()*5+1); // Generates bombs onto the game
		neighbors[cntr] = is_bomb; // Marks tiles as bombs globally
		clicked = false; // Sets tile default state to unclicked
		flagged = false; // Sets tile default state to unflagged
		cn = cntr; // Allow communication with neighbors by having accses to both local and 
		cntr++; // Increments counter
		
		// Block allows the for loops to run unrestricted, looks at values surrounding the clicked tile
		checker[0] = cn-1;
		checker[1] = cn+1;
		checker[2] = cn-43;
		checker[3] = cn+43;
		checker[4] = cn-42;
		checker[5] = cn+42;
		checker[6] = cn-44;
		checker[7] = cn+44;
		
		setIcon(Default); // Sets all tiles to Default by default
		this.addMouseListener(this); // Adds a action listener for detecting to player actions
	}
	
	public ImageIcon n2im(int a) { // Converts a number 1-8 to a corresponding image, out of range inputs use empty
		if (a == 1) {
			return One;
		} else if (a == 2) {
			return Two;
		} else if (a == 3) {
			return Three;
		} else if (a == 4) {
			return Four;
		} else if (a == 5) {
			return Five;
		} else if (a == 6) {
			return Six;
		} else if (a == 7) {
			return Seven;
		} else if (a == 8) {
			return Eight;
		} else {
			return Empty;
		}
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		bombN = 0; // Resets bombN. might not be necessary, but whatever
		
		if (e.getButton() == 3) { // Tests for right clicks, used for planting flags
			if (gameover) { // Runs gameover state on game, prevents all game tiles from being pressed 
				
			} else if (!clicked) { // Only runs flagging mechanic if a tile has not been opened
				if (flagged) { // Checks if a tile is flagged
					flagged = false; // Sets flagged to false as to unflag computationally
					setIcon(Default); // Sets tile to Default to unflag visually
				} else {
					flagged = true; // Sets flagged to true as to flag computationally
					setIcon(Flag); // Sets tile to Flag to flag visually
				}
			}
		} else if (e.getButton() == 1) { // Tests for left clicks if no right clicks have been made, used for mine sniffing
			if (gameover) { // Runs gameover state on game, prevents all game tiles from being pressed
				
			} else if (clicked && !flagged) { // Prevents prevents tiles from being clicked more than once
			} else if (is_bomb == 5 && !flagged) { // Runs events if a clicked tile is a bomb
				setIcon(Bomb);
				//gameover = true;
			} else if (!flagged) { // Runs events if a clicked tile isn't a bomb
				
				for (int x = 0; x < 8; x++) {  // Checks all 8 positions around selected tile
					if (0 <= checker[x] && checker[x] <= 645) { // Only acts on tiles if in the margins of the actual game
						if (neighbors[checker[x]] == 5) { // Only counts tiles trapped with bombs
							bombN++; // Adds to surround count
						}
					}
				}
				setIcon(n2im(bombN)); // Grabs image from n2im using bombN, for convenience and elegance
				clicked = true; // Prevents re-clicking
				//((JButton) MineSweeper.pan.getComponent(cn - 1)).setIcon(n2im(bombN));
			}
		}
	}
	// Bunch of code that is needed, due to the nature of MouseListener, but redundant
	@Override
	public void mouseEntered(MouseEvent e) {
	}
	@Override
	public void mouseExited(MouseEvent e) {
	}
	@Override
	public void mousePressed(MouseEvent e) {	
	}
	@Override
	public void mouseReleased(MouseEvent e) {
	}
}
