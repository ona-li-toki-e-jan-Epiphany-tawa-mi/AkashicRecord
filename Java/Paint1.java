import java.util.InputMismatchException;
import java.util.Scanner;

public class Paint1 {
    /**
     * Takes in a double from stdin from a user. If they provide faulty input, this will repeatedly pull until a number within
     *      the acceptable range is found.
     * 
     * @param stdin       A scanner reading from stdin.
     * @param canNegative If false, will ensure that the number is not negative.
     * @param canZero     If false, will ensure that the number is not zero.
     * @return A number from the user in the specified range.
     */
    public static double inputDoubleSafely(Scanner stdin, boolean canNegative, boolean canZero) {
        while (true) 
            try {
                double result = stdin.nextDouble();
                
                if (!canNegative && result < 0) {
                    System.out.println("Error: cannot be negative!");
                    continue;
                }

                if (!canNegative && result == 0) {
                    System.out.println("Error: cannot be zero!");
                    continue;
                }

                return result;

            } catch (InputMismatchException invalidNumber) {
                System.out.println("Error: invalid number!");
            }
    }

    public static void main(String[] args) {
        Scanner scnr = new Scanner(System.in);
        double wallHeight = 0.0;
        double wallWidth = 0.0;
        double wallArea = 0.0;
        double gallonsPaintNeeded = 0.0;
        
        final double squareFeetPerGallons = 350.0;
        
        // Implement a do-while loop to ensure input is valid
        // Prompt user to input wall's height
        System.out.println("Enter wall height (feet): ");
        wallHeight = inputDoubleSafely(scnr, false);

        // Implement a do-while loop to ensure input is valid
        // Prompt user to input wall's width
        System.out.println("Enter wall width (feet): ");
        //wallHeight = scnr.nextDouble();
        wallWidth = inputDoubleSafely(scnr, false);

        // Calculate and output wall area
        wallArea = wallHeight * wallWidth;
        System.out.println("Wall area: " + wallArea + " square feet");

        // Calculate and output the amount of paint (in gallons) needed to paint the wall
        gallonsPaintNeeded = wallArea / squareFeetPerGallons;
        System.out.println("Paint needed: " + gallonsPaintNeeded + " gallons");

    }
}
