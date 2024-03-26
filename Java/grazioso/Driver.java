import java.util.ArrayList;
import java.util.Scanner;

public class Driver {
    private static ArrayList<Dog> dogList = new ArrayList<Dog>();
    private static ArrayList<Monkey> monkeyList = new ArrayList<>();

    public static void main(String[] args) {
        initializeDogList();
        initializeMonkeyList();

        Scanner scanner = new Scanner(System.in);

    MenuLoop:
        while (true) {
            displayMenu();

        InputLoop:
            while (true) {
                System.out.print("> ");
                // User's original input is separated from the sanatized version so that it can be printed
                //      out as they wrote it. Printing the sanatized version might be a little confusing.
                String input = scanner.nextLine();
                String sanatizedInput = input.strip().toLowerCase();
                
                // and takes the appropriate action.
                if (sanatizedInput.length() == 0)
                    continue;

                switch (sanatizedInput.charAt(0)) {
                    case '1': // Intake a new dog.
                        intakeNewDog(scanner);
                        break InputLoop;

                    case '2': // Intake a new monkey.
                        intakeNewMonkey(scanner);
                        break InputLoop;

                    case '3': // Reserve an animal.
                        reserveAnimal(scanner);
                        break InputLoop;

                    case '4': // Print a list of all dogs.
                        printAnimals(AnimalListType.Dog);
                        break InputLoop;

                    case '5': // Print a list of all monkeys.
                        printAnimals(AnimalListType.Monkey);
                        break InputLoop;

                    case '6': // Print a list of all animals that are not reserved.
                        printAnimals(AnimalListType.Avalible);
                        break InputLoop;

                    case 'q': // Quit application.
                        break MenuLoop;

                    default:
                        System.out.println("ERROR: Invalid choice '" + input + "'");
                }
            }

            System.out.print("Press ENTER to continue.");
            scanner.nextLine();
        }
    }

    /**
     * Prints menu options.
     */
    public static void displayMenu() {
        System.out.println("\n\n");
        System.out.println("\t\t\t\tRescue Animal System Menu");
        System.out.println("[1] Intake a new dog");
        System.out.println("[2] Intake a new monkey");
        System.out.println("[3] Reserve an animal");
        System.out.println("[4] Print a list of all dogs");
        System.out.println("[5] Print a list of all monkeys");
        System.out.println("[6] Print a list of all animals that are not reserved");
        System.out.println("[q] Quit application");
        System.out.println();
        System.out.println("Enter a menu selection");
    }


    // Adds dogs to a list for testing
    public static void initializeDogList() {
        Dog dog1 = new Dog("Spot", "German Shepherd", "male", "1", "25.6", "05-12-2019", "United States", "intake", false, "United States");
        Dog dog2 = new Dog("Rex", "Great Dane", "male", "3", "35.2", "02-03-2020", "United States", "Phase I", false, "United States");
        Dog dog3 = new Dog("Bella", "Chihuahua", "female", "4", "25.6", "12-12-2019", "Canada", "in service", true, "Canada");

        dogList.add(dog1);
        dogList.add(dog2);
        dogList.add(dog3);
    }


    // Adds monkeys to a list for testing
    //Optional for testing
    public static void initializeMonkeyList() {

    }


    /**
     * Runs the menu for intaking a new dog into the system.
     * @param scanner A reference to user input.
     */
    public static void intakeNewDog(Scanner scanner) {
        System.out.print("What is the dog's name? ");//modified.
        String name = scanner.nextLine().strip();//modified.
        for(Dog dog: dogList) {
            if(dog.getName().equalsIgnoreCase(name)) {
                System.out.println("\n\nThis dog is already in our system\n\n");
                return; //returns to menu
            }
        }

        // Add the code to instantiate a new dog and add it to the appropriate list.
        System.out.print("What is the dog's breed? ");
        String breed = scanner.nextLine().strip();
        System.out.print("What is the dog's gender? ");
        String gender = scanner.nextLine().strip();
        System.out.print("What is the dog's age? ");
        String age = scanner.nextLine().strip();
        System.out.print("What is the dog's weight? ");
        String weight = scanner.nextLine().strip();
        System.out.print("What is the date when the dog was aquired? ");
        String aquisitionDate = scanner.nextLine().strip();
        System.out.print("What is the country in which the dog was aquired? ");
        String aquisitionCountry = scanner.nextLine().strip();
        System.out.print("What is the dog's training status? ");
        String trainingStatus = scanner.nextLine().strip();
        System.out.print("What country does the dog serve in? ");
        String inServiceCountry = scanner.nextLine().strip();

        dogList.add(new Dog(name, breed, gender, age, weight, aquisitionDate, aquisitionCountry
                           , trainingStatus, false, inServiceCountry));
    }


    /**
     * Runs the menu for intaking a new monkey into the system.
     * @param scanner A reference to user input.
     */
    public static void intakeNewMonkey(Scanner scanner) {
        System.out.print("What is the monkey's name? ");
        String name = scanner.nextLine().strip();
        for(Monkey monkey : monkeyList) {
            if(monkey.getName().equalsIgnoreCase(name)) {
                System.out.println("\n\nThis monkey is already in our system\n\n");
                scanner.nextLine();
                return; 
            }
        }

        System.out.print("What is the monekey's species? ");
        String species = scanner.nextLine().strip();
        System.out.print("What is the monekey's gender? ");
        String gender = scanner.nextLine().strip();
        System.out.print("What is the monekey's age? ");
        String age = scanner.nextLine().strip();
        System.out.print("What is the monekey's weight? ");
        String weight = scanner.nextLine().strip();
        System.out.print("What is the date when the monekey was aquired? ");
        String aquisitionDate = scanner.nextLine().strip();
        System.out.print("What is the country in which the monekey was aquired? ");
        String aquisitionCountry = scanner.nextLine().strip();
        System.out.print("What is the monekey's training status? ");
        String trainingStatus = scanner.nextLine().strip();
        System.out.print("What country does the monekey serve in? ");
        String inServiceCountry = scanner.nextLine().strip();
        System.out.print("What is the monekey's tail length? ");
        String tailLength = scanner.nextLine().strip();
        System.out.print("What is the monekey's body length? ");
        String bodyLength = scanner.nextLine().strip();
        System.out.print("What is the monekey's height? ");
        String height = scanner.nextLine().strip();

        monkeyList.add(new Monkey(name, species, gender, age, weight, aquisitionDate, aquisitionCountry
                                 , trainingStatus, false, inServiceCountry, tailLength
                                 , bodyLength, height));
    }

    /**
     * Runs the menu for reserving an animal.
     * @param scanner A reference to user input.
     */
    public static void reserveAnimal(Scanner scanner) {
        ArrayList<? extends RescueAnimal> searchList = null;

    InputLoop:
        while (true) {
            System.out.print("Is the animal a (d)og or a (m)onkey? ");
            String animalType = scanner.nextLine();
            String sanatizedAnimalType = animalType.strip();

            if (!sanatizedAnimalType.isEmpty())
                switch (sanatizedAnimalType.toLowerCase().charAt(0)) {
                    case 'd':
                        searchList = dogList;
                        break InputLoop;
                        
                    case 'm':
                        searchList = monkeyList;
                        break InputLoop;
                }

            System.out.println("ERROR: Unknown animal type '" + animalType + "'!");
        }

        System.out.println("What is the animal's service country?");
        String serviceCountry = scanner.nextLine();
        String sanatizedServiceCountry = serviceCountry.strip();


        for (RescueAnimal animal : searchList)
            if (!animal.getReserved() && 
                    "in service".equalsIgnoreCase(animal.getTrainingStatus()) &&
                    animal.getInServiceLocation().equalsIgnoreCase(sanatizedServiceCountry)) {
                animal.setReserved(true);
                System.out.println("Successfully reserved '" + animal.getName() + 
                                   "' for given service country " + serviceCountry);
                return;
            }

        System.out.println("Unable to find animal to reserve under given service country '" + 
                           serviceCountry + "'");        
    }

        // Complete printAnimals
        // Include the animal name, status, acquisition country and if the animal is reserved.
	// Remember that this method connects to three different menu items.
        // The printAnimals() method has three different outputs
        // based on the listType parameter
        // dog - prints the list of dogs
        // monkey - prints the list of monkeys
        // available - prints a combined list of all animals that are
        // fully trained ("in service") but not reserved 
	// Remember that you only have to fully implement ONE of these lists. 
	// The other lists can have a print statement saying "This option needs to be implemented".
	// To score "exemplary" you must correctly implement the "available" list.
    /**
     * Used to select which list type to print out when using the {@see Driver#printAnimals()} method.
     */
    public static enum AnimalListType {
        Dog,         // Prints out dogs.
        Monkey,      // Prints out monkeys.
        Avalible,    // Prints out any avalible animals.
    }

    /**
     * Prints out an animal's information.
     * 
     * @param animal      The animal to print out.
     * @param entryNumber The 1-indexed number of the entry in the list of animals to print.
     */
    private static void printAnimal(RescueAnimal animal, int entryNumber) {
        System.out.println("Entry " + entryNumber + ":");
        System.out.println("    Type: " + animal.getClass().getName());
        System.out.println("    Name: " + animal.getName());
        System.out.println("    Status: " + animal.getTrainingStatus());
        System.out.println("    Aquisition Country: " + animal.getAcquisitionLocation());
        System.out.println("    Is Reserved?: " + (animal.getReserved() ? "yes" : "no"));
    }

    /**
     * Prints out a list of animals of the specified type.
     * @param animalListType {@see Driver#AnimalListType}.
     */
    public static void printAnimals(AnimalListType animalListType) {
        System.out.println();

        switch (animalListType) {
            case Dog:
                for (int i = 0; i < dogList.size(); i++)
                    printAnimal(dogList.get(i), i + 1);

                break;

            case Monkey:
                for (int i = 0; i < monkeyList.size(); i++) 
                    printAnimal(monkeyList.get(i), i + 1);

                break;

            case Avalible:
                int entryNumber = 0;

                for (; entryNumber < dogList.size(); entryNumber++)
                    if (!dogList.get(entryNumber).getReserved())
                        printAnimal(dogList.get(entryNumber), entryNumber + 1);

                for (int i = 0; i < monkeyList.size(); i++, entryNumber++) 
                    if (!monkeyList.get(i).getReserved())
                        printAnimal(monkeyList.get(i), entryNumber + 1);

                break;

            default:
                System.out.println("ERROR: Unimplemented animal list type '" + animalListType.name() +
                                   "'' supplied to Driver#printAnimals()!");          
        }

        System.out.println();
    }
}

