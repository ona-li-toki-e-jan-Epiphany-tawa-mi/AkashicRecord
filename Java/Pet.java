import java.math.BigDecimal;
import java.util.ArrayList;

public class Pet {
    // NOTE: Unable to tell exactly what these are supossed to be; insufficient information. Probably 
    //      static, being the amount of spaces to place pets in is a global concept. Assuming
    //      BoardingSpace<? extends Pet> type container and not number due to space counts being in Cat 
    //      and Dog class specifictions. Side note: why? There is no need to keep separate track of 
    //      spaces avalible in different classes when they could be stored in whatever type BoardingSpace
    //      would actually be. UML Class Diagram is poorly designed and will need to be clarified.
    private static BoardingSpace<Dog> dogSpaces = new BoardingSpace<>(30);
    private static BoardingSpace<Cat> catSpaces = new BoardingSpace<>(12);

    public static BoardingSpace<Dog> getDogSpaces() {
        return dogSpaces;
    }

    public static setDogSpaces(BoardingSpace<Dog> newDogSpaces) {
        dogSpaces = newDogSpaces;
    }

    public static BoardingSpace<Cat> getCatSpaces() {
        return catSpaces;
    }

    public static setDogSpaces(BoardingSpace<Cat> newCatSpaces) {
        catSpaces = newCatSpaces;
    }



    private PetType petType; // NOTE: Unless the user needs to define custom pets (i.e. not dog or cat,)
                             //     this can and should be an Enum (thus 'PetType'.)
    private String petName       = "";
    private int petAge           = -1;
    private int daysStay         = -1;
    private BigDecimal amountDue = new BigDecimal(-1); // NOTE: When dealing with monetary types,
                                                       //   variable percision is not acceptable
                                    
    public Pet(PetType type, String name, int age, int stayDuration, BigDecimal amountDue) {
        this.petType   = type;
        this.petName   = name;
        this.petAge    = age;
        this.daysStay  = stayDuration;
        this.amountDue = amountDue;
    }

    public PetType getPetType() {
        return this.petType;
    }

    public setPetType(PetType type) {
        this.petType = type;
    }

    public String getPetName() {
        return this.petName;
    }

    public setPetName(String name) {
        this.petName = name;
    }

    public int getPetAge() {
        return this.petAge;
    }

    public setPetAge(int age) {
        this.petAge = age;
    }

    public BigDecimal getAmountDue() {
        return this.amountDue;
    }

    public setAmountDue(BigDecimal amount) {
        this.amountDue = amount;
    }
}