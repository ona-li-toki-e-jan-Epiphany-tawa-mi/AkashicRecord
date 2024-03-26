package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values;

/**
 * Represents an object reference.
 */
public class Reference {
    private final int pointer;
    private final ReferenceType referenceType;

    public Reference(int pointer, ReferenceType referenceType) {
        this.pointer = pointer;
        this.referenceType = referenceType;
    }

    /**
     * @return The pointer of the reference, which is an address to an object instance.
     */
    public int getPointer() {
        return pointer;
    }

    /**
     * @return The type of the object that is referenced.
     */
    public ReferenceType getReferenceType() {
        return referenceType;
    }



    /**
     * Represents the type of object that is being referenced.
     */
    public enum ReferenceType {
        CLASS_REFERENCE,
        ARRAY_REFERENCE,
        INTERFACE_REFERENCE
    }
}
