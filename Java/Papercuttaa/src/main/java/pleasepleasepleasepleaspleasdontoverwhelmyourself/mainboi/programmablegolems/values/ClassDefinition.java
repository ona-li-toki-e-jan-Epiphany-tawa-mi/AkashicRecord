package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values;

public class ClassDefinition {
    public final Object[] constantPool;

    private final ClassAccessFlags[] accessFlags;

    private final int[] interfaceIndexes;

    private final FieldDefinition[] fields;
    private final MethodDefinition[] methods;
    private final Attribute[] attributes;


    private final String name;
    private final String superName;

    public ClassDefinition(Object[] constants, ClassAccessFlags[] accessFlags, int thisIndex, int superIndex, int[] interfaceIndexes, FieldDefinition[] fields, MethodDefinition[] methods,
                           Attribute[] attributes) {
        constantPool = constants;
        this.accessFlags = accessFlags;
        this.interfaceIndexes = interfaceIndexes;
        this.fields = fields;
        this.methods = methods;
        this.attributes = attributes;

        name = ((ClassInfoStructure) constantPool[thisIndex]).name;
        superName = ((ClassInfoStructure) constantPool[superIndex]).name;
    }



    public enum ClassAccessFlags {
        ACC_PUBLIC,
        ACC_SYNTHETIC,

        ACC_SUPER,

        ACC_FINAL,
        ACC_ABSTRACT,

        ACC_INTERFACE,
        ACC_ANNOTATION,
        ACC_ENUM,
        ACC_MODULE
    }


    private static class ClassInfoStructure {
        private final String name;

        private ClassInfoStructure(String name) {
            this.name = name;
        }
    }
}
