package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.instructions;

import java.util.HashMap;

public enum InstructionSetEnum {
    REFERENCE_ARRAY_LOAD((byte) 0x32, "aaload", (byte) 0),
    REFERENCE_ARRAY_STORE((byte) 0x53, "aastore", (byte) 0),
    REFERENCE_CONST_NULL((byte) 0x01, "aconst_null", (byte) 0), // Check.
    REFERENCE_LOAD((byte) 0x19, "aload", (byte) 1), // Check.
    REFERENCE_LOAD_0((byte) 0x2A, "aload_0", (byte) 0), // Check.
    REFERENCE_LOAD_1((byte) 0x2B, "aload_1", (byte) 0), // Check.
    REFERENCE_LOAD_2((byte) 0x2C, "aload_2", (byte) 0), // Check.
    REFERENCE_LOAD_3((byte) 0x2D, "aload_3", (byte) 0), // Check.
    REFERENCE_NEW_ARRAY((byte) 0xBD, "anewarray", (byte) 2),
    REFERENCE_RETURN((byte) 0xB0, "areturn", (byte) 0),

    ARRAY_LENGTH((byte) 0xBE, "arraylength", (byte) 0),

    REFERENCE_STORE((byte) 0x3A, "astore", (byte) 1),
    REFERENCE_STORE_0((byte) 0x4B, "astore_0", (byte) 0),
    REFERENCE_STORE_1((byte) 0x4C, "astore_1", (byte) 0),
    REFERENCE_STORE_2((byte) 0x4D, "astore_2", (byte) 0),
    REFERENCE_STORE_3((byte) 0x4E, "astore_3", (byte) 0),

    REFERENCE_THROW((byte) 0xBF, "athrow", (byte) 0),

    BYTE_ARRAY_LOAD((byte) 0x33, "baload", (byte) 0),
    BYTE_ARRAY_STORE((byte) 0x54, "bastore", (byte) 0),
    BYTE_INT_PUSH((byte) 0x10, "bipush", (byte) 1),

    BREAK_POINT((byte) 0xCA, "breakpoint", (byte) 0),

    CHARACTER_ARRAY_LOAD((byte) 0x34, "caload", (byte) 0),
    CHARACTER_ARRAY_STORE((byte) 0x55, "castore", (byte) 0),

    CHECK_CAST((byte) 0xC0, "checkcast", (byte) 2),

    DOUBLE_TO_FLOAT((byte) 0x90, "d2f", (byte) 0),
    DOUBLE_TO_INTEGER((byte) 0x8E, "d2i", (byte) 0),
    DOUBLE_TO_LONG((byte) 0x8F, "d2l", (byte) 0),
    DOUBLE_ADD((byte) 0x63, "dadd", (byte) 0),
    DOUBLE_ARRAY_LOAD((byte) 0x31, "daload", (byte) 0),
    DOUBLE_ARRAY_STORE((byte) 0x52, "dastore", (byte) 0),
    DOUBLE_COMPARE_GREATER((byte) 0x98, "dcmpg", (byte) 0),
    DOUBLE_COMPARE_LESSER((byte) 0x97, "dcmpl", (byte) 0),
    DOUBLE_CONSTANT_0((byte) 0x0E, "dconst_0", (byte) 0),
    DOUBLE_CONSTANT_1((byte) 0x0F, "dconst_1", (byte) 0),
    DOUBLE_DIVIDE((byte) 0x6F, "ddiv", (byte) 0),
    DOUBLE_LOAD((byte) 0x18, "dload", (byte) 1),
    DOUBLE_LOAD_0((byte) 0x26, "dload_0", (byte) 0),
    DOUBLE_LOAD_1((byte) 0x27, "dload_1", (byte) 0),
    DOUBLE_LOAD_2((byte) 0x28, "dload_2", (byte) 0),
    DOUBLE_LOAD_3((byte) 0x29, "dload_3", (byte) 0),
    DOUBLE_MULTIPLY((byte) 0x6B, "dmul", (byte) 0),
    DOUBLE_NEGATE((byte) 0x77, "dneg", (byte) 0),
    DOUBLE_REMAINDER((byte) 0x73, "drem", (byte) 0),
    DOUBLE_RETURN((byte) 0xAF, "dreturn", (byte) 0),
    DOUBLE_STORE((byte) 0x39, "dstore", (byte) 1),
    DOUBLE_STORE_0((byte) 0x47, "dstore_0", (byte) 0),
    DOUBLE_STORE_1((byte) 0x48, "dstore_1", (byte) 0),
    DOUBLE_STORE_2((byte) 0x49, "dstore_2", (byte) 0),
    DOUBLE_STORE_3((byte) 0x4A, "dstore_3", (byte) 0),
    DOUBLE_SUBTRACT((byte) 0x67, "dsub", (byte) 0),

    DUPLICATE((byte) 0x59, "dup", (byte) 0),
    DUPLICATE_X1((byte) 0x5A, "dup_x1", (byte) 0),
    DUPLICATE_X2((byte) 0x5B, "dup_x2", (byte) 0),
    DUPLICATE_2((byte) 0x5C, "dup2", (byte) 0),
    DUPLICATE_2_X1((byte) 0x5D, "dup2_x1", (byte) 0),
    DUPLICATE_2_X2((byte) 0x5E, "dup2_x2", (byte) 0),

    FLOAT_TO_DOUBLE((byte) 0x8D, "f2d", (byte) 0),
    FLOAT_TO_INTEGER((byte) 0x8B, "f2i", (byte) 0),
    FLOAT_TO_LONG((byte) 0x8C, "f2l", (byte) 0),
    FLOAT_ADD((byte) 0x62, "fadd", (byte) 0),
    FLOAT_ARRAY_LOAD((byte) 0x30, "faload", (byte) 0),
    FLOAT_ARRAY_STORE((byte) 0x51, "fastore", (byte) 0),
    FLOAT_COMPARE_GREATER((byte) 0x96, "fcmpg", (byte) 0),
    FLOAT_COMPARE_LESSER((byte) 0x95, "fcmpl", (byte) 0),
    FLOAT_CONSTANT_0((byte) 0x0B, "fconst_0", (byte) 0),
    FLOAT_CONSTANT_1((byte) 0x0C, "fconst_1", (byte) 0),
    FLOAT_CONSTANT_2((byte) 0x0D, "fconst_2", (byte) 0),
    FLOAT_DIVIDE((byte) 0x6E, "fdiv", (byte) 0),
    FLOAT_LOAD((byte) 0x17, "fload", (byte) 1),
    FLOAT_LOAD_0((byte) 0x22, "fload_0", (byte) 0),
    FLOAT_LOAD_1((byte) 0x23, "fload_1", (byte) 0),
    FLOAT_LOAD_2((byte) 0x24, "fload_2", (byte) 0),
    FLOAT_LOAD_3((byte) 0x25, "fload_3", (byte) 0),
    FLOAT_MULTIPLY((byte) 0x6A, "fmul", (byte) 0),
    FLOAT_NEGATION((byte) 0x76, "fneg", (byte) 0),
    FLOAT_REMAINDER((byte) 0x72, "frem", (byte) 0),
    FLOAT_RETURN((byte) 0xAE, "freturn", (byte) 0),
    FLOAT_STORE((byte) 0x38, "fstore", (byte) 1),
    FLOAT_STORE_0((byte) 0x43, "fstore_0", (byte) 0),
    FLOAT_STORE_1((byte) 0x44, "fstore_1", (byte) 0),
    FLOAT_STORE_2((byte) 0x45, "fstore_2", (byte) 0),
    FLOAT_STORE_3((byte) 0x46, "fstore_3", (byte) 0),
    FLOAT_SUBTRACT((byte) 0x66, "fsub", (byte) 0),

    GET_FIELD((byte) 0xB4, "getfield", (byte) 2),
    GET_STATIC((byte) 0xB2, "getstatic", (byte) 2),

    GOTO((byte) 0xA7, "goto", (byte) 2),
    GOTO_WORD((byte) 0xC8, "goto_w", (byte) 4),

    INTEGER_TO_BYTE((byte) 0x91, "i2b", (byte) 0),
    INTEGER_TO_CHARACTER((byte) 0x92, "i2c", (byte) 0),
    INTEGER_TO_DOUBLE((byte) 0x87, "i2d", (byte) 0),
    INTEGER_TO_FLOAT((byte) 0x86, "i2f", (byte) 0),
    INTEGER_TO_LONG((byte) 0x85, "i2l", (byte) 0),
    INTEGER_TO_SHORT((byte) 0x93, "i2s", (byte) 0),
    INTEGER_ADD((byte) 0x60, "iadd", (byte) 0),
    INTEGER_ARRAY_LOAD((byte) 0x2E, "iaload", (byte) 0),
    INTEGER_AND((byte) 0x7E, "iand", (byte) 0),
    INTEGER_ARRAY_STORE((byte) 0x4F, "iastore", (byte) 0),
    INTEGER_CONSTANT_M1((byte) 0x02, "iconst_m1", (byte) 0),
    INTEGER_CONSTANT_0((byte) 0x03, "iconst_0", (byte) 0),
    INTEGER_CONSTANT_1((byte) 0x04, "iconst_1", (byte) 0),
    INTEGER_CONSTANT_2((byte) 0x05, "iconst_2", (byte) 0),
    INTEGER_CONSTANT_3((byte) 0x06, "iconst_3", (byte) 0),
    INTEGER_CONSTANT_4((byte) 0x07, "iconst_4", (byte) 0),
    INTEGER_CONSTANT_5((byte) 0x08, "iconst_5", (byte) 0),
    INTEGER_DIVISION((byte) 0x6C, "idiv", (byte) 0),

    IF_REFERENCE_COMPARE_EQUALS((byte) 0xA5, "if_acmpeq", (byte) 2),
    IF_REFERENCE_COMPARE_NOT_EQUALS((byte) 0xA6, "if_acmpne", (byte) 2),

    IF_INTEGER_COMPARE_EQUALS((byte) 0x9F, "if_icmpeq", (byte) 2),
    IF_INTEGER_COMPARE_GREATER_OR_EQUALS((byte) 0xA2, "if_icmpge", (byte) 2),
    IF_INTEGER_COMPARE_GREATER((byte) 0xA3, "if_icmpgt", (byte) 2),
    IF_INTEGER_COMPARE_LESSER_OR_EQUALS((byte) 0xA4, "if_icmple", (byte) 2),
    IF_INTEGER_COMPARE_LESSER((byte) 0xA1, "if_icmplt", (byte) 2),
    IF_INTEGER_COMPARE_NOT_EQUALS((byte) 0xA0, "if_icmpne", (byte) 2),

    IF_EQUALS_0((byte) 0x99, "ifeq", (byte) 2),
    IF_GREATER_OR_EQUALS_0((byte) 0x9C, "ifge", (byte) 2),
    IF_GREATER_0((byte) 0x9D, "ifgt", (byte) 2),
    IF_LESSER_OR_EQUALS_0((byte) 0x9E, "ifle", (byte) 2),
    IF_LESSER_0((byte) 0x9B, "iflt", (byte) 2),
    IF_NOT_EQUALS_0((byte) 0x9A, "ifne", (byte) 2),

    IF_NOT_NULL((byte) 0xC7, "ifnonnull", (byte) 2),
    IF_NULL((byte) 0xC6, "ifnull", (byte) 2),

    INCREMENT_INDEX_CONSTANT((byte) 0x84, "iinc", (byte) 2),

    INTEGER_LOAD((byte) 0x15, "iload", (byte) 1),
    INTEGER_LOAD_0((byte) 0x1A, "iload_0", (byte) 0),
    INTEGER_LOAD_1((byte) 0x1B, "iload_1", (byte) 0),
    INTEGER_LOAD_2((byte) 0x1C, "iload_2", (byte) 0),
    INTEGER_LOAD_3((byte) 0x1D, "iload_3", (byte) 0),

    IMPLEMENTATION_DEPENDENT_1((byte) 0xFE, "impdep1", (byte) 1),
    IMPLEMENTATION_DEPENDENT_2((byte) 0xFF, "impdep2", (byte) 0),

    INTEGER_MULTIPLY((byte) 0x68, "imul", (byte) 0),
    INTEGER_NEGATE((byte) 0x74, "ineg", (byte) 0),

    INSTANCE_OF((byte) 0xC1, "instanceof", (byte) 2),

    INVOKE_DYNAMIC((byte) 0xBA, "invokedynamic", (byte) 4),
    INVOKE_INTERFACE((byte) 0xB9, "invokeinterface", (byte) 4),
    INVOKE_SPECIAL((byte) 0xB7, "invokespecial", (byte) 2),
    INVOKE_STATIC((byte) 0xB8, "invokestatic", (byte) 2),
    INVOKE_VIRTUAL((byte) 0xB6, "invokevirtual", (byte) 2),

    INTEGER_OR((byte) 0x80, "ior", (byte) 0),
    INTEGER_REMAINDER((byte) 0x70, "irem", (byte) 0),
    INTEGER_RETURN((byte) 0xAC, "ireturn", (byte) 0),
    INTEGER_SHIFT_LEFT((byte) 0x78, "ishl", (byte) 0),
    INTEGER_ARITHMETIC_SHIFT_RIGHT((byte) 0x7A, "ishr", (byte) 0),
    INTEGER_STORE((byte) 0x36, "istore", (byte) 1),
    INTEGER_STORE_0((byte) 0x3B, "istore_0", (byte) 0),
    INTEGER_STORE_1((byte) 0x3C, "istore_1", (byte) 0),
    INTEGER_STORE_2((byte) 0x3D, "istore_2", (byte) 0),
    INTEGER_STORE_3((byte) 0x3E, "istore_3", (byte) 0),
    INTEGER_SUBTRACT((byte) 0x64, "isub", (byte) 0),
    INTEGER_LOGICAL_SHIFT_RIGHT((byte) 0x7C, "iushr", (byte) 0),
    INTEGER_EXCLUSIVE_OR((byte) 0x82, "ixor", (byte) 0),

    JUMP_SUBROUTINE((byte) 0xA8, "jsr", (byte) 2),
    JUMP_SUBROUTINE_WORD((byte) 0xC9, "jsr_w", (byte) 4),

    LONG_TO_DOUBLE((byte) 0x8A, "l2d", (byte) 0),
    LONG_TO_FLOAT((byte) 0x89, "l2f", (byte) 0),
    LONG_TO_INTEGER((byte) 0x88, "l2i", (byte) 0),
    LONG_ADD((byte) 0x61, "ladd", (byte) 0),
    LONG_ARRAY_LOAD((byte) 0x2F, "laload", (byte) 0),
    LONG_AND((byte) 0x7F, "land", (byte) 0),
    LONG_ARRAY_STORE((byte) 0x50, "lastore", (byte) 0),
    LONG_COMPARE((byte) 0x94, "lcmp", (byte) 0),
    LONG_CONSTANT_0((byte) 0x09, "lconst_0", (byte) 0),
    LONG_CONSTANT_1((byte) 0x0A, "lconst_1", (byte) 0),

    LOAD_CONSTANT((byte) 0x12, "ldc", (byte) 1),
    LOAD_CONSTANT_WORD((byte) 0x13, "ldc_w", (byte) 2),
    LOAD_CONSTANT_2_WORDS((byte) 0x14, "ldc_w", (byte) 2),

    LONG_DIVIDE((byte) 0x6D, "ldiv", (byte) 0),
    LONG_LOAD((byte) 0x16, "lload", (byte) 1),
    LONG_LOAD_0((byte) 0x1E, "lload_0", (byte) 0),
    LONG_LOAD_1((byte) 0x1F, "lload_1", (byte) 0),
    LONG_LOAD_2((byte) 0x20, "lload_2", (byte) 0),
    LONG_LOAD_3((byte) 0x21, "lload_3", (byte) 0),
    LONG_MULTIPLY((byte) 0x69, "lmul", (byte) 0),
    LONG_NEGATE((byte) 0x75, "lneg", (byte) 0),

    LOOKUP_SWITCH((byte) 0xAB, "lookupswitch", (byte) 8), // TODO This instruction will need examination of its argument amount

    LONG_OR((byte) 0x81, "lor", (byte) 0),
    LONG_REMAINDER((byte) 0x71, "lrem", (byte) 0),
    LONG_RETURN((byte) 0xAD, "lreturn", (byte) 0),
    LONG_SHIFT_LEFT((byte) 0x79, "lshl", (byte) 0),
    LONG_ARITHMETIC_SHIFT_RIGHT((byte) 0x7B, "lshr", (byte) 0),
    LONG_STORE((byte) 0x37, "lstore", (byte) 1),
    LONG_STORE_0((byte) 0x3F, "lstore_0", (byte) 0),
    LONG_STORE_1((byte) 0x40, "lstore_1", (byte) 0),
    LONG_STORE_2((byte) 0x41, "lstore_2", (byte) 0),
    LONG_STORE_3((byte) 0x42, "lstore_3", (byte) 0),
    LONG_SUBTRACT((byte) 0x65, "lsub", (byte) 0),
    LONG_LOGICAL_SHIFT_RIGHT((byte) 0x7D, "lushr", (byte) 0),
    LONG_EXCLUSIVE_OR((byte) 0x83, "lxor", (byte) 0),

    MONITOR_ENTER((byte) 0xC2, "monitorenter", (byte) 0),
    MONITOR_EXIT((byte) 0xC3, "monitorexit", (byte) 0),

    MULTIDIMENSIONAL_REFERENCE_NEW_ARRAY((byte) 0xC5, "multianewarray", (byte) 3),
    NEW((byte) 0xBB, "new", (byte) 2),
    NEW_ARRAY((byte) 0xBC, "newarray", (byte) 1),

    NO_OPERATION((byte) 0x00, "nop", (byte) 0),

    POP((byte) 0x57, "pop", (byte) 0),
    POP_2((byte) 0x58, "pop2", (byte) 0),

    PUT_FIELD((byte) 0xB5, "putfield", (byte) 2),
    PUT_STATIC((byte) 0xB3, "putstatic", (byte) 2),

    RETURN_SUBROUTINE((byte) 0xA9, "ret", (byte) 1),
    RETURN((byte) 0xB1, "ret", (byte) 0),

    SHORT_ARRAY_LOAD((byte) 0x35, "saload", (byte) 0),
    SHORT_ARRAY_STORE((byte) 0x56, "sastore", (byte) 0),
    SHORT_INTEGER_PUSH((byte) 0x11, "sipush", (byte) 2),

    SWAP((byte) 0x5F, "swap", (byte) 0),

    TABLE_SWITCH((byte) 0xAA, "tableswitch", (byte) 16), // TODO This instruction will need examination of its argument amount

    WIDE((byte) 0xC4, "swap", (byte) 3); // TODO This instruction will need examination of its argument amount



    private final byte instructionCode, parameterCount;
    private final String mnemonic;

    InstructionSetEnum(byte instructionCode, String mnemonic, byte parameterCount) {
        this.instructionCode = instructionCode;
        this.mnemonic = mnemonic;
        this.parameterCount = parameterCount;
    }

    public byte getInstructionCode() {
        return instructionCode;
    }

    public String getMnemonic() {
        return mnemonic;
    }

    public byte getParameterCount() {
        return parameterCount;
    }



    // Lists for quickly converting instruction codes and mnemonics to instructions.
    private static final InstructionSetEnum[] sortedInstructionList = new InstructionSetEnum[256];
    private static final HashMap<String, InstructionSetEnum> mnemonicToInstruction = new HashMap<>();

    public static void onEnable() {
        for (InstructionSetEnum instruction : InstructionSetEnum.values()) {
            sortedInstructionList[((int) instruction.getInstructionCode()) & 0xFF] = instruction;
            mnemonicToInstruction.put(instruction.getMnemonic(), instruction);
        }
    }

    /**
     * Gets the instruction with the byte code, or null, if it cannot be found.
     *
     * @param instructionCode The byte code of the instruction to get.
     *
     * @return The corresponding instruction.
     */
    public static InstructionSetEnum getInstructionByCode(byte instructionCode) {
        return sortedInstructionList[((int) instructionCode) & 0xFF];
    }

    /**
     * Gets the instruction with the mnemonic given, or null, if it cannot be found.
     *
     * @param mnemonic The mnemonic of the instruction to get.
     *
     * @return The corresponding instruction.
     */
    public static InstructionSetEnum getInstructionByMnemonic(String mnemonic) {
        return mnemonicToInstruction.get(mnemonic);
    }
}
