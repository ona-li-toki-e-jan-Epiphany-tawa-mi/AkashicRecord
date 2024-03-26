package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values;

import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.instructions.AbstractInstruction;

public class MethodDefinition {
    private final String name;

    public final AbstractInstruction[] instructions;
    public final int localVariableCount;
    public final int operandStackSize;

    public MethodDefinition(String name, AbstractInstruction[] instructions, int localVariableCount, int operandStackSize) {
        this.name = name;
        this.instructions = instructions;
        this.localVariableCount = localVariableCount;
        this.operandStackSize = operandStackSize;
    }
}
