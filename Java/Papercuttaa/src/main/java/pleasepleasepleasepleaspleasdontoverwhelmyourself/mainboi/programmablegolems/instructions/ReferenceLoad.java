package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.instructions;

import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.Frame;

/**
 * Loads a reference from local variables at the given index.
 */
public class ReferenceLoad extends AbstractInstruction {
    private final int variablesIndex;

    public ReferenceLoad(int index) {
        if (index <= 3) {
            opCode = (short) (0x2A + index);
            mnemonic = "aload_" + index;

        } else {
            opCode = 0x19;
            mnemonic = "aload";
        }

        variablesIndex = index;
    }

    @Override
    public void runInstruction(Frame callingFrame) {
        callingFrame.operandStackPush(callingFrame.localVariables[variablesIndex]);

        callingFrame.containingThread.programCounter++;
    }
}
