package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.instructions;

import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.Frame;

/**
 * Pushes a null reference to the stack.
 */
public class ReferenceConstantNull extends AbstractInstruction {
    public ReferenceConstantNull() {
        opCode = 0x01;
        mnemonic = "aconst_null";
    }

    @Override
    public void runInstruction(Frame callingFrame) {
        callingFrame.operandStackPush(null);

        callingFrame.containingThread.programCounter++;
    }
}
