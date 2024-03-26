package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.instructions;

import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.Frame;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values.Reference;

// TODO Needs error handling.

/**
 * Loads a reference onto the stack from an array reference and an index to that array.
 */
public class ReferenceArrayLoad extends AbstractInstruction {
    public ReferenceArrayLoad() {
        opCode = 0x32;
        mnemonic = "aaload";
    }

    @Override
    public void runInstruction(Frame callingFrame) {
        int index = (int) callingFrame.operandStackPop();
        callingFrame.operandStackPush(
                ((Reference[]) callingFrame.operandStackPop())[index]
        );

        callingFrame.containingThread.programCounter++;
    }
}
