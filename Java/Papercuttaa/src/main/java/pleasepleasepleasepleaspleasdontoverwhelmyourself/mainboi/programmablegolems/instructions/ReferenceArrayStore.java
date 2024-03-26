package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.instructions;

import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.Frame;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values.Reference;

/**
 * Loads a reference into an array reference at the given index.
 */
public class ReferenceArrayStore extends AbstractInstruction {
    public ReferenceArrayStore() {
        opCode = 0x53;
        mnemonic = "aastore";
    }

    @Override
    public void runInstruction(Frame callingFrame) {
        Reference reference = (Reference) callingFrame.operandStackPop();
        int index = (int) callingFrame.operandStackPop();

        if (reference == null) {
            ((Reference[]) callingFrame.operandStackPop())[index] = null;

        } else
            if (reference.getReferenceType().equals(Reference.ReferenceType.CLASS_REFERENCE)) {

            }

        callingFrame.containingThread.programCounter++;
    }
}
