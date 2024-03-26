package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.instructions;

import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.Frame;

/**
 * Represents an instruction for the Java Virtual Machine.
 */
public abstract class AbstractInstruction {
    public short opCode;
    public String mnemonic;

    /**
     * @param callingFrame The frame that is running the instruction.
     */
    public abstract void runInstruction(Frame callingFrame);
}
