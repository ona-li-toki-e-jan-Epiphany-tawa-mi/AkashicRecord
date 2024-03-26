package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.instructions;

import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.Frame;



public class ReferenceNewArray extends AbstractInstruction {
    public ReferenceNewArray() {
        opCode = 0xBD;
        mnemonic = "anewarray";
    }

    @Override
    public void runInstruction(Frame callingFrame) {

    }
}
