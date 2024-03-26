package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems;

import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.instructions.AbstractInstruction;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values.ClassDefinition;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values.MethodDefinition;
import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values.Void;

import java.util.ArrayDeque;
import java.util.Deque;

public class Frame {
    public final Thread containingThread;

    private final Object[] constantPoolReference;
    public final Object[] localVariables;
    private final Deque<Object> operandStack;
    private final int operandStackSize;

    private final AbstractInstruction[] instructions;

    public boolean isComplete;
    public Object returnValue;

    Frame(Thread containingThread, ClassDefinition methodClass, MethodDefinition method) {
        this.containingThread = containingThread;
        constantPoolReference = methodClass.constantPool;
        localVariables = new Object[method.localVariableCount];
        operandStack = new ArrayDeque<>(method.operandStackSize);
        operandStackSize = method.operandStackSize;
        instructions = method.instructions;
    }



    /**
     * Steps forward the current frame, if it's not done yet.
     */
    void runFrame() {
        if (containingThread.programCounter > instructions.length - 1) {
            isComplete = true;
            returnValue = new Void();

        } else
            instructions[containingThread.programCounter].runInstruction(this);
    }



    public void operandStackPush(Object object) {
        if (operandStack.size() < operandStackSize)
            operandStack.push(object);
    }

    public Object operandStackPop() {
        return operandStack.pop();
    }

    public Object operandStackPeek() {
        return operandStack.peek();
    }



    /**
     * Gets the return value of the method in the frame.
     * Will throw an exception if the method is not complete.
     *
     * @throws MethodNotCompleteException if the method is not complete.
     *  Use {@code isComplete()} to check if the method is complete.
     *
     * @return Gets the return value of the method.
     */
    Object getReturnValue() throws MethodNotCompleteException {
        if (isComplete) {
            return returnValue;

        } else
            throw new MethodNotCompleteException();
    }

    public static class MethodNotCompleteException extends Exception {
        private MethodNotCompleteException() {
            super("Method has not yet completed, and cannot give a return value.");
        }
    }
}
