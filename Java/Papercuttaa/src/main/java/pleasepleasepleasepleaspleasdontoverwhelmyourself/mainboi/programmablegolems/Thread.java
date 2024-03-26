package pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems;

import pleasepleasepleasepleaspleasdontoverwhelmyourself.mainboi.programmablegolems.values.Void;

import java.util.ArrayDeque;
import java.util.Deque;

public class Thread {
    public final ProgrammableGolemInstance containingGolem;

    public int programCounter;
    private final Deque<Frame> frameStack;

    public Thread(ProgrammableGolemInstance programmableGolem) {
        containingGolem = programmableGolem;
        frameStack = new ArrayDeque<>();
    }

    /**
     * Steps forward the current thread.
     */
    void runThread() {
        if (!frameStack.isEmpty()) {
            Frame currentFrame = frameStack.peek();

            if (currentFrame.isComplete) {
                frameStack.pop();

                // Shoves the return value of the old frame onto the current one, if there is a frame left and the return type is not void.
                try {
                    Object returnValue = currentFrame.getReturnValue();

                    if (!(returnValue instanceof Void) && !frameStack.isEmpty()) {
                        frameStack.peek().operandStackPush(returnValue);
                    }

                } catch (Frame.MethodNotCompleteException exception) {
                    exception.printStackTrace();
                }

            } else
                currentFrame.runFrame();

        // Removes thread from execution if it has run out of frames.
        } else
            containingGolem.threads.remove(this);
    }
}
