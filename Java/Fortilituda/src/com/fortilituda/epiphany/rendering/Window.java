package com.fortilituda.epiphany.rendering;

import javax.swing.*;
import java.awt.*;

public class Window {
    private JFrame frame;
    private Canvas canvas;

    public Window() {
        frame = new JFrame("Default Name");
        frame.setSize(420, 420);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);

        canvas = new Canvas();
        Dimension canvasDimensions = new Dimension(420, 420);
        canvas.setPreferredSize(canvasDimensions);
        canvas.setMaximumSize(canvasDimensions);
        canvas.setMinimumSize(canvasDimensions);

        frame.add(canvas);
        frame.pack();
    }

    public Window(String title, int width, int height) {
        frame = new JFrame(title);
        frame.setSize(width, height);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLocationRelativeTo(null);
        frame.setVisible(true);

        canvas = new Canvas();
        Dimension canvasDimensions = new Dimension(width, height);
        canvas.setPreferredSize(canvasDimensions);
        canvas.setMaximumSize(canvasDimensions);
        canvas.setMinimumSize(canvasDimensions);

        frame.add(canvas);
        frame.pack();
    }

    public Canvas getCanvas() {
        return canvas;
    }

    public int getWindowWidth() {
        return frame.getWidth();
    }

    public int getWindowHeight() {
        return frame.getWidth();
    }

    public Dimension getWindowSize() {
        return new Dimension(frame.getWidth(), frame.getHeight());
    }

    public void setWindowSize(int width, int height) {
        frame.setSize(width, height);
    }

    public void setWindowSize(Dimension dimensions) {
        frame.setSize(dimensions);
    }

    public void setCanvasSize(int width, int height) {
        Dimension dimensions = new Dimension(width, height);

        canvas.setPreferredSize(dimensions);
        canvas.setMaximumSize(dimensions);
        canvas.setMinimumSize(dimensions);
    }

    public void setCanvasSize(Dimension dimensions) {
        canvas.setPreferredSize(dimensions);
        canvas.setMaximumSize(dimensions);
        canvas.setMinimumSize(dimensions);
    }
}
