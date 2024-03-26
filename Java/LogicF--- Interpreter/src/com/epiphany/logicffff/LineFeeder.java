package com.epiphany.logicffff;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * An interpreter for the LineFeed language.
 */
public class LineFeeder {
    private static final List<String> helpCommands = Arrays.asList("-h", "--help");

    public static void main(String[] args) {
        File codeFile = null;

        // Gets file from command line, displays help messages.
        if (args.length >= 1) {
            String argument = args[0];

            if (helpCommands.contains(argument.toLowerCase())) {
                System.out.println("Usage: LineFeeder <file>");
                System.exit(0);
            }

            codeFile = new File(argument);

            if (!codeFile.isFile()) {
                System.err.println(codeFile.exists() ? "FileTypeError: " + argument + " is not a LineFeed file" : "FileError: " + argument + " does not exist");
                System.exit(1);
            }

            String[] splitPath = argument.split("\\.");
            if (!splitPath[splitPath.length - 1].toLowerCase().equals("lf")) {
                System.err.println("FileTypeError: " + argument + " is not a LineFeed file");
                System.exit(1);
            }

        } else {
            System.out.println("Usage: LineFeeder <file>");
            System.exit(0);
        }

        interpretFile(codeFile);
    }

    /**
     * Interprets the code in the given file as a LineFeed program.
     */
    public static void interpretFile(File sourceFile) {
        try {
            BufferedReader fileReader = new BufferedReader(new FileReader(sourceFile));
            long line = 1;

            for (int command = fileReader.read(); command != -1; command = fileReader.read()) {
                if (command != '\n') {
                    System.err.println("InvalidCommandError: Invalid character '" + (char) command + "' at " + line + ":1, only newlines are allowed in LineFeed programs");
                    System.exit(1);
                }

                line++;
            }

            fileReader.close();

        } catch (IOException exception) {
            exception.printStackTrace();
            System.exit(-1);
        }
    }
}
