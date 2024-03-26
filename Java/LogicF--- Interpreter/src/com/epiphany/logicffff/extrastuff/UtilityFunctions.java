package com.epiphany.logicffff.extrastuff;

/**
 * A couple convenience things.
 */
public class UtilityFunctions {
    /**
     * Finds the "distance" between two strings.
     * Copied from: (https://en.wikipedia.org/wiki/Levenshtein_distance#Computing_Levenshtein_distance).
     * TODO Swap with 2-column version.
     * TODO Swap with 2-column version.
     * TODO Swap with 2-column version.
     *
     * @param string1 The first string to compare.
     * @param string2 The second string to compare.
     * @return The Levenshtein distance between the two strings.
     */
    public static int levenshteinDistance(String string1, String string2) {
        int i1 = string1.length() + 1;
        int j1 = string2.length() + 1;
        int[][] distanceMatrix = new int[i1][j1];

        // Sets initial values.
        for (int i = 1; i < i1; i++)
            distanceMatrix[i][0] = i;

        for (int i = 1; i < j1; i++)
            distanceMatrix[0][i] = i;

        // Computes distances.
        for (int i = 1; i < i1; i++)
            for (int k = 1; k < j1; k++) {
                int substitutionCost;

                if (string1.charAt(i - 1) == string2.charAt(k - 1)) {
                    substitutionCost = 0;

                } else
                    substitutionCost = 1;

                distanceMatrix[i][k] = Math.min(Math.min(distanceMatrix[i][k - 1], distanceMatrix[i - 1][k]) + 1, distanceMatrix[i - 1][k - 1] + substitutionCost);
            }

        return distanceMatrix[i1 - 1][j1 - 1];
    }

    /**
     * DWISOTT.
     * @param milliseconds Time in milliseconds to wait.
     */
    public static void delay(long milliseconds) {
        try {
            Thread.sleep(milliseconds);
        } catch (InterruptedException exception) {
            exception.printStackTrace();
        }
    }
}
