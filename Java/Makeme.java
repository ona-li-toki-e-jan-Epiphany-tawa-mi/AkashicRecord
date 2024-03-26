import java.security.SecureRandom;

public class Makeme {
	private static SecureRandom st = new SecureRandom();
	private static final int epochs = 1000;
	private static final double chnce = 0.0914;
	
	public static main(String[] args) {
		int[] results = new int[epochs];
		
		for (int x = 0; x < epochs; x++) {
			results[x] = (calculateChance(chnce));
		}
		
		long average = 0;
		int max = results[0];
		int min = results[0];
		for (int rt : results) {
			average += (long) rt;
			if (max < rt)
				max = rt;
			if (min > rt)
				min = rt;
		}
		average /= (long) epochs;
		
		System.out.println("Average deaths per trait is: " + average);
		System.out.println("Maximum deaths per trait is: " + max);
		System.out.println("Minimum deaths per trait is: " + min);
	}
	
	private static int calculateChance(double chance) {
		double r;
		int cntr = 0;
		
		while (true) {
			r = st.nextInt(10000) / 100.0;
			cntr++;
			if (r <= chance)
				return cntr;
		}
	}
}