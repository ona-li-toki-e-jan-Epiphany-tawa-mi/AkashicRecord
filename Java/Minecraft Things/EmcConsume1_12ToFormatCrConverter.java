import java.util.Scanner;
import java.util.regex.Pattern;

public class EmcConsume1_12ToFormatCrConverter {
	static Scanner Femc = new Scanner(System.in);
	public static void main(String[] args) {
		//Asks For Commands
		System.out.println("Command?:");
		String pre1 = Femc.nextLine();
		String pre2 = Femc.nextLine();
		String pre3 = Femc.nextLine(); 
		//Rips Apart Commands For The Variables
		String[] present1 = pre1.split(Pattern.quote("\""));
		String[] present2 = pre2.split(" ");
		String[] pre4 = pre3.split("\\]");
		String[] present3 = pre4[0].split("=");
		//Placeholder
		System.out.println(present1[1]);
		System.out.println(present2[10]);
		System.out.println(present3[2]);
} }
 
