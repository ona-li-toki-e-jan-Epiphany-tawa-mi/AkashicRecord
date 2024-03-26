import java.util.Scanner;

public class EmcToConsume1_12Converter {
	static Scanner Cemc = new Scanner(System.in);
    public static void main(String[] args) {
    	//Asks For Details Of Command
    	System.out.print("Item Type: ");
    	String itemtype = Cemc.nextLine();
    	System.out.print("Tag Name: ");
    	String itemtag = Cemc.nextLine();
    	System.out.print("Emc Value: ");
    	try {
    	int emc = Cemc.nextInt();
    	//Gives Commands
    	System.out.print("--------------------\nexecute @a[team=abyssal,tag=devour] ~ ~ ~ scoreboard players tag @e[type=item,r=1] add "+itemtag+" {Item:{id:\"minecraft:"+itemtype+"\"}} \nexecute @e[type=item,tag="+itemtag+"] ~ ~ ~ scoreboard players add @a[team=abyssal,tag=devour,r=1] voidlvl "+emc+" \nkill @e[type=item,tag="+itemtag+"]");
    	} catch (Exception e) {
    	System.out.println("Put An Integer Value!"); }
}	}