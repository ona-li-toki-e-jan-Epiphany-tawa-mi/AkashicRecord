public class CreeperConga {
	public static void main(String[] args) {
		//The second creeper in line-up
		int conPos = 2;
		//While loop
		while (conPos <= 100) {
			//Accounts for the creeper before the target
			int buddy = conPos - 1;
			//Assigns positions
			System.out.println("execute at @e[type=creeper,scores={congaPos="+buddy+"}] as @e[type=creeper,scores={congaPos=0},distance=..10] unless entity @e[type=creeper,scores={congaPos="+conPos+"}] run scoreboard players set @s congaPos "+conPos);
			//Moves to it's 'buddy'
			System.out.println("execute as @e[type=creeper,scores={congaPos="+conPos+"}] at @s if entity @e[type=creeper,scores={congaPos="+buddy+"},distance=1..20] facing entity @e[type=creeper,scores={congaPos="+buddy+"}] feet run teleport ^ ^ ^0.15");
			//Allows the line to correct itself
			System.out.println("execute as @e[type=creeper,scores={congaPos="+conPos+"}] at @s unless entity @e[type=creeper,scores={congaPos="+buddy+"},distance=..10] run scoreboard players set @s congaPos "+buddy);
			//Progresses the loop
			conPos = conPos + 1;
		}
		
	
}	}
