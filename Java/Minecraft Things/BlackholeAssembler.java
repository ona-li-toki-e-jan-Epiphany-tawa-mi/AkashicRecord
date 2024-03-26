public class BlackholeAssembler {
	
	public static void main(String args[]) {
		double speed = 1;
		int distance = 1;
		
		while (distance <= 100) {
			int distance2 = distance - 1;
			System.out.println("execute at @e[type=armor_stand,name=blackhole2] as @e[tag=!blackhole,distance="+distance2+".."+distance+"] at @s facing entity @e[type=armor_stand,name=blackhole,distance="+distance2+".."+distance+",sort=nearest] eyes if block ~ ~ ~ #air if block ~ ~1 ~ #air run teleport @s ^ ^0.0001 ^"+String.format("%.2g",speed));
			distance = distance + 1;
			speed = speed / 1.08;
			// String.format("%.2g",speed)
		
		}
		
	}
}
