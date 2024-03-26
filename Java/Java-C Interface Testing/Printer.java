class Printer {
	static {
			System.loadLibrary("ppprint");
		}
		
	private native void printfff(String string);
		
	public static void main(String[] args) {
		new Printer().printfff("Hi!");
	}
}