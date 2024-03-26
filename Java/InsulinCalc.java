import javax.swing.JOptionPane;

public class InsulinCalc {
    public static void main(String[] args){
    	//Defines Input Variables
    	int N = 0;
    	try {
    	//Asks For Variable Inputs
    	Integer bsugar = Integer.parseInt(JOptionPane.showInputDialog("What Is Current Blood Sugar?"));
    	Double carb = Double.parseDouble(JOptionPane.showInputDialog("How Many Carbs Are In Meal?"));
        Double carbr = Double.parseDouble(JOptionPane.showInputDialog("What Is The Carb Ratio?"));
        if (bsugar > 120) {
        	//Calculates For High Blood Sugar;
        	Double inSense = Double.parseDouble(JOptionPane.showInputDialog("Insulin Sensitivity Is: "));
        	double tempCarb = carb / carbr;
        	double tempSugar = bsugar - 120;
        	double preInsulin = tempSugar / inSense;
        	double Insulin = preInsulin + tempCarb;
        	JOptionPane.showMessageDialog(null,"The Insulin Dosage Is: "+Insulin+" Units"); 
        }else {N = N + 1; }
        if (bsugar <= 75){
        	//Calculate For Low
        	double tempCarb = carb / carbr;
        	double Insulin = tempCarb - 0.5;
        	JOptionPane.showMessageDialog(null,"The Insulin Dosage Is: "+Insulin+" Units");}
        else{N = N + 1; }
        if (N == 2) {
        	//Calculates For Stable Blood Sugar
        	double tempCarb = carb / carbr;
        	double Insulin = tempCarb;
        	JOptionPane.showMessageDialog(null,"The Insulin Dosage Is: "+Insulin+" Units"); }
    	} catch (Exception e) {
    		//A Basic Error Catcher To Avoid Problems
    		JOptionPane.showMessageDialog(null,"Awnser Correctly!"); }
}	}	
        