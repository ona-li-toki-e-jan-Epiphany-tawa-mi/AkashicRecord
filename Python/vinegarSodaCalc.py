import sys
import math

print()

pressure = 0
volume = 0
CH3COOH_concentration = 0


try:
    pressure = float(input("Enter the pressure that needs to be generated (in psi): "))

except Exception as exception:
    sys.exit("Error: " + str(exception))

if pressure <= 0:
    sys.exit("Pressure must be greater than 0!")

try:
    volume = float(input("Enter the volume of the container (in L): "))

except Exception as exception:
    sys.exit("Error: " + str(exception))

if volume <= 0:
    sys.exit("Volume must be greater than 0!")

try:
    CH3COOH_concentration = float(input("Enter the vinegar's concentration of CH3COOH (acetic acid): "))

except Exception as exception:
    sys.exit("Error: " + str(exception))

if CH3COOH_concentration <= 0 or CH3COOH_concentration >= 100:
    sys.exit("The concentration must be between 0 and 100 (exclusive)!")

# Converts psi to atm, accounts for vapor pressure of water.
pressure = pressure * 0.06804596379 - 0.0245
CH3COOH_concentration_p = CH3COOH_concentration / 100


# Finds the mass of the air in the container, assuming temperature is 21*C and the starting pressure is 1 atm. 
mol_air = volume / 0.08205736608096 / 294.15
# Finds the mass of CO2 needed to generate the extra pressure, assuming the starting pressure is 1 atm. 
mol_CO2 = mol_air * pressure - mol_air
# Finds the volume of vinegar needed, converts mol to g to L. Vinegar is CH3COOH_concentration% acetic acid by weight.
L_vinegar = mol_CO2 * 60 * (0.1 / CH3COOH_concentration)

print()
print(str(mol_CO2 * 84) + "g of NaHCO3 (baking soda) is needed")
print(str(L_vinegar) + " L of vinegar is needed")
