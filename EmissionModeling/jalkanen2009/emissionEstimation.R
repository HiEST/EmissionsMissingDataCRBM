################################################################################
######################### Emissions assumptions ################################
################################################################################

# The specific fuel oil consumption (SFOC) depends on the engine type;
# two-stroke engines consume less (160– 200 g/kWh) and four-stroke
# engines slightly more (180– 250 g/kWh) (IMO, 2009)


################################################################################
######################### NOx Emission factor (g/kWh)###########################
################################################################################

# Using IMO NOx Tier 1 curve (For ships built from year 2000 onwards)
# Emission factor = 17		for engines less than 130rpm
# Emission factor = 45*n^(-0.2)	for engines 130 < n < 2000 rpm
# Emission factor = 9.8		for engines more than 2000 rpm

calcNOxEmissionFactor <- function(rpm) {
  # TODO: Now there won't be more than one engine
  cat("[Estimation] NOx Emission Factor... ")
	nf <- sapply(rpm, function(n) { #TODO: Improve with vectorization
			if (n > 0) {
				if (n < 130) {
					17
				} else if (n < 2000) {
					45*(n^(-0.2))
				} else {
					9.8
				}
			} else { # TODO: Why some ships have an engine with 0 kw and 0 rpm? 
				0
			}
		}
	)
	f <- sum(nf) # Return the sum of all the factors for each motor.
	cat(f, "\n")
	return(f)
}

################################################################################
########################## SO2 Emission factor #################################
################################################################################

# Variables:
#  - SFOC = Specific Fuel Oil Consumption (g/kWh)
#  - SC = Sulphur content of fuel (mass-%)
#  - M(S) = Molar mass of sulphur (g/mol)
#  - M(SO2) = Molar mass of sulphur dioxide (g/mol)
#  - n(S) = number of moles of sulphur (mol)
#  - n(SO2) = number of mols of sulphur dioxide (mol)
#  - m(S) = mass of sulphur (g) m(SO2) = mass of sulphur dioxide (g)

# n(S) = m(S)/M(S) = SFOC*SC/M(S) =
#		(200 g/kWh*0.015)/32.0655 g/mol = 0.09356 mol/kWh
# n(S) = n(SO2)
# m(SO2) = M(SO2) * n(SO2) = 64.06436 g/mol * 0.09356 mol/kWh = 5.994 g/kWh

calcSOxEmissionFactor <- function(SFOC = 200, SC = 0.001) {
	cat("[Estimation] SOx Emission Factor... ")
	nS <- (SFOC*SC)/32.0655
	mSOx <- 64.06436 * nS
	cat(mSOx, "\n")
	return(mSOx)
}

################################################################################
########################## CO2 Emission factor #################################
################################################################################

# Variables:
#  - SFOC = Specific Fuel Oil Consumption (g/kWh)
#  - CC = Carbon content of fuel (mass-%)
#  - M(C) = Molar mass of carbon (g/mol)
#  - n(C) = number of mols of carbon (mol)
#  - m(C) = mass of carbon (g)
#  - M(CO2) = molar mass of carbon dioxide (g/mol)
#  - n(CO2) = number of mols of carbon dioxide (mol)
#  - m(CO2) = mass of carbon dioxide (g)


# n(C) = m(C)/M(C) = SFOC*CC/M(S) =
#		(200 g/kWh*0.85)/12.01 g/mol = 14.15487 mol/kWh
# n(C) = n(CO2)
# m(CO2) = M(CO2) * n(CO2) = 44.00886 g/mol * 14.15487 mol/kWh = 622.94 g/kWh

calcCO2EmissionFactor <- function(SFOC = 200, CC = 0.85) {
	cat("[Estimation] CO2 Emission Factor... ")
	nC <- (SFOC*CC)/12.01
	mCO2 <-  44.00886 * nC
	cat(mCO2, "\n")
	return(mCO2)
}


