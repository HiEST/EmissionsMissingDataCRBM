####################### EMISSION EVALUATION	## #################################

################################################################################
#################										  ######################
################################################################################

# Emission factors of the total PM, SO4 and associated H2O (i.e. H2O attached to
#  sulphate) are assumed to be linearly dependant to the fuel of sulphur content

# The values come from linear regressions applied over data from Buhaug et al.
#	2009. Check Jalkanen 2012.

# EF_{SO4} = 0.312 * S (Eq. 13a)
# Where:
#	- S: Percentage of sulphur in fuel
calcEmissionFactorSO4 <- function(S = 0.001) {
	0.312 * S
}

# EF_{H2O} = 0.244 * S (Eq. 13b)
calcEmissionFactorH2O <- function(S = 0.001) {
	0.244 * S
}

# EC, OC and ash are independent of sulphur factor.

# Calculate the Organic Carbon in base to the Engine Load
# OC_{EL} =>	(Eq.13c)
#		  = 3.333				[If EL < 0.15]
#		  = (a/(1+b*e^(-c*EL))) [If EL >= 0.15]
# Where:
#	- a = 1.024		[Dimensionless constant]
#	- b = -47.660	[Dimensionless constant]
#	- c = 32.547	[Dimensionless constant]
calcOCELFactor <- function(EL, a=1.024, b=-47.660, c=32.547) {
	res <- rep(3.333, length(EL)) # res[EL < 0.15] <- 3.3333
	res[EL >= 0.15] <- a/(1+b*exp(-c*EL[EL >= 0.15]))
    return(res)
}


# EF_{EC} = 0.08 g/kWh  (Eq. 13d)
calcEmissionFactorEC <- function() {
	return(0.08)
}

# EF_{OC} = 0.02 g/kWh  (Eq. 13d)
calcEmissionFactorOC <- function() {
	return(0.02)
}

# EF_{Ash} = 0.06 g/kWh (Eq. 13d)
calcEmissionFactorAsh <- function() {
	return(0.06)
}



# Particle matter emission factor:
# EF_{PM} = SFOC_{relative} * (EF_{SO4} + EF_{H2O} + EF_{OC} * OC_{EL} + EF_{EC}
#			+ EF_{Ash})  (Eq. 14)
# Where:
#	- SFOC_{relative}:
#	- EF_{SO4}: Emission factor for SO4 (g/kWh)
#	- EF_{H2O}: Emission factor for H2O (g/kWh)
#	- EF_{OC}: Emission factor for Organic Carbon (g/kWh)
#	- OC_{EL}: Organic Carbon in base to Engine Load
#	- EF_{EC}: Emission factor for Elementary Carbon (g/kWh)
#	- EF_{Ash}: Emission factor for ashes (g/kWh)


calcEmissionFactorPM <- function(relSFOC, EFSO4, EFH2O, OCEL,
								 EFOC = 0.02, EFEC = 0.08, EFAsh = 0.06){#Eq 13d
	relSFOC * (EFSO4 + EFH2O + (EFOC * OCEL) + EFEC + EFAsh)
}



# Acceleration Based Component
# ABC = max(alpha*(|incremental of v|/ incremental of t), 1)  (Eq.16)
# Where:
#	- Incremental of v: v_{n} - v_{n-1}
#	- Incremental of t: t_{n} - t_{n-1}
# Nota: Sample n, and the previous sample, sample n-1

calcABC <- function(speed, timestamps, alpha=582) {
	size <- length(speed)
	incV <- speed[-1] - speed[-size]	#speed_{n} - speed_{n-1}
	incT <- timestamps[-1] - timestamps[-size] #TODO: transform into seconds int
	abc <- alpha*(abs(incV)/incT)

	abc[abc > 1] <- 1	#If greater than 1, value is 1

	return(abc)
}

# EF_{CO} = CO_{base} * ABC (Eq. 15)
calcEmissionFactorCO <- function(COBase, ABC) {
	COBase * ABC
}

