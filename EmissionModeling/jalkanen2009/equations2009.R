# Jalkanen 2009

source("tools/interpolation.R")
source("jalkanen2009/powerEstimation.R")
source("jalkanen2009/emissionEstimation.R")

################################################################################
############################ Data flow chart ###################################
################################################################################

# 1 - [DATA] Receive AIS message
# 2 - [DATA] Get technical information from IHS tables
# 3 - [DATA] Determine ship type
#				(Default: Small craft. GT 620, main engine 2380 kW)
#	If insufficient data: use AIS data.
# 4 - [DATA] Determine Main Engine stroke type and RPM
#	Default: Medium speed diesel 500 RPM
# 5 - [CALC] Calculate Instantaneos Propeling Power. [Optional: Wave penalty]
# 6 - [CALC/DATA] Estimate Auxiliary Engine usage (including Boilers)
# 	Passenger, RoPax and Cruise ships: 4000kW always
#		ShiptypeLevel4:
#		[24] "Passenger (Cruise) Ship"
#		[25] "Passenger/Ro-Ro Cargo Ship"
#		[26] "Passenger Ship"
#	Others:
#		- Cruising (> 5 knots): 750 kW
#		- Manouvering (1-5 knots): 1250 kW
#		- Hotelling (< 1 knot): 1000 kW
#	(Do not exceed installed maximum in both cases)
# 7 - [DATA?] Apply measured emission values or emission abatement
#	If not persent: calculate NOx from IMO curve
# 8 - [CALC] Calculate NOx & SOx
# 9 - Put emissions to grid and plot.

estimateShipEmissions2009 <- function(dAISInt, shipParameters, sampleGranularity, unit="g") {
    # Scale the output. grams, kilograms and tonnes
    unit <- switch(unit, g = 1, kg = 10^3, t = 10^6) 


    # Retrieve IHS preprocessed parameters.
    attach(shipParameters, warn.conflicts=FALSE)
  
    # Calculate Instantaneous (Transient) Propeling Power
    transPME <- transientPowerME(dAISInt$sog, designSpeed, installedPowerME)
    
    # Estimate Aux Engine Usage
    transPAE <- transientPowerAE(dAISInt$sog, type, installedPowerAE)

    # Calculate CO2, SOx and NOx factors. Initially they are g/Kwh.
    SOxFactME <- calcSOxEmissionFactor(SC=0.001) # Set SFOC y SC/CC
    SOxFactAE <- calcSOxEmissionFactor(SC=0.001) #	Now using paper assump.
    CO2Fact <- calcCO2EmissionFactor()

    # We calculate the factor for all the engines and then sum it.
	NOxFactME <- calcNOxEmissionFactor(MainEngineRPM)
   	NOxFactAE <- calcNOxEmissionFactor(AuxiliaryEngineRPM)


	# Each sample is treated as an independent second if granularity is 1.
	# (g/Kwh * kW)/ h/s = g/s -> Grams for this second.
    # If the samples are not taken by second, we multiply them by the space
    # left between samples (sampleGranularity)
	SOxME <- (SOxFactME * transPME / 3600)/unit*sampleGranularity
	SOxAE <- (SOxFactAE * transPAE / 3600)/unit*sampleGranularity

	CO2ME <- (CO2Fact * transPME / 3600)/unit*sampleGranularity
	CO2AE <- (CO2Fact * transPAE / 3600)/unit*sampleGranularity

	NOxME <- (NOxFactME * transPME / 3600)/unit*sampleGranularity
	NOxAE <- (NOxFactAE * transPAE / 3600)/unit*sampleGranularity
    
    return(
        list(
            emissions=
                data.frame(
                    dAISInt, SOxME=SOxME, SOxAE=SOxAE, 
                    CO2ME=CO2ME, CO2AE=CO2AE, NOxME=NOxME, NOxAE=NOxAE,
                    transPME=transPME, transPAE=transPAE
                ),
            emissionFactors= 
                data.frame(
                    SOXFactME=SOxFactME, SOxFactAE=SOxFactAE, CO2Fact=CO2Fact, 
            			NOxFactME=NOxFactME, NOxFactAE=NOxFactAE
                )
        )
    )
}

