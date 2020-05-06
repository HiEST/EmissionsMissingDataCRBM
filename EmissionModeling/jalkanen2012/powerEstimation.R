################################################################################
######################## Engine load balancing #################################
################################################################################

# Model assumes that all main engines are identical. Load values <= 85%.

# Number of engines operational (eq. 9):
#
# n_{OE} = P_{total}/P_{e} + 1
# Where:
#	- P_{total} = Total power requiered (calc. before)
#	- P_{e} = Installed engine power at MRC (Max. Continuous Rating)

calcNumOperativeEngines <- function(totalPower, enginePower) {
	# TODO: We assume that n_{OE} is an integer, so we need to truncate.
	trunc(totalPower/enginePower + 1)
}

# Engine load (eq. 10):
# 
# EL = P_{total}/(P_{e}*n_{oe})
# Where:
#	- n_{oe} = number of operational engines. 


calcEngineLoad <- function(totalPower, enginePower, nOperationalEngines) {
	totalPower/(enginePower*nOperationalEngines)
}


# Relative SFOC:
# SFOC_{relative} = 0.455*EL^2 - 0.71*EL + 1.28
# Where:
#	- EL: Engine load ranging from 0 to 1.


calcRelativeSFOC <- function(EL) {
	0.455*(EL^2) - 0.71*EL + 1.28
}
