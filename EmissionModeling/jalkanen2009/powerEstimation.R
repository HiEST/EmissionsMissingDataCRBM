################################################################################
################## 5. ME Transient power calculation (simplif.) ################
################################################################################

transientPowerME <- function(VTransient, VDesign, PInstalled, EpsilonP = 0.8,
							   VSafety = 0.5) {
  cat("[Estimation] Main Engines Power\n")
	# Ship Specific Constant (Eq. 4)
	# Variables:
	#   - Epsilon_{p}: Main engine load at Maximum Continuous Rating.
	#						Assumed 80%.
	#   - Vdesign and Vsafety must be in m/s in the original. Better in knots.
    #	- Vsafety = 0.257 m/s = 0.5 knots
	# k <- (0.514)^3*(EpsilonP*PInstalled)/(Vdesign+Vsafety)^3 #Paper
	k <- (EpsilonP*PInstalled)/(VDesign+VSafety)^3 # In knots

	#Transient power calculation (Eq. 3)
	#MEPtransient <- k*(Vtransient^3)/(0.514^3)
	MEPtransient <- k*(VTransient^3) # In knots

	return(MEPtransient)
}

################################################################################
################## 6. AE Transient power calculation ###########################
################################################################################

speedToUsedBasePower <- function(speed, maxPower=NULL) {
	res <- rep(1000, length(speed)) #Hotelling as default	
	res[speed > 1 & speed <= 5] <- 1250
	res[speed > 5] <- 750
	if (!is.null(maxPower)) res[res > maxPower] <- maxPower #Limit max power
	res
}



transientPowerAE <- function(speed, type, instPow = NULL) {
  cat("[Estimation] Auxiliary Engines Power\n")
  # If unknown, no restriction.
  if (!is.null(instPow)) {
    if (is.na(instPow) | instPow <= 0) instPow <- NULL 
  }

  if (type == "Passenger (Cruise) Ship"    |
      type == "Passenger/Ro-Ro Cargo Ship" |
      type == "Passenger Ship") {
    # Return a vector of the size of input with minimum of 4000 or Max Power.
    rep(min(4000,instPow), length(speed))
  } else {
	speedToUsedBasePower(speed, instPow)
  }
}


