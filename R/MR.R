

MR <- function(v, time, O2_data, CO2_data, RQ = 0.85, method = c("essie", "chris")){
	# Note that "channel" values are the ex current fractional value for oxygen and carbon dioxide at the peaks. Oxygen is inverted and so needs to be absolute.

	 changeC <-  mean(O2_data[O2_data$marker == "c", "change"])
	channelC <-  abs(mean(O2_data[O2_data$marker == "c", "channel"])) / 100
	
	if(method == "essie"){
		# Calculate water vapor pressure contribution
			ViH2O = (v*(changeC)) / (channelC)

		# Calculate metabolic rate with O2
			VolO2 = ( (v - ViH2O) * ((O2_data$change) - (abs(O2_data$channel/100)*(CO2_data$change))) )  / (1-(abs(O2_data$channel/100))
		
		# Calculate metabolic rate.
			 mO2 <- VolO2 / time

		# Calculate metabolic rate for CO2
			mCO2 <- (v*(CO2_data$change)) / time # eqn. 4.21, we are looking at the difference from baseline. 
	}

	if(method == "chris"){
		 
		 mO2 <- (v*(O2_data$change)) / time # Change in volume of oxygen, assuming little contribution of water
		mCO2 <- (v*(CO2_data$change)) / time # Change in volume of oxygen

	}
	
	data_list <- list(O2 = data.frame(O2_data, mO2), CO2 = data.frame(CO2_data, mCO2))
	data_new <- plyr::ldply(data_list)
	return(data_new)
}