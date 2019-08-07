#' @title MR
#' @param v The volume of air sample for closed system respirometry. This volume excludes the mass of the animal
#' @param time The duration animal was enclosed within the chamber
#' @param O2_data The metabR resp data for oxygen
#' @param CO2_data The metabR resp data for carbon dioxide
#' @param method The method for calculation of metabolic rate using oxygen and carbon dioxide.
#' @description Finds the relevant matching marker and peak data and merges the two data frames together. 
#' @author Daniel Noble – daniel.noble@anu.edu.au
#' @export

	MR <- function(v, time, O2_data, CO2_data){
		# Note that "channel" values are the ex current fractional value for oxygen and carbon dioxide at the peaks. Oxygen is inverted and so needs to be absolute.

		 changeC <-  mean(O2_data[O2_data$marker == "c", "change"])
		channelC <-  abs(mean(O2_data[O2_data$marker == "c", "channel"])) / 100
		
			# Calculate water vapor pressure contribution from dry air versus "wet" air control
				ViH2O = (v*(changeC)) / (channelC)

			# Calculate metabolic rate with O2
				#Volo2 = (V- ViH2O)[(Fio2 – Feo2) - Feo2(FeCo2-FiCo2)] / (1-Feo2)
				VolO2 = ((v-ViH2O)*((O2_data$change) - (abs(O2_data$channel/100)*(CO2_data$change))) )  / (1-(abs(O2_data$channel/100)))

			# Calculate metabolic rate
				mO2 <- VolO2 / time

			# Calculate metabolic rate for CO2
				#VolCo2 = V (FeCo2 – FiCo2) / (1-FeCo2) # Denominator assumed 1
				mCO2 <- (v*(CO2_data$change)) / time # eqn. 4.21 (Lighton), we are looking at the difference from baseline. 
		
		data_list <- list(O2 = data.frame(O2_data, mO2), CO2 = data.frame(CO2_data, mCO2))
		data_new <- plyr::ldply(data_list)
		return(data_new)
	}

#' @title MRO2
#' @param v The volume of air sample for closed system respirometry. This volume excludes the mass of the animal
#' @param time The duration animal was enclosed within the chamber
#' @param bp The barometric pressure at the time the chamber was closed
#' @param t The temperature of the chamber air in degrees Celsius (note this is converted to Kelvins)
#' @param O2_data The metabR resp data for oxygen
#' @param CO2_data The metabR resp data for carbon dioxide
#' @description Finds the relevant matching marker and peak data and merges the two data frames together. 
#' @author Daniel Noble – daniel.noble@anu.edu.au
#' @export
MRO2 <- function(v, time, bp, t, O2_dataChangeChannel, CO2_dataChange){
			## Notes on definitions
				# change = the change in % gas from baseline to the maximum of the peak
				# channel = the actual ex-current (i.e., in sample) % gas concentration at the peak
				# changeC = the change in %O2 gas from baseline to the maximum of the peak for the control samples
				# channelC = the excurrent value of % oxygen (i.e., the max of the peak)

			# Calculate water vapor pressure contribution from dry air versus "wet" air control
				ViH2O = (v*(changeC)) / (channelC)

			# Calculate metabolic rate with O2
				#Volo2 = (V- ViH2O)[(Fio2 – Feo2) - Feo2(FeCo2-FiCo2)] / (1-Feo2)
				VolO2 = ((v-ViH2O)*((O2_data$change) - (abs(O2_data$channel/100)*(CO2_data$change))) )  / (1-(abs(O2_data$channel/100)))

			# Convert temperature, t, in degrees C to Kelvin
				k <- t + 273.15

			# Corrected volume by temperature and pressure
				CorVolO2 = (VolO2*bp*273.15) / (101.325*k)

			# Calculate metabolic rate
				mO2 <- CorVolO2 / time

	return(mO2)
}


#' @title MRCO2
#' @param v The volume of air sample for closed system respirometry. This volume excludes the mass of the animal
#' @param time The duration animal was enclosed within the chamber
#' @param bp The barometric pressure at the time the chamber was closed
#' @param t The temperature of the chamber air in degrees Celsius (note this is converted to Kelvins)
#' @param CO2_data The metabR resp data for carbon dioxide
#' @description Finds the relevant matching marker and peak data and merges the two data frames together. 
#' @author Daniel Noble – daniel.noble@anu.edu.au
#' @export

MRCO2 <- function(v, time, bp, t, CO2_dataChange){
		    ## Notes on definitions
				# change = the change in %CO2 gas from baseline to the maximum of the peak
				# channel = the actual ex-current (i.e., in sample) %CO2 gas concentration at the peak

		    # Volume of CO2
				VolCO2 = v*(CO2_data$change)

		    # Convert temperature, t, in degrees C to Kelvin
				k <- t + 273.15

			# Corrected volume by temperature and pressure
				CorVolCO2 = (VolCO2*bp*273.15) / (101.325*k)

			# Calculate metabolic rate for CO2
				#VolCo2 = V (FeCo2 – FiCo2) / (1-FeCo2) # Denominator assumed 1
				mCO2 <- (CorVolCO2) / time # eqn. 4.21 (Lighton), we are looking at the difference from baseline. 
	return(mCO2)
}