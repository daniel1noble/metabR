#' @title resp_data
#' @param metabR_data The processed data metabR data list from plot_resp.
#' @description Finds the relevant matching marker and peak data and merges the two data frames together. 
#' @author Daniel Noble – daniel.noble@anu.edu.au
#' @export

resp_data <- function(metabR_data){
	
	if(class(metabR_data) != "metabR") {stop("Object not of class metabR: Make sure you use plot_resp() and save resulting output to an object first")}
	# Save some important data and attributes
	duration <- attr(metabR_data, "duration")
	  median <- attr(metabR_data, "median")
	CO2peaks <- metabR_data$peak_data
   CO2Marker <- metabR_data$marker_data

	# First, clean up data by excluding and peaks detected before the first marker
	between_markers <- CO2peaks$time >= range(CO2Marker$marker_sample)[1] & CO2peaks$time <= duration 
	  CO2_peaks_fix <- CO2peaks[between_markers,]

	 # Create an empty data frame
	data_new <- data.frame()  

	# Loop through all the marker data.
	suppressWarnings(
	for(i in 1:nrow(CO2Marker)){
	 
	  if(CO2Marker$marker[i] != "c"){
	      get_peak_for_marker <- CO2_peaks_fix[dplyr::between(CO2_peaks_fix$time, CO2Marker$marker_time[i], CO2Marker$marker_time[i+1]), ]

	      if(nrow(get_peak_for_marker) == 0){
	      	get_peak_for_marker[1,] <- NA
	      }

	      merge_marker <- cbind(CO2Marker[i,], get_peak_for_marker)
	      data_new[i,colnames(merge_marker)] <- merge_marker
	  }

	   if(CO2Marker$marker[i] == "c" | CO2Marker$marker[i] == "C"){
	        merge_marker <- cbind(CO2Marker[i,], data.frame(channel = median, time = CO2Marker[i,"marker_time"], change = 0))
	        data_new[i,colnames(merge_marker)] <- merge_marker
	   }

	   if(CO2Marker$marker[i] != "c" & i == nrow(CO2Marker)){
	        get_peak_for_marker <- CO2_peaks_fix[dplyr::between(CO2_peaks_fix$time, CO2Marker$marker_time[i], duration), ]

		if(nrow(get_peak_for_marker) == 0){
	      	get_peak_for_marker[1,] <- NA
	      }

	      merge_marker <- cbind(CO2Marker[i,], get_peak_for_marker)
	      data_new[i,colnames(merge_marker)] <- merge_marker
	   }

	})

	attr(data_new, "class") <- c("metabR", "data.frame")
	return(data_new)
}