#' @title plot.resp
#' @param data The ExpeData (".exp" – Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.
#' @param
#' @description Plots channels with relevant data on oxygen, carbon dioxide, water vapor etc. Finds the local maximum of peaks (carbon dioxide and water vapor only) and spits out the time, marker and maximal value in percentage. It also calculated the percentage change from baseline (uses the median value of entire distribution)
#' @author Daniel Noble – daniel.noble@anu.edu.au
#' @export

plot.resp <- function(data, channel, threshold_peak = 0.10, tau = 0.5, ...){

	# Extract the marker data and the file name
		marker <- attr(data, "marker")

	if(channel == "O2"){
	
		# Extract and plot data. For oxygen, multiple by -1 to invert the curves so they are upward facing
			dat <- extract_data(data)
			dat$O2_2 <- dat$O2*(-1)
			plot_simple(dat, channel  = "O2_2", marker, ...)

		# Find peaks in data.
			peaks <- ggpmisc:::find_peaks(dat[,"O2_2"], span = length(dat[,"O2_2"])/nrow(marker)-1)

		# Get the peak values from the raw data and the time points that these peaks exist at
			vals_peaks <- dat[,"O2_2"][peaks]
			time <- as.numeric(rownames(data.frame(dat))[peaks == TRUE])
			peak_data <- data.frame(vals_peaks, time)

		#Plot the maximum of the peaks
			points(vals_peaks ~ time, pch = 16)

		#Quantile regression of median to deal with drift over time
			median_pred <- quantile_median(dat, channel  = "O2_2", tau = tau)

		# Plot the median to ensure we can see that it is appropriate. Can change tau to offset median a bit.
			lines(median_pred ~ dat$time)

	}

	if(channel == "CO2"){

		# Extract and plot data. For oxygen, multiple by -1 to invert the curves so they are upward facing
			data <- extract_data(data)
			plot_simple(data, channel, marker, ...)

		# Find peaks in data.
			peaks <- ggpmisc:::find_peaks(data[,channel], span = length(data[,channel])/nrow(marker)-1)

		# Get the peak values from the raw data and the time points that these peaks exist at
			vals_peaks <- data[,channel][peaks]
			time <- as.numeric(rownames(data.frame(data))[peaks == TRUE])
			peak_data <- data.frame(vals_peaks, time)

		#Plot the maximum of the peaks
			points(vals_peaks ~ time, pch = 16)

		#Quantile regression of median to deal with drift over time
			median_pred <- quantile_median(data, channel  = "CO2", tau = tau)

		# Plot the median to ensure we can see that it is appropriate. Can change tau to offset median a bit.
			lines(median_pred ~ data$time)
	}

	# Check whether the marker number and the identified number of peaks match before creating dataframe
		if(length(peak_data$vals_peaks) != length(marker$text)){
				peak_data[peak_data$vals_peaks >= ((threshold_peak*median_pred[peak_data$time])+median_pred[peak_data$time]),]
		}

	# Return the data frame of change in percentage of gas in the air sample from baseline along with marker data.
		return(data.frame(
			       channel = peak_data$vals_peaks, 
			          time = peak_data$time, 
	                change = (peak_data$vals_peaks - median_pred[peak_data$time]),
		            marker = marker$text,
		     marker_sample = marker$sample,
		       marker_time = marker$time)
		)

}



extract_data <- function(data){
	dat <- data.frame(data[,])
	dat$time <- as.numeric(rownames(data.frame(data)))
	return(dat)
}


plot_simple <- function(data, channel, marker, ...){
	plot(data[,channel], ylim = c(min(data[,channel]) - 0.01, max(data[,channel]) + 0.01), ylab = paste0("%", channel), main = "", type = "l", ...)
	abline(v = (marker$sample), col = "black")
	text(marker$text, x = marker$sample - 5, y = max(data[,channel]) + 0.005)
}

quantile_median <- function(data, channel, tau = 0.5){
	      rqfit <- rq(data[,channel] ~ data$time, tau = tau)
	median_pred <- rqfit$coefficients[1] + rqfit$coefficients[2]*data$time
	return(median_pred)
}