#' @title plot_resp
#' @param data The ExpeData (".exp" – Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.
#' @param channel The channel of interest (e.g., "O2", "CO2", "H2O" etc.). Note that for oxygen, the channel is automatically inverted so that maximal peaks can be found
#' @param threshold_peak The threshold which peaks are found at. Higher values mean less noise is detected. Note: Currently this is not functioning normally.
#' @param tau The quantile that one wishes to model across time. Default is currently the median, tau = 0.5
#' @param df The degrees of freedom used for fitting splines. Only used with method = "spline".
#' @param method There are two choices of method that can be used. The first, "norm", will model the data using a linear quantile regression, modeling the relevant quantile (defined by tau). The second method, "spline", will make use of a cubic spline to model the time series. The "spline" approach works very well for drifting and waving baselines, as is often the case for oxygen data. One needs to control both tau and df to avoid over or underfitting the baseline data.
#' @param ... Additional arguments passed to plot
#' @description Plots channels with relevant data on oxygen, carbon dioxide, water vapor etc. Finds the local maximum of peaks (carbon dioxide and water vapor only) and spits out the time, marker and maximal value in percentage. It also calculated the percentage change from baseline (uses the median value of entire distribution)
#' @author Daniel Noble – daniel.noble@anu.edu.au
#' @export

	plot_resp <- function(data, channel, threshold_peak = 0.10, tau = 0.5, df = 12, method = c("norm", "spline"), ...){

		# Extract the marker data and the file name
			marker <- attr(data, "marker")
			method <- match.arg(method)

		if(channel == "O2" | channel == "Oxygen"){

			# Extract and plot data. For oxygen, multiple by -1 to invert the curves so they are upward facing
				data <- extract_data(data)
				data$O2_2 <- data[,channel]*(-1)
				plot_simple(data, channel  = "O2_2", marker, ylab = paste0("%", channel), ...)

			# Find peaks in data.
				peaks <- ggpmisc:::find_peaks(data[,"O2_2"], span = length(data[,"O2_2"])/nrow(marker)-1)

			# Get the peak values from the raw data and the time points that these peaks exist at
				vals_peaks <- data[,"O2_2"][peaks]
				time <- as.numeric(rownames(data.frame(data))[peaks == TRUE])
				peak_data <- data.frame(vals_peaks, time)

			#Plot the maximum of the peaks
				graphics::points(vals_peaks ~ time, pch = 16)

			#Quantile regression of median to deal with drift over time
				median_pred <- quantile_median(data, channel  = "O2_2", df = df, tau = tau, method = method)

			# Plot the median to ensure we can see that it is appropriate. Can change tau to offset median a bit.
				graphics::lines(median_pred ~ data$time)

		}

		if(channel != "O2" | channel != "Oxygen"){

			# Extract and plot data. For oxygen, multiple by -1 to invert the curves so they are upward facing
				data <- extract_data(data)
				plot_simple(data, channel, marker, ylab = paste0("%", channel), ...)

			# Find peaks in data.
				peaks <- ggpmisc:::find_peaks(data[,channel], span = length(data[,channel])/nrow(marker)-1)

			# Get the peak values from the raw data and the time points that these peaks exist at
				vals_peaks <- data[,channel][peaks]
				time <- as.numeric(rownames(data.frame(data))[peaks == TRUE])
				peak_data <- data.frame(vals_peaks, time)

			#Plot the maximum of the peaks
				graphics::points(vals_peaks ~ time, pch = 16)

			#Quantile regression of median to deal with drift over time
				median_pred <- quantile_median(data, channel  = "CO2", df = df, tau = tau, method = method)

			# Plot the median to ensure we can see that it is appropriate. Can change tau to offset median a bit.
				graphics::lines(median_pred ~ data$time)
		}

		# Check whether the marker number and the identified number of peaks match before creating data frame
			if(length(peak_data$vals_peaks) != length(marker$text)){
					peak_data[peak_data$vals_peaks >= ((threshold_peak*median_pred[peak_data$time])+median_pred[peak_data$time]),]
			}

		# Return the data frame of change in percentage of gas in the air sample from baseline along with marker data.
			data_list <- list(peak_data = data.frame(
				       channel = peak_data$vals_peaks,
				          time = peak_data$time,
		                change = (peak_data$vals_peaks - median_pred[peak_data$time])),
					marker_data = data.frame(
			            marker = marker$text,
			     marker_sample = marker$sample,
			       marker_time = marker$time))

			class(data_list) <- "metabR"
			attr(data_list, "median") <- mean(median_pred)
			attr(data_list, "duration") <- max(data$time)
			return(data_list)
	}


#' @title extract_data
#' @param data The ExpeData (".exp" – Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.
#' @description Extracts data from SableBase loaded file that is relevant to the user
#' @author Daniel Noble – daniel.noble@anu.edu.au
#' @export
	extract_data <- function(data){
		dat <- data.frame(data[,])
		dat$time <- as.numeric(rownames(data.frame(data)))
		return(dat)
	}

#' @title plot_simple
#' @param data The ExpeData (".exp" – Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.
#' @param channel The specific channel, or data column from `extract_data` that is relevant for plotting.
#' @param marker The marker data that should be plotted.
#' @param ... Additional arguments passed to plot
#' @description Plots the relevant data as a function of time with the marker text plotted as well.
#' @author Daniel Noble – daniel.noble@anu.edu.au
	plot_simple <- function(data, channel, marker, ...){
		graphics::plot(data[,channel], ylim = c(min(data[,channel]) - 0.01, max(data[,channel]) + 0.01), type = "l", xlab = "Time", ...)
		graphics::abline(v = (marker$sample), col = "black")
		graphics::text(marker$text, x = marker$sample - 5, y = max(data[,channel]) + 0.005)
	}


#' @title quantile_median
#' @param data The ExpeData (".exp" – Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.
#' @param channel The specific channel, or data column from `extract_data` that is relevant for plotting.
#' @param df The degrees of freedom to use in cubic spline
#' @param method Which method to use to fit the quantile. Method = "norm" indicates you expect a simple linear quantile regression, method = "spline" indicates you would like to use a cubic spline with default df = 12.
#' @param tau The relevant quantile of interest. Default is the median, 0.5, but this can be modified depending on what is desired and the fit of the model.
#' @description Fits a quantile regression model to data, modeling the quantile described by tau – usually the median – to determine the baseline. This is modeled across time to account for drift, particularity in oxygen and water vapor.
#' @author Daniel Noble – daniel.noble@anu.edu.au
	quantile_median <- function(data, channel, tau = 0.5, df = 12, method = c("norm", "spline")){
		method = match.arg(method)

		if(method == "spline"){
				  rqfit <- quantreg::rq(data[,channel] ~ splines::bs(data$time, df = df), tau = tau)
			median_pred <- predict(rqfit, newdata = data.frame(data$time))
		}

		if(method == "norm"){
			      rqfit <- quantreg::rq(data[,channel] ~ data$time, tau = tau)
			median_pred <- rqfit$coefficients[1] + rqfit$coefficients[2]*data$time
		}

		return(median_pred)
}
