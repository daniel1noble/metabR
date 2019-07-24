#' @title plot.resp
#' @param data The ExpeData (".exp" – Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.
#' @param
#' @description Plots channels with relevant data on oxygen, carbon dioxide, water vapor etc. Finds the local maximum of peaks (carbon dioxide and water vapor only) and spits out the time, marker and maximal value in percentage. It also calculated the percentage change from baseline (uses the median value of entire distribution)
#' @author Daniel Noble – daniel.noble@anu.edu.au
#' @export

plot.resp <- function(data, channel, ...){
	plot(data[,channel], ylim = c(min(data[,channel]) - 0.01, max(data[,channel]) + 0.01), ylab = paste0("%", channel), main = "", ...)
	abline(v = (attr(data, "marker")$sample), col = "black")
	text(attr(data, "marker")$text, x = attr(data, "marker")$sample - 5, y = max(data[,channel]) + 0.005)

	peaks <- ggpmisc:::find_peaks(data[,channel], span = length(data[,channel])/nrow(attr(data, "marker"))-1)

	
	time <- as.numeric(rownames(data.frame(data))[peaks == TRUE])
	vals_peaks <- data[,channel][peaks]

	points(vals_peaks ~ time, pch = 16)

	median <- median(data[,channel])
	marker <- attr(data, "marker")$text

	return(data.frame(
		channel = vals_peaks, 
		               time = time, 
   change = (vals_peaks - median),
	marker = marker)
	)

}

