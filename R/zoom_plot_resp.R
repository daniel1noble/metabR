#' @title zoom_plot_resp
#' @param data The ExpeData (".exp" ??? Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.
#' @param channel The channel of interest (e.g., "O2", "CO2", "H2O" etc.). Note that for oxygen, the channel is automatically inverted so that maximal peaks can be found
#' @param threshold_peak The threshold which peaks are found at. Higher values mean less noise is detected. Note: Currently this is not functioning normally.
#' @param tau The quantile that one wishes to model across time. Default is currently the median, tau = 0.5
#' @param cut_start Where you want to start detecting peaks i.e. region you want to zoom in on
#' @param cut_end Where you want to end peak detection
#' @param ... Additional arguments passed to plot
#' @description Plots channels with relevant data on oxygen, carbon dioxide, water vapor etc. Finds the local maximum of peaks (carbon dioxide and water vapor only) and spits out the time, marker and maximal value in percentage. It also calculated the percentage change from baseline (uses the median value of entire distribution)
#' @author Daniel Noble ??? daniel.noble@anu.edu.au and Fonti Kar fonti.kar@gmail.com
#' @export

zoom_plot_resp <- function(data, channel, threshold_peak = 0.10, tau = 0.5, cut_start, cut_end, ...){
  
  # Extract the marker data and the file name
  marker <- attr(data, "marker")
  
  if(channel == "O2"){
    
    # Extract and plot data. For oxygen, multiple by -1 to invert the curves so they are upward facing
    data <- extract_data(data)
    data$O2_2 <- data$O2*(-1)
    my_plot_simple(data, channel  = "O2_2", marker, ylab = paste0("%", channel), cut_start, cut_end,...)
    
    # Find peaks in data.
    #peaks <- ggpmisc:::find_peaks(data[,"O2_2"], span = length(data[,"O2_2"])/nrow(marker)-1)
    
    peaks <- ggpmisc:::find_peaks(data[data$time >= cut_start & data$time <= cut_end, "O2_2"], span = length(data[data$time >= cut_start & data$time <= cut_end, "O2_2"])/nrow(marker[marker$time >= cut_start & marker$time <= cut_end,])-1)
    
    # Get the peak values from the raw data and the time points that these peaks exist at
    #vals_peaks <- data[,"O2_2"][peaks]
    
    vals_peaks <- data[data$time >= cut_start & data$time <= cut_end, "O2_2"][peaks]
    
    time <- data$time[cut_start:cut_end][peaks == TRUE]
    
    peak_data <- data.frame(vals_peaks, time)
    
    #Plot the maximum of the peaks
    graphics::points(vals_peaks ~ time, pch = 16)
    
    #Quantile regression of median to deal with drift over time
    median_pred <- quantile_median(data[data$time >= cut_start & data$time <= cut_end,], channel  = "O2_2", tau = tau)
    
    # Plot the median to ensure we can see that it is appropriate. Can change tau to offset median a bit.
    graphics::lines(median_pred ~ data[data$time >= cut_start & data$time <= cut_end,]$time)
    
  }
  
  if(channel != "O2"){
    
    # Extract and plot data.
    data <- extract_data(data)
    my_plot_simple(data, channel, marker, ylab = paste0("%", channel),cut_start, cut_end, ...)
    
    # Find peaks in data
    peaks <- ggpmisc:::find_peaks(data[data$time >= cut_start & data$time <= cut_end, channel], span = length(data[data$time >= cut_start & data$time <= cut_end, channel])/nrow(marker[marker$time >= cut_start & marker$time <= cut_end,])-1)
    
    # Get the peak values from the raw data and the time points that these peaks exist at
    vals_peaks <- data[data$time >= cut_start & data$time <= cut_end, channel][peaks]
    
    time <- data$time[cut_start:cut_end][peaks == TRUE]
    
    peak_data <- data.frame(vals_peaks, time)
    
    #Plot the maximum of the peaks
    graphics::points(vals_peaks ~ time, pch = 16)
    
    #Quantile regression of median to deal with drift over time
    median_pred <- quantile_median(data[data$time >= cut_start & data$time <= cut_end,], channel  = "CO2", tau = tau)
    
    # Plot the median to ensure we can see that it is appropriate. Can change tau to offset median a bit.
    graphics::lines(median_pred ~ data[data$time >= cut_start & data$time <= cut_end, ]$time)
  }
  
  # Check whether the marker number and the identified number of peaks match before creating data frame
  if(length(peak_data$vals_peaks) != length(marker[marker$time >= cut_start & marker$time <= cut_end,]$text)){
    peak_data[peak_data$vals_peaks >= ((threshold_peak*median_pred[peak_data$time - cut_start])+median_pred[peak_data$time - cut_start]),]
  }
  
  # Return the data frame of change in percentage of gas in the air sample from baseline along with marker data.
  data_list <- list(peak_data = data.frame(
    channel = peak_data$vals_peaks, 
    time = peak_data$time, 
    change = (peak_data$vals_peaks - median_pred[peak_data$time - cut_start])), 
    marker_data = data.frame(
      marker = marker[marker$time >= cut_start & marker$time <= cut_end,]$text,
      marker_sample = marker[marker$time >= cut_start & marker$time <= cut_end,]$sample,
      marker_time = marker[marker$time >= cut_start & marker$time <= cut_end,]$time))
  
  class(data_list) <- "metabR"
  attr(data_list, "median") <- mean(median_pred)
  attr(data_list, "duration") <- max(data$time)
  return(data_list)
}

##My version of resp_data 
#c is not set to control or the median baseline
my_resp_data <- function(metabR_data){
  
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
      
      if(CO2Marker$marker[i] != "z"){
        get_peak_for_marker <- CO2_peaks_fix[dplyr::between(CO2_peaks_fix$time, CO2Marker$marker_time[i], CO2Marker$marker_time[i+1]), ]
        
        if(nrow(get_peak_for_marker) == 0){
          get_peak_for_marker[1,] <- NA
        }
        
        merge_marker <- cbind(CO2Marker[i,], get_peak_for_marker)
        data_new[i,colnames(merge_marker)] <- merge_marker
      }
      
      if(CO2Marker$marker[i] == "z"){
        merge_marker <- cbind(CO2Marker[i,], data.frame(channel = median, time = CO2Marker[i,"marker_time"], change = 0))
        data_new[i,colnames(merge_marker)] <- merge_marker
      }
      
      if(CO2Marker$marker[i] != "z" & i == nrow(CO2Marker)){
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


#' @title zoom_plot_simple
#' @param data The ExpeData (".exp" ??? Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.
#' @param channel The specific channel, or data column from `extract_data` that is relevant for plotting. 
#' @param marker The marker data that should be plotted.
#' @param cut_start Where you want to start the plotting from i.e. region you want to zoom in on
#' @param cut_end Where you want to end the plotting
#' @param ... Additional arguments passed to plot
#' @description Plots the relevant data as a function of time with the marker text plotted as well within a specified part of the trace
#' @author Daniel Noble ??? daniel.noble@anu.edu.au and Fonti Kar fonti.kar@gmail.com
#' @export

zoom_plot_simple <- function(data, channel, marker, cut_start, cut_end, ...){
  graphics::plot(data[,channel], ylim = c(min(data[,channel]) - 0.01, max(data[,channel]) + 0.01), type = "l", xlab = "Time", xlim = c(cut_start, cut_end))
  graphics::abline(v = (marker$sample), col = "black")
  graphics::text(marker$text, x = marker$sample - 5, y = max(data[,channel]) + 0.005)
}

#' @title plot_simple_y_adj
#' @param data The ExpeData (".exp" ??? Sable Systems) file with the spectrograph information for oxygen, carbon dioxide, water vapor flow rate etc.
#' @param channel The specific channel, or data column from `extract_data` that is relevant for plotting. 
#' @param marker The marker data that should be plotted.
#' @param cut_start Where you want to start the plotting from i.e. region you want to zoom in on
#' @param cut_end Where you want to end the plotting
#' @param ... Additional arguments passed to plot
#' @description Plots the relevant data as a function of time with the marker text plotted as well, adjusts y axes in case there is leakage and you want to truncate the y axis
#' @author Daniel Noble ??? daniel.noble@anu.edu.au and Fonti Kar fonti.kar@gmail.com
#' @export
plot_simple_y_adj <- function(data, channel, marker, y_lim_min, y_lim_max, ...){
  graphics::plot(data[,channel], ylim = c(y_lim_min - 0.01, y_lim_max + 0.01), type = "l", xlab = "Time")
  graphics::abline(v = (marker$sample), col = "black")
  graphics::text(marker$text, x = marker$sample - 5, y = y_lim_max + 0.005)
}
