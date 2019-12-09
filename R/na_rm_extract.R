#' @title na_rm_extract
#' @param metabR_data The processed data metabR data list from plot_resp
#' @description Removes NA peaks that were detected, caution in case if you forgot to put in marker
#' @author Daniel Noble ??? daniel.noble@anu.edu.au and Fonti Kar fonti.kar@gmail.com
#' @export

na_rm_extract <- function(data){
  complete_data <- list(peak_data = data$peak_data[-c(which(is.na(data$marker_data$marker))),],
                        marker_data = data$marker_data[-c(which(is.na(data$marker_data$marker))),])
  attr(complete_data, "class") <- attr(data, "class")
  attr(complete_data, "median") <- attr(data, "median")
  attr(complete_data, "duration") <- attr(data, "duration")
  
  return(complete_data)
}