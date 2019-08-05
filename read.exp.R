install.packages("remotes")
install.packages("ggpmisc")
library(quantreg)
library(devtools)
install_github("hawkmoth/sablebase", force = TRUE)
library(SableBase)
install.packages("remotes")
remotes::install_github("mnblonsky/REMI")
remotes::install_github("daniel1noble/metabR")

#############################################################################
# Test code

rm(list=ls())

test <- read.sscf(".filename.exp")
test <- read.sscf("./resp data/QuantGen_07-18-2019_142-166.exp")
test <- read.sscf("./resp data/QuantGen_07-23-2019_787-811.exp")


par(mfrow = c(2,1), mar = c(4,4,1,1))
   O2 <- plot_resp(test, channel = "O2", col = "blue", threshold_peak = 0.10, tau = 0.35)
  CO2 <- plot_resp(test, channel = "CO2", col = "red", threshold_peak = 0.50,  tau = 0.50)


tester1 <- resp_data(O2)
tester2 <- resp_data(CO2)


CO2Marker <- data.frame(CO2$marker_data)
 CO2peaks <- data.frame(CO2$peak_data)

 CO2peaks[CO2Marker, list(marker, marker_time , time, channel, change) , roll = "nearest" ]



data_new <- data.frame()

# First exclude values below the first marker. Cannot be possible
between_markers <- CO2peaks$time >= range(CO2Marker$marker_sample)[1] & CO2peaks$time <= 1600 # change 1600 to length of dataframe/ samples

CO2_peaks_fix <- CO2peaks[between_markers,]

CO2_peaks_fix

CO2Marker
i = 1

data_new <- data.frame()  
for(i in 1:nrow(CO2Marker)){
 
  if(CO2Marker$marker[i] != "c"){
      get_peak_for_marker <- CO2_peaks_fix[between(CO2_peaks_fix$time, CO2Marker$marker_time[i], CO2Marker$marker_time[i+1]), ]
      merge_marker <- cbind(CO2Marker[i,], get_peak_for_marker)
      data_new[i,colnames(merge_marker)] <- merge_marker
  }

   if(CO2Marker$marker[i] == "c"){
        merge_marker <- cbind(CO2Marker[i,], data.frame(channel = 0.05, time = CO2Marker[i,"marker_time"], change = 0))
        data_new[i,colnames(merge_marker)] <- merge_marker
   }
}






   if(CO2Marker$marker[i])

i = 1

for(i in 1:nrow(CO2_peaks_fix)){
  extratMarker_dat <- CO2Marker[CO2Marker$marker_time[i+1] < CO2_peaks_fix[i,]$time
}


for(i in 1:nrow(CO2peaks)){
      time1 <- CO2peaks$time[i]
      
      if(time1 < CO2Marker$marker_time[1]){
        time1 <- CO2peaks$time[-i]
      }

}


############################################################################

test[test$CO2 == vals_peaks,]
data = test
channel="CO2"
name = "O2"
data <- data.frame()

test[, which(name %in% attr(test, "dimnames")[[2]])]$O2_2 <- test[, which(name %in% attr(test, "dimnames")[[2]])]*(-1)


data$O2_2 <- data$O2*(-1)
plot(data$O2_2)




dat <- plot.resp(test, "CO2", col = "red")
plot.resp(test, c("O2","CO2"))

par(mfrow= c(1,2))
per = 0.03
hist(test$CO2, breaks = 100)
abline(v = median(test$CO2), col = "red")
abline(v = median(test$CO2)+per*median(test$CO2), lty = 2, col = "red")
abline(v = median(test$CO2)-per*median(test$CO2), lty = 2, col = "red")

plot.resp(test, "CO2", col = "red")
abline(h = median(test$CO2), col = "forestgreen")


## Do not use below. Use above and get the read.sscf
read.expdat <- function(filename) {
  f <- file(filename, "rb")

  header <- read.header(f)
  
  # data block
  if (header$nsamp > 0) { # normally terminated file
    data <- readBin(f, numeric(), n=header$nchannel*header$nsamp, size=4)
    data <- matrix(data, nrow=header$nsamp, ncol=header$nchannel, byrow=T)

  } else { # cut-off file
    warning("Error: Number of samples indicates a truncated file.")
    return (NULL)
  }

  # extended metadata block in file versions > 1.0
  if (header$version > 1) {
    footer <- read.footer(f, header)
  } else {
    footer <- NULL
  }

  #TODO: catch exceptions/errors 
  close(f) 

  ### post processing
  result <- embellish.data(data, header, footer)
  result <- build.metadata(result, header, footer)
  
  class(result) <- c("expdat", class(result))
  
  return(result)
}