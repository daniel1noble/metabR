install.packages("remotes")
install.packages("ggpmisc")
library(quantmod)
library(devtools)
install_github("hawkmoth/sablebase", force = TRUE)
library(SableBase)

test <- read.sscf("./resp data/QuantGen_07-13-2019_1-2.exp")
test <- read.sscf("./resp data/QuantGen_07-23-2019_762-786.exp")

test[test$CO2 == vals_peaks,]
data = test
channel="CO2"

plot.resp(test, "O2", col = "blue")
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