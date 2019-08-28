# metabR
Loading, analysing and plotting oxygen, carbon dioxide and water vapour from FMS Sable Systems

# How to install and dependencies
```
	install.packages("devtools")
	library(devtools)

	devtools::install_github("daniel1noble/metabR", dependencies = "Depends", force = TRUE)
	library(metabR)
```

# How to use it

```
rm(list=ls())

# Read in sable files; uses function from SableBase
sableDat1 <- read_exp("./resp data/QuantGen_07-13-2019_1-2.exp")
sableDat2 <- read_exp("./resp data/QuantGen_07-23-2019_762-786.exp")

# Plot gas time series'
par(mfrow = c(2,1), mar = c(4,4,1,1))
   O2_1 <- plot_resp(sableDat1, channel = "O2", col = "blue", threshold_peak = 0.10, tau = 0.35)
  CO2_1 <- plot_resp(sableDat1, channel = "CO2", col = "red", threshold_peak = 0.50,  tau = 0.50)

# Extract formatted data
 data1_O2 <- resp_data(O2_1)
data1_CO2 <- resp_data(CO2_1)


```