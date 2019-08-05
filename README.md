# metabR
Loading, analysing and plotting oxygen, carbon dioxide and water vapour from FMS Sable Systems

# How to install and dependencies
```
	install.packages("devtools")
	library(devtools)

	install_github("hawkmoth/sablebase", force = TRUE)
	library(SableBase)

	install_github("daniel1noble/metabR")
	library(metabR)
```

# How to use it

```
rm(list=ls())

sableDat1 <- read.sscf("./resp data/QuantGen_07-13-2019_1-2.exp")
sableDat2 <- read.sscf("./resp data/QuantGen_07-23-2019_762-786.exp")

par(mfrow = c(2,1), mar = c(4,4,1,1))
   O2_1 <- plot_resp(sableDat1, channel = "O2", col = "blue", threshold_peak = 0.10, tau = 0.35)
  CO2_1 <- plot_resp(sableDat1, channel = "CO2", col = "red", threshold_peak = 0.50,  tau = 0.50)

 data1_O2 <- resp_data(O2_1)
data1_CO2 <- resp_data(CO2_1)

par(mfrow = c(2,1), mar = c(4,4,1,1))
   O2_2 <- plot_resp(sableDat2, channel = "O2", col = "blue", threshold_peak = 0.10, tau = 0.35)
  CO2_2 <- plot_resp(sableDat2, channel = "CO2", col = "red", threshold_peak = 0.50,  tau = 0.50)

data2_O2 <- resp_data(O2_2)
data2_CO2 <- resp_data(CO2_2)

```