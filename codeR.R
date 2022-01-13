library(ggplot2)
library(FactoMineR)

red_wine <- read.csv("wineQualityReds.csv",header=TRUE,sep=",",dec=",")
white_wine <- read.csv("wineQualityWhites.csv", header=TRUE, sep=",", dec=",")


Xmax = max(red_wine$X)
white_wine$X = white_wine$X + Xmax

red_wine$color = "red"
white_wine$color = "white"

all_wine = rbind(red_wine, white_wine)

