install.packages("devtools")
library("devtools")
devtools::install_github("AnneChao/SpadeR")
library(SpadeR)

# Read in data
dat <- read.csv("ChaoMatrixSpecies.csv")
head(dat)

dat<-as.data.frame(dat)
dat
out1 = ChaoSpecies(dat,datatype = "incidence_raw", k = 10, conf= 0.95)
out1
