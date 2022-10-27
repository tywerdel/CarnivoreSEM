rm(list=ls())
#####################################
#Load libraries
#####################################
library(lavaan)
library(lavaanPlot)
library(sp)
library(corrplot) 
library(semPlot)
#####################################

# Read in data
dat <- read.csv("SEMchao.csv")
head(dat)

#Site Covariates
RowcropPrp = dat$RowcropPrp
CRPPrp = dat$CRPPrp
RowcropTE = dat$RowcropTE
CRPTE = dat$CRPTE
SHDI = dat$SHDI
StreamsDist = dat$StreamsDist
StreamDensity = dat$StreamDensity
LoamyTablePct2k = dat$LoamyTablePct2k
Richness = dat$Chao2
DaysActive = dat$DaysActive3yr
Native = dat$Native
LagObs = dat$LagObs

RowcropPrp <- scale(RowcropPrp)
CRPPrp <- scale(CRPPrp)
RowcropTE <- scale(RowcropTE)
CRPTE = scale(CRPTE)
SHDI <- scale(SHDI)
StreamsDist <- scale(StreamsDist)
StreamDensity <- scale(StreamDensity)
LoamyTablePct2k <- scale(LoamyTablePct2k)
Richness <- scale(Richness)
DaysActive <- scale(DaysActive)
Native <- scale(Native)
LagObs <- scale(LagObs)

t.dat <- data.frame(RowcropPrp, CRPPrp, RowcropTE,CRPTE, SHDI, StreamsDist,
                    StreamDensity, LoamyTablePct2k, Richness, DaysActive,
                    Native, LagObs)
t.dat

#BEST MODEL

mod.1 <-'
    #declare latent variables
    
    Crop=~RowcropPrp+RowcropTE
    CRP=~CRPPrp+CRPTE
    Strm=~StreamDensity+StreamsDist
    
    #declare regressions
    
    CRP~ Strm
    Native~ CRP+Crop
    SHDI~ Native+CRP+Crop+LoamyTablePct2k+Strm
    Crop~ LoamyTablePct2k+CRP+Strm
    LagObs~ Crop+DaysActive+Native
    Richness~ Crop+Native+DaysActive+Strm+LagObs+CRP+SHDI
    
    #declare covariances
    
    RowcropTE ~~            SHDI
    RowcropTE ~~          CRPPrp
    RowcropTE ~~ LoamyTablePct2k
    RowcropTE ~~          Native
    RowcropTE ~~   StreamDensity
    CRPTE ~~            SHDI
    CRPTE ~~ LoamyTablePct2k
    CRPTE ~~   StreamDensity
    CRPTE ~~     StreamsDist 
    CRPTE ~~      DaysActive
    RowcropPrp ~~      DaysActive
    RowcropPrp ~~ LoamyTablePct2k
    RowcropPrp ~~       RowcropTE
    RowcropPrp ~~   StreamDensity
    RowcropPrp ~~          CRPPrp
    StreamDensity ~~ LoamyTablePct2k
    StreamDensity ~~      DaysActive
    StreamsDist ~~        Richness

'

mod.1.fit <- sem(mod.1, data=t.dat) 

summary(mod.1.fit, rsq=T, standardized=T, fit.measures=TRUE, modindices=F)

#see if additional pathways would improve model
mi <- modindices(mod.1.fit, sort. = T)
print(mi)
#look only at improvements to regressions
print(mi[mi$op == "~",])

#different plots for the model. I didn't like either, so made one in powerpoint
lavaanPlot(name = "plot",mod.1.fit , graph_options = list(overlap = "true", fontsize = "10"), labels = NULL, coefs = T, sig=.05)
semPaths(mod.1.fit, "std",layout="circle", title = FALSE, rotation = 3)

