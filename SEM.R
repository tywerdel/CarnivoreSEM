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
#Load Data - CHANGE YOUR WORING DIRECTORY
#####################################
setwd("C:/Users/tywer/OneDrive/TyFiles/Dissertation/Chapter_4/SEM")

# Read in data
dat <- read.csv("SEMCovs.csv")
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
Richness = dat$Richness3yr
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
#Fitting an RSF
RSF.fit <- glm(Richness ~  RowcropPrp+ CRPPrp+ RowcropTE+ CRPTE+ SHDI+ StreamsDist+
               StreamDensity+ LoamyTablePct2k+ DaysActive+
               Native+ LagObs, data = t.dat)

RSF.fit <- glm(Richness~Native,data=t.dat)
#look at coefficients
summary(RSF.fit)$coef

#plot coefficients
plot_model(RSF.fit)

#BEST MODEL
cor(t.dat[-1],t.dat$RatRate,use="complete.obs")
cortdat<-cor(t.dat, use = "complete.obs")
write.csv(cortdat,file = "cortdat.csv")
#This is where I stopped
#Rich .287  SRMR .024  P .657
#CoyRate Indication of Prey Densities
###########RIGHT NOW THIS IS THE TOP MODEL#########
mod.1 <-'
    #declare latent variables
    #Con=~SHDI+TotalPatches
    Crop=~RowcropPrp+RowcropTE
    CRP=~CRPPrp+CRPTE
    #Nat=~Native
    #NatE=~SGPTE+MGPTE
    Strm=~StreamDensity+StreamsDist
    #Fallow=~FallowPrp+FallowTE
    
    CRP ~ Strm
    Native~CRP+Crop+LoamyTablePct2k
    
    SHDI~Native+CRP+Crop+LoamyTablePct2k+Strm
    
    Crop~LoamyTablePct2k+CRP+Strm
    LagObs  ~            Crop+DaysActive+Native
    Richness~Crop+Native+DaysActive+Strm+LagObs+CRP+SHDI
    
   
    
    
    
    #declare covariances
    RowcropTE ~~            SHDI
    CRPTE ~~            SHDI
    RowcropTE ~~          CRPPrp
    RowcropTE ~~ LoamyTablePct2k
    CRPTE ~~ LoamyTablePct2k
    RowcropTE ~~          Native 
    RowcropPrp ~~      DaysActive
    CRPTE ~~      DaysActive
    StreamDensity ~~ LoamyTablePct2k
    
    RowcropPrp ~~ LoamyTablePct2k
    CRPTE ~~   StreamDensity
    RowcropTE ~~   StreamDensity
    CRPTE ~~     StreamsDist
    StreamDensity ~~      DaysActive
'

#mean patch size for different covariates
#photo rates for coyotes
#R .245 SMER .111 Chi 1987
#Add Water 218 09 1868
#Add Coyote 336    079   1958
#p.dat <- data.frame(SHDI,TotalEdge,TotalPatches,Contag)
#cor(p.dat)

#cor(t.dat)
mod.1.fit <- sem(mod.1, data=t.dat) 

summary(mod.1.fit, rsq=T, standardized=T, fit.measures=TRUE, modindices=F)
mi <- modindices(mod.1.fit, sort. = T)
print(mi)
print(mi[mi$op == "~",])
lavaanPlot(name = "plot",mod.1.fit , graph_options = list(overlap = "true", fontsize = "10"), labels = NULL, coefs = T, sig=.05)
#lavaanPlot(model = mod.1.fit,  edge_options = list(color = "grey"), coefs = T, covs = F, stand=F)#, stars="latent", sig=.05)

?lavaanPlot

semPaths(mod.1.fit, "std",layout="circle", title = FALSE, rotation = 3)

?semPaths
mi <- modindices(mod.1.fit)
print(mi[mi$op == "~~",])
print(mi)
options(max.print=2000)











plot(mod.1.fit)

summary(mod.1.fit, rsq=T, standardized=T, fit.measures=TRUE)

t.dat <- data.frame(richness, sg, mg, forest, rowcrop, precip, doy, ldiv, edge, ss,
                    tg, crp, crpt, crps, wetland, pasture, shrubland, barren, dev,
                    fallow, grasses, forbs, shrubs, openg, crops, vegh, days, lat, cdiv)





mod.1 <- '
  #declare latent variables
    NativePraire =~ '

mod.1.fit <- sem(mod.1, data=t.dat, estimator="MLR")
summary(mod.1.fit, rsq=T, standardized=T, fit.measures=TRUE, modindices=TRUE)
summary(mod.1.fit)

fitMeasures(mod.1.fit, c("gfi", "agfi", "cfi", "rni"))

Native ~ LandRestore + NonNative + LandStruct + Climate + Det
LandRestore ~ Native + NonNative + LandStruct + Climate + Det
NonNative ~  Native + LandRestore + LandStruct + Climate + Det
LandStruct ~ Native + LandRestore + NonNative + Climate + Det
Climate ~ Native + LandRestore + NonNative + LandStruct + Det
Det ~ Native + LandRestore + NonNative + LandStruct + Climate 

mod.2 <- '
Native =~ sg
LandRestore =~ crp
NonNative =~ rowcrop + fallow
LandStruct =~ ldiv + edge
Climate =~ precip + lat
Det =~ days + vegh
CarnDiv =~ cdiv

LandRestore ~~ NonNative
ldiv ~~ edge

Native ~ LandStruct + LandRestore 
LandRestore ~ NonNative + Native
NonNative ~ LandStruct
LandStruct ~ LandRestore
CarnDiv ~ Native + LandRestore + NonNative + LandStruct + Climate + Det'

mod.2.fit <- sem(mod.2, data=t.dat, estimator="MLR")
summary(mod.2.fit)
fitMeasures(mod.2.fit, c("gfi", "agfi", "cfi", "rni"))
summary(mod.2.fit, rsq=T, standardized=T, fit.measures=TRUE,  modindices=TRUE)

mod.3 <- '
Native =~ sg
LandRestore =~ crp
LandStruct =~ ldiv + edge
CarnDiv =~ cdiv

Native ~ LandRestore
LandStruct ~ LandRestore
CarnDiv ~ Native + LandRestore + LandStruct'

mod.3.fit <- sem(mod.3, data=t.dat, estimator="MLR")
summary(mod.3.fit)
fitMeasures(mod.3.fit, c("gfi", "agfi", "cfi", "rni"))
summary(mod.3.fit, rsq=T, standardized=T, fit.measures=TRUE,  modindices=TRUE)

#model
lv.m1 <- '
HistLC =~ for.64.logit
Veg =~ basal + herb.sqrt + leaf.cm + can.s
Temp =~ temp.avg
Frag =~ dist.s
Abu =~ plci.log

Veg ~ post_ag
Temp ~ Veg + Frag
Frag ~ HistLC + post_ag
cov.s ~ Frag + post_ag
Abu ~ Veg + Temp + Frag +  cov.s + HistLC + post_ag
HistLC ~ post_ag
herb.sqrt ~~ cov.s
Temp ~~ cov.s

'
#fit
m1.est <- sem(lv.m1, data=Data, estimator="MLR")
summary(m1.est, rsq=T, standardized=T, fit.measures=TRUE)

