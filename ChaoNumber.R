install.packages("devtools")
install.packages("purrr")
install.packages("fossil")
library("devtools")
library("purrr")
library("fossil")

# Read in data
dat <- read.csv("AllSitesAllSamples.csv")
dat<-as.data.frame(dat)
dat

for (i in 1:381) {

  #Pull out each species 84 sampling occasions #i is for each site #need column names to be the same for rbind
  bad <- dat[i,2:85]
  colnames(bad) <- c(1:84)
  bob <- dat[i,86:169]
  colnames(bob) <- c(1:84)
  coy <- dat[i,170:253]
  colnames(coy) <- c(1:84)
  ltw <- dat[i,254:337]
  colnames(ltw) <- c(1:84)
  rac <- dat[i,338:421]
  colnames(rac) <- c(1:84)
  sku <- dat[i,422:505]
  colnames(sku) <- c(1:84)
  swf <- dat[i,506:589]
  colnames(swf) <- c(1:84)

  #bind all species
  samp <- rbind(bad,bob,coy,ltw,rac,sku,swf)
  #remove sampling occasions where all NAs
  samp <- samp [ , colSums (is.na(samp))==0]
  
  
  #Create new dataframes for each site
  assign(paste0("Site", i), data.frame(samp))
   
}


#create list of dataframes, there is obviously a better way to do this, but I used excel to quickly separate by commas
samplist <- list(Site1,	Site2,	Site3,	Site4,	Site5,	Site6,	Site7,	Site8,	Site9,	Site10,	
     Site11,	Site12,	Site13,	Site14,	Site15,	Site16,	Site17,	Site18,	Site19,	Site20,	
     Site21,	Site22,	Site23,	Site24,	Site25,	Site26,	Site27,	Site28,	Site29,	Site30,	
     Site31,	Site32,	Site33,	Site34,	Site35,	Site36,	Site37,	Site38,	Site39,	Site40,	
     Site41,	Site42,	Site43,	Site44,	Site45,	Site46,	Site47,	Site48,	Site49,	Site50,	
     Site51,	Site52,	Site53,	Site54,	Site55,	Site56,	Site57,	Site58,	Site59,	Site60,	
     Site61,	Site62,	Site63,	Site64,	Site65,	Site66,	Site67,	Site68,	Site69,	Site70,	
     Site71,	Site72,	Site73,	Site74,	Site75,	Site76,	Site77,	Site78,	Site79,	Site80,	
     Site81,	Site82,	Site83,	Site84,	Site85,	Site86,	Site87,	Site88,	Site89,	Site90,	
     Site91,	Site92,	Site93,	Site94,	Site95,	Site96,	Site97,	Site98,	Site99,	Site100,	
     Site101,	Site102,	Site103,	Site104,	Site105,	Site106,	Site107,	Site108,	Site109,	Site110,	
     Site111,	Site112,	Site113,	Site114,	Site115,	Site116,	Site117,	Site118,	Site119,	Site120,
     Site121,	Site122,	Site123,	Site124,	Site125,	Site126,	Site127,	Site128,	Site129,	Site130,
     Site131,	Site132,	Site133,	Site134,	Site135,	Site136,	Site137,	Site138,	Site139,	Site140,
     Site141,	Site142,	Site143,	Site144,	Site145,	Site146,	Site147,	Site148,	Site149,	Site150,
     Site151,	Site152,	Site153,	Site154,	Site155,	Site156,	Site157,	Site158,	Site159,	Site160,
     Site161,	Site162,	Site163,	Site164,	Site165,	Site166,	Site167,	Site168,	Site169,	Site170,
     Site171,	Site172,	Site173,	Site174,	Site175,	Site176,	Site177,	Site178,	Site179,	Site180,
     Site181,	Site182,	Site183,	Site184,	Site185,	Site186,	Site187,	Site188,	Site189,	Site190,
     Site191,	Site192,	Site193,	Site194,	Site195,	Site196,	Site197,	Site198,	Site199,	Site200,
     Site201,	Site202,	Site203,	Site204,	Site205,	Site206,	Site207,	Site208,	Site209,	Site210,	
     Site211,	Site212,	Site213,	Site214,	Site215,	Site216,	Site217,	Site218,	Site219,	Site220,
     Site221,	Site222,	Site223,	Site224,	Site225,	Site226,	Site227,	Site228,	Site229,	Site230,
     Site231,	Site232,	Site233,	Site234,	Site235,	Site236,	Site237,	Site238,	Site239,	Site240,
     Site241,	Site242,	Site243,	Site244,	Site245,	Site246,	Site247,	Site248,	Site249,	Site250,	
     Site251,	Site252,	Site253,	Site254,	Site255,	Site256,	Site257,	Site258,	Site259,	Site260,
     Site261,	Site262,	Site263,	Site264,	Site265,	Site266,	Site267,	Site268,	Site269,	Site270,	
     Site271,	Site272,	Site273,	Site274,	Site275,	Site276,	Site277,	Site278,	Site279,	Site280,	
     Site281,	Site282,	Site283,	Site284,	Site285,	Site286,	Site287,	Site288,	Site289,	Site290,
     Site291,	Site292,	Site293,	Site294,	Site295,	Site296,	Site297,	Site298,	Site299,	Site300,
     Site301,	Site302,	Site303,	Site304,	Site305,	Site306,	Site307,	Site308,	Site309,	Site310,
     Site311,	Site312,	Site313,	Site314,	Site315,	Site316,	Site317,	Site318,	Site319,	Site320,
     Site321,	Site322,	Site323,	Site324,	Site325,	Site326,	Site327,	Site328,	Site329,	Site330,
     Site331,	Site332,	Site333,	Site334,	Site335,	Site336,	Site337,	Site338,	Site339,	Site340,	
     Site341,	Site342,	Site343,	Site344,	Site345,	Site346,	Site347,	Site348,	Site349,	Site350,	
     Site351,	Site352,	Site353,	Site354,	Site355,	Site356,	Site357,	Site358,	Site359,	Site360,
     Site361,	Site362,	Site363,	Site364,	Site365,	Site366,	Site367,	Site368,	Site369,	Site370,
     Site371,	Site372,	Site373,	Site374,	Site375,	Site376,	Site377,	Site378,	Site379,	Site380,
     Site381)

#use purrr package to iterate through all estimates of chao2
#this needs to be cleared before each run so you have an empty data frame
chao2bc = data.frame()
chao2se = data.frame()
chao2lo = data.frame()
chao2up = data.frame()

for (i in 1:381) {
  
  tryCatch({
  
  esite <- as.data.frame(samplist[i])
  
  out1 =tryCatch(ChaoSpecies(esite,datatype = "incidence_raw", k = 10, conf= 0.95))
  
 
  print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  #create list of chao2bc estimates [3,1], se [3,2], 95 lower [3,3], 95 upper [3,4]
  chao2bc = rbind(chao2bc,out1$Species_table[3,1])
  chao2se = rbind(chao2se,out1$Species_table[3,2])
  chao2lo = rbind(chao2lo,out1$Species_table[3,3])
  chao2up = rbind(chao2up,out1$Species_table[3,4])
}

result <-cbind(chao2bc,chao2se,chao2lo,chao2up)
colnames(result) <- c("chao2","chao2se","chao2lo","chao2up")

result
write.csv(result, "chao2bc_results.csv")

####Run chao estimates with different package (No Errors)

chao2fossil = data.frame()
for (i in 1:381) {
    
    esite <- as.data.frame(samplist[i])
    
    out1 = chao2(esite)
  
  #create list of chao2 estimates
    chao2fossil = rbind(chao2fossil,out1)
}

write.csv(chao2fossil, "chao2_fossil_results.csv")
