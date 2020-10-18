library(dplyr)

data1451 <- read.csv("C:/Users/Gebruiker/Desktop/Fax/thesiss/adni delimited/UCBERKELEYAV1451_09_05_18.csv")
datacsf <- read.csv("C:/Users/Gebruiker/Desktop/Fax/thesiss/adni delimited/UPENNBIOMK_MASTER.csv")
dataadni <- read.csv("C:/Users/Gebruiker/Desktop/Fax/thesiss/adni delimited/adnimergee.csv")
adnimerge <- read.csv("C:/Users/Gebruiker/Desktop/Fax/thesiss/adni delimited/ADNIMERGE.csv")

data1451$FRONTAL <- (data1451$CTX_LH_CAUDALMIDDLEFRONTAL_SUVR + 
                       data1451$CTX_LH_LATERALORBITOFRONTAL_SUVR + 
                       data1451$CTX_LH_MEDIALORBITOFRONTAL_SUVR + 
                       data1451$CTX_LH_PARSOPERCULARIS_SUVR +
                       data1451$CTX_LH_PARSORBITALIS_SUVR + 
                       data1451$CTX_LH_PARSTRIANGULARIS_SUVR + 
                       data1451$CTX_LH_ROSTRALMIDDLEFRONTAL_SUVR + 
                       data1451$CTX_LH_SUPERIORFRONTAL_SUVR + 
                       data1451$CTX_LH_FRONTALPOLE_SUVR +
                       data1451$CTX_RH_CAUDALMIDDLEFRONTAL_SUVR + 
                       data1451$CTX_RH_LATERALORBITOFRONTAL_SUVR + 
                       data1451$CTX_RH_MEDIALORBITOFRONTAL_SUVR + 
                       data1451$CTX_RH_PARSOPERCULARIS_SUVR +
                       data1451$CTX_RH_PARSORBITALIS_SUVR + 
                       data1451$CTX_RH_PARSTRIANGULARIS_SUVR + 
                       data1451$CTX_RH_ROSTRALMIDDLEFRONTAL_SUVR + 
                       data1451$CTX_RH_SUPERIORFRONTAL_SUVR + 
                       data1451$CTX_RH_FRONTALPOLE_SUVR) / 18


data1451$CINGULATE <- (data1451$CTX_LH_CAUDALANTERIORCINGULATE_SUVR + 
                         data1451$CTX_LH_ISTHMUSCINGULATE_SUVR + 
                         data1451$CTX_LH_POSTERIORCINGULATE_SUVR + 
                         data1451$CTX_LH_ROSTRALANTERIORCINGULATE_SUVR +
                         data1451$CTX_RH_CAUDALANTERIORCINGULATE_SUVR + 
                         data1451$CTX_RH_ISTHMUSCINGULATE_SUVR + 
                         data1451$CTX_RH_POSTERIORCINGULATE_SUVR + 
                         data1451$CTX_RH_ROSTRALANTERIORCINGULATE_SUVR) / 8


data1451$PARIETAL <- (data1451$CTX_LH_INFERIORPARIETAL_SUVR + 
                        data1451$CTX_LH_PRECUNEUS_SUVR + 
                        data1451$CTX_LH_SUPERIORPARIETAL_SUVR + 
                        data1451$CTX_LH_SUPRAMARGINAL_SUVR +
                        data1451$CTX_RH_INFERIORPARIETAL_SUVR + 
                        data1451$CTX_RH_PRECUNEUS_SUVR + 
                        data1451$CTX_RH_SUPERIORPARIETAL_SUVR + 
                        data1451$CTX_RH_SUPRAMARGINAL_SUVR) / 8


data1451$TEMPORAL <- (data1451$CTX_LH_MIDDLETEMPORAL_SUVR + 
                        data1451$CTX_LH_SUPERIORTEMPORAL_SUVR + 
                        data1451$CTX_RH_MIDDLETEMPORAL_SUVR + 
                        data1451$CTX_RH_SUPERIORTEMPORAL_SUVR) / 4


data1451$av1451totalcort <- (data1451$FRONTAL/data1451$CEREBELLUMGREYMATTER_SUVR + 
                               data1451$CINGULATE /data1451$CEREBELLUMGREYMATTER_SUVR +
                               data1451$PARIETAL /data1451$CEREBELLUMGREYMATTER_SUVR +
                               data1451$TEMPORAL / data1451$CEREBELLUMGREYMATTER_SUVR)/4



data1451$FRONTAL <- (data1451$CTX_LH_CAUDALMIDDLEFRONTAL_SUVR * data1451$CTX_LH_CAUDALMIDDLEFRONTAL_VOLUME + 
                       data1451$CTX_LH_LATERALORBITOFRONTAL_SUVR*data1451$CTX_LH_LATERALORBITOFRONTAL_VOLUME + 
                       data1451$CTX_LH_MEDIALORBITOFRONTAL_SUVR*data1451$CTX_LH_MEDIALORBITOFRONTAL_VOLUME + 
                       data1451$CTX_LH_PARSOPERCULARIS_SUVR*data1451$CTX_LH_PARSOPERCULARIS_VOLUME +
                       data1451$CTX_LH_PARSORBITALIS_SUVR*data1451$CTX_LH_PARSORBITALIS_VOLUME + 
                       data1451$CTX_LH_PARSTRIANGULARIS_SUVR*data1451$CTX_LH_PARSTRIANGULARIS_VOLUME + 
                       data1451$CTX_LH_ROSTRALMIDDLEFRONTAL_SUVR*data1451$CTX_LH_ROSTRALMIDDLEFRONTAL_VOLUME + 
                       data1451$CTX_LH_SUPERIORFRONTAL_SUVR *data1451$CTX_LH_SUPERIORFRONTAL_VOLUME + 
                       data1451$CTX_LH_FRONTALPOLE_SUVR*data1451$CTX_LH_FRONTALPOLE_VOLUME +
                       data1451$CTX_RH_CAUDALMIDDLEFRONTAL_SUVR * data1451$CTX_RH_CAUDALMIDDLEFRONTAL_VOLUME + 
                       data1451$CTX_RH_LATERALORBITOFRONTAL_SUVR*data1451$CTX_RH_LATERALORBITOFRONTAL_VOLUME + 
                       data1451$CTX_RH_MEDIALORBITOFRONTAL_SUVR*data1451$CTX_RH_MEDIALORBITOFRONTAL_VOLUME + 
                       data1451$CTX_RH_PARSOPERCULARIS_SUVR*data1451$CTX_RH_PARSOPERCULARIS_VOLUME +
                       data1451$CTX_RH_PARSORBITALIS_SUVR*data1451$CTX_RH_PARSORBITALIS_VOLUME + 
                       data1451$CTX_RH_PARSTRIANGULARIS_SUVR*data1451$CTX_RH_PARSTRIANGULARIS_VOLUME + 
                       data1451$CTX_RH_ROSTRALMIDDLEFRONTAL_SUVR*data1451$CTX_RH_ROSTRALMIDDLEFRONTAL_VOLUME + 
                       data1451$CTX_RH_SUPERIORFRONTAL_SUVR *data1451$CTX_RH_SUPERIORFRONTAL_VOLUME + 
                       data1451$CTX_RH_FRONTALPOLE_SUVR*data1451$CTX_RH_FRONTALPOLE_VOLUME) /
  (data1451$CTX_LH_CAUDALMIDDLEFRONTAL_VOLUME + data1451$CTX_LH_LATERALORBITOFRONTAL_VOLUME +
     data1451$CTX_LH_MEDIALORBITOFRONTAL_VOLUME +
     data1451$CTX_LH_PARSOPERCULARIS_VOLUME + data1451$CTX_LH_PARSORBITALIS_VOLUME +
     data1451$CTX_LH_PARSTRIANGULARIS_VOLUME + data1451$CTX_LH_ROSTRALMIDDLEFRONTAL_VOLUME +
     data1451$CTX_LH_SUPERIORFRONTAL_VOLUME + data1451$CTX_LH_FRONTALPOLE_VOLUME +
     data1451$CTX_RH_CAUDALMIDDLEFRONTAL_VOLUME + data1451$CTX_RH_LATERALORBITOFRONTAL_VOLUME +
     data1451$CTX_RH_MEDIALORBITOFRONTAL_VOLUME +
     data1451$CTX_RH_PARSOPERCULARIS_VOLUME + data1451$CTX_RH_PARSORBITALIS_VOLUME +
     data1451$CTX_RH_PARSTRIANGULARIS_VOLUME + data1451$CTX_RH_ROSTRALMIDDLEFRONTAL_VOLUME +
     data1451$CTX_RH_SUPERIORFRONTAL_VOLUME + data1451$CTX_RH_FRONTALPOLE_VOLUME)


data1451$CINGULATE <- (data1451$CTX_LH_CAUDALANTERIORCINGULATE_SUVR * data1451$CTX_LH_CAUDALANTERIORCINGULATE_VOLUME + 
                         data1451$CTX_LH_ISTHMUSCINGULATE_SUVR*data1451$CTX_LH_ISTHMUSCINGULATE_VOLUME + 
                         data1451$CTX_LH_POSTERIORCINGULATE_SUVR*data1451$CTX_LH_POSTERIORCINGULATE_VOLUME + 
                         data1451$CTX_LH_ROSTRALANTERIORCINGULATE_SUVR*data1451$CTX_LH_ROSTRALANTERIORCINGULATE_VOLUME +
                         data1451$CTX_RH_CAUDALANTERIORCINGULATE_SUVR * data1451$CTX_RH_CAUDALANTERIORCINGULATE_VOLUME + 
                         data1451$CTX_RH_ISTHMUSCINGULATE_SUVR*data1451$CTX_RH_ISTHMUSCINGULATE_VOLUME + 
                         data1451$CTX_RH_POSTERIORCINGULATE_SUVR*data1451$CTX_RH_POSTERIORCINGULATE_VOLUME + 
                         data1451$CTX_RH_ROSTRALANTERIORCINGULATE_SUVR*data1451$CTX_RH_ROSTRALANTERIORCINGULATE_VOLUME) /
  (data1451$CTX_LH_CAUDALANTERIORCINGULATE_VOLUME + data1451$CTX_LH_ISTHMUSCINGULATE_VOLUME +
     data1451$CTX_LH_POSTERIORCINGULATE_VOLUME +
     data1451$CTX_LH_ROSTRALANTERIORCINGULATE_VOLUME + data1451$CTX_RH_CAUDALANTERIORCINGULATE_VOLUME +
     data1451$CTX_RH_ISTHMUSCINGULATE_VOLUME + data1451$CTX_RH_POSTERIORCINGULATE_VOLUME +
     data1451$CTX_RH_ROSTRALANTERIORCINGULATE_VOLUME)





data1451$PARIETAL <- (data1451$CTX_LH_INFERIORPARIETAL_SUVR * data1451$CTX_LH_INFERIORPARIETAL_VOLUME + 
                        data1451$CTX_LH_PRECUNEUS_SUVR*data1451$CTX_LH_PRECUNEUS_VOLUME + 
                        data1451$CTX_LH_SUPERIORPARIETAL_SUVR*data1451$CTX_LH_SUPERIORPARIETAL_VOLUME + 
                        data1451$CTX_LH_SUPRAMARGINAL_SUVR*data1451$CTX_LH_SUPRAMARGINAL_VOLUME +
                        data1451$CTX_RH_INFERIORPARIETAL_SUVR * data1451$CTX_RH_INFERIORPARIETAL_VOLUME + 
                        data1451$CTX_RH_PRECUNEUS_SUVR*data1451$CTX_RH_PRECUNEUS_VOLUME + 
                        data1451$CTX_RH_SUPERIORPARIETAL_SUVR*data1451$CTX_RH_SUPERIORPARIETAL_VOLUME + 
                        data1451$CTX_RH_SUPRAMARGINAL_SUVR*data1451$CTX_RH_SUPRAMARGINAL_VOLUME) /
  (data1451$CTX_LH_INFERIORPARIETAL_VOLUME + data1451$CTX_LH_PRECUNEUS_VOLUME +
     data1451$CTX_LH_SUPERIORPARIETAL_VOLUME +
     data1451$CTX_LH_SUPRAMARGINAL_VOLUME + data1451$CTX_RH_INFERIORPARIETAL_VOLUME +
     data1451$CTX_RH_PRECUNEUS_VOLUME + data1451$CTX_RH_SUPERIORPARIETAL_VOLUME +
     data1451$CTX_RH_SUPRAMARGINAL_VOLUME)

data1451$TEMPORAL <- (data1451$CTX_LH_MIDDLETEMPORAL_SUVR * data1451$CTX_LH_MIDDLETEMPORAL_VOLUME + 
                        data1451$CTX_LH_SUPERIORTEMPORAL_SUVR*data1451$CTX_LH_SUPERIORTEMPORAL_VOLUME + 
                        data1451$CTX_RH_MIDDLETEMPORAL_SUVR*data1451$CTX_RH_MIDDLETEMPORAL_VOLUME + 
                        data1451$CTX_RH_SUPERIORTEMPORAL_SUVR*data1451$CTX_RH_SUPERIORTEMPORAL_VOLUME) /
  (data1451$CTX_LH_MIDDLETEMPORAL_VOLUME + data1451$CTX_LH_SUPERIORTEMPORAL_VOLUME +
     data1451$CTX_RH_MIDDLETEMPORAL_VOLUME + data1451$CTX_RH_SUPERIORTEMPORAL_VOLUME)


data1451$av1451totalcort <- (data1451$FRONTAL/data1451$CEREBELLUMGREYMATTER_SUVR + 
                               data1451$CINGULATE /data1451$CEREBELLUMGREYMATTER_SUVR +
                               data1451$PARIETAL /data1451$CEREBELLUMGREYMATTER_SUVR +
                               data1451$TEMPORAL / data1451$CEREBELLUMGREYMATTER_SUVR)/4

adnimerge$x <- paste(adnimerge$RID, "-", adnimerge$VISCODE)
dataadni$x <- paste(dataadni$RID, "-", dataadni$viscodedx)
datacsf$x <- paste(datacsf$RID, "-", datacsf$VISCODE)
data1451$x <- paste(data1451$RID, "-", data1451$VISCODE2)

dataadni$viscodedx <- as.character(dataadni$viscodedx)
datacsf$VISCODE <- as.character(datacsf$VISCODE)

dataadni$viscodedx[dataadni$viscodedx == "bl"] <- "m0"
datacsf$VISCODE[datacsf$VISCODE == "bl"] <- "m0"
dataadni <- dataadni[, -(1)]

datacsf <- datacsf[, -(11:14)]
datacsf <- datacsf[, -(4:5)]

CSF <- merge(dataadni, datacsf, by=c("RID", "x"))
CSF <- CSF[grepl("MEDIAN", CSF$BATCH) , ]
duplicated(CSF$RID)

tauPETCSF <- merge(CSF, data1451, by=c("RID"))
tauPETCSF$ID <- seq.int(nrow(tauPETCSF))
duplicated(tauPETCSF$RID)
n_occur <- data.frame(table(tauPETCSF$RID))
more <- n_occur[n_occur$Freq > 1,]    
stupid_duplicate <- (tauPETCSF$RID %in% more$Var1)
dfReduced <- tauPETCSF[stupid_duplicate,]
myvars <- c("RID", "x.x", "x.y", "ID", "VISCODE.x", "VISCODE2")
newdata <- dfReduced[myvars]
library(stringr)
newdata$VISCODEcsf <- str_sub(newdata$VISCODE.x, 2)
newdata$VISCODEcsf <- as.numeric(newdata$VISCODEcsf)
newdata$VISCODEpet <- str_sub(newdata$VISCODE2, 2)
newdata$VISCODEpet <- as.numeric(newdata$VISCODEpet)
newdata$diff <- (newdata$VISCODEpet - newdata$VISCODEcsf)
aa <- newdata[order(newdata$RID, abs(newdata$diff) ), ] #sort by id and reverse of abs(value)
aa <- aa[ !duplicated(aa$RID), ]
stupid_duplicate <- (tauPETCSF$x.x %in% aa$x.x)
dfReduced2 <- tauPETCSF[stupid_duplicate,]
subtracted <- tauPETCSF[ !(tauPETCSF$RID %in% dfReduced2$RID), ]
tauPETCSF <- rbind(subtracted, dfReduced2)
duplicated(tauPETCSF$RID)
stupid_duplicate <- (tauPETCSF$x.y %in% aa$x.y)
dfReduced2 <- tauPETCSF[stupid_duplicate,]
subtracted <- tauPETCSF[ !(tauPETCSF$RID %in% dfReduced2$RID), ]
tauPETCSF <- rbind(subtracted, dfReduced2)
duplicated(tauPETCSF$RID)
tauPETCSF <- tauPETCSF[complete.cases(tauPETCSF$yob), ]
duplicated(tauPETCSF$RID)

table(tauPETCSF$DX)

adnimerge <- adnimerge[, -(2)]
adnimerge <- adnimerge[, -(3:19)]
adnimerge <- adnimerge[, -(5:33)]
adnimerge <- adnimerge[, -(6:47)]
adnimerge$VISCODE <- as.character(adnimerge$VISCODE)
adnimerge$VISCODE[adnimerge$VISCODE == "bl"] <- "m0"

colnames(adnimerge)[which(names(adnimerge) == "VISCODE")] <- "VISCODEadas13"

fulldata <- merge(tauPETCSF, adnimerge, by=c("RID"))
fulldata <- fulldata[complete.cases(fulldata$ADAS13), ]

fulldata$ID <- seq.int(nrow(fulldata))
duplicated(fulldata$RID)
n_occur <- data.frame(table(fulldata$RID))
more <- n_occur[n_occur$Freq > 1,]    
stupid_duplicate <- (fulldata$RID %in% more$Var1)
dfReduced <- fulldata[stupid_duplicate,]
myvars <- c("RID","ID", "x", "x.y", "VISCODEadas13", "VISCODE2")
newdata <- dfReduced[myvars]
library(stringr)
newdata$VISCODEadas <- str_sub(newdata$VISCODEadas13, 2)
newdata$VISCODEadas <- as.numeric(newdata$VISCODEadas)
newdata$VISCODEpet <- str_sub(newdata$VISCODE2, 2)
newdata$VISCODEpet <- as.numeric(newdata$VISCODEpet)
newdata$diff <- (newdata$VISCODEpet - newdata$VISCODEadas)
aa <- newdata[order(newdata$RID, abs(newdata$diff) ), ] #sort by id and reverse of abs(value)
aa <- aa[ !duplicated(aa$RID), ]
stupid_duplicate <- (fulldata$x %in% aa$x)
dfReduced2 <- fulldata[stupid_duplicate,]
subtracted <- fulldata[ !(fulldata$RID %in% dfReduced2$RID), ]
fulldata <- rbind(subtracted, dfReduced2)
duplicated(fulldata$RID)
stupid_duplicate <- (fulldata$x.y %in% aa$x.y)
dfReduced2 <- fulldata[stupid_duplicate,]
subtracted <- fulldata[ !(fulldata$RID %in% dfReduced2$RID), ]
fulldata <- rbind(subtracted, dfReduced2)
duplicated(fulldata$RID)
fulldata <- fulldata[complete.cases(fulldata$yob), ]
duplicated(fulldata$RID)

colnames(fulldata)[which(names(fulldata) == "VISCODE.x")] <- "VISCODEcsf"
colnames(fulldata)[which(names(fulldata) == "VISCODE2")] <- "VISCODEpet"

fulldata$VISCODEcsf1 <- str_sub(fulldata$VISCODEcsf, 2)
fulldata$VISCODEcsf1 <- as.numeric(fulldata$VISCODEcsf1)
fulldata$VISCODEpet1 <- str_sub(fulldata$VISCODEpet, 2)
fulldata$VISCODEpet1 <- as.numeric(fulldata$VISCODEpet1)
fulldata$delaycsfpet <- (fulldata$VISCODEpet1 - fulldata$VISCODEcsf1)


fulldata <- fulldata[, -(267:269)]
fulldata <- fulldata[, -(261:262)]
fulldata <- fulldata[, -(255)]
fulldata <- fulldata[, -(16)]
fulldata <- fulldata[, -(12)]
fulldata <- fulldata[, -(2)]

colnames(fulldata)[which(names(fulldata) == "DX.x")] <- "DX"
colnames(fulldata)[which(names(fulldata) == "DX.y")] <- "DX.orig"

fulldata$TAU[12] = 78.5 # RID 498
fulldata$TAU[30] = 115 # RID 2133
 # 4119 4817 4952 removed
fulldata <- fulldata[complete.cases(fulldata$TAU),]


which(is.na(fulldata$REALAGE)) # 58 153 182 201 216 228

library(lubridate)
fulldata$delay=interval(ymd(fulldata$DRWDTE),ymd(fulldata$EXAMDATE))
fulldata$delay = fulldata$delay %/% months(1)
print(fulldata$delay)
fulldata$REALAGE <- (fulldata$delay/12) + fulldata$AGE

# CUt-off
fulldata <- fulldata[with(fulldata, !(DX == 'MCI' & ABETA > 192)),]
fulldata <- fulldata[complete.cases(fulldata[,46:251]),]


write.csv(fulldata, file = "fulldata.csv")
