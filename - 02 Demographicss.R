
table(fulldata$DX.orig)   
table(fulldata$PTGENDER, fulldata$DX) 
table(fulldata$PTGENDER)
barplot(table(fulldata$DX))
hist(fulldata$REALAGE, breaks=20)
#fulldata$cort <- rowMeans(fulldata[16:85], na.rm=TRUE)

fulldata$DX <- ifelse(fulldata$DX == "CN", 0,
                      ifelse(fulldata$DX == "MCI", 2,
                                1))

## mean CSF values per diagnosis

#sorted_data <- arrange(fulldata, DX)
fulldata <- group_by(fulldata, DX)
per_DX_tau <- summarise(fulldata, mean(TAU, na.rm = TRUE), sd(TAU, na.rm = TRUE), n()) 
per_DX_abeta <- summarise(fulldata, mean(ABETA), sd(ABETA, na.rm = TRUE), n()) 
per_DX_ptau <- summarise(fulldata, mean(PTAU, na.rm = TRUE), sd(PTAU, na.rm = TRUE), n()) 
per_DX_tau
per_DX_abeta
per_DX_ptau
per_DX_age <- summarise(fulldata, mean(REALAGE, na.rm = TRUE), sd(REALAGE, na.rm = TRUE), n()) 
per_DX_age
per_DX_ent <- summarise(fulldata, mean(av1451entorhinal, na.rm = TRUE), sd(av1451entorhinal, na.rm = TRUE), n()) 
per_DX_ent
per_DX_inf <- summarise(fulldata, mean(av1451inferiotemporal, na.rm = TRUE), sd(av1451inferiotemporal, na.rm = TRUE), n()) 
per_DX_inf
per_DX_tot <- summarise(fulldata, mean(av1451totalcort, na.rm = TRUE), sd(av1451totalcort, na.rm = TRUE), n()) 
per_DX_tot


## gender per diagnosis


fulldata <- group_by(fulldata, PTGENDER, DX)
dx_per_gender <- summarise(fulldata, Mean.Tau = mean(TAU, na.rm = TRUE)) 
dx_per_gender



## minimum, maximum CSF values per dx


fulldata <- group_by(fulldata, DX)
tau_summary <- summarise(fulldata, Mean.Tau = mean(TAU, na.rm = TRUE), 
                         Min.Tau = min(TAU, na.rm = TRUE), Max.Tau = max(TAU, na.rm = TRUE))
ptau_summary <- summarise(fulldata, Mean.PTau = mean(PTAU), 
                          Min.PTau = min(PTAU), Max.PTau = max(PTAU))
av_summary <- summarise(fulldata, Mean.Av = mean(ABETA), 
                        Min.Av = min(ABETA), Max.Av = max(ABETA))
tau_summary
ptau_summary
av_summary





##################################################

fulldata$DX <- ifelse(fulldata$DX == "0", 0,
                             1)

## mean CSF values per diagnosis

#sorted_data <- arrange(fulldata, DX)
fulldata <- group_by(fulldata, DX)
per_DX_tau <- summarise(fulldata, mean(TAU, na.rm = TRUE), sd(TAU, na.rm = TRUE), n()) 
per_DX_abeta <- summarise(fulldata, mean(ABETA), sd(ABETA, na.rm = TRUE), n()) 
per_DX_ptau <- summarise(fulldata, mean(PTAU, na.rm = TRUE), sd(PTAU, na.rm = TRUE), n()) 
per_DX_tau
per_DX_abeta
per_DX_ptau
per_DX_age <- summarise(fulldata, mean(REALAGE, na.rm = TRUE), sd(REALAGE, na.rm = TRUE), n()) 
per_DX_age
per_DX_ent <- summarise(fulldata, mean(av1451entorhinal, na.rm = TRUE), sd(av1451entorhinal, na.rm = TRUE), n()) 
per_DX_ent
per_DX_inf <- summarise(fulldata, mean(av1451inferiotemporal, na.rm = TRUE), sd(av1451inferiotemporal, na.rm = TRUE), n()) 
per_DX_inf
per_DX_tot <- summarise(fulldata, mean(av1451totalcort, na.rm = TRUE), sd(av1451totalcort, na.rm = TRUE), n()) 
per_DX_tot



## gender per diagnosis


fulldata <- group_by(fulldata, PTGENDER, DX)
dx_per_gender <- summarise(fulldata, Mean.Tau = mean(TAU, na.rm = TRUE)) 
dx_per_gender



## minimum, maximum CSF values per dx


fulldata <- group_by(fulldata, DX)
tau_summary <- summarise(fulldata, Mean.Tau = mean(TAU, na.rm = TRUE), 
                         Min.Tau = min(TAU, na.rm = TRUE), Max.Tau = max(TAU, na.rm = TRUE))
ptau_summary <- summarise(fulldata, Mean.PTau = mean(PTAU), 
                          Min.PTau = min(PTAU), Max.PTau = max(PTAU))
av_summary <- summarise(fulldata, Mean.Av = mean(ABETA), 
                        Min.Av = min(ABETA), Max.Av = max(ABETA))
tau_summary
ptau_summary
av_summary