MRI <- read.csv("C:/Users/vindy/Google Drive/SHSU/Longitudinal Data Analysis/Project/Data.csv") 
View(MRI)
attach(MRI)
Nondemented <- subset(MRI, Group == 'Nondemented')
View(Nondemented)
aggregate(x = Nondemented$MMSE, by = list(MRI$Visit),FUN = mean)
table(mean(MMSE))
new1 <- subset(Nondemented,Visit==1)
new1
new4 <- subset(Nondemented, Visit==4)
mean(new4$MMSE)

MMSE.data <- rnorm(100,mean = mean(new3$MMSE), sd = sd(new3$MMSE))
MMSE.data
eTIV.data <- rnorm(100,mean = mean(new3$eTIV), sd = sd(new3$eTIV))
nwbv.data <- rnorm(100, mean=mean(new3$nWBV), sd = sd(new3$nWBV))
ASF.data <- rnorm(100, mean = mean(new3$ASF), sd = sd(new3$ASF))
cbind(eTIV.data,nwbv.data,ASF.data)


library(tidyverse)
MRI %>%
  group_by(Group) %>% 
  summarise(missing=sum(is.na(MMSE)))
is.data.frame(MRI)

MRI %>% group_by(Group,Visit) %>% 
  summarise(missing=sum(is.na(MMSE)))

Nondemented <- Nondemented %>%
  group_by(Visit) %>%
  mutate(CDR = ifelse(is.na(CDR), median(CDR, na.rm = TRUE), CDR))

Nondemented

Demented <- subset(MRI, Group == 'Demented')
View(Demented)
Demented <- Demented %>%
  group_by(Visit) %>%
  mutate(CDR = ifelse(is.na(CDR), median(CDR, na.rm = TRUE), CDR))

Demented <- Demented %>%
  group_by(Visit) %>%
  mutate(nWBV = ifelse(is.na(nWBV), mean(nWBV, na.rm = TRUE), nWBV))
Demented

Converted <- subset(MRI, Group == 'Converted')
View(Converted)
Converted <- Converted %>%
  group_by(Visit) %>%
  mutate(CDR = ifelse(is.na(CDR), median(CDR, na.rm = TRUE), CDR))

Converted <- Converted %>%
  group_by(Visit) %>%
  mutate(ASF = ifelse(is.na(ASF), mean(ASF, na.rm = TRUE), ASF))
Converted

MRI.data <- rbind(Nondemented, Demented, Converted)
MRI.data
is.data.frame(MRI.data)

MRI.data <- MRI.data[order(MRI.data[,1]),]
View(MRI.data)
MRI.data$nWBV <- round(MRI.data$nWBV,3)
MRI.data$eTIV <- round(MRI.data$eTIV,0)
MRI.data$MMSE <- round(MRI.data$MMSE,0)
View(MRI.data)
table(MRI.data$CDR)

MRI.data[,9] <- ifelse(MRI.data[,9] == 0, 0, ifelse(MRI.data[,9] == 0.5, 1, ifelse(MRI.data[,9] == 0.75, 2, ifelse(MRI.data[,9] == 1, 2, ifelse(MRI.data[,9] == 2,3,99))))
View(MRI.data)


write.csv(MRI.data,"C:/Users/vindy/Google Drive/SHSU/Longitudinal Data Analysis/Project/MRI_data.csv", row.names = TRUE)
