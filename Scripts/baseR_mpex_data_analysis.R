
file_loc <- "C:/Users/User/Documents/OpRiskPHDGitHub/OpRisk_PHD_Dissertate/OpRisk_PHD_Dissertation"
setwd(file_loc)
list.files(file_loc)

frequency <- openxlsx::read.xlsx("Raw_Formatted_Data.xlsx", check.names = TRUE, sheet = "Frequency")
severity <- openxlsx::read.xlsx("Raw_Formatted_Data.xlsx", check.names = TRUE, sheet = "Severity")
projdata <- openxlsx::read.xlsx("OPriskDataSet_exposure.xlsx", check.names = TRUE, sheet = "CleanedData")

#___________________________________________________________________________________________________
### Contents of Raw Formatted Data columns 
str(frequency)
str(severity)
str(projdata)

### Number of unique trade entries in contents of Raw Formatted Data
length(unique(frequency$Related.Trade))
length(unique(severity$Trd.Nbr))

### Number of intersecting trades in the frequency (from amendment tracker) and severity (from MOPL attribution summary) data sets from Raw Formatted Data file
length(intersect(frequency$Related.Trade, severity$Trd.Nbr))

#___________________________________________________________________________________________________
### Column labels in Raw Formatted Data (frequency & severity) and Cleaned Data
names(frequency)
names(severity)
dput(names(projdata))

### Renaming column entries in Cleaned Data  
names(projdata)[names(projdata) %in% "EventTypeCategoryLevel1"] <- "EventTypeCategoryLevel"
names(projdata)[names(projdata) %in% "BusinessLineLevel1"] <- "BusinessLineLevel"
names(projdata) <- sub("\\.", "", names(projdata))
dput(names(projdata))

projdata[] <- lapply(projdata, function(x) if (is.character(x)) toupper(trimws(x)) else x)

#___________________________________________________________________________________________________
# Exploratory data analysis for Update Time
### summary statistics
summary(projdata$UpdatedTime)
### Histograms
par(mfrow=c(1,3)) 
### ALL Losses
hist(projdata$UpdatedTime, col = "blue", main = "All losses", xlab = "Update Time", ylab = "Frequency")
### Near Misses/Pending Losses
hist(projdata$UpdatedTime[projdata$LossIndicator == 0], col = "red", main = "Near Misses", xlab = "Update Time", ylab = "Frequency")
### Realised losses
hist(projdata$UpdatedTime[projdata$LossIndicator == 1], col = "green", main = "Realised losses", xlab = "Update Time", ylab = "Frequency")
par(mfrow=c(1,1))
### 
plot(projdata$UpdatedTime, log(projdata$Loss+0.000000001), ylim = c(6, 18), col = "navy", xlab = "Updated Time", ylab = "Log. Loss")
do.call("rbind", lapply(split(projdata$Loss, projdata$UpdatedTime), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

#___________________________________________________________________________________________________
# Exploratory data analysis for Trade Time
### summary statistics
summary(projdata$TradedTime)
### Histograms 
par(mfrow=c(1,3)) 
### ALL Losses
hist(projdata$TradedTime, col = "navy", main = "All losses", xlab = "Trade Time", ylab = "Frequency")
### Near Misses/Pending Losses
hist(projdata$TradedTime[projdata$LossIndicator == 0], col = "red", main = "Near Misses", xlab = "Trade Time", ylab = "Frequency")
### Realised losses
hist(projdata$TradedTime[projdata$LossIndicator == 1], col = "green", main = "Realised losses", xlab = "Trade Time", ylab = "Frequency")
par(mfrow=c(1,1))

### 
plot(projdata$TradedTime, log(projdata$Loss+0.000000001), ylim = c(6, 18), col = "navy", xlab = "Traded Time", ylab = "Log. Loss")
do.call("rbind", lapply(split(projdata$Loss, projdata$TradedTime), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))
#___________________________________________________________________________________________________
# Update Day
summary(projdata$UpdatedDay)
# LossIndicator
par(mfrow=c(1,3)) 
hist(projdata$UpdatedDay, col = "#9999CC", main = "All losses", xlab = "Updated Day", ylab = "Frequency")
hist(projdata$UpdatedDay[projdata$LossIndicator == 0], col = "#CC6666", main = "Near Misses", xlab = "Updated Day", ylab = "Frequency")
hist(projdata$UpdatedDay[projdata$LossIndicator == 1], col = "#66CC99", main = "Realised losses", xlab = "Updated Day", ylab = "Frequency")
par(mfrow=c(1,1))
# Losses
plot(projdata$UpdatedDay, log(projdata$Loss+0.000000001), ylim = c(0, 20), col = "navy", xlab = "Updated Day", ylab = "Log. Loss")
do.call("rbind", lapply(split(projdata$Loss, projdata$UpdatedDay), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

#___________________________________________________________________________________________________
# Traded Day
data.frame(table(projdata$TradedDay)) 
hist(projdata$TradedDay)
plot(projdata$TradedDay, log(projdata$Loss+0.000000001), ylim = c(0, 20), col = "navy", xlab = "Traded Day", ylab = "Log. Loss")

do.call("rbind", lapply(split(projdata$Loss, projdata$TradedDay), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

#___________________________________________________________________________________________________
# Desk
table(projdata$Desk, projdata$LossIndicator)
addmargins(table(projdata$Desk, projdata$LossIndicator), 2)
round(addmargins(prop.table(table(projdata$Desk, projdata$LossIndicator), 1), 2)*100, 1)

do.call("rbind", lapply(split(projdata$Loss, projdata$Desk), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

#___________________________________________________________________________________________________
# Captured By
table(projdata$CapturedBy, projdata$LossIndicator)
round(addmargins(prop.table(table(projdata$CapturedBy, projdata$LossIndicator), 1), 2)*100, 1)
plot(table(projdata$LossIndicator, projdata$CapturedBy), main="By Tech Support", col=rainbow(20), las=1, cex.axis=1.0)

do.call("rbind", lapply(split(projdata$Loss, projdata$CapturedBy), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

#___________________________________________________________________________________________________
# Trader Status
table(projdata$TradeStatus, projdata$LossIndicator)

do.call("rbind", lapply(split(projdata$Loss, projdata$TradeStatus), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

#___________________________________________________________________________________________________
# Trader
unique(projdata$TraderId)
table(projdata$TraderId, projdata$LossIndicator)
round(addmargins(prop.table(table(projdata$TraderId, projdata$LossIndicator), 1), 2)*100, 1)
plot(table(projdata$LossIndicator, projdata$TraderId), main="By Trader", col=rainbow(20), las=1, cex.axis=1.0)

tapply(projdata$Loss, INDEX = projdata$TraderId, function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x)))

hist(projdata$Loss[projdata$TraderId == "ANALYST"])

#___________________________________________________________________________________________________
library("lattice")

xyplot(Loss ~ as.factor(TraderId) , data = projdata)

do.call("rbind", lapply(split(projdata$Loss, projdata$TraderId), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

#___________________________________________________________________________________________________
# Instrument
unique(projdata$Instrument)
table(projdata$LossIndicator, projdata$Instrument)
plot(table(projdata$LossIndicator, projdata$Instrument), main="By Instrument", col=rainbow(20), las=1, cex.axis=1.0)

round(addmargins(prop.table(table(projdata$LossIndicator, projdata$Instrument), 1), 2)*100, 1)

#___________________________________________________________________________________________________
pander::pander(tapply(projdata$Loss, INDEX = projdata$Instrument, function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

tablex <- do.call("rbind", lapply(split(projdata$Loss, projdata$Instrument), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))
tablex <- cbind(Instrument = rownames(tablex), tablex)
tablex <- as.data.frame(tablex)
tablex$Mean <- as.numeric(as.character(tablex$Mean))

tablex <- tablex[order(tablex$Mean), ]

#___________________________________________________________________________________________________
stargazer::stargazer(tablex)

openxlsx::write.xlsx(tablex, "tablex.xlsx", rownames = TRUE)

#___________________________________________________________________________________________________
# Reason
unique(projdata$Reason)

#___________________________________________________________________________________________________
# Loss
sum(projdata$Loss)
sum(projdata$Loss[projdata$LossIndicator == 0])
sum(projdata$Loss[projdata$LossIndicator == 1])

#___________________________________________________________________________________________________
# Event Type
table(projdata$EventTypeCategoryLevel, projdata$LossIndicator)
round(addmargins(prop.table(table(projdata$EventTypeCategoryLevel, projdata$LossIndicator), 1), 2)*100, 1)

do.call("rbind", lapply(split(projdata$Loss, projdata$EventTypeCategoryLevel), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

#___________________________________________________________________________________________________
# Business Line Level
table(projdata$BusinessLineLevel, projdata$LossIndicator)
round(addmargins(prop.table(table(projdata$BusinessLineLevel, projdata$LossIndicator), 1), 2)*100, 1)

do.call("rbind", lapply(split(projdata$Loss, projdata$BusinessLineLevel), function(x) c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x))))

#___________________________________________________________________________________________________
is_missing <- is.na(projdata$LastResetRate)
sum(is_missing)
plot(log(projdata$Loss[!is_missing]+0.000001), projdata$LastResetRate[!is_missing])

data.frame(table(projdata$LastResetRate))

#___________________________________________________________________________________________________
# Theta
plot(projdata$UpdatedTime[projdata$LossIndicator == 0], projdata$Theta[projdata$LossIndicator == 0])

plot(projdata$UpdatedTime[projdata$LossIndicator == 1], projdata$Theta[projdata$LossIndicator == 1])

projdata$Theta[projdata$LossIndicator == 0]
projdata$Theta[projdata$LossIndicator == 1]

plot(projdata$UpdatedDay[projdata$LossIndicator == 0], projdata$Theta[projdata$LossIndicator == 0])

plot(projdata$UpdatedDay[projdata$LossIndicator == 1], projdata$Theta[projdata$LossIndicator == 1])


par(mfrow=c(1,2))
plot(table(projdata$LossIndicator, projdata$Instrument), main="By Instrument", col=rainbow(20), las=1)
plot(table(projdata$LossIndicator, projdata$TraderId), main="By Trader", col=rainbow(20), las=1)
plot(table(projdata$LossIndicator, projdata$CapturedBy), main="By Tech Support", col=rainbow(20), las=1)
par(mfrow = c(1, 1))

#____________________________________________________________________________________________________
# Contingency Table
STD <- structable(~TradeStatus + TraderId + LossIndicator, data = projdata)
# Mosaic plot
mosaic(STD, condvars = 'TradeStatus', col=rainbow(20),  split_horizontal = c(TRUE, FALSE, TRUE))


