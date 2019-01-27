
options(scipen = 999)
# Load packages
library(rattle, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(Hmisc, quietly = TRUE)
library(chron, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(caret)
library(ggplot2)

# Set parameter values
# Load data
D <- read.csv("OPriskDataSet_exposure_severity.csv",
              sep=";",
              dec=",",
              na.strings=c(".", "NA", "", "?"),
              strip.white=TRUE, encoding="UTF-8")

set.seed(123)
####
## 75% of the sample size
smp_size <- floor(0.75 * nrow(D))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(D)), size = smp_size)

train <- D[train_ind, ]
test <- D[-train_ind, ]

exposure <- D[,ncol(D)] 
class(exposure)
length(exposure)

summary(D)
attach(D)

D1 <- D %>%
  group_by(UpdatedDay,
           UpdatedTime,
           TradedDay,
           TradedTime,
           Desk,
           CapturedBy,
           TradeStatus,
           TraderId,
           Instrument,
           Reason,
           EventTypeCategoryLevel1,
           BusinessLineLevel1) %>% 
transmute(LossesIndicator = LossIndicator,
            Losses = Loss,
            exposure = exposure)

#write.csv(D1, file="D1.csv")
#####
library(gamlss)

sf <- gamlss(Losses~pb(UpdatedDay+Desk), 
sigma.formula=~pb(UpdatedDay+Desk),
nu.formula=~pb(UpdatedDay+Desk),
tau.formula=~pb(UpdatedDay+Desk),
data=D1, mu.start = NULL,  sigma.start = NULL, nu.start = NULL, 
tau.start = NULL, family=NET)

summary(sf)
class(sf)
drop1(sf)
wp(sf)
term.plot(sf, what="sigma", ask=FALSE)

pred1<-predict(sf, newdata=train, type="response")


names(sf)


####################

sf <- gamlss(Losses~cs(UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
               + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1), 
sigma.formula=~cs(UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
               + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1),
nu.formula=~cs(UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
               + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1),
 tau.formula=~cs(UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
               + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1),
data=D1, mu.start = NULL,  sigma.start = NULL, nu.start = NULL, tau.start = NULL, family=BCPE)







### Let us fit a GAMLSS to our data. This will be our global model. We will use "Losses" as the
### dependent variable, while the other variables will be predictor variables.

severfit <- glm(Losses ~ UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus 
               + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1,
               data=D1, family=Gamma(link = 'log'), offset = log(exposure))
summary(severfit)











### Load "MuMIn" package



require(MuMIn)

### Then, we use "dredge" function to generate models using combinations of the terms in the global model. The
### function will also calculate AICc values and rank models according to it. Note that AICc is AIC corrected for
### finite sample sizes


options(na.action=na.fail)
severfits <- dredge(severfit)

### Ok, let us use "get.models" function to generate a list in which its objects are the fitted models. We will
### also use the "model.avg" function to do a model averaging based on AICc. Note that "subset=TRUE" will make the
### function calculate the average model (or mean model) using all models.

# summary(model.avg(get.models(severfits, subset = TRUE)))

### However, if you want to get only the models that have delta AICc < 2, use "subset=delta<2"

summary(model.avg(get.models(severfits, subset=delta<2)))

### That's it! Now we have AICc values for our models and we have the average model (or mean model).