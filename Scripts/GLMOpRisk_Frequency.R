options(scipen = 999)
# Load packages
library(rattle, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(Hmisc, quietly = TRUE)
library(chron, quietly = TRUE)
library(dplyr, quietly = TRUE)

# Set parameter values
crv$seed <- 42 # set random seed
crv$taining.proportion <- 0.7 # proportion of data used for training
crv$validation.proportion <- 0.15 # proportion of data used for validation

# Load data
d <- read.csv("OPriskDataSet_exposure.csv",
              sep=";",
              dec=",",
              na.strings=c(".", "NA", "", "?"),
              strip.white=TRUE, encoding="UTF-8")

install.packages("caret")
library(caret)
library(ggplot2)
#remove.packages("caTools")

set.seed(123)
split = sample.split(d$Trade, SplitRatio = 2/3) # for training set
split
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

####
## 75% of the sample size
smp_size <- floor(0.75 * nrow(d))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(d)), size = smp_size)

train <- d[train_ind, ]
test <- d[-train_ind, ]

exposure <- train[,ncol(train)] 
class(exposure)
length(exposure)

summary(train)

d1 <- train %>%
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

getmode <- function(x){
  u <- unique(x)
  as.integer(u[which.max(tabulate(match(x,u)))])
}

for (i in 5:(ncol(d1) - 3)){
     d1[[i]] <- relevel(d1[[i]], getmode(d1[[i]]))
}

### Let us fit a GLM to our data. This will be our global model. We will use "LossesIndicator" as the dependent
### variable, while the other variables will be predictor variables.

freqfit <- glm(LossesIndicator ~ UpdatedDay + UpdatedTime + TradedDay + TradedTime + Desk + CapturedBy + TradeStatus + TraderId + Instrument + Reason + EventTypeCategoryLevel1 + BusinessLineLevel1, data=d1, family=poisson(link = 'log'), offset = log(exposure))

summary(freqfit)
length(freqfit)

names(freqfit)
names(freqfit$data)

Y_Pred <- predict(freqfit, d1[1:10, setdiff(names(d1), "LossesIndicator")])
Y_Pred

freqfit$fitted.values[1:10]

exp(predict(freqfit, d1[1:10, setdiff(names(d1), "LossesIndicator")]))

### Load "MuMIn" package

require(MuMIn)

### Then, we use "dredge" function to generate models using combinations of the terms in the global model. The
### function will also calculate AICc values and rank models according to it. Note that AICc is AIC corrected for
### finite sample sizes

options(na.action=na.fail)
freqfits <- dredge(freqfit)
freqfits

dim(freqfits)
freqfits[1]
freqfits[,1]
class(freqfits)

### Ok, let us use "get.models" function to generate a list in which its objects are the fitted models. We will
### also use the "model.avg" function to do a model averaging based on AICc. Note that "subset=TRUE" will make the
### function calculate the average model (or mean model) using all models.

amodel <- model.avg(get.models(freqfits, subset = TRUE))
 

### However, if you want to get only the models that have delta AICc < 2, use "subset=delta<2"

adelmodel <- (model.avg(get.models(freqfits, subset=delta<2)))
summary(adelmodel)
### That's it! Now we have AICc values for our models and we have the average model (or mean model).


#data_test1 <- c(UpdatedDay = d1$UpdatedDay, UpdatedTime = d1$UpdatedTime,TradedDay = d1$TradedDay,
 #               TradedTime = d1$TradedTime)
  #            length(d1$UpdatedDay) 
              
# predicting test set results
y_pred = predict(freqfit, newdata = ft)
y_pred
summary(y_pred)

dim(adelmodel)
names(adelmodel)
class(adelmodel)
head(adelmodel$msTable,1)
adelmodel[1]
freqfits[,1]
class(freqfits)

av.pred <- predict(adelmodel, ft)
dim(av.pred)
length(av.pred)
av.pred

# ,se.fit = NULL, interval = NULLl,type=c("log", "response"), ...)

ft <- read.csv("Hoohlo1.csv",
               sep=";",
               dec=",",
               na.strings=c(".", "NA", "", "?"),
               strip.white=TRUE, encoding="UTF-8",header=T)
names(ft)
names(d)
dim(ft)
dim(d)




