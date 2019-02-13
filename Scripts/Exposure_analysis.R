# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script.

library(rattle)   # Access weather dataset and utilities.
library(magrittr) # For the %>% and %<>% pipeline operators.


#The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

 crv$seed <- 42 

# Load the dataset from file.

fname <- "file:///C:/Users/User/Documents/OpRiskPHDGitHub/OpRisk_PHD_Dissertate/OpRisk_PHD_Dissertation/OPriskDataSet_exposure.csv" 
crs$dataset <- read.csv(fname,
                        sep=";",
                        dec=",",
                        na.strings=c(".", "NA", "", "?"),
                        strip.white=TRUE, encoding="UTF-8")

# Build the train/validate/test datasets.

# nobs=2330 train=1631 validate=349 test=350

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("Trade", "UpdateTime", "UpdatedDay", "UpdatedTime",
                   "TradeTime", "TradedDay", "TradedTime", "CapturedBy",
                   "TradeStatus", "TraderId", "Instrument", "Reason",
                   "Nominal", "FloatRef", "LastResetDate", "LastResetRate",
                   "Theta", "Loss", "Unexplained", "EventTypeCategoryLevel1",
                   "BusinessLineLevel1", "LossIndicator", "exposure")

crs$numeric   <- c("Trade", "UpdatedDay", "UpdatedTime", "TradedDay",
                   "TradedTime", "Nominal", "LastResetRate", "Theta", "Loss",
                   "Unexplained", "LossIndicator", "exposure")

crs$categoric <- c("UpdateTime", "TradeTime", "CapturedBy", "TradeStatus",
                   "TraderId", "Instrument", "Reason", "FloatRef",
                   "LastResetDate", "EventTypeCategoryLevel1",
                   "BusinessLineLevel1")

crs$target    <- "LossIndicator"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL


# Generate a cummulative distribution function (ECDF) plot of the variable 'exposure'.
#============================================================
# Generate just the data for an Ecdf 
ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"exposure"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.
library(Hmisc, quietly=TRUE)
# Plot the data.
p05<-Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="exposure", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)

## BaseR codes for plot p05b<-plot(p05$x,p05$y,type='l',col='red')

# Benford's Law 
#============================================================
# The 'ggplot2' package provides the 'ggplot' function.
library(ggplot2, quietly=TRUE)
# The 'reshape' package provides the 'melt' function.
library(reshape, quietly=TRUE)
# Initialies the parameters.
var    <- "exposure"
digit  <- 1
len    <- 1

# Build the dataset
ds <- merge(benfordDistr(digit, len),
            digitDistr(crs$dataset[crs$sample,][var], digit, len, "All"))

# Plot the digital distribution
p <- plotDigitFreq(ds)
#print(p)

# Display histogram plots for the selected variables. 
#============================================================
# Generate the plot.

p04 <- crs %>%
  with(dataset[sample,]) %>%
  dplyr::select(exposure) %>%
  ggplot2::ggplot(ggplot2::aes(x=exposure)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("exposure") +
  ggplot2::ggtitle("Distribution of exposure") +
  ggplot2::labs(y="Density")

# Display the plots.

#gridExtra::grid.arrange(p04, nrow=1)






 

