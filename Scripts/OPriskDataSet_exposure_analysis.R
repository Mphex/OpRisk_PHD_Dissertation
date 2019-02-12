#============================================================

# Rattle is Copyright (c) 2006-2017 Togaware Pty Ltd.
# It is open source software and is freely available.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#============================================================
# Rattle timestamp: 2019-02-05 10:15:25 x86_64-w64-mingw32 

# Rattle version 5.1.0 user 'User'

# This log captures Rattle interactions as an R script. 

# For repeatability export this log of all activity to a 
# file using the Export button or the Tools menu. This 
# script can serve as a starting point for developing your 
# own scripts. Exporting to a file called 'model.R' will 
# allow you to type into a new R Console the command 
#"source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access weather dataset and utilities.
library(magrittr) # For the %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2019-02-05 10:15:59 x86_64-w64-mingw32 

# Load the dataset from file.

fname <- "file:///C:/Users/User/Documents/OpRiskPHDGitHub/OpRisk_PHD_Dissertate/OpRisk_PHD_Dissertation/OPriskDataSet_exposure.csv" 
crs$dataset <- read.csv(fname,
			sep=";",
			dec=",",
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2019-02-05 10:16:01 x86_64-w64-mingw32 

# Note the user selections. 

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

crs$target    <- "Desk"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2019-02-05 10:16:57 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for exposure

# Generate a box plot.

p01 <- crs %>%
  with(dataset[sample,]) %>%
  dplyr::mutate(Desk=as.factor(Desk)) %>%
  ggplot2::ggplot(ggplot2::aes(y=exposure)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=Desk, fill=Desk), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=Desk), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Desk\n\nRattle 2019-Feb-05 10:16:57 User") +
  ggplot2::ggtitle("Distribution of exposure (sample)\nby Desk") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#============================================================
# Rattle timestamp: 2019-02-05 10:17:05 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for exposure

# Generate the plot.

p01 <- crs %>%
  with(dataset[sample,]) %>%
  dplyr::mutate(Desk=as.factor(Desk)) %>%
  dplyr::select(exposure, Desk) %>%
  ggplot2::ggplot(ggplot2::aes(x=exposure)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Desk, colour=Desk), alpha=0.55) +
  ggplot2::xlab("exposure\n\nRattle 2019-Feb-05 10:17:05 User") +
  ggplot2::ggtitle("Distribution of exposure (sample)\nby Desk") +
  ggplot2::labs(fill="Desk", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)

#============================================================
# Rattle timestamp: 2019-02-05 10:17:09 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'exposure'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"exposure"], grp="All"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="Africa","exposure"], grp="Africa"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="Bonds/Repos","exposure"], grp="Bonds/Repos"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="Commodities","exposure"], grp="Commodities"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="Derivatives","exposure"], grp="Derivatives"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="Equity","exposure"], grp="Equity"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="Management/Other","exposure"], grp="Management/Other"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="MM","exposure"], grp="MM"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="Prime Services","exposure"], grp="Prime Services"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="Rates","exposure"], grp="Rates"),
            data.frame(dat=crs$dataset[crs$sample,][crs$dataset[crs$sample,]$Desk=="SND","exposure"], grp="SND"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="exposure", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)
Ecdf(ds[ds$grp=="Africa",1], col="#D99E82", lty=2, xlab="", lwd=2, subtitles=FALSE, add=TRUE)
Ecdf(ds[ds$grp=="Bonds/Repos",1], col="#C3A869", lty=3, xlab="", lwd=2, subtitles=FALSE, add=TRUE)
Ecdf(ds[ds$grp=="Commodities",1], col="#A2B367", lty=4, xlab="", lwd=2, subtitles=FALSE, add=TRUE)
Ecdf(ds[ds$grp=="Derivatives",1], col="#77BA7F", lty=5, xlab="", lwd=2, subtitles=FALSE, add=TRUE)
Ecdf(ds[ds$grp=="Equity",1], col="#49BEA0", lty=6, xlab="", lwd=2, subtitles=FALSE, add=TRUE)
Ecdf(ds[ds$grp=="Management/Other",1], col="#3CBCC0", lty=7, xlab="", lwd=2, subtitles=FALSE, add=TRUE)
Ecdf(ds[ds$grp=="MM",1], col="#6BB4D8", lty=8, xlab="", lwd=2, subtitles=FALSE, add=TRUE)
Ecdf(ds[ds$grp=="Prime Services",1], col="#A0A7E2", lty=9, xlab="", lwd=2, subtitles=FALSE, add=TRUE)
Ecdf(ds[ds$grp=="Rates",1], col="#C89BDB", lty=10, xlab="", lwd=2, subtitles=FALSE, add=TRUE)
Ecdf(ds[ds$grp=="SND",1], col="#DF94C5", lty=11, xlab="", lwd=2, subtitles=FALSE, add=TRUE)


# Add a legend to the plot.

legend("bottomright", c("All","Africa","Bonds/Repos","Commodities","Derivatives","Equity","Management/Other","MM","Prime Services","Rates","SND"), bty="n",  col=colorspace::rainbow_hcl(11) , lwd=2, lty=1:11, inset=c(0.05,0.05))

# Add a title to the plot.

title(main="Distribution of exposure (sample)\nby Desk",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2019-02-05 10:17:13 x86_64-w64-mingw32 

# Benford's Law 

# The 'ggplot2' package provides the 'ggplot' function.

library(ggplot2, quietly=TRUE)

# The 'reshape' package provides the 'melt' function.

library(reshape, quietly=TRUE)

# Initialies the parameters.

target <- "Desk"
var    <- "exposure"
digit  <- 1
len    <- 1

# Build the dataset

ds <- merge(benfordDistr(digit, len),
            digitDistr(crs$dataset[crs$sample,][var], digit, len, "All"))
for (i in unique(crs$dataset[crs$sample,][[target]]))
  ds <- merge(ds, digitDistr(crs$dataset[crs$sample,][crs$dataset[crs$sample,][target]==i, var], digit, len, i))

# Plot the digital distribution

p <- plotDigitFreq(ds)
p <- p + ggtitle("Digital Analysis of First Digit 
of exposure by Desk")
print(p)

#============================================================
# Rattle timestamp: 2019-02-05 10:26:40 x86_64-w64-mingw32 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for exposure

# Generate a box plot.

p01 <- crs %>%
  with(dataset[sample,]) %>%
  ggplot2::ggplot(ggplot2::aes(y=exposure)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Rattle 2019-Feb-05 10:26:40 User") +
  ggplot2::ggtitle("Distribution of exposure (sample)") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01)

#============================================================
# Rattle timestamp: 2019-02-05 10:26:40 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for exposure

# Generate the plot.

p01 <- crs %>%
  with(dataset[sample,]) %>%
  dplyr::select(exposure) %>%
  ggplot2::ggplot(ggplot2::aes(x=exposure)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::xlab("exposure\n\nRattle 2019-Feb-05 10:26:40 User") +
  ggplot2::ggtitle("Distribution of exposure (sample)") +
  ggplot2::labs(y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)

#============================================================
# Rattle timestamp: 2019-02-05 10:26:41 x86_64-w64-mingw32 

# Generate just the data for an Ecdf plot of the variable 'exposure'.

ds <- rbind(data.frame(dat=crs$dataset[crs$sample,][,"exposure"], grp="All"))

# The 'Hmisc' package provides the 'Ecdf' function.

library(Hmisc, quietly=TRUE)

# Plot the data.

Ecdf(ds[ds$grp=="All",1], col="#E495A5", xlab="exposure", lwd=2, ylab=expression(Proportion <= x), subtitles=FALSE)


# Add a title to the plot.

title(main="Distribution of exposure (sample)",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2019-02-05 10:26:41 x86_64-w64-mingw32 

# Benford's Law 

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
p <- p + ggtitle("Digital Analysis of First Digit 
of exposure ")
print(p)
