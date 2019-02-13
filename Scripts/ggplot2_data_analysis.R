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
  crs$dataset <- read.csv("OPriskDataSet_exposure.csv",
                          sep=";",
                          dec=",",
                          na.strings=c(".", "NA", "", "?"),
                          strip.white=TRUE, encoding="UTF-8")

# Build the train/validate/test datasets.
set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, crv$taining.proportion * crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), crv$validation.proportion * crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# Select variables for loss incident model
crs$input     <- c("UpdatedDay", "UpdatedTime", "TradedDay", "TradedTime",
                   "Desk", "CapturedBy", "TradeStatus", "TraderId",
                   "Instrument", "Reason", "Nominal", "Theta", "Unexplained",
                   "EventTypeCategoryLevel1", "BusinessLineLevel1")

crs$numeric   <- c("UpdatedDay", "UpdatedTime", "TradedDay", "TradedTime",
                   "Nominal", "Theta", "Unexplained")

crs$categoric <- c("Desk", "CapturedBy", "TradeStatus", "TraderId",
                   "Instrument", "Reason", "EventTypeCategoryLevel1",
                   "BusinessLineLevel1")

crs$target    <- "LossIndicator"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("FloatRef", "LastResetDate", "LastResetRate", "Loss")
crs$weights   <- NULL

#Data summary
#contents(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])
#summary(crs$dataset[crs$sample, c(crs$input, crs$risk, crs$target)])

# Density Plot for Updated Day 
p01 <- crs %>%
  with(dataset[sample,]) %>%
  dplyr::mutate(LossIndicator=as.factor(LossIndicator)) %>%
  dplyr::select(UpdatedDay, LossIndicator) %>%
  ggplot2::ggplot(ggplot2::aes(x=UpdatedDay)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=LossIndicator, colour=LossIndicator), alpha=0.55) +
  ggplot2::ggtitle("Distribution of Day Trade was Updated by Loss Indicator") +
  ggplot2::labs(fill="LossIndicator", y="Density") +
  ggplot2::xlab("Day of Month that Trade Was Updated")  

# Density Plot for Traded day
p02 <- crs %>%
  with(dataset[sample,]) %>%
  dplyr::mutate(LossIndicator=as.factor(LossIndicator)) %>%
  dplyr::select(TradedDay, LossIndicator) %>%
  ggplot2::ggplot(ggplot2::aes(x=TradedDay)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=LossIndicator, colour=LossIndicator), alpha=0.55) +
  ggplot2::ggtitle("Distribution of TradedDay by Loss Indicator") +
  ggplot2::labs(fill="LossIndicator", y="Density") +
  ggplot2::xlab("Day of Month for Trade")  

p03 <- crs %>%
  with(dataset[sample,]) %>%
  dplyr::mutate(LossIndicator=as.factor(LossIndicator)) %>%
  dplyr::select(Desk, LossIndicator) %>%
  dplyr::group_by(Desk, LossIndicator) %>%
  dplyr::summarise(n = n()) %>%
  ggplot2::ggplot(ggplot2::aes(x=Desk, y=n, fill=LossIndicator)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::ggtitle("Desk category distribution") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggplot2::ylab("Frequency")


# Display the plots.

gridExtra::grid.arrange(p01, p02, nrow = 1)

gridExtra::grid.arrange(p03)

T01 <- crs %>%
  with(dataset[sample,]) %>%
  dplyr::mutate(LossIndicator=as.factor(LossIndicator)) %>%
  dplyr::select(Desk, LossIndicator) %>%
  dplyr::group_by(Desk, LossIndicator) %>%
  dplyr::summarise(n = n())
head(T01)
T01

T02 <- T01 %>%
  group_by(Desk) %>%
  summarise(N=sum(n))
head(T02)
T02

T03 <- inner_join(T01, T02)
head(T03)
T03

T04 <- T03 %>%
  mutate(Prob=n/N) %>%
  filter(LossIndicator==1) %>%
  select(Desk, Prob) %>%
  arrange(desc(Prob)) %>%
  ggplot2::ggplot(ggplot2::aes(x=Desk, y=Prob, fill=Desk), alpha=0.55) +
  ggplot2::geom_bar(stat="identity", fill="grey", colour="black", show.legend = FALSE)+
  ggplot2::ggtitle("Proportion of losses per Desk") +
  ggplot2::theme_minimal()+
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggplot2::ylab("Loss Ratio (n/N)")+
  ggplot2::xlab("Desk")
head(T04)
T04 

gridExtra::grid.arrange(p03, T04, nrow = 1)



#============================================================
# Rattle timestamp: 2018-12-13 00:46:08 x86_64-w64-mingw32 

# GGobi Data Exploration 

# The 'rggobi' package provides the 'rggobi' function.

library(rggobi, quietly=TRUE)

#============================================================
# Rattle timestamp: 2018-12-13 00:47:11 x86_64-w64-mingw32 

# GGobi Data Exploration 

# The 'rggobi' package provides the 'rggobi' function.

library(rggobi, quietly=TRUE)

#============================================================
# Rattle timestamp: 2018-12-13 00:48:45 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation OPriskDataSet_exposure.csv using Pearson",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))


library(corrplot)
col1 <- colorRampPalette(c("black","grey"))
corrplot(vcov(m00, type="cor"), col=col1(2), outline=TRUE,
         tl.col = "black", addCoef.col = "white")
corrplot(vcov(m0, type="cor"), col=col1(2), outline=TRUE,
         tl.col = "black", addCoef.col = "white")



 

