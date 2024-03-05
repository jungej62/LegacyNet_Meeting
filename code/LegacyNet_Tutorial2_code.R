
################################################################################
## LegacyNet Conference Tutorial 2
## Analysing data from multiple harvests across the LegacyNet grassland 
## ley phase
################################################################################

## Begin by installing/updating the following packages
## If you already have these installed (and up to date) you can skip this line
install.packages(c("readxl", "dplyr", "ggplot2", "clipr", "DImodels", "PieGlyph", 
                   "DImodelsVis", "DImodelsMulti"))

## Load the packages
library(readxl)
library(dplyr)
library(ggplot2)
library(clipr)
library(DImodels)
library(PieGlyph)
library(DImodelsVis)
library(DImodelsMulti)



# Task 1 -----------------------------------------------------------------------


#####################################
## Pre-processing - tidying the data
#####################################

## Set the working directory to the folder where you have saved this file and 
## your data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Read in your data
## If you are analysing your own data then change the file name to match
## whatever you have named your Excel file (don't forget the .xlsx). 
filename <- "LegacyNet_SimSite.xlsx"
dat0 <- read_xlsx(filename, "Stage1_grass", skip = 12, na = ".")

## We can view the first few rows of our data to make sure everything imported 
## correctly
head(dat0)

## Restrict the data to only the core design communities
dat0 %>%
  filter(Community %in% 1:34) %>% 
  mutate(across(c(Plot, Community, Nfert, HarvestN, RichnessSP, RichnessFG), as.factor)) -> dat0

## Create a dummy variable called NHigh for the high N grass reference community
## We also want the proportion of G1 to be 0 for the HighN grass reference community, 
## this will help the interpretability of our model fitting later on.
dat0 <- dat0 %>%
  mutate(NHigh = case_when(Nfert == "High" ~ 1,    
                           .default = 0),
         G1 = case_when(Nfert == "High" ~ 0,       
                        .default = G1))

## Rescale the DMYield response variable
dat0$DMYield <-  dat0$DMYield/1000

#Remove any empty columns
dat0 <- dat0[, !sapply(dat0, function(x) all(is.na(x)))]

## View the first few rows of the data again
head(dat0)



####################
## Examine the data
####################


## Group results by year or harvest, and take the mean of the ecosystem function response
dat_year <- summarise(dplyr::group_by(dat0, Year),
                      DMYield = mean(DMYield))
dat_year


dat_harvest <- summarise(dplyr::group_by(dat0, HarvestN),
                         DMYield = mean(DMYield))
dat_harvest


## Plot a histogram for each harvest
hist <- ggplot(dat0, aes(x = DMYield)) + 
  geom_histogram(fill = "#669aff") +
  facet_wrap(~ HarvestN, labeller = label_both) +
  ggtitle("DMYield by Harvest")

## View the histograms
hist

## Save the plot and copy to your lab book
ggsave("Tutorial2_histograms.png", plot = hist, width = 10, height = 7, 
       units = "in")
## COPY TO YOUR LAB BOOK




# Task 2 -----------------------------------------------------------------------


#####################################################################
## Fitting the FG Model using DImulti() in the DImodelsMulti package
#####################################################################

## To view the help file for the function DImulti() run
?DImulti

## If an error occurs, see vignette:
vignette("DImulti_commonErrors")

#Functional group interaction model using our value of theta
model_FG_UN <- DImulti(y = "DMYield", time = c("HarvestN", "UN"), unit_IDs = "Plot",
                   prop = c("G1", "G2", "L1", "L2", "H1", "H2"), data = dat0,
                   FG = c("G", "G", "L", "L", "H", "H"), extra_fixed = ~ NHigh,
                   DImodel = "FG", method = "REML", estimate_theta = TRUE)
## You may see warnings here for the species proportions not summing to 1, this 
## is expected due to floating decimal points and our high nitrogen plots.

## View the fixed effect estimates for the fitted FG model. There are identity
## effect and interaction estimates for each harvest. Note that theta is 
## assumed to be the same across all harvests. 
print(model_FG_UN)

## View and maintain the theta estimate
theta_est <- model_FG_UN$theta
theta_est



#######################################
## Compare autocorrelation structures
#######################################

## When comparing covariance structures, it is important to use REML estimation
## and to keep the fixed effects constant. 

## Fit the repeated measures model with all fixed effects crossed with time and 
## the UN covariance structure
model_FG_UN <- DImulti(y = "DMYield", time = c("HarvestN", "UN"), unit_IDs = "Plot",
                       prop = c("G1", "G2", "L1", "L2", "H1", "H2"), data = dat0,
                       FG = c("G", "G", "L", "L", "H", "H"), extra_fixed = ~ NHigh,
                       DImodel = "FG", method = "REML", theta = theta_est)

## Fit the repeated measures model with all fixed effects crossed with time and 
## the AR(1) covariance structure
model_FG_AR1 <- DImulti(y = "DMYield", time = c("HarvestN", "AR1"), unit_IDs = "Plot",
                   prop = c("G1", "G2", "L1", "L2", "H1", "H2"), data = dat0,
                   FG = c("G", "G", "L", "L", "H", "H"), extra_fixed = ~ NHigh,
                   DImodel = "FG", method = "REML", theta = theta_est)

## Fit the repeated measures model with all fixed effects crossed with time and 
## the CS covariance structure
model_FG_CS <- DImulti(y = "DMYield", time = c("HarvestN", "CS"), unit_IDs = "Plot",
                       prop = c("G1", "G2", "L1", "L2", "H1", "H2"), data = dat0,
                       FG = c("G", "G", "L", "L", "H", "H"), extra_fixed = ~ NHigh,
                       DImodel = "FG", method = "REML", theta = theta_est)

#Use information criteria
AICc(model_FG_UN)
AICc(model_FG_AR1)
AICc(model_FG_CS)
#COPY TO YOUR LAB BOOK



#########################################
## Compare between interaction structures
#########################################

## When comparing models with different fixed effects, it is important to use 
## ML estimation and to keep the covariance structure the same. 

## Use ML estimation to fit the FG model with CS covariance structure
model_FG <- DImulti(y = "DMYield", time = c("HarvestN", "CS"), unit_IDs = "Plot",
                      prop = c("G1", "G2", "L1", "L2", "H1", "H2"), data = dat0,
                      FG = c("G", "G", "L", "L", "H", "H"), extra_fixed = ~ NHigh,
                      DImodel = "FG", method = "ML", theta = theta_est)

## Use ML estimation to fit the AV model with CS covariance structure
model_AV <- DImulti(y = "DMYield", time = c("HarvestN", "CS"), unit_IDs = "Plot",
                   prop = c("G1", "G2", "L1", "L2", "H1", "H2"), data = dat0,
                   extra_fixed = ~ NHigh,
                   DImodel = "AV", method = "ML", theta = theta_est)

anova(model_AV, model_FG)
#COPY TO YOUR LAB BOOK





# Task 3 -----------------------------------------------------------------------


#######################
## Fit the final model 
#######################

## While the model selection process above was not exhaustive, take the best
## model from AV or FG, and take the best covariance structure for your data and 
## fit your final model here. Note, the code below assumes FG interactions and
## CS covariance, you may need to adjust this for your data. 

## Refit our chosen model using REML
model_FG_harv <- DImulti(y = "DMYield", time = c("HarvestN", "CS"), unit_IDs = "Plot",
                    prop = c("G1", "G2", "L1", "L2", "H1", "H2"), data = dat0,
                    FG = c("G", "G", "L", "L", "H", "H"), extra_fixed = ~ NHigh,
                    DImodel = "FG", method = "REML", theta = theta_est)

## View the fitted model
print(model_FG_harv)

## USE THESE LINES OF CODE TO COPY YOUR ESTIMATED COEFFICIENTS
## PASTE THEM TO THE TABLE OF COEFFICIENTS IN YOUR LAB BOOK
coefs <- summary(model_FG_harv)$tTable[, 1:2]

cbind(
coefs %>% 
  as_tibble() %>%
  filter(grepl("HarvestN1.*", rownames(coefs))),
coefs %>% 
  as_tibble() %>%
  filter(grepl("HarvestN2.*", rownames(coefs))),
coefs %>% 
  as_tibble() %>%
  filter(grepl("HarvestN3.*", rownames(coefs))),
coefs %>% 
  as_tibble() %>%
  filter(grepl("HarvestN4.*", rownames(coefs)))
) %>% unname() %>%  
  clipr::write_clip()
#COPY TO YOUR LAB BOOK


model_FG_harv$theta
#COPY TO YOUR LAB BOOK (last row of the coefficients table)




##########################
## Predict from the model
##########################

## All rows
preds <- predict(model_FG_harv)

## Custom communities
comms <- data.frame(Plot     = c(1,   2,   3,    4, 5),
                    #HarvestN = c(1,   1,   1,    1, 1),
                    G1       = c(1/6, 1/3, 0.25, 1, 0),
                    G2       = c(1/6, 0,   0.25, 0, 0),
                    L1       = c(1/6, 1/3, 0.25, 0, 0),
                    L2       = c(1/6, 0,   0.25, 0, 0),
                    H1       = c(1/6, 1/3, 0,    0, 0),
                    H2       = c(1/6, 0,   0,    0, 0),
                    NHigh    = c(0,   0,   0,    0, 1)
                    )
#comms$HarvestN <- as.factor(comms$HarvestN)

# Predict for select communities and merge with community identifier
preds_comms <- predict(model_FG_harv, newdata = comms)
preds_comms <- merge(comms, preds_comms)

## View the predicted values for the selected communities
preds_comms

## For more information on predicting from a DImulti model, see vignette:
vignette("DImulti_prediction")


##########################
## Summary plot
##########################

## Generate a line plot of the predicted yields over time
line <- ggplot(data = preds_comms, aes(x = HarvestN, y = Yvalue, group = Plot)) +
  geom_line(linewidth = 2, lty = "11") +
  geom_pie_glyph(slices = c("G1", "G2", "L1", "L2", "H1", "H2", "NHigh"), radius = 0.3) +
  theme_bw() + 
  scale_fill_manual(values = c(c("#6a994e","#a7c957", "#f9a03f", "#f3c053", 
                                 "#96b8db", "#c5dbf0", "darkred"))) +
  labs(title = "Predicted Dry Matter Yield", subtitle = "by HarvestN", y = "DMYield") 

## View the plot
line

## Save the plot
ggsave("Tutorial2_line_plot.png", plot = line, width = 10, height = 7, 
       units = "in")
#COPY TO YOUR LAB BOOK

