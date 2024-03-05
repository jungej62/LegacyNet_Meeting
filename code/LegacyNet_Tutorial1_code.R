
################################################################################
## LegacyNet Conference Tutorial 1
## Reading your LegacyNet data into R and analysing it using the 
## Diversity-Interactions modelling approach.
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

##################
## Pre-processing
##################

## Once you have saved this R script file to your LegacyNet conference folder 
## you can proceed with the next steps

## Set the working directory to the folder where you have saved this file and 
## your data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
## Read in your data
## If you are analysing your own data then change the file name to match
## whatever you have named your Excel file (don't forget the .xlsx). 
dat0 <- read_xlsx("/Users/junge037/Documents/Projects/LegacyNet_Meeting/data/LegacyNet_US1_v04_1Dec2022.xlsx", "Stage1_grass", skip = 12, na = ".")

## We can view the first few rows of our data to make sure everything imported 
## correctly
head(dat0)

## We will remove any missing DMYield values
dat0 <- dat0[!is.na(dat0$DMYield),]

## Tidying the data
## We will restrict our analysis to only the core design 52 plots
dat0 <-  dat0 %>%
          filter(Community %in% 1:34) %>% 
          mutate(across(c(Plot, Community, Nfert, RichnessSP, RichnessFG), as.factor))
       
## We will create a dummy variable called NHigh for the high N grass reference community
## We also want the proportion of G1 to be 0 for the HighN grass reference community, 
## this will help the interpretability of our model
dat0 <- dat0 %>%
          mutate(NHigh = case_when(Nfert == "High" ~ 1,    
                        .default = 0),
                 G1 = case_when(Nfert == "High" ~ 0,       
                        .default = G1))

## Here we will calculate the total yield per plot over the whole ley phase.
## This is the yield summed over each harvest (via the sum function).
## Note, we divide by 1000 to convert the yield into tonnes
dat <-  dat0 %>%
          summarise(TotalYield = sum(DMYield)/1000, 
                    .by = all_of(c("Plot", "Community", "Nfert", "NHigh", "RichnessFG",
                                   "RichnessSP", "FG1", "FG2", "FG3",
                                   "G1", "G2", "L1", "L2", "H1", "H2")))

## Altering some columns
## We want to create a column which indicates if the plot is a monoculture, 
## a mixture, or a High N reference community
dat <- dat %>%
        mutate(plotType = case_when(RichnessSP == 1 & Nfert == "Low" ~ "Monoculture",
                                    Nfert == "High" ~ "HighN",
                                    .default = "Mixture"), .before = "Nfert")

## Create a numeric species richness variable
dat$Richness <- as.numeric(as.character(dat$RichnessSP))


###########################################
## Summary Statistics and data exploration
###########################################

## Calculating the mean and variance of total yield for ALL plots 
## RECORD THE OUTPUT IN THE "CHARACTERISTICS OF YOUR GRASSLAND LEY" SLIDE IN YOUR LAB BOOK
dat %>%
  summarise(Total_Yield_Mean = mean(TotalYield),
            Total_Yield_Variance = var(TotalYield))

## Calculating the mean and variance of total yield for the MONOCULTURE plots
## RECORD THE OUTPUT IN THE "CHARACTERISTICS OF YOUR GRASSLAND LEY" SLIDE IN YOUR LAB BOOK
dat %>%
  filter(plotType == "Monoculture") %>%
  summarise(Total_Yield_Mean = mean(TotalYield),
            Total_Yield_Variance = var(TotalYield))

## Calculating the mean and variance of total yield for the MIXTURE plots
## RECORD THE OUTPUT IN THE "CHARACTERISTICS OF YOUR GRASSLAND LEY" SLIDE IN YOUR LAB BOOK
dat %>%
  filter(plotType == "Mixture") %>%
  summarise(Total_Yield_Mean = mean(TotalYield),
            Total_Yield_Variance = var(TotalYield))



## Creating a boxplot of mixtures and monos yield
## This will create a boxplot showing the distribution of responses for monocultures 
## and mixtures. A dashed line will be plotted at the level of the average yield of 
## the High N reference communities

## SAVE THIS PLOT TO INSERT IT INTO YOUR LAB BOOK

## To plot points for each monoculture we will first create a summary
## dataframe of the average total yield of grasses, legumes, and herbs
monos <- dat %>% 
          filter(plotType == "Monoculture") %>%
          mutate(Mono_type = case_when(FG1 == 1 ~ "Grass",
                                       FG2 == 1 ~ "Legume",
                                       FG3 == 1 ~ "Herb")) %>%
          summarise(TotalYield = mean(TotalYield),
                    .by = "Mono_type") %>%
          mutate(plotType = "Monoculture") 

## The ggplot function will create your plot
p1 <- ggplot(data = dat %>% filter(NHigh == 0), 
             aes(x = plotType, y = TotalYield, fill = plotType))+
  geom_boxplot() +
  stat_summary(data = monos, aes(shape = Mono_type),
               fun = "mean",
               colour = "black",
               size = 0.9) +
  stat_summary(data = monos, aes(shape = Mono_type),
               fun = "mean",
               colour = "yellow",
               size = 0.7) +
  geom_hline(yintercept = mean(subset(dat$TotalYield, dat$plotType == "HighN")),
             linetype = "dashed", colour = "#333333") +
  annotate("text", x = 0.55, y = mean(subset(dat$TotalYield, dat$plotType == "HighN")) + 0.2,
           label = "High N") +
  scale_shape_manual(name = "Monoculture type", values = c(15:17)) +
  guides(fill = "none") +
  labs(y = "Total yield (t/ha)", x = "Plot type")

## Run this line to view the plot
p1

## This will save the plot as a .png file in your working directory
ggsave(filename = "/Users/junge037/Documents/Projects/LegacyNet_Meeting/figures/Tutorial1_boxplot.png", plot = p1, width = 10, height = 8)
## COPY TO YOUR LAB BOOK




## Create a scatter plot of yield versus richness with pie-glyphs to show the
## species proportions that were sown in each community at each level of richness
## Change the limits of the y-axis if required
p2 <- ggplot(data = dat, aes(x = Richness, y = TotalYield))+
  geom_point(size = 2)+
  ylim(0, 15)+
  theme_bw()+
  theme(text = element_text(size=20))+
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6))+
  geom_pie_glyph(slices = c("G1", "G2", "L1", "L2", "H1", "H2", "NHigh"),
                 data = dat, radius = 0.3)+
  scale_fill_manual(values = c("#6a994e","#a7c957", "#f9a03f", "#f3c053", 
                               "#96b8db", "#c5dbf0", "darkred"))+
  labs(x = "Richness", y = "Total annual yield (t/ha)", fill = "Species")

## Run this line to view the plot
p2

## This will save the plot as a .png file in your working directory
ggsave(filename = "/Users/junge037/Documents/Projects/LegacyNet_Meeting/figures/Tutorial1_scatterplot.png", plot = p2, width = 10, height = 8)
## COPY TO YOUR LAB BOOK





# Task 2 -----------------------------------------------------------------------

##########################
## Model fitting - autoDI
##########################

## We will start by fitting our model using autoDI
## Run this code and spend some time reading through each step of the output

mod_auto <- autoDI(y = "TotalYield", ## the response variable
                  prop = c("G1", "G2", "L1", "L2", "H1", "H2"), 
                  ## column names identifying the species proportions in our data
                  FG = c("G", "G", "L", "L", "H", "H"), 
                  ## functional groups associated with each of the species
                  treat = "NHigh", ## the treatment variable
                  data = dat) ## name of our data

## Run the summary function to view the best fitting model selected by autoDI
summary(mod_auto)


###############################################
## Model fitting - Functional group (FG) model
###############################################

## We will now fit the functional group model manually using the DI function
## The arguments to this function are similar to autoDI but we now include the 
## DImodel = "FG" to fit a functional group model.

## NB: This function will present a warning that some of our rows have ALL 
## proportions equal to zero. This warning would usually warrant investigation, 
## however we know that the species proportions have all been set to zero for 
## our HighN communities, thus we can ignore this warning.

mod_FG <- DI(y = "TotalYield",  
             prop = c("G1", "G2", "L1", "L2", "H1", "H2"), 
             FG = c("G", "G", "L", "L", "H", "H"),
             treat = "NHigh",
             data = dat,
             DImodel = "FG") ## argument specifying what DI model to fit

## Take some time to look at the output of the model
summary(mod_FG)



###################
## Model selection
###################

## Now fit a range of DI models and identify the best fitting model for 
## your site

## Start by fitting the ID model
mod_ID <- DI(y = "TotalYield",
             prop = c("G1", "G2", "L1", "L2", "H1", "H2"),
             treat = "NHigh",
             data = dat,
             DImodel = "ID")


## Fitting the full pairwise model with and without theta to see if theta is 
## significantly different from one

mod_Full <- DI(y = "TotalYield",
               prop = c("G1", "G2", "L1", "L2", "H1", "H2"),
               treat = "NHigh",
               data = dat,
               DImodel = "FULL")

mod_Full_theta <- DI(y = "TotalYield",
               prop = c("G1", "G2", "L1", "L2", "H1", "H2"),
               treat = "NHigh",
               data = dat,
               estimate_theta = TRUE,
               DImodel = "FULL")

## We will conduct an F test to test if the model with theta is a better fit 
## than the model without theta
## What is the result?
anova(mod_Full, mod_Full_theta, test = "F")

## INPUT THE NAME OF THE BEST FITTING FULL PAIRWISE MODEL HERE (mod_Full OR mod_Full_theta)
## For the simulated data, this is mod_Full but you will need to change this if 
## your model required theta
mod_Full <- mod_Full

  
## NB if the previous step identified the model with theta as a better fit then the 
## estimate_theta argument in all of the following functions should be set to TRUE

## Fitting the AV model
mod_AV <- DI(y = "TotalYield",
             prop = c("G1", "G2", "L1", "L2", "H1", "H2"),
             treat = "NHigh",
             data = dat,
             estimate_theta = FALSE, ## Change this to TRUE if required
             DImodel = "AV")

## Fitting the FG model
mod_FG <- DI(y = "TotalYield",
          prop = c("G1", "G2", "L1", "L2", "H1", "H2"),
          FG = c("G", "G", "L", "L", "H", "H"),
          treat = "NHigh",
          data = dat,
          estimate_theta = FALSE, ## Change this to TRUE if required
          DImodel = "FG")

## We will now conduct F tests on our models to see which is the best fit
## Take some time to read the output.
## What is the best fitting model?

anova(mod_ID, mod_AV, mod_FG, mod_Full, test = "F")

## NB INPUT THE NAME OF YOUR BEST FITTING MODEL HERE
## For the simulated data, this is mod_FG but you will need to change this if 
## your model is different, select from: mod_ID / mod_AV / mod_FG / mod_Full
mod_best <- mod_FG
  

## View the coefficient estimates of your best model
summary(mod_best)

## USE THESE LINES OF CODE TO COPY YOUR ESTIMATED COEFFICIENTS
## PASTE THEM TO THE TABLE OF COEFFICIENTS IN YOUR LAB BOOK
coefs <- coefficients(summary(mod_best))[,1:2]

coefs %>% unname() %>%
  clipr::write_clip()





# Task 3 ------------------------------------------------------------------

#####################
## Model predictions
#####################

## We will now make some predictions using our best fitting model

## Creating some nice colours for our plot
bar_cols <- c("#6a994e","#a7c957", "#f9a03f", "#f3c053", 
              "#96b8db", "#c5dbf0", "#333333")

## We will plot six communities from our design
dat2 <- dat %>%
          filter(Community %in% c(1:6,30,33)) %>%
          group_by(Community) %>%
          slice(1) %>%
          select(Community, RichnessSP, G1:H2, NHigh) %>%
          as.data.frame()


## The prediction contributions function from the DImodelsVis package will plot the barplot for us
## NB if your best model is NOT the FG model then replace the names of the interactions in the groups argument
## with the names of the interaction effects as they appear in your model. Do not remove the "NHigh" effect.

## SAVE THIS PLOT AND INSERT IT INTO YOUR LAB BOOK WHERE INDICATED

p3 <- prediction_contributions(mod_best,
                         ## Replace the names of the interaction effects here if required
                         groups = list("Net interactions" = c("NHigh", "FG_bfg_G_H", "FG_bfg_G_L", "FG_bfg_H_L", 
                                                          "FG_wfg_G", "FG_wfg_H", "FG_wfg_L")),
                         data = dat2,
                         colours = bar_cols,
                         bar_labs = c("G1", "G2", "L1", "L2", "H1", "H2", "G:L Mix", "Six-species")) +
  geom_hline(yintercept = coef(mod_FG)["NHigh"], linetype = "dashed", colour = "#333333") +
  annotate(geom = "text", x = 1, y = coef(mod_FG)["NHigh"] + 0.3, label = "High N Yield", colour = "#333333") +
  labs(y = "Predicted yield (t/ha)")

## Run this line to view the plot
p3

## This will save the plot as a .png file in your working directory
ggsave(filename = "Tutorial1_Predictions.png", plot = p3, width = 14, height = 8)
## COPY TO YOUR LAB BOOK


#########################
## Comparing predictions
#########################


## Reminder of the final estimated model
## Check all predictors in the model, noting the order of each one. 
summary(mod_best)

## Compare three pairs of model predictions. Compare:
##    - The G1 monoculture community to the G2 monoculture community
##    - The low N G1 monoculture community to the high N G1 monoculture community. 
##    - A four-species equi-proportional mixture of G1, G2, L1 and L2 to the 
##      high N G1 monoculture community. 

## The entries in each list are the values that the corresponding parameter
## estimate will be multiplied by to create the contrast.
## Note that if your final model is not the same as the simulated dataset, 
## the values for the contrasts will need to be changed. And if your final
## model included theta, you need to include the theta power in your calculation 
## of interaction variables. Please ask for help with this if needed! 
comparison <- contrasts_DI(object = mod_best,
                contrast = 
                  list("p1 v p2 ID" = c(1,-1,0,0,0,0,0,0,0,0,0,0,0),
                       "p1 v HighN" = c(1,0,0,0,0,0,0,0,0,0,0,0,-1),
                       "G:L v HighN" = c(0.25,0.25,0.25,0.25,0,0, 0,0.25,0,0.0625,0,0.0625,-1)))
## View the tests of comparison
summary(comparison)               
                     
                   


###########################################
## Assessing the model fit using residuals    
###########################################

## Generate residual plots for the selected model
## Note that the plots may take a few seconds to load
model_diagnostics(model = mod_best,
                  prop = c("G1", "G2", "L1", "L2", "H1", "H2", "NHigh"),
                  pie_colours = c("#6a994e","#a7c957", "#f9a03f", "#f3c053", 
                                  "#96b8db", "#c5dbf0", "darkred"))


