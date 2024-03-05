
################################################################################
## LegacyNet Conference Tutorial 3
## Visualising and interpreting the output of a Diversity-Interactions model.
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


## Create a helper function that will be used later for adding a dotted line to 
## predication plots for comparison with the high N treatment
add_comparison <- function(plot, model = mod_best, coeff_name = "NHigh",
                           label = "High N Yield"){
  y_int <- coef(model)[coeff_name]
  if(is.na(y_int)){
    warning("Didn't find coefficient in model. Returning plot as is.")
    return(plot)
  }
  plot$layers <- c(geom_hline(yintercept = y_int,
                              lty = "11", linewidth  = 1, colour = "#333333"),
                   plot$layers)
  
  plot <- plot + annotate(geom = "text", x = 0.9,
                          y = y_int + 0.25,
                          label = "High N",
                          colour = "#333333",
                          size = 4)
  return(plot)
}





# Task 1 -----------------------------------------------------------------------


#######################################################################
## Read in the data and create the total yield variable to be analysed
#######################################################################


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

## Make the RichnessSP variable numeric
dat$RichnessSP <- as.numeric(as.character(dat$RichnessSP))



###################################################
## Re-fit the final selected model from tutorial 1    
###################################################

## Re-fit your final model from tutorial 1
## For the simulated data, this was the FG model, with theta set to 1 but if 
## your site's final model was different, amend this code as required.
mod_best <- DI(y = "TotalYield", prop = c("G1", "G2", "L1", "L2", "H1", "H2"),
             FG = c("G", "G", "L", "L", "H", "H"), treat = "NHigh",
             data = dat, estimate_theta = FALSE, DImodel = "FG")
## View the parameter estimates and other summaries of the model fitted
summary(mod_best)





# Task 2 -----------------------------------------------------------------------


###################################################################
## Visualise species contributions to the predicted total DM yield   
###################################################################


## We will generate the plot for a specific set of communities. We select the 
## six monocultures, the 50-50 mixtures of G1-G2, L1-L2 and H1-H2 and the six
## species equi-proportional mixture by their row-number in the data
comms <- slice(dat, 1, 4, 7, 10, 13, 16, 19, 20, 21, 42, 43, 44, 47)
## View the communities we will predict for
comms

## Create a vector containing the labels we want to use for each bar
bar_labels <- c("G1", "G2", "L1", "L2", "H1", "H2", 
                "G1:G2", "L1:L2", "H1:H2", "G:L", "G:H", "L:H", "Six species")

## Create a vector for the colours for the pie-glyph slices
pie_cols <- c("#6a994e","#a7c957", "#f9a03f", "#f3c053", "#96b8db", "#c5dbf0")
## Note that we will re-use this vector throughout the tutorial.

## Create a vector for the colours for the bar-chart segments
bar_cols <- c(pie_cols, get_shades("#808080", 6)[[1]], "tomato")
## Note that we will re-use this vector throughout the tutorial.

## Use the `prediction_contributions` function from the `DImodelsVis` package
prediction_contributions(model = mod_best, data = comms,
                         colour = bar_cols,
                         bar_labs = bar_labels) %>%
  ## Add the high N comparison line
  add_comparison()



## Modify the previous plot to separate the communities into different
## panels based on the species richness in the communities.
## Use the `facet_grid` function from `ggplot2` to separate the communities by
## species richness. The "RichnessFG" column in the data has species richness,
## so we specify it in facet_grid
p1 <- prediction_contributions(model = mod_best, data = comms,
                               colour = bar_cols,
                               bar_labs = bar_labels) %>%
  ## Add the high N comparison line
  add_comparison() +
  ## The `space = "free_x"` parameter is to ensure the bars have equal widths
  facet_grid(~ RichnessSP, label = label_both,
             scales = "free_x", space = "free_x")+
  ## Change the font size in the facet labelling
  theme(strip.text = element_text(size = 10))

## View the plot
p1

### Save the plot and paste it your lab-book
ggsave("Tutorial3_prediction_contributions.png", plot = p1, 
       width = 12, height = 7, units = "in")
## COPY TO YOUR LAB BOOK

## See ?prediction_contributions for more information on function parameters





# Task 3 -----------------------------------------------------------------------


###################################################################
## Visualise the change in total DM yield with respect to species
## composition and richness   
###################################################################


## Create data frames for the high and low fertiliser plots separately
low_N <- dat %>% filter(NHigh == 0)
high_N <- dat %>% filter(NHigh == 1)

## Use the gradient_change function from the `DImodelsVis` package to visualise
## the low_N plots
## Add a dotted line to show the performance of the highN treatment
gradient_change(model = mod_best, data = low_N,
                pie_colours = pie_cols) %>%
  ## Add the high N comparison line
  add_comparison()




## Create a data frame that contains the species proportions for all possible
## equi-proportional communities at each level of richness
all_equi <- get_equi_comms(nvars = 6,
                           variables = c("G1", "G2", "L1", "L2", "H1", "H2"))
## Add the NHigh variable to the dataset but set to 0 for each
all_equi$NHigh <- 0

## Plot predicted yield versus richness for all all possible equi-proportional 
## communities at each level of richness
p2 <- gradient_change(model = mod_best, data = all_equi,
                      pie_colours = pie_cols) %>%
        ### Add the high N comparison line
        add_comparison()

## View the plot
p2

### Save the plot and paste it into your lab-book
ggsave("Tutorial3_gradient_change.png", plot = p2,
       width = 10, height = 7, units = "in")
## COPY TO YOUR LAB BOOK

## See ?gradient_change for more information on function parameters





# Task 4 -----------------------------------------------------------------------


##################################################################
## Visualise the change in the total yield across the functional 
## group ternary space  
##################################################################


## Visualise the variation in the predicted total DM yield across the
## functional group ternary space  assuming an equal proportion of the species
## with each functional group
## We use the `grouped_ternary` function from the `DImodelsVis` package
## The two species within a functional group are split 50:50 by default
grouped_ternary(model = mod_best,
                FG = c("G", "G", "L", "L", "H", "H"))



## Visualise the ternary diagram but this time have the relative proportions
## of the two species within each functional group as: 
##   - Grass (G) consists of 100% G1 and 0% G2.
##   - Legume (L) consists of 100%  L1 and 0% L2.
##   - Herb (H) consists of 100% H1 and 0% H2.
## Use the `values` parameter to adjust the proportion of species within an FG
grouped_ternary(model = mod_best,
                FG = c("G", "G", "L","L","H","H"),
                values = c(1, 0, 1, 0, 1, 0))



## Visualise the ternary diagram but this time have the relative proportions
## of the two species within each functional group as: 
##   - Grass (G) consists of 20% G1 and 80% G2.
##   - Legume (L) consists of 30%  L1 and 70% L2.
##   - Herb (H) consists of 10% H1 and 90% H2.
p3 <- grouped_ternary(model = mod_best,
                      FG = c("G", "G", "L","L","H","H"),
                      values = c(0.2, 0.8, 0.3, 0.7, 0.1, 0.9))

## View the plot
p3

### Save the plot and paste it into your lab-book
ggsave("Tutorial3_grouped_ternary.png", plot = p3,
       width = 7, height = 7, units = "in",
       bg = "white")
## COPY TO YOUR LAB BOOK

## See ?grouped_ternary for more information on function parameters





# Task 5 -----------------------------------------------------------------------


#########################################################################
## Visualise the change in the total DM yield across the species simplex
## space by conditioning certain species to have specific values 
#########################################################################

## Visualise the variation in the predicted total DM yield across the
## species G1, L2 and H1 while G2, L1 and H2 are conditioned to have the
## following values:
##   - G2 = 0.2, L1 = 0.3 and H2 = 0
##   - G2 = 0.1, L1 = 0.1 and H2 = 0.1
## We use the `conditional_ternary` function from the `DImodelsVis` package
## The `tern_vars` parameter is used to specify the three species within the
## ternary, while the `conditional` parameter can be used to specify the
## conditioning values for the species.
## Create the data frame that contains the conditioned on values
cond <- data.frame("G2" = c(0.2, 0.1),
                   "L1" = c(0.3, 0.1),
                   "H2" = c(0,   0.1))
## View the cond data frame
print(cond)

## Generate the ternary diagram
conditional_ternary(model = mod_best,
                    tern_vars = c("G1", "L2", "H1"),
                    conditional = cond)



## Visualise the variation in the predicted total DM yield across G1, G2
## and L2 while L1 is conditioned to have values 0, 0.2, and 0.5. Assume H1 and
## H2 to be 0.
## Any species not specified in `tern_vars` or `conditional` is assumed
## to be 0. Thus we can avoid specifying H1 and H2 in the `cond` data-frame
cond <- data.frame("L1" = c(0, 0.2, 0.5))
## View the data frame
print(cond)

## Generate the conditional ternary diagram
p4 <- conditional_ternary(model = mod_best,
                          tern_vars = c("G1", "G2", "L2"),
                          conditional = cond)
## View the plot
p4

## Save this plot and paste this plot into your lab-book
ggsave("Tutorial3_conditional_ternary.png", plot = p4,
       width = 10, height = 4.5, units = "in",
       bg = "white")
## COPY TO YOUR LAB BOOK

## See ?conditional_ternary for more information on function parameters





# Task 6 -----------------------------------------------------------------------


######################################################################
## Visualising the effect of the addition or loss of a species from a
## community on the total DM yield
######################################################################


## Visualise the effect of increasing the proportion of each of six
## species in two-species mixtures of G1 and G2, L1 and L2, and H1 and H2,
## along with all the four species and the six species equi-proportional mixture.

## Select the communities of interest from the data frame. Choose the 
## two-species mixtures of G1-G2, L1-L2 and H1-H2, along with all the four 
## species mixtures and the centroid mixture from the data using their
## row numbers
comms_int <- dat %>% slice(19, 20, 21, 42, 43, 44, 45)

## We use the `visualise_effects` function from the `DImodelsVis` package
visualise_effects(model = mod_best, data = comms_int, pie_colours = pie_cols) %>%
  ### Add the high N comparison line
  add_comparison()




## This time, visualise the effect of both, increasing and decreasing the 
## proportion of all six species from the two-species mixtures of G1 and G2, L1 
## and L2, and H1 and H2, along with all the four species and the six species 
## equi-proportional mixture using the effect parameter. Also show the 
## confidence interval along the predictions.
## By default, the effect of species addition is shown.
## This can be adjusted by the `effect` parameter to show the effect of
## addition or loss of a species. We can also visualise both as shown here
p5 <- visualise_effects(model = mod_best, data = comms_int,
                        pie_colours = pie_cols, effect = "both") %>%
        ### Add the high N comparison line
        add_comparison()

## View the plot
p5

## Save the plot and paste it into your lab-book
ggsave("Tutorial3_effects_plot.png", plot = p5,
       width = 13, height = 8, units = "in",
       bg = "white")
## COPY TO YOUR LAB BOOK

## See ?visualise_effects for more information on function parameters





# Task 7 -----------------------------------------------------------------------


#########################################################################
## Visualising the change in predicted total yield for a path through a 
## simplex space    
#########################################################################

## Visualise the change in the predicted total yield as the six-species
## centroid community evolves to the 50-50 mixtures of 1) G1 and G2, 
## 2) L1 and L2, and 3) H1 and H2. 
## Also show the uncertainty along the predictions.
## We use the `simplex_path` function from the `DImodelsVis` package
## We select the starting and ending communities to show in the plot using
## their row numbers
## Row 45 in dat is an equi-proportional six-species mixture
start <- dat %>% slice(45)
## Rows 19, 20 and 21 are 50:50 mixtures of G1:G2, L1:L2, and H1:H2 respectively
end <- dat %>% slice(19, 20, 21)
start
end

## Generate the plot using the 'simplex_path' function
## Add the uncertainty in the predictions (se = TRUE)
simplex_path(starts = start, ends = end, model = mod_best,
             pie_colours = pie_cols, se = TRUE) %>%
  ### Add the high N comparison line
  add_comparison()




## This time, show the path from the 25% mixture of G1, G2, L1 and L2 to 
## to the 25% mixtures of L1, L2, H1, and H2 and ot the 25% mixture of G1, G2, 
## H1, and H2. Add the uncertainty in the predictions.
## Select the necessary communities from the data
start <- dat %>% slice(42)
end <- dat %>% slice(43, 44)

## Generate the plot
p6 <- simplex_path(starts = start, ends = end, model = mod_best,
                   pie_colours = pie_cols, se = TRUE) %>%
        ### Add the high N comparison line
        add_comparison()

## View the plot
p6

## Save the plot and paste it into your lab-book
ggsave("Tutorial3_simplex_path.png", plot = p6,
       width = 8, height = 5, units = "in",
       bg = "white")

## See ?simplex_path for more information on function parameters


