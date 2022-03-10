#Some nice code from Hannah to analyse the snail data

install.packages("sjPlot")
install.packages("sjlabelled")
install.packages("sjmisc")
install.packages("visreg")
install.packages("performance")

# here you'll load in the packages you need
# don't worry if they say that they mask certain functions in other packages,
# I have this coded so there should be no confusion

library(tidyverse)
library(lme4)
library(car)
library(ggplot2)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(visreg)
library(performance)
library(nlme)

#I've included brms and rstan if you want to try out the bayesian stuff, but 
#you'll get error messages when loading them if you don't have a C++ compiler so
#I'll leave them commented out for now
#library(brms)
#library(rstan)
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)

setwd("C:/Users/Isa/Dropbox/Papers - Active/Batillaria/Barnacle study")
getwd()

snail_movement <- read_csv("Snail Data - Behavioural_observations.csv")
head(snail_movement)

#PART 1: Do barnacles affect snail speed?

#We can do this in different ways depending on the goal of the analysis
#although not the best way to do it, the most straightforward would be a linear
#mixed-effects model. This is the most conceptually straightforward model, but
#has issues in that it may end up predicting negative values of speed which is 
#impossible. Use with caution.

# This one just uses presence/absence of barnacles

mod_lm1 <- lmer(distance ~ barnacled + length + gonads + (1|pond), 
               data = snail_movement)
summary(mod_lm1)

#check assumptions with performance package
check_model(mod_lm1)
# this indicates that the data are pretty much meeting all assumptions although
# some data points have high leverage

#plot
plot_model(mod_lm1)
visreg(mod_lm1)

# This second model uses the weight of barnacles carried

mod_lm2 <- lmer(distance ~ wet_wgt_barnacles + wet_wgt_snail + gonads + (1|pond), 
                data = snail_movement)
summary(mod_lm2)

plot_model(mod_lm2)

# This final  model uses the ratio of barnacle weight to snail weight

snail_movement$wgt_ratio <- (snail_movement$wet_wgt_barnacles / snail_movement$wet_wgt_snail)

mod_lm3 <- lmer(distance ~ wgt_ratio + gonads + (1|pond), 
             data = snail_movement)
summary(mod_lm3)

plot_model(mod_lm3)


#The other option is to run a proper hurdle model. This will be two submodels:
#one a logistic regression that deals with snails that moved vs didn't move and the other a Gamma 
# model that just considers snails that moved. 
# We have to recode the data to make this work

# First, create a weight ratio variable 

snail_movement$wgt_ratio <- (snail_movement$wet_wgt_barnacles / snail_movement$wet_wgt_snail)

#Starting with presence/absence of barnacles   

snail_movement_logistic <- snail_movement %>% 
  #create a new variables of zeros and ones for speed, where 0 means they didn't
  #move and 1 means they did
  mutate(distance_logistic = case_when(distance == 0 ~ 0,
                                    TRUE ~ 1))
snail_movement_gamma <- snail_movement %>% 
  #get rid of all the zeros for the analysis of the non-zero values
  filter(distance != 0)

hurdle_mod1 <- glmer(distance_logistic ~ barnacled + length + 
                       trematodes + (1|pond), 
                     family = binomial,
                     data = snail_movement_logistic)

hurdle_mod2 <- glmer(distance ~ barnacled + length + 
                       trematodes + (1|pond),
                     family = Gamma(link = "log"),
                     data = snail_movement_gamma)

summary(hurdle_mod1) #this will tell you which predictors are related to 
#whether a snail moves at all

plot_model(hurdle_mod1)

summary(hurdle_mod2) #this will tell you which predictors are related to 
#how far a snail moves

plot_model(hurdle_mod2)

# Second hurdle model uses the weight of barnacles carried

hurdle_mod3 <- glmer(distance_logistic ~ wet_wgt_barnacles + length + 
                       trematodes + (1|pond), 
                     family = binomial,
                     data = snail_movement_logistic)

hurdle_mod4 <- glmer(distance ~ wet_wgt_barnacles + length + 
                       trematodes + (1|pond),
                     family = Gamma(link = "log"),
                     data = snail_movement_gamma)

summary(hurdle_mod3)

plot_model(hurdle_mod3)

summary(hurdle_mod4) 

plot_model(hurdle_mod4)

# Plot relationship between barnacle weight and distance moved (zeros removed)ie Hurdle model 4
distance_bwgt_plot <- 
  
  ggplot(snail_movement_gamma, aes(x = wet_wgt_barnacles, y = distance)) +
  
  geom_point() +
  
  labs(x = 'Wet weight of barnacles carried (g)', y = 'Distance moved in 5 min (cm)') + 
  
  theme_classic() +
  
  geom_smooth(method = "glm", method.args = list(family = "gamma"))

distance_bwgt_plot


# Third hurdle model uses the ratio of barnacle to snail weights


hurdle_mod5 <- glmer(distance_logistic ~ wgt_ratio + length + 
                       trematodes + (1|pond), 
                     family = binomial,
                     data = snail_movement_logistic)

hurdle_mod6 <- glmer(distance ~ wgt_ratio + length + 
                       trematodes + (1|pond),
                     family = Gamma(link = "log"),
                     data = snail_movement_gamma)

summary(hurdle_mod5)

plot_model(hurdle_mod5)

summary(hurdle_mod6) 

plot_model(hurdle_mod6)



# Plot relationship between relative barnacle weight and distance moved (zeros removed)ie Hurdle model 6

distance_relgt_plot <- 
  
  ggplot(snail_movement_gamma, aes(x = wgt_ratio, y = distance)) +
  
  geom_point() +
  
  labs(x = 'Ratio of barnacle to snail weight', y = 'Distance moved in 5 min (cm)') + 
  
  theme_classic() +
  
  geom_smooth(method = "glm", method.args = list(family = "gamma"))

distance_relgt_plot

# **********************************************************************
#Bayesian analysis - we're not doing this

#if you wanted to be able to combine the two models into a single output, the
#only way I know how is with brms. Running it in brms with uninformative priors
#will give you the same results as above, but it will be easier to plot the
#combined output of the two submodels if that's something you want to do
#if you do decide you want to be able to do that, you'll need to make sure you
#have a C++ compiler to properly install brms - let me know if you need help


mod_brms <- brm(#bf() allows you to combine two model components into a single
  #one
  bf(#this first component is the non-zero part
    speed ~ barnacles_factor + snail_size + 
      gonad + (1|pond), 
    #and this is the hurdle component
    hu ~ barnacles_factor + snail_size + 
      gonad + (1|pond)), 
  data = snails2, 
  #note that we don't have to subset the data in either component
  #above, because brms knows that we want to treat 0s and non-0s 
  #differently when we set the family here
  family = hurdle_gamma(),
  #set seed for reproducibility
  seed = 123)

#******************************************************************************

#PART 2: Do barnacles affect growth allometry?
  
# Comparison of length by weight relationships with and without barnacles
# Make sure "barnacles" is a factor in your first dataset - if you entered the
# data as "yes" and "no", then you'll be fine, but if you entered it as 0 and 1,
# you'll need to use the following line to change it:
# snails <- snails %>% 
#           mutate(barnacles_factor = case_when(barnacles == 0 ~ "N",
#                                               TRUE ~ "Y"))
  
# Use the snail_movement data for this first analysis because we can look at the effect of
# trematodes too

mod_allometry <- lmer(wet_wgt_snail ~ length * barnacled + 
                     trematodes + (1|pond), 
                   data = snail_movement)
#test to see if any of the predictors are way too correlated to use 
#(specifically distance and trematodes)
car::vif(mod_allometry)
#examine your model output
summary(mod_allometry)

# Allometry plot

allometry_plot <- 
  
  ggplot(snail_movement, aes(x = length, y = wet_wgt_snail, color = barnacled)) +
  
  geom_point() +
  
  scale_color_manual(values = c("yes" = "red",
                                "no" = "black")) +
  
  labs(x = 'Snail length (mm)', y = 'Snail weight (g)') + 
  
  theme_classic() +
  
  geom_smooth(method = "lm")

allometry_plot



#**************************************************************
# PART 3 - Mark-recapture

snail_recapture <- read_csv("snail_recapture.csv")
head(snail_recapture)

recapture_glmer <- glmer(num_recap ~ barnacled + (1|date/pond), 
                         data = snail_recapture, family = poisson()) 
summary(recapture_glmer)

# Check for overdispersion
check_overdispersion(recapture_glmer)
# No evidence of overdispersion so Poisson is OK

#**************************************************************

# PART 4 - Distribution of barnacled snails

snail_distribution <- read_csv("Snail_Data_Transects.csv")
head(snail_distribution)

# Scale the continuous variables and create a variable for quadrat
snail_distribution_scaled <- snail_distribution %>%
    mutate(scale_distance_from_water = scale(distance_from_water)) %>%
    mutate(scale_length = scale(length)) %>%
    unite("quadrat", c(Transect,distance_from_water), remove = FALSE)

### Is there a relationship between snail abundance per quadrat and distance from the water?

#Need to get rid of NAs first

snail_distribution_noNA <- snail_distribution_scaled %>%
  filter(!is.na(total_num))

#Then get rid of outliers (not needed for glmer but I still used it)
snail_distribution_nooutliers <- snail_distribution_noNA %>%
  filter(total_num < 20)

# Just with no NA
total_num_lme <- lme(total_num ~ distance_from_water, random = ~1|Transect, 
                  data = snail_distribution_noNA)
summary(total_num_lme)

# No NA and no outliers
total_num_no_lme <- lme(total_num ~ distance_from_water, random = ~1|Transect, 
                     data = snail_distribution_nooutliers)
summary(total_num_no_lme)

# Check assumptions
check_model(total_num_no_lme)
# Points 40, 41, 42 seem to be outliers. Otherwise, it looks OK.

# Plot 

total_num_plot <- 
  
  ggplot(snail_distribution_nooutliers, aes(x = distance_from_water, y = total_num)) +
  
  geom_point() +
  
  xlim(0,150) +
  
  labs(x = 'Distance from the water (m)', y = 'Number of mud snails per 400 cm2') + 
  
  theme_classic() +
  
  geom_smooth(method = "glm", method.args = list(family = "poisson"))

  total_num_plot

  # Really need a Poisson distribution because we have counts, so back to glmer to see
  # if Poisson is the right one
  # The outliers are back in
  # Scale standardises and centres the variable
  
  total_num_glmer <- glmer(total_num ~ scale_distance_from_water + (1|Transect), 
                       data = snail_distribution_noNA, family = poisson()) 
  summary(total_num_glmer)
  
  # Check for overdispersion
  check_overdispersion(total_num_glmer)
  # Overdispersion detected so use negative binomial distribution instead 
  
  total_num_glmer_nb <- glmer.nb(total_num ~ scale_distance_from_water + (1|Transect), 
                           data = snail_distribution_nooutliers) 
  summary(total_num_glmer_nb)

  
  # Just checking differences among transects --- not much!
  ggplot(snail_distribution_nooutliers, aes(Transect,total_num))+
    geom_jitter() +
    geom_violin()
  
  snail_distribution_nooutliers %>%
    group_by(Transect) %>%
    summarise(mean = mean(total_num), sd = sd(total_num))
  
  
  
### Does the likelihood of being barnacled change with distance from the water and with 
  # snail length?
  
  # First, fill in NA with relevant total_num, and then remove quadrats where no snails were found
  snail_distribution_nozero <- snail_distribution_scaled %>%
    fill(total_num)%>%
    filter(total_num > 0)%>%
    mutate(barnacled = as.factor(barnacled), 
           barnacled_num = case_when(barnacled == "no"~0, barnacled == "yes"~1))
  

  # This model fails to converge, probably because variance in transect is so low
  prob_barn_glmer <- glmer(barnacled ~ distance_from_water * length +(1|Transect/quadrat), 
                           data = snail_distribution_nozero, family = binomial()) 
  summary(prob_barn_glmer)
  

  prob_barn_glmer2 <- glmer(barnacled ~ distance_from_water * length + Transect +(1|quadrat), 
                           data = snail_distribution_nozero, family = binomial()) 
  summary(prob_barn_glmer2)
  
  # Scaling at the start includes all zeros. Trying here with scaling without zeros
  snail_distribution_nozero_scaled <- snail_distribution %>%
    fill(total_num)%>%
    filter(total_num > 0)%>%
    mutate(scale_distance_from_water = scale(distance_from_water)) %>%
    mutate(scale_length = scale(length)) %>%
    unite("quadrat", c(Transect,distance_from_water), remove = FALSE)%>%
    mutate(barnacled = as.factor(barnacled), 
           barnacled_num = case_when(barnacled == "no"~0, barnacled == "yes"~1))
  
  prob_barn_glmer3 <- glmer(barnacled_num ~ scale_distance_from_water * scale_length +(1|Transect/quadrat), 
                           data = snail_distribution_nozero_scaled, family = binomial()) 
  summary(prob_barn_glmer3)
  # message: boundary (singular) fit: see ?isSingular
  
  # Trying now with transect as main effect
  prob_barn_glmer4 <- glmer(barnacled_num ~ scale_distance_from_water * scale_length + Transect + (1|quadrat), 
                            data = snail_distribution_nozero_scaled, family = binomial()) 
  summary(prob_barn_glmer4)
  
  
  #PLOT
  
  prob_barn_plot <- 
    
    ggplot(snail_distribution_nozero_scaled, aes(x = length, y = barnacled_num)) +
    
    geom_point() +
    
    labs(x = 'Snail length (mm)', y = 'Likelihood of carrying a barnacle') + 
    
    theme_classic() +
    
    geom_smooth(method = "glm", method.args = list(family = "binomial"))
  
  prob_barn_plot

  
  
### Is there a relationship between snail length and distance from the water?
  
  #Remove NA from length data
  
  snail_length_noNA <- snail_distribution_scaled %>%
    filter(!is.na(length))
  
  snail_length_lmer <- lmer(scale_length ~ distance_from_water + (1|Transect/quadrat), 
                         data = snail_length_noNA)
  summary(snail_length_lmer)
  

  # Plot 
 
  snail_length_plot <- 
    
    ggplot(snail_length_noNA, aes(x = distance_from_water, y = length)) +
             
             geom_point() +
             
             xlim(0,150) +
             
             labs(x = 'Distance from the water (m)', y = 'Snail length') + 
             
             geom_smooth(method = "lm") +
             
             theme_classic() 
  
  snail_length_plot
  