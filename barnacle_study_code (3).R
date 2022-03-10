#Some nice code from Hannah to analyse the snail data

install.packages("sjPlot")
install.packages("sjlabelled")
install.packages("sjmisc")
install.packages("visreg")
install.packages("performance")
install.packages("lmerTest")

# here you'll load in the packages you need
# don't worry if they say that they mask certain functions in other packages,
# I have this coded so there should be no confusion

library(tidyverse)
library(lme4)
library(lmerTest)
library(car)
library(ggplot2)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(visreg)
# vis reg is the nice package that helps visualize the summarized data
library(performance)

#I've included brms and rstan if you want to try out the bayesian stuff, but 
#you'll get error messages when loading them if you don't have a C++ compiler so
#I'll leave them commented out for now
#library(brms)
#library(rstan)
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)

setwd("C:/Users/Isa/Dropbox/Papers - Active/Batillaria/Barnacle study")
getwd()

snail_movement <- read_csv ("Behavioural_observations1.csv")
head(snail_movement)

#PART 1: Do barnacles affect snail speed?

#We can do this in different ways depending on the goal of the analysis
#although not the best way to do it, the most straightforward would be a linear
#mixed-effects model. This is the most conceptually straightforward model, but
#has issues in that it may end up predicting negative values of speed which is 
#impossible. Use with caution.

# This one just uses presence/absence of barnacles

mod_lm1 <- lmer(Distance ~ Barnacled + Length + Gonads + (1|Pond), 
               data = snail_movement)
#this is a linear effects model that is accounting for random effects. Here we are looking at distance as the y variable and then seeing if the barnacle presence with effects from length, or gonads have effects on the distance. It is also accounting for the random effect of the ponds and saying that each values from the same pond (eg all snails in pond A) are slightly more similar than those from other ponds and accounts for this in the analysis

summary(mod_lm1)
#there are slightly negative relationships seen between the distance vs barnacled with effects from length, gonads but nothing major/significant.

#check assumptions with performance package
check_model(mod_lm1)
# this indicates that the data are pretty much meeting all assumptions although
# some data points have high leverage

#plot
plot_model(mod_lm1)
visreg(mod_lm1)

# This second model uses the weight of barnacles carried

mod_lm2 <- lmer(Distance ~ Wet_weight_barnacles + Wet_wgt_snail + Gonads + (1|Pond), 
                data = snail_movement)
#this is also a linear model that is accounting for random effects from the ponds. It is seeing if the weight of barancles is influenceing the the disatnce travelled while also accounting for the weight of the snail  

summary(mod_lm2)
#no real effects of the wet weight of barnacles on the distance

plot_model(mod_lm2)

# This final  model uses the ratio of barnacle weight to snail weight
#this below is hannahs version
snail_movement$wgt_ratio <- (snail_movement$Wet_weight_barnacles / snail_movement$Wet_wgt_snail)

#this below is Ems version, using tidyverse which may be more efficient, you should use this one. It creates a new dataframe called snail_movement2. The %>% (works if all varaibles is in seperate columns and each row is a new sample/replicate) filters it into a pipe that is mutated to create the wgt ratio from two other coloums which is the wet weight barnacles and the wet weight snail
snail_movement2 <- snail_movement %>%
  mutate(wgt_ratio = Wet_weight_barnacles/Wet_wgt_snail)


mod_lm3 <- lmer(Distance ~ wgt_ratio + Gonads + (1|Pond), 
             data = snail_movement2)
#another linear effects model looking at the effects of the weight ratio on the distance, with effects from the gonads while accounting for pond effects
summary(mod_lm3)

plot_model(mod_lm3)


#The other option is to run a proper hurdle model. This will be two submodels:
#one a logistic regression that deals with snails that moved vs didn't move and the other a Gamma 
# model that just considers snails that moved. 
# We have to recode the data to make this work

#Starting with presence/absence of barnacles
#this is looking at the movement of snails in a few different ways
snail_movement_logistic <- snail_movement2 %>% 
  #create a new variables of zeros and ones for speed, where 0 means they didn't move and 1 means they did. Says 0 = 0 and 1 means that they did move
  mutate(distance_logistic = case_when(Distance == 0 ~ 0,
                                    TRUE ~ 1))

#this gets rid of the snails that didnt move so there will be no observations that have a value of 0 for distance if you look at the data set after this line
snail_movement_gamma <- snail_movement2 %>% 
  #get rid of all the zeros for the analysis of the non-zero values
  filter(Distance != 0)

#this hurdle model is a generalized linear model that is first looking at the distance_logistic that has both snails that moved and snails that didnt move, and it is seeing if barncles, length, and gonads influenced if the snails moved or not. Family binomial means that its categorical (moved? yes or no)
hurdle_mod1 <- glmer(distance_logistic ~ Barnacled + Length + 
                       Gonads + (1|Pond), 
                     family = binomial,
                     data = snail_movement_logistic)

# this is looking at if the distanced moved by those that did move was influenced by the barncles, length, or gonads. Family gamma means its a continuous non zero scale for distance that we are looking at 
hurdle_mod2 <- glmer(Distance ~ Barnacled + Length + 
                       Gonads + (1|Pond),
                     family = Gamma(link = "log"),
                     data = snail_movement_gamma)

summary(hurdle_mod1) #this will tell you which predictors are related to 
#whether a snail moves at all
# !! Here we found that the presence of barnacles influenced if a snail moved or not as well as the presence of gonads. The length didnt influence if the snails moved


plot_model(hurdle_mod1)

summary(hurdle_mod2) #this will tell you which predictors are related to 
#how far a snail moves
# this is looking at the snails that did move, and those that did if the presence of barnacles, length, or gonads influenced how far they moved. 
# !! here we found that there is no effect on the distance of the moving snails due to barnacles, length, or gonads

plot_model(hurdle_mod2)

# Second hurdle model uses the weight of barnacles carried

hurdle_mod3 <- glmer(distance_logistic ~ Wet_weight_barnacles + Length + 
                       Gonads + (1|Pond), 
                     family = binomial,
                     data = snail_movement_logistic)


hurdle_mod4 <- glmer(Distance ~ Wet_weight_barnacles + Length + 
                       Gonads + (1|Pond),
                     family = Gamma(link = "log"),
                     data = snail_movement_gamma)

summary(hurdle_mod3)
# this looked at all the snails (moved yes or no) and if the wet weight of barnacles and length and gonads influenced if they moved or not. there was no significant effect from any of them

plot_model(hurdle_mod3)

summary(hurdle_mod4) 
# !!this looked at only the snails that did move, and investigated if the wet weight of barnacles, length, or gonads influenced the distance. The wet weight of barnacles did influence the distance of those snails that did move

plot_model(hurdle_mod4)


# Third hurdle model uses the ratio of barnacle to snail weights


hurdle_mod5 <- glmer(distance_logistic ~ wgt_ratio + Length + 
                       Gonads + (1|Pond), 
                     family = binomial,
                     data = snail_movement_logistic)

hurdle_mod6 <- glmer(Distance ~ wgt_ratio + Length + 
                       Gonads + (1|Pond),
                     family = Gamma(link = "log"),
                     data = snail_movement_gamma)

summary(hurdle_mod5)
#this looked at all the snails and if the snails moved or not, to see if the weight ratio had an influence on if they moved or not. It was found that the weight ratio does influence if the snails moved or not

plot_model(hurdle_mod5)

summary(hurdle_mod6) 
#this looked at if the the weight ratio influenced the distance moved by snails in those snails that did move. It found that the weight ratio did impact how far the snails that were moving travelled.

plot_model(hurdle_mod6)


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



#PART 2: Do barnacles affect growth allometry?
  
# Comparison of length by weight relationships with and without barnacles
# Make sure "barnacles" is a factor in your first dataset - if you entered the
# data as "yes" and "no", then you'll be fine, but if you entered it as 0 and 1,
# you'll need to use the following line to change it:
# snails <- snails %>% 
#           mutate(barnacles_factor = case_when(barnacles == 0 ~ "N",
#                                               TRUE ~ "Y"))
  
snail_allometry <- read_csv("Allometry_dataset.csv")

mod_weight <- lmer(Snail_wet_wgt (g) ~ Snail_length (mm) * Barnacled (yes/no) + Distance_from_water (m) + 
                     Trematodes (yes/no) + (1|Observer), 
                   data = snail_allometry)
#test to see if any of the predictors are way too correlated to use 
#(specifically distance and trematodes)
car::vif(mod_weight)
#examine your model output
summary(mod_weight)

