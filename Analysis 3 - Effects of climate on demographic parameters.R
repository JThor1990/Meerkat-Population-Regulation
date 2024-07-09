################################################################

# R Script for "Environmental change and population regulation in Kalahari meerkats"

# ANALYIS 3: MODELLING THE EFFECTS OF CLIMATE ON DEMOGRAPHIC PARAMETERS
# Author: Jack Thorley (jbt27@cam.ac.uk)
# Produced using R version 4.2.2

###############################################################

# The script carries out all analyses of the effect of variation in temperature and rainfall on changes in group size and population density, and on various constituent demographic parameters, while controlling for the often strong effects of group size. 

# load in the packages 
lapply(c("ggplot2", "lubridate", "patchwork", "tidyverse"), FUN = library, character.only = TRUE)

# set the working directory (edit to location of data files on own machine)
setwd("INSERT FILE PATH")

# load in the required data sets
  # Rainfall (monthly)
  rain <- read.csv("MeerkatMonthlyRain.csv", header = TRUE) %>% 
    mutate(rain_onsite = as.numeric(rain_onsite), 
           month_start = as.Date(strptime(month_start, format = "%d/%m/%Y")),
           breeding_season = if_else(month %in% 1:6, year - 1, year)) %>% 
    mutate(breeding_season = paste0(breeding_season, "/", breeding_season + 1))
  # Temperature (Daily)
  temp <- read.csv("MeerkatDailyTemp.csv", header = TRUE) %>% 
    mutate(date = as.Date(strptime(date, format = "%d/%m/%Y")), 
           month_start = as.Date(strptime(month_start, format = "%d/%m/%Y")), 
           breeding_season = if_else(month %in% 1:6, year - 1, year)) %>% 
    mutate(breeding_season = paste0(breeding_season, "/", breeding_season + 1))
  # SPEI
  spei <- read.csv("MeerkatSPEI.csv", header= TRUE) %>% 
    mutate(date = as.Date(strptime(date, format = "%d/%m/%Y")),
           month = month(date), year = year(date)) %>% 
    dplyr::select(date, month, year, SPEI_3, SPEI_6) %>% 
    mutate(breeding_season = if_else(month %in% 1:6, year - 1, year)) %>% 
    mutate(breeding_season = paste0(breeding_season, "/", breeding_season + 1))
  # Demographic information 
  df <- read.csv("MeerkatDemography.csv", header = TRUE) 
  # Population density information
  density_df <- read.csv("MeekatPopulationDensity.csv", header = TRUE) %>% 
    mutate(Date = as.Date(strptime(Date, format = "%d/%m/%Y")))

#=====================================================================

# GENERATE THE CLIMATE VARIABLES FOR USE IN THE MULTIPLE REGRESSIONS

#===================================================================== 

# Rainfall variables     
 # (1) Early season rainfall (Sep-Nov)
 # (2) Breeding season rainfall (Sep-Apr)
 # (3) Combined two year rainfall
  
# (1) Early season rainfall (Sep to Nov)
rain_earlysummer <- rain %>% 
  filter(year %in% 1997:2024, 
         month %in% 9:11,
         breeding_season != "2023/2024") %>% 
  group_by(breeding_season) %>% 
  summarise(rain_earlysummer = sum(rain_onsite)) %>% 
  data.frame()
  
# (2) Breeding season rainfall (Sep-Apr) 
rain_breedingseason <- rain %>% 
  filter(year %in% 1997:2024, 
         month %in% c(9:12, 1:4),
         breeding_season != "2023/2024") %>% 
  group_by(breeding_season) %>% 
  summarise(rain_breedingseason = sum(rain_onsite)) %>% 
  data.frame()
  
# (3) # total rainfall across the year and previous years rainfall (Jul - Jun)
rain_twoyear <- rain %>% 
   filter(year %in% 1997:2024, 
          breeding_season != "2023/2024") %>% 
   group_by(breeding_season) %>% 
   summarise(rain_annual = sum(rain_onsite)) %>% 
   mutate(rain_lagannual = lag(rain_annual), 
          rain_twoyear = rain_annual + rain_lagannual) %>% 
  data.frame()
  
# put the rainfall periods in a single data frame 
rain_periods <- left_join(rain_earlysummer, rain_breedingseason) %>% 
  left_join(rain_twoyear) 
  
#------------------------- 
  
# Temperature variables   
  # (1) Mean maximum summer temperatures (Dec-Apr)
  # (2) Mean maximum yearly temperature (Jul to June of next year)

# (1) Mean maximum summer temperatures (Dec-Apr)
temp_summer <- temp %>% 
  filter(year %in% 1997:2024, 
         month %in% c(12, 1:4),
         breeding_season != "2023/2024") %>% 
  group_by(breeding_season) %>% 
  summarise(temp_summer = mean(tempmax_noaa, na.rm = T)) %>% 
  data.frame()
  
# (2) Mean maximum yearly temperature (Jul-Jun of next year)
temp_allyear <- temp %>% 
  filter(year %in% 1997:2024, 
         breeding_season != "2023/2024") %>% 
  group_by(breeding_season) %>% 
  summarise(temp_allyear = mean(tempmax_noaa, na.rm = T)) %>% 
  data.frame()
  
# put the temperature periods in a single data frame 
temp_periods <- left_join(temp_summer, temp_allyear) 
 
#------------------------- 

# SPEI variables 
  # (1) Mean annual SPEI-6 (Jul to June of next year)
  # (2) Combined two year mean SPEI-6
  
# (1) Mean annual SPEI-6 (Jul to June of next year)
spei_allyear <- spei %>% 
  filter(year %in% 1997:2024, 
         breeding_season != "2023/2024") %>% 
  group_by(breeding_season) %>% 
  summarise(spei6_allyear = mean(SPEI_6)) %>% 
  data.frame()
  
# (2) Combined two year mean SPEI-6
spei_twoyear <- spei %>% 
  group_by(breeding_season) %>% 
  mutate(breeding_count = cur_group_id()) 

spei_sub <- data.frame(breeding_season = unique(spei_twoyear$breeding_season), 
                       breeding_count = unique(spei_twoyear$breeding_count),
                       spei6_twoyear = as.numeric(NA))
  
for (i in unique(spei_twoyear$breeding_count)) {
    
  sub <- spei_twoyear %>% 
      filter(breeding_count %in% c(i, i - 1)) %>% 
      data.frame()
  
  spei_sub$spei6_twoyear[i] <- mean(sub$SPEI_6, na.rm = T) 
}
  
spei_twoyear <- filter(spei_sub, breeding_season != "2023/2024")
  
# put the SPEI periods in a single data frame 
spei_periods <- left_join(spei_allyear, spei_twoyear)
  
#------------------------- 
  
# Join all the climate variable datasets together: 
envdata <- left_join(rain_periods, temp_periods) %>% 
  left_join(spei_periods) %>% 
  mutate(year = 1:n()) # a counter for use in detrending below
  
# and clean up to working environment at this point
rm(rain_earlysummer, rain_breedingseason, rain_twoyear, rain_periods,
   temp_summer, temp_allyear, temp_periods,
   spei_allyear, spei_twoyear, spei_periods)
  
# And log the rainfall data 
envdata$lograin_earlysummer  <- log(envdata$rain_earlysummer)
envdata$lograin_breedingseason <- log(envdata$rain_breedingseason)
envdata$lograin_twoyear <- log(envdata$rain_twoyear)

#=====================================================================

# DETREND THE CLIMATE AND DEMOGRAPHIC VARIABLES 

#===================================================================== 

# Note that there are not strong trends in most of the rainfall patterns but we detrend for consistency nonetheless 
str(envdata) ; unique(envdata$breeding_season)
# Note also that there are 26 rows of data for the climate variable, versus 25 for the demography. 
# This is because we needed to include the 1997/1998 breeding season so that two-year measures of climate could be incorporated. I am noting this because the detrending of the demographic data needs to line up temporally (see below)
# In each case I plot the original trend through time and also the the detrended data (to show that it worked)

# Detrended early season rainfall 
par(mfrow = c(1,2)) 
plot(lograin_earlysummer  ~  year, data = envdata, las = 1) 
abline(lm(lograin_earlysummer  ~  year, data = envdata))
envdata$resid_lograin_earlysummer <- resid(lm(lograin_earlysummer  ~  year, data = envdata))
plot(resid_lograin_earlysummer  ~  year, data = envdata, las = 1)
abline(lm(resid_lograin_earlysummer  ~  year, data = envdata))

# detrended breeding season rainfall
plot(lograin_breedingseason  ~  year, data = envdata, las = 1)
abline(lm(lograin_breedingseason  ~  year, data = envdata))
envdata$resid_lograin_breedingseason <- resid(lm(lograin_breedingseason  ~  year, data = envdata))
plot(resid_lograin_breedingseason  ~  year, data = envdata, las = 1)
abline(lm(resid_lograin_breedingseason  ~  year, data = envdata))

# detrended two year rainfall
plot(lograin_twoyear  ~  year, data = envdata, las = 1)
abline(lm(lograin_twoyear  ~  year, data = envdata))
envdata$resid_lograin_twoyear <- resid(lm(lograin_twoyear  ~  year, data = envdata))
plot(resid_lograin_twoyear  ~  year, data = envdata, las = 1)
abline(lm(resid_lograin_twoyear  ~  year, data = envdata))

# detrended summer temperatures
plot(temp_summer  ~  year, data = envdata, las = 1)
abline(lm(temp_summer  ~  year, data = envdata))
envdata$resid_temp_summer <- resid(lm(temp_summer  ~  year, data = envdata))
plot(resid_temp_summer  ~  year, data = envdata, las = 1)
abline(lm(resid_temp_summer  ~  year, data = envdata))

# detrended yearly temperature
plot(temp_allyear  ~  year, data = envdata, las = 1)
abline(lm(temp_allyear  ~  year, data = envdata))
envdata$resid_temp_allyear <- resid(lm(temp_allyear  ~  year, data = envdata))
plot(resid_temp_allyear  ~  year, data = envdata, las = 1)
abline(lm(resid_temp_allyear  ~  year, data = envdata))

# detrended yearly SPEI-6
plot(spei6_allyear  ~  year, data = envdata, las = 1)
abline(lm(spei6_allyear  ~  year, data = envdata))
envdata$resid_spei6_allyear <- resid(lm(spei6_allyear  ~  year, data = envdata))
plot(resid_spei6_allyear  ~  year, data = envdata, las = 1)
abline(lm(resid_spei6_allyear  ~  year, data = envdata))

# detrended two-year SPEI-6
plot(spei6_twoyear  ~  year, data = envdata, las = 1)
abline(lm(spei6_twoyear  ~  year, data = envdata))
envdata$resid_spei6_twoyear <- resid(lm(spei6_twoyear  ~  year, data = envdata))
plot(resid_spei6_twoyear  ~  year, data = envdata, las = 1)
abline(lm(resid_spei6_twoyear  ~  year, data = envdata))

#------------------------- 

# Equally we need to detrend the population aggregated responses 
# For this we need the years to line up in df and envdata
df$year <- 2:(nrow(df) + 1) 

# dominant pregnancies 
par(mfrow = c(1,2))
plot(meanDomPregnancies  ~  year, data = df, las = 1)
abline(lm(meanDomPregnancies  ~  year, data = df))
df$resid_meanDomPregnancies <- resid(lm(meanDomPregnancies  ~  year, data = df))
plot(resid_meanDomPregnancies  ~  year, data = df, las = 1)
abline(lm(resid_meanDomPregnancies  ~  year, data = df))

# dominant litters 
plot(meanDomLitters ~  year, data = df, las = 1)
abline(lm(meanDomLitters  ~  year, data = df)) 
df$resid_meanDomLitters <- resid(lm(meanDomLitters  ~  year, data = df))
plot(resid_meanDomLitters  ~  year, data = df, las = 1)
abline(lm(resid_meanDomLitters  ~  year, data = df))

# pups emerged 
plot(meanPupsEmerged  ~  year, data = df, las = 1)
abline(lm(meanPupsEmerged  ~  year, data = df)) 
df$resid_meanPupsEmerged <- resid(lm(meanPupsEmerged  ~  year, data = df))
plot(resid_meanPupsEmerged  ~  year, data = df, las = 1)
abline(lm(resid_meanPupsEmerged  ~  year, data = df))

# pups recruited 
plot(meanPupsRecruited  ~  year, data = df, las = 1)
abline(lm(meanPupsRecruited  ~  year, data = df)) 
df$resid_meanPupsRecruited <- resid(lm(meanPupsRecruited  ~  year, data = df))
plot(resid_meanPupsRecruited  ~  year, data = df, las = 1)
abline(lm(resid_meanPupsRecruited  ~  year, data = df))

# pup survival 
plot(meanPupSurvival ~  year, data = df, las = 1)
abline(lm(meanPupSurvival  ~  year, data = df)) 
df$resid_meanPupSurvival <- resid(lm(meanPupSurvival  ~  year, data = df))
plot(resid_meanPupSurvival  ~  year, data = df, las = 1)
abline(lm(resid_meanPupSurvival  ~  year, data = df))

# non-pup survival (juv + subadult + adult)
plot(meanNonPupSurvival ~  year, data = df, las = 1)
abline(lm(meanNonPupSurvival  ~  year, data = df)) 
df$resid_meanNonPupSurvival <- resid(lm(meanNonPupSurvival  ~  year, data = df))
plot(resid_meanNonPupSurvival  ~  year, data = df, las = 1)
abline(lm(resid_meanNonPupSurvival  ~  year, data = df))

# juvenile survival 
plot(meanJuvSurvival ~  year, data = df, las = 1)
abline(lm(meanJuvSurvival  ~  year, data = df))
df$resid_meanJuvSurvival <- NA
df$resid_meanJuvSurvival[c(1:15, 17:25)] <- resid(lm(meanJuvSurvival  ~  year, data = df))
plot(resid_meanJuvSurvival  ~  year, data = df, las = 1)
abline(lm(resid_meanJuvSurvival  ~  year, data = df))

# subadult survival 
plot(meanSubAdultSurvival ~  year, data = df, las = 1)
abline(lm(meanSubAdultSurvival  ~  year, data = df)) 
df$resid_meanSubAdultSurvival <- resid(lm(meanSubAdultSurvival  ~  year, data = df))
plot(resid_meanSubAdultSurvival  ~  year, data = df, las = 1)
abline(lm(resid_meanSubAdultSurvival  ~  year, data = df))

#  juv + subadult survival 
plot(meanJuvSubAdultSurvival ~  year, data = df, las = 1)
abline(lm(meanSubAdultSurvival  ~  year, data = df)) 
df$resid_meanJuvSubAdultSurvival <- resid(lm(meanJuvSubAdultSurvival  ~  year, data = df))
plot(resid_meanJuvSubAdultSurvival  ~  year, data = df, las = 1)
abline(lm(resid_meanJuvSubAdultSurvival  ~  year, data = df))

# adult survival 
plot(meanAdultSurvival ~  year, data = df, las = 1)
abline(lm(meanAdultSurvival  ~  year, data = df)) 
df$resid_meanAdultSurvival <- resid(lm(meanAdultSurvival  ~  year, data = df))
plot(resid_meanAdultSurvival  ~  year, data = df, las = 1)
abline(lm(resid_meanAdultSurvival  ~  year, data = df))

#------------------------- 

# Merge the data sets  year-detrended measures of the climatic and demographic variables 
df <- left_join(df, envdata)

# Set the population changes at the relative change in density and group size from July to July
# Need to make sure that the lag is correct
# The way the data is structured, it's easier to do it using the lead rather than lag, 
# as this prevents the need to rework the climate variables
density_change <- density_df %>% 
  filter(Month %in% 7) %>%
  dplyr::select(Year, Date, IndDensity_KDE, MeanGroupSize) %>% 
  mutate(DensityChange_prop = lead(IndDensity_KDE)/IndDensity_KDE, 
    GroupSizeChange_prop = lead(MeanGroupSize)/MeanGroupSize, 
    breeding_season = paste0(Year, "/", Year + 1)) %>% 
  data.frame()
density_change <- left_join(density_change, df)

# Finall want to detrend density itself and mean group size 
# Density detrend
par(mfrow = c(1,2))
density_change$year[26] <- 27 # again to line it all up
plot(IndDensity_KDE ~  year, data = density_change, las = 1)
abline(lm(IndDensity_KDE  ~  year, data = density_change)) 
density_change$resid_IndDensity_KDE <- NA
density_change$resid_IndDensity_KDE[6:26] <- resid(lm(IndDensity_KDE  ~  year, data = density_change))
plot(resid_IndDensity_KDE  ~  year, data = density_change, las = 1)
abline(lm(resid_IndDensity_KDE  ~  year, data = density_change))

# Mean group size detrend 
plot(MeanGroupSize ~  year, data = density_change, las = 1)
abline(lm(MeanGroupSize  ~  year, data = density_change)) 
density_change$resid_MeanGroupSize <- resid(lm(MeanGroupSize  ~  year, data = density_change))
plot(resid_IndDensity_KDE  ~  year, data = density_change, las = 1)
abline(lm(resid_IndDensity_KDE  ~  year, data = density_change))

# Taking stock we now have two main data sets 
density_change # This has the measures of relative population changes in group size and density
df # This has the annual dempographic rates
# These two datasets will now be used in the multiple regressions

#==========================================

# Model the ANNUAL CHANGES IN POPULATION DENSITY AND THE MEAN GROUP SIZE

#===========================================

# Note that the group size change is a slightly larger data so I will filter this first
groupsize_change <- density_change %>% 
  filter(!is.na(GroupSizeChange_prop)) %>% 
  dplyr::select(-DensityChange_prop, -IndDensity_KDE)
dim(groupsize_change) 

density_change <- density_change %>% 
  filter(!is.na(DensityChange_prop)) 
dim(density_change) 

# Standardise the variables being entered in the models 
groupsize_change$MeanGroupSize_z <- as.numeric(scale(groupsize_change$MeanGroupSize))
groupsize_change$lograin_earlysummer_z <- as.numeric(scale(groupsize_change$lograin_earlysummer))
groupsize_change$lograin_breedingseason_z <- as.numeric(scale(groupsize_change$lograin_breedingseason))
groupsize_change$lograin_twoyear_z <- as.numeric(scale(groupsize_change$lograin_twoyear))
groupsize_change$temp_summer_z <- as.numeric(scale(groupsize_change$temp_summer))
groupsize_change$temp_allyear_z <- as.numeric(scale(groupsize_change$temp_allyear))
groupsize_change$spei6_allyear_z <- as.numeric(scale(groupsize_change$spei6_allyear))
groupsize_change$spei6_twoyear_z <- as.numeric(scale(groupsize_change$spei6_twoyear))

density_change$IndDensity_KDE_z <- as.numeric(scale(density_change$IndDensity_KDE))
density_change$lograin_earlysummer_z <- as.numeric(scale(density_change$lograin_earlysummer))
density_change$lograin_breedingseason_z <- as.numeric(scale(density_change$lograin_breedingseason))
density_change$lograin_twoyear_z <- as.numeric(scale(density_change$lograin_twoyear))
density_change$temp_summer_z <- as.numeric(scale(density_change$temp_summer))
density_change$temp_allyear_z <- as.numeric(scale(density_change$temp_allyear))
density_change$spei6_allyear_z <- as.numeric(scale(density_change$spei6_allyear))
density_change$spei6_twoyear_z <- as.numeric(scale(density_change$spei6_twoyear))

# Check the collinearity among the predictors for density change
#source("VIFfunctions.R")
#corvif(density_change[, c("lograin_earlysummer", "lograin_breedingseason", "lograin_twoyear",
#                          "temp_summer", "temp_allyear", "spei6_allyear", "spei6_twoyear", 
#                          "IndDensity_KDE")])
# confirms, as expected, quite high collinearity among terms, which we need to be aware of for multiple reg.

#------------------------- 

# Model the changes in population density
  # Set up the alternative models for density change
popchange_m0 <- lm(DensityChange_prop  ~ IndDensity_KDE_z, data = density_change)
popchange_m1 <- lm(DensityChange_prop  ~ IndDensity_KDE_z + lograin_earlysummer_z, data = density_change)
popchange_m2 <- update(popchange_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
popchange_m3 <- update(popchange_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
popchange_m4 <- update(popchange_m1, ~. -lograin_earlysummer_z + temp_summer_z)
popchange_m5 <- update(popchange_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
popchange_m6 <- update(popchange_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
popchange_m7 <- update(popchange_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z)

# compare models and rank by AIC
env_terms <- c("null", "rainfall_earlysummer", "rainfall_breedingseason", "rainfall_twoyear", "temp_summer", "temp_allyear", "spei6_allyear", "spei6_twoyear")

popchange_list <- list(popchange_m0, 
                       popchange_m1, popchange_m2, popchange_m3, popchange_m4, popchange_m5,
                       popchange_m6, popchange_m7)

popchange_AICc <- sapply(popchange_list, FUN = AICc)
popchange_beta <- as.numeric(sapply(popchange_list, FUN = function(x) {signif(coef(x)[3], 3)}))
popchange_beta_se <- as.numeric(sapply(popchange_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
popchange_densbeta <- as.numeric(sapply(popchange_list, FUN = function(x) {signif(coef(x)[2], 3)}))
popchange_densbeta_se <- as.numeric(sapply(popchange_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
popchange_r2 <- sapply(popchange_list, FUN = function(x) {summary(x)$r.squared})
popchange_p <- c(NA, unlist(sapply(popchange_list, 
                                   FUN = function(x) { broom::tidy(x) %>%
                                       slice(3) %>% 
                                       pull(p.value) %>% 
                                       signif(4) })))

# summary table for population change candidate models 
popchange_modtable <- data.frame(variable = env_terms, 
                                 AICc =  popchange_AICc,
                                 beta  = popchange_beta, 
                                 beta_se  = popchange_beta_se,
                                 densbeta = popchange_densbeta, 
                                 densbeta_se = popchange_densbeta_se,
                                 r2 = popchange_r2,
                                 pval = popchange_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, densbeta, densbeta_se)

# Also modelling temperature alone so it is independent of density
summary(lm(DensityChange_prop  ~ temp_summer_z, data = density_change))
summary(lm(DensityChange_prop  ~ temp_allyear_z, data = density_change))

# And fit a multiple regression with rainfall + temp + density
popchange_m8 <- lm(DensityChange_prop  ~ IndDensity_KDE_z + lograin_breedingseason_z + temp_summer_z, 
                   data = density_change)
summary(popchange_m8)
AICc(popchange_m2) - AICc(popchange_m8)
#cor.test(density_change$lograin_breedingseason_z, density_change$temp_summer_z)

# Generate relevant plots for density change 

# plot 1: the effect of rainfall on the proportional change in population density
  pr1 <- predict_response(popchange_m2, "lograin_breedingseason_z [all]")
  # plot(pr, show_data = TRUE)
  # I also want the residuals to remove the effect of density from the raw data
  plot1 <- plot(pr1, show_residuals = TRUE) 
  # I'm going to 'highjack' the plot by getting its consituents so that I can edit the appearance
  plot1_qq <- ggplot_build(plot1) 
  #parts 1, 2, and 3, refer to the lines, points and shapes respectively an
  plot1_qq1 <- plot1_qq$data[[1]] 
  plot1_qq2 <- plot1_qq$data[[2]]
  plot1_qq3 <- plot1_qq$data[[3]] 
  # I also want to put the x variable back into it's unstandardised form 
  val1 <- density_change$lograin_breedingseason
  plot1_qq1$x <- (plot1_qq1$x*sd(val1))+mean(val1)
  plot1_qq2$x <- (plot1_qq2$x*sd(val1))+mean(val1)
  plot1_qq3$x <- (plot1_qq3$x*sd(val1))+mean(val1)

plot1 <- ggplot(data = plot1_qq1, aes(x, y)) + 
  geom_hline(yintercept = 1, linetype = 1, colour = "lightgrey") +
  geom_ribbon(data = plot1_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
              colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 1, size = 2) + 
  geom_line(data = plot1_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "ln Rainfall (mm)", y = "Change in population density") + 
  scale_y_continuous(breaks = seq(0.4, 1.6, 0.2), limits = c(0.35, 1.6)) + 
  scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                     labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
  ggplot2::annotate("text", x = 5.5, y = 1.6, label = "β = 0.21 ± 0.05, p < 0.001", size = 3.25, colour = "darkblue")

# plot 2: the effect of density on the proportional change in population density
  pr2 <- predict_response(popchange_m2, "IndDensity_KDE_z [all]")
  # plot(pr, show_data = TRUE)
  plot2 <- plot(pr2, show_residuals = TRUE) 
  plot2_qq <- ggplot_build(plot2) 
  plot2_qq1 <- plot2_qq$data[[1]] 
  plot2_qq2 <- plot2_qq$data[[2]]
  plot2_qq3 <- plot2_qq$data[[3]]
  val2 <- density_change$IndDensity_KDE
  plot2_qq1$x <- (plot2_qq1$x*sd(val2))+mean(val2)
  plot2_qq2$x <- (plot2_qq2$x*sd(val2))+mean(val2)
  plot2_qq3$x <- (plot2_qq3$x*sd(val2))+mean(val2)

plot2 <- ggplot(data = plot2_qq1, aes(x, y)) + 
  geom_hline(yintercept = 1, linetype = 1, colour = "lightgrey") +
  geom_ribbon(data = plot2_qq3, aes(ymin = ymin, ymax = ymax), 
              fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 1, size = 2) + 
  geom_line(data = plot2_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Population density\n(meerkats/km²)", 
       y = "Change in population density") + 
  scale_y_continuous(breaks = seq(0.4, 1.6, 0.2), limits = c(0.35, 1.6))  + 
  scale_x_continuous(breaks = seq(4, 18, 2), 
                     labels = c("4", "", "8", "", "12", "", "16", "")) + 
  ggplot2::annotate("text", x = 10.5, y = 1.6, label = "β = -0.16 ± 0.05, p = 0.005", size = 3.25, colour = "darkblue")

# plot 3: the effect of summer temperatures on the proportional change in population density
  pr3 <- predict_response(popchange_m4, "temp_summer_z [all]")
  plot3 <- plot(pr3, show_residuals = TRUE) 
  plot3_qq <- ggplot_build(plot3) 
  plot3_qq1 <- plot3_qq$data[[1]] 
  plot3_qq2 <- plot3_qq$data[[2]]
  plot3_qq3 <- plot3_qq$data[[3]]
  val3 <- density_change$temp_summer
  plot3_qq1$x <- (plot3_qq1$x*sd(val3))+mean(val3)
  plot3_qq2$x <- (plot3_qq2$x*sd(val3))+mean(val3)
  plot3_qq3$x <- (plot3_qq3$x*sd(val3))+mean(val3)

plot3 <- ggplot(data = plot3_qq1, aes(x, y)) + 
  geom_hline(yintercept = 1, linetype = 1, colour = "lightgrey") +
  geom_ribbon(data = plot3_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 1, size = 2) + 
  geom_line(data = plot3_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Average summer\ntemperature (°C)", 
       y = "Change in population density") + 
  scale_y_continuous(breaks = seq(0.4, 1.6, 0.2), limits = c(0.35, 1.6)) +
  scale_x_continuous(breaks = seq(32, 37, 0.5), 
                     labels = c("32", "", "33", "", "34", "", "35", "",
                                "36", "", "37")) + 
  ggplot2::annotate("text", x = 34.5, y = 1.6, label = "β = -0.08 ± 0.07, p = 0.28", size = 3.25, colour = "darkblue")

# View all the density change plots together
plot2_final <- plot2 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y= element_blank())
plot3_final <- plot3 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y= element_blank())
(plot1 | plot3_final | plot2_final)

#------------------------- 

# Model the changes in group size
  # Set up the alternative models for group size change
groupsizechange_m0 <- lm(GroupSizeChange_prop  ~ MeanGroupSize_z, data = groupsize_change)
groupsizechange_m1 <- lm(GroupSizeChange_prop  ~ MeanGroupSize_z + lograin_earlysummer_z,  data = groupsize_change)
groupsizechange_m2 <- update(groupsizechange_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
groupsizechange_m3 <- update(groupsizechange_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
groupsizechange_m4 <- update(groupsizechange_m1, ~. -lograin_earlysummer_z + temp_summer_z)
groupsizechange_m5 <- update(groupsizechange_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
groupsizechange_m6 <- update(groupsizechange_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
groupsizechange_m7 <- update(groupsizechange_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z) 

groupsizechange_list <- list(groupsizechange_m0, groupsizechange_m1, groupsizechange_m2,
                             groupsizechange_m3, groupsizechange_m4, groupsizechange_m5,
                             groupsizechange_m6, groupsizechange_m7)

groupsizechange_AICc <- sapply(groupsizechange_list, FUN = AICc)
groupsizechange_beta <- as.numeric(sapply(groupsizechange_list, FUN = function(x) {signif(coef(x)[3], 3)}))
groupsizechange_beta_se <- as.numeric(sapply(groupsizechange_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
groupsizechange_gsbeta <- as.numeric(sapply(groupsizechange_list, FUN = function(x) {signif(coef(x)[2], 3)}))
groupsizechange_gsbeta_se <- as.numeric(sapply(groupsizechange_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
groupsizechange_r2 <- sapply(groupsizechange_list, FUN = function(x) {summary(x)$r.squared})
groupsizechange_p <- c(NA, unlist(sapply(groupsizechange_list, 
                                         FUN = function(x) { broom::tidy(x) %>%
                                             slice(3) %>% 
                                             pull(p.value) %>% 
                                             signif(4) })))

# summary table for population change models
groupsizechange_modtable <- data.frame(variable = env_terms, 
                                       AICc =  groupsizechange_AICc,
                                       beta  = groupsizechange_beta, 
                                       beta_se  = groupsizechange_beta_se,
                                       gsbeta = groupsizechange_gsbeta, 
                                       gsbeta_se = groupsizechange_gsbeta_se,
                                       r2 = groupsizechange_r2,
                                       pval = groupsizechange_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se)

# Multiple regression with rainfall + temp + group size
groupsizechange_m8 <- lm(GroupSizeChange_prop  ~ MeanGroupSize_z + lograin_breedingseason_z + temp_allyear_z, 
                         data = groupsize_change)
summary(groupsizechange_m8)
AICc(groupsizechange_m8) - AICc(groupsizechange_m2)
cor.test(groupsize_change$lograin_breedingseason_z, groupsize_change$temp_allyear_z)

# Generate relevant plots for group size change

# plot 1: the effect of rainfall on the proportional change in group size 
  pr4 <- predict_response(groupsizechange_m2, "lograin_breedingseason_z [all]")
  plot4 <- plot(pr4, show_residuals = TRUE) 
  plot4_qq <- ggplot_build(plot4) 
  plot4_qq1 <- plot4_qq$data[[1]] 
  plot4_qq2 <- plot4_qq$data[[2]]
  plot4_qq3 <- plot4_qq$data[[3]] 
  val4 <- groupsize_change$lograin_breedingseason
  plot4_qq1$x <- (plot4_qq1$x*sd(val4))+mean(val4)
  plot4_qq2$x <- (plot4_qq2$x*sd(val4))+mean(val4)
  plot4_qq3$x <- (plot4_qq3$x*sd(val4))+mean(val4)

  plot4 <- ggplot(data = plot4_qq1, aes(x, y)) + 
    geom_hline(yintercept = 1, linetype = 1, colour = "lightgrey") +
    geom_ribbon(data = plot4_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, colour = "black", linetype = 2) +
    geom_point(shape = 1, stroke = 1, size = 2) + 
    geom_line(data = plot4_qq2, linewidth = 1) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = "black", size = 9.5), 
          axis.title = element_text(colour = "black", size = 10.5)) +
    labs(title = NULL, x = "ln Rainfall (mm)", y = "Change in mean group size") + 
    scale_y_continuous(breaks = seq(0.6, 1.6, 0.2), limits = c(0.45, 1.7)) + 
    scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                       labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
    ggplot2::annotate("text", x = 5.5, y = 1.7, label = "β = 0.14 ± 0.04, p = 0.004", size = 3.25, colour = "darkblue")

# plot 2: the effect of group size on the proportional change in group size 
  pr5 <- predict_response(groupsizechange_m2, "MeanGroupSize_z [all]")
  plot5 <- plot(pr5, show_residuals = TRUE) 
  plot5_qq <- ggplot_build(plot5) 
  plot5_qq1 <- plot5_qq$data[[1]] 
  plot5_qq2 <- plot5_qq$data[[2]]
  plot5_qq3 <- plot5_qq$data[[3]]
  val5 <- groupsize_change$MeanGroupSize
  plot5_qq1$x <- (plot5_qq1$x*sd(val5))+mean(val5)
  plot5_qq2$x <- (plot5_qq2$x*sd(val5))+mean(val5)
  plot5_qq3$x <- (plot5_qq3$x*sd(val5))+mean(val5)

  plot5 <- ggplot(data = plot5_qq1, aes(x, y)) + 
    geom_hline(yintercept = 1, linetype = 1, colour = "lightgrey") +
    geom_ribbon(data = plot5_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, colour = "black", linetype = 2) +
    geom_point(shape = 1, stroke = 1, size = 2) + 
    geom_line(data = plot5_qq2, linewidth = 1) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = "black", size = 9.5), 
          axis.title = element_text(colour = "black", size = 10.5)) +
    labs(title = NULL, x = "Mean group size", y = "Change in mean group size") +
    scale_y_continuous(breaks = seq(0.6, 1.6, 0.2), limits = c(0.45, 1.7)) + 
    scale_x_continuous(breaks = seq(8, 24, 2), 
                       labels = c("8", "", "12", "", "16", "", "20", "", "24")) + 
    ggplot2::annotate("text", x = 16, y = 1.7, label = "β = -0.16 ± 0.04, p = 0.001", size = 3.25, colour = "darkblue")

# plot 3: the effect of yearly temperatures on the proportional change in population density
  pr6 <- predict_response(groupsizechange_m5, "temp_allyear_z [all]")
  plot6 <- plot(pr6, show_residuals = TRUE) 
  plot6_qq <- ggplot_build(plot6) 
  plot6_qq1 <- plot6_qq$data[[1]] 
  plot6_qq2 <- plot6_qq$data[[2]]
  plot6_qq3 <- plot6_qq$data[[3]]
  val6 <- groupsize_change$temp_allyear
  plot6_qq1$x <- (plot6_qq1$x*sd(val6))+mean(val6)
  plot6_qq2$x <- (plot6_qq2$x*sd(val6))+mean(val6)
  plot6_qq3$x <- (plot6_qq3$x*sd(val6))+mean(val6)

  plot6 <- ggplot(data = plot6_qq1, aes(x, y)) + 
    geom_hline(yintercept = 1, linetype = 1, colour = "lightgrey") +
    geom_ribbon(data = plot6_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
                colour = "black", linetype = 2) +
    geom_point(shape = 1, stroke = 1, size = 2) + 
    geom_line(data = plot6_qq2, linewidth = 1) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = "black", size = 9.5), 
          axis.title = element_text(colour = "black", size = 10.5)) +
    labs(title = NULL, 
         x = "Average yearly\ntemperature (°C)", 
         y = "Change in mean group size") + 
    scale_y_continuous(breaks = seq(0.6, 1.6, 0.2), limits = c(0.45, 1.7)) + 
    scale_x_continuous(breaks = seq(28, 32, 0.5), 
                       labels = c("28", "", "29", "", "30", "", "31", "", "32")) + 
    ggplot2::annotate("text", x = 30, y = 1.7, label = "β = -0.12 ± 0.06, p = 0.11", size = 3.25, colour = "darkblue")

# view all the group size change plots together
plot5_final <- plot5 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y= element_blank())
plot6_final <- plot6 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y= element_blank())
(plot4 | plot6_final | plot5_final)

# and now view add the two on top of each other
# This is figure 4 in the main MS
(plot1 + labs(tag = "A") + theme(plot.tag = element_text(size = 14)) | plot3_final | plot2_final) /
  (plot4 + labs(tag = "B") + theme(plot.tag = element_text(size = 14)) | plot6_final | plot5_final)

#==========================================

# Model the EFFECTS OF CLIMATE ON THE VARIOUS DEMOGRAPHIC RATES 

  # (i)    dominant female pregnancy rates 
  # (ii)   dominant female number of litters
  # (iii)  pups emerged  
  # (iv)   pups recruited
  # (v)    pup survival 
  # (vi)   non-pup survival 
  # (vii)  juvenile + sub-adult survival
  # (viii) adult survival 
  
#===========================================

#==========================================

# (i) Model the NO. OF DOMINANT FEMALE PREGNANCIES 

#===========================================

  # set up the alternative models for no. dom female pregnancies
dompreg_m0 <- lm(meanDomPregnancies  ~ MeanGroupSize_z, data = groupsize_change)
dompreg_m1 <- lm(meanDomPregnancies  ~ MeanGroupSize_z + lograin_earlysummer_z, data = groupsize_change)
dompreg_m2 <- update(dompreg_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
dompreg_m3 <- update(dompreg_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
dompreg_m4 <- update(dompreg_m1, ~. -lograin_earlysummer_z + temp_summer_z)
dompreg_m5 <- update(dompreg_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
dompreg_m6 <- update(dompreg_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
dompreg_m7 <- update(dompreg_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z)

dompreg_list <- list(dompreg_m0, 
                     dompreg_m1, dompreg_m2, dompreg_m3, dompreg_m4, dompreg_m5,
                     dompreg_m6, dompreg_m7)

dompreg_AICc <- sapply(dompreg_list, FUN = AICc)
dompreg_beta <- as.numeric(sapply(dompreg_list, FUN = function(x) {signif(coef(x)[3], 3)}))
dompreg_beta_se <- as.numeric(sapply(dompreg_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
dompreg_gsbeta <- as.numeric(sapply(dompreg_list, FUN = function(x) {signif(coef(x)[2], 3)}))
dompreg_gsbeta_se <- as.numeric(sapply(dompreg_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
dompreg_r2 <- sapply(dompreg_list, FUN = function(x) {summary(x)$r.squared})
dompreg_p <- c(NA, unlist(sapply(dompreg_list, 
                                 FUN = function(x) { broom::tidy(x) %>%
                                     slice(3) %>% 
                                     pull(p.value) %>% 
                                     signif(4) })))

# summary table for no dom female pregnancies models 
dompreg_modtable <- data.frame(variable = env_terms, 
                               AICc =  dompreg_AICc,
                               beta  = dompreg_beta, 
                               beta_se  = dompreg_beta_se,
                               gsbeta = dompreg_gsbeta, 
                               gsbeta_se = dompreg_gsbeta_se,
                               r2 = dompreg_r2,
                               pval = dompreg_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se)

# Model output for the best model
summary(dompreg_list[[5]])  

# Multiple regression with rainfall + temp + group size
dompreg_m8 <- lm(meanDomPregnancies  ~ MeanGroupSize_z + lograin_breedingseason_z + temp_summer_z, 
                         data = groupsize_change)
summary(dompreg_m8)
AICc(dompreg_m8) - AICc(dompreg_m4)

# Generate relevant plots  -------------------

# plot 1: the effect of rainfall on no. dom pregnancies
pr7 <- predict_response(dompreg_m2, "lograin_breedingseason_z [all]")
  plot7 <- plot(pr7, show_residuals = TRUE) 
  plot7_qq <- ggplot_build(plot7) 
  plot7_qq1 <- plot7_qq$data[[1]] 
  plot7_qq2 <- plot7_qq$data[[2]]
  plot7_qq3 <- plot7_qq$data[[3]] 
  val7 <- groupsize_change$lograin_breedingseason
  plot7_qq1$x <- (plot7_qq1$x*sd(val7))+mean(val7)
  plot7_qq2$x <- (plot7_qq2$x*sd(val7))+mean(val7)
  plot7_qq3$x <- (plot7_qq3$x*sd(val7))+mean(val7)

plot7 <- ggplot(data = plot7_qq1, aes(x, y)) + 
  geom_ribbon(data = plot7_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot7_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "ln Rainfall (mm)", y = "Dominant female pregnancies") + 
  scale_y_continuous(breaks = seq(2, 4.5, 0.5), 
                     limits = c(2.1, 4.5)) + 
  scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                     labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
  ggplot2::annotate("text", x = 5.5, y = 4.5, label = "β = 0.14 ± 0.09, p = 0.14", size = 3.25, colour = "darkblue")

# plot 2: the effect of group size on pup emergence
pr8 <- predict_response(dompreg_m4, "MeanGroupSize_z [all]")
  plot8 <- plot(pr8, show_residuals = TRUE) 
  plot8_qq <- ggplot_build(plot8) 
  plot8_qq1 <- plot8_qq$data[[1]] 
  plot8_qq2 <- plot8_qq$data[[2]]
  plot8_qq3 <- plot8_qq$data[[3]]
  val8 <- groupsize_change$MeanGroupSize
  plot8_qq1$x <- (plot8_qq1$x*sd(val8))+mean(val8)
  plot8_qq2$x <- (plot8_qq2$x*sd(val8))+mean(val8)
  plot8_qq3$x <- (plot8_qq3$x*sd(val8))+mean(val8)

plot8 <- ggplot(data = plot8_qq1, aes(x, y)) + 
  geom_ribbon(data = plot8_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
              colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot8_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x =  "Mean group size", y = "Dominant female pregnancies") + 
  scale_y_continuous(breaks = seq(2, 4.5, 0.5), 
                     limits = c(2.1, 4.5)) + 
  scale_x_continuous(breaks = seq(8, 24, 2), 
                     labels = c("8", "", "12", "", "16", "", "20", "", "24")) + 
  ggplot2::annotate("text", x = 16, y = 4.5, label = "β = -0.21 ± 0.09, p = 0.029",
                    size = 3.25, colour = "darkblue")

# plot 3: the effect of summer temperatures on number of dominant pregnancies
pr9 <- predict_response(dompreg_m4, "temp_summer_z [all]")
  plot9 <- plot(pr9, show_residuals = TRUE) 
  plot9_qq <- ggplot_build(plot9) 
  plot9_qq1 <- plot9_qq$data[[1]] 
  plot9_qq2 <- plot9_qq$data[[2]]
  plot9_qq3 <- plot9_qq$data[[3]]
  val9 <- groupsize_change$temp_summer
  plot9_qq1$x <- (plot9_qq1$x*sd(val9))+mean(val9)
  plot9_qq2$x <- (plot9_qq2$x*sd(val9))+mean(val9)
  plot9_qq3$x <- (plot9_qq3$x*sd(val9))+mean(val9)

plot9 <- ggplot(data = plot9_qq1, aes(x, y)) + 
  geom_ribbon(data = plot9_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
              colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot9_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Average summer\ntemperature (°C)", y = "Dominant female pregnancies") + 
  scale_y_continuous(breaks = seq(2, 4.5, 0.5), 
                     limits = c(2.1, 4.5)) + 
  scale_x_continuous(breaks = seq(30, 37, 1), 
                     labels = c("", "31", "", "33", "", "35", "", "37")) + 
  ggplot2::annotate("text", x = 33.5, y = 4.5, label = "β = -0.17 ± 0.09, p = 0.086",
                    size = 3.25, colour = "darkblue")

(plot7 | plot9 | plot8)

#------------------------

# Working with DETRENDED predictors and response
# set up the alternative models detrended no. dom female pregnancies ~ detrended predictors
# still need to scale to produce comparable coefficients
groupsize_change$resid_MeanGroupSize_z <- as.numeric(scale(groupsize_change$resid_MeanGroupSize))
groupsize_change$resid_lograin_earlysummer_z <- as.numeric(scale(groupsize_change$resid_lograin_earlysummer))
groupsize_change$resid_lograin_breedingseason_z <- as.numeric(scale(groupsize_change$resid_lograin_breedingseason))
groupsize_change$resid_lograin_twoyear_z <- as.numeric(scale(groupsize_change$resid_lograin_twoyear))
groupsize_change$resid_temp_summer_Z <- as.numeric(scale(groupsize_change$resid_temp_summer))
groupsize_change$resid_temp_allyear_z <- as.numeric(scale(groupsize_change$resid_temp_allyear))
groupsize_change$resid_spei6_allyear_z <- as.numeric(scale(groupsize_change$resid_spei6_allyear))
groupsize_change$resid_spei6_twoyear_z <- as.numeric(scale(groupsize_change$resid_spei6_twoyear))

detrend_dompreg_m0 <- lm(resid_meanDomPregnancies  ~ resid_MeanGroupSize_z, data = groupsize_change)
detrend_dompreg_m1 <- lm(resid_meanDomPregnancies  ~ resid_MeanGroupSize_z +  resid_lograin_earlysummer_z, data = groupsize_change)
detrend_dompreg_m2 <- update(detrend_dompreg_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_breedingseason_z)
detrend_dompreg_m3 <- update(detrend_dompreg_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_twoyear_z)
detrend_dompreg_m4 <- update(detrend_dompreg_m1, ~. -resid_lograin_earlysummer_z + resid_temp_summer_Z)
detrend_dompreg_m5 <- update(detrend_dompreg_m1, ~. -resid_lograin_earlysummer_z + resid_temp_allyear_z)
detrend_dompreg_m6 <- update(detrend_dompreg_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_allyear_z)
detrend_dompreg_m7 <- update(detrend_dompreg_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_twoyear_z)

detrend_dompreg_list <- list(detrend_dompreg_m0, 
                             detrend_dompreg_m1, detrend_dompreg_m2, detrend_dompreg_m3, 
                             detrend_dompreg_m4, detrend_dompreg_m5, detrend_dompreg_m6, detrend_dompreg_m7)

detrend_dompreg_AICc <- sapply(detrend_dompreg_list, FUN = AICc)
detrend_dompreg_beta <- as.numeric(sapply(detrend_dompreg_list, FUN = function(x) {signif(coef(x)[3], 3)}))
detrend_dompreg_beta_se <- as.numeric(sapply(detrend_dompreg_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
detrend_dompreg_gsbeta <- as.numeric(sapply(detrend_dompreg_list, FUN = function(x) {signif(coef(x)[2], 3)}))
detrend_dompreg_gsbeta_se <- as.numeric(sapply(detrend_dompreg_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
detrend_dompreg_r2 <- sapply(detrend_dompreg_list, FUN = function(x) {summary(x)$r.squared})
detrend_dompreg_p <- c(NA, unlist(sapply(detrend_dompreg_list, 
                                         FUN = function(x) { broom::tidy(x) %>%
                                             slice(3) %>% 
                                             pull(p.value) %>% 
                                             signif(4) })))

# summary table for population change 
detrend_dompreg_modtable <- data.frame(variable = env_terms, 
                                       AICc =  detrend_dompreg_AICc,
                                       beta  = detrend_dompreg_beta, 
                                       beta_se  = detrend_dompreg_beta_se,
                                       gsbeta = detrend_dompreg_gsbeta, 
                                       gsbeta_se = detrend_dompreg_gsbeta_se,
                                       r2 = detrend_dompreg_r2,
                                       pval = detrend_dompreg_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se) 

# Model output for the best model
summary(detrend_dompreg_list[[1]])

#==========================================

# (ii) Model the NO. OF DOMINANT FEMALE LITTERS 

#===========================================

  # set up the alternative models for dom female litters
domlitt_m0 <- lm(meanDomLitters  ~ MeanGroupSize_z, data = groupsize_change)
domlitt_m1 <- lm(meanDomLitters  ~ MeanGroupSize_z + lograin_earlysummer_z, data = groupsize_change)
domlitt_m2 <- update(domlitt_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
domlitt_m3 <- update(domlitt_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
domlitt_m4 <- update(domlitt_m1, ~. -lograin_earlysummer_z + temp_summer_z)
domlitt_m5 <- update(domlitt_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
domlitt_m6 <- update(domlitt_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
domlitt_m7 <- update(domlitt_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z)

domlitt_list <- list(domlitt_m0, 
                     domlitt_m1, domlitt_m2, domlitt_m3, domlitt_m4, domlitt_m5,
                     domlitt_m6, domlitt_m7)

domlitt_AICc <- sapply(domlitt_list, FUN = AICc)
domlitt_beta <- as.numeric(sapply(domlitt_list, FUN = function(x) {signif(coef(x)[3], 3)}))
domlitt_beta_se <- as.numeric(sapply(domlitt_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
domlitt_gsbeta <- as.numeric(sapply(domlitt_list, FUN = function(x) {signif(coef(x)[2], 3)}))
domlitt_gsbeta_se <- as.numeric(sapply(domlitt_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
domlitt_r2 <- sapply(domlitt_list, FUN = function(x) {summary(x)$r.squared})
domlitt_p <- c(NA, unlist(sapply(domlitt_list, 
                                 FUN = function(x) { broom::tidy(x) %>%
                                     slice(3) %>% 
                                     pull(p.value) %>% 
                                     signif(4) })))

# summary table for no. dom female litters models
domlitt_modtable <- data.frame(variable = env_terms, 
                               AICc =  domlitt_AICc,
                               beta  = domlitt_beta, 
                               beta_se  = domlitt_beta_se,
                               gsbeta = domlitt_gsbeta, 
                               gsbeta_se = domlitt_gsbeta_se,
                               r2 = domlitt_r2,
                               pval = domlitt_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se)

# Model output for the best model
summary(domlitt_list[[3]])  

# Multiple regression with rainfall + temp + group size
domlitt_m8 <- lm(meanDomLitters  ~ MeanGroupSize_z + lograin_breedingseason_z + temp_allyear_z, 
                 data = groupsize_change)
summary(domlitt_m8)
AICc(domlitt_m8) - AICc(domlitt_m2)

# Generate relevant plots for no. dom litter -------------------

# plot 1: the effect of rainfall on number of litters
pr10 <- predict_response(domlitt_m2, "lograin_breedingseason_z [all]")
  plot10 <- plot(pr10, show_residuals = TRUE) 
  plot10_qq <- ggplot_build(plot10) 
  plot10_qq1 <- plot10_qq$data[[1]] 
  plot10_qq2 <- plot10_qq$data[[2]]
  plot10_qq3 <- plot10_qq$data[[3]] 
  val10 <- groupsize_change$lograin_breedingseason
  plot10_qq1$x <- (plot10_qq1$x*sd(val10))+mean(val10)
  plot10_qq2$x <- (plot10_qq2$x*sd(val10))+mean(val10)
  plot10_qq3$x <- (plot10_qq3$x*sd(val10))+mean(val10)

plot10 <- ggplot(data = plot10_qq1, aes(x, y)) + 
  geom_ribbon(data = plot10_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot10_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "ln Rainfall (mm)", y = "Dominant female litters born") + 
  scale_y_continuous(breaks = seq(1, 3, 0.5), 
                     limits = c(1, 3.21)) + 
  scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                     labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
  ggplot2::annotate("text", x = 5.5, y = 3.2, label = "β = 0.28 ± 0.08, p = 0.002", size = 3.25, colour = "darkblue")

# plot 2: the effect of group size on number of litters
pr11 <- predict_response(domlitt_m2, "MeanGroupSize_z [all]")
  plot11 <- plot(pr11, show_residuals = TRUE) 
  plot11_qq <- ggplot_build(plot11) 
  plot11_qq1 <- plot11_qq$data[[1]] 
  plot11_qq2 <- plot11_qq$data[[2]]
  plot11_qq3 <- plot11_qq$data[[3]]
  val11 <- groupsize_change$MeanGroupSize
  plot11_qq1$x <- (plot11_qq1$x*sd(val11))+mean(val11)
  plot11_qq2$x <- (plot11_qq2$x*sd(val11))+mean(val11)
  plot11_qq3$x <- (plot11_qq3$x*sd(val11))+mean(val11)

plot11 <- ggplot(data = plot11_qq1, aes(x, y)) + 
  geom_ribbon(data = plot11_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
              colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot11_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x =  "Mean group size",  y = "Dominant female litters born") + 
  scale_y_continuous(breaks = seq(1, 3.5, 0.5), 
                     limits = c(1, 3.21)) + 
  scale_x_continuous(breaks = seq(8, 24, 2), 
                     labels = c("8", "", "12", "", "16", "", "20", "", "24")) + 
  ggplot2::annotate("text", x = 16, y = 3.2, label = "β = -0.05 ± 0.09, p = 0.47",
                    size = 3.25, colour = "darkblue")

# plot 3: the effect of summer temperatures on number of litters emergence
pr12 <- predict_response(domlitt_m4, "temp_summer_z [all]")
  plot12 <- plot(pr12, show_residuals = TRUE) 
  plot12_qq <- ggplot_build(plot12) 
  plot12_qq1 <- plot12_qq$data[[1]] 
  plot12_qq2 <- plot12_qq$data[[2]]
  plot12_qq3 <- plot12_qq$data[[3]]
  val12 <- groupsize_change$temp_summer
  plot12_qq1$x <- (plot12_qq1$x*sd(val12))+mean(val12)
  plot12_qq2$x <- (plot12_qq2$x*sd(val12))+mean(val12)
  plot12_qq3$x <- (plot12_qq3$x*sd(val12))+mean(val12)

plot12 <- ggplot(data = plot12_qq1, aes(x, y)) + 
  geom_line(data = plot12_qq3, aes(y = ymin), colour = "black", linetype = 2) +
  geom_line(data = plot12_qq3, aes(y = ymax), colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot12_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Average summer\ntemperature (°C)", y = "Dominant female litters born") + 
  scale_y_continuous(breaks = seq(1, 3, 0.5), 
                     limits = c(1, 3.21)) + 
  scale_x_continuous(breaks = seq(30, 37, 1), 
                     labels = c("", "31", "", "33", "", "35", "", "37")) + 
  ggplot2::annotate("text", x = 33.5, y = 3.2, label = "β = -0.23 ± 0.09, p = 0.022",
                    size = 3.25, colour = "darkblue")

(plot10 | plot12 | plot11)

#--------------------
# Working with DETRENDED predictors and response
# set up the alternative models detrended no. dom female litters ~ detrended predictors
detrend_domlitt_m0 <- lm(resid_meanDomLitters  ~ resid_MeanGroupSize_z, data = groupsize_change)
detrend_domlitt_m1 <- lm(resid_meanDomLitters  ~ resid_MeanGroupSize_z +  resid_lograin_earlysummer_z, 
                         data = groupsize_change)
detrend_domlitt_m2 <- update(detrend_domlitt_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_breedingseason_z)
detrend_domlitt_m3 <- update(detrend_domlitt_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_twoyear_z)
detrend_domlitt_m4 <- update(detrend_domlitt_m1, ~. -resid_lograin_earlysummer_z + resid_temp_summer_Z)
detrend_domlitt_m5 <- update(detrend_domlitt_m1, ~. -resid_lograin_earlysummer_z + resid_temp_allyear_z)
detrend_domlitt_m6 <- update(detrend_domlitt_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_allyear_z)
detrend_domlitt_m7 <- update(detrend_domlitt_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_twoyear_z)

detrend_domlitt_list <- list(detrend_domlitt_m0, 
                             detrend_domlitt_m1, detrend_domlitt_m2, detrend_domlitt_m3, 
                             detrend_domlitt_m4, detrend_domlitt_m5, detrend_domlitt_m6, detrend_domlitt_m7)

detrend_domlitt_AICc <- sapply(detrend_domlitt_list, FUN = AICc)
detrend_domlitt_beta <- as.numeric(sapply(detrend_domlitt_list, FUN = function(x) {signif(coef(x)[3], 3)}))
detrend_domlitt_beta_se <- as.numeric(sapply(detrend_domlitt_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
detrend_domlitt_gsbeta <- as.numeric(sapply(detrend_domlitt_list, FUN = function(x) {signif(coef(x)[2], 3)}))
detrend_domlitt_gsbeta_se <- as.numeric(sapply(detrend_domlitt_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
detrend_domlitt_r2 <- sapply(detrend_domlitt_list, FUN = function(x) {summary(x)$r.squared})
detrend_domlitt_p <- c(NA, unlist(sapply(detrend_domlitt_list, 
                                         FUN = function(x) { broom::tidy(x) %>%
                                             slice(3) %>% 
                                             pull(p.value) %>% 
                                             signif(4) })))

# summary table for population change 
detrend_domlitt_modtable <- data.frame(variable = env_terms, 
                                       AICc =  detrend_domlitt_AICc,
                                       beta  = detrend_domlitt_beta, 
                                       beta_se  = detrend_domlitt_beta_se,
                                       gsbeta = detrend_domlitt_gsbeta, 
                                       gsbeta_se = detrend_domlitt_gsbeta_se,
                                       r2 = detrend_domlitt_r2,
                                       pval = detrend_domlitt_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se) 

# Model output for the best model
summary(detrend_domlitt_list[[3]])

#--------------------

# Make the plot for supplementary of dom pregnancies and dom litters
# put on top of each other
(plot7 | plot9 | plot8) / 
  (plot10 | plot12 | plot11)

plot7_final <- plot7 + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot8_final <- plot8 + 
  theme(axis.text = element_blank(), 
        axis.title= element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot9_final <- plot9 + 
  theme(axis.text = element_blank(), 
        axis.title= element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot10_final <- plot10 + 
  theme(plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot11_final <- plot11 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot12_final <- plot12 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))

(plot7_final | plot9_final | plot8_final) /
  (plot10_final | plot12_final | plot11_final)

#==========================================

# (iii) Model the NO. OF PUPS EMERGED 

#===========================================

# set up the alternative models for pup emergence (groups that survive the year)
pupemer_m0 <- lm(meanPupsEmerged  ~ MeanGroupSize_z + I(MeanGroupSize_z^2), data = groupsize_change)
pupemer_m1 <- lm(meanPupsEmerged  ~ MeanGroupSize_z + I(MeanGroupSize_z^2) + lograin_earlysummer_z, 
                 data = groupsize_change)
pupemer_m2 <- update(pupemer_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
pupemer_m3 <- update(pupemer_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
pupemer_m4 <- update(pupemer_m1, ~. -lograin_earlysummer_z + temp_summer_z)
pupemer_m5 <- update(pupemer_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
pupemer_m6 <- update(pupemer_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
pupemer_m7 <- update(pupemer_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z)

pupemer_list <- list(pupemer_m0, 
                     pupemer_m1, pupemer_m2, pupemer_m3, pupemer_m4, pupemer_m5,
                     pupemer_m6, pupemer_m7)

pupemer_AICc <- sapply(pupemer_list, FUN = AICc)
pupemer_beta <- as.numeric(sapply(pupemer_list, FUN = function(x) {signif(coef(x)[4], 3)}))
pupemer_beta_se <- as.numeric(sapply(pupemer_list, FUN = function(x) {signif(summary(x)$coef[,2][4], 3)}))
pupemer_gsbeta <- as.numeric(sapply(pupemer_list, FUN = function(x) {signif(coef(x)[2], 3)}))
pupemer_gsbeta_se <- as.numeric(sapply(pupemer_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
pupemer_gs2beta <- as.numeric(sapply(pupemer_list, FUN = function(x) {signif(coef(x)[3], 3)}))
pupemer_gs2beta_se <- as.numeric(sapply(pupemer_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
pupemer_r2 <- sapply(pupemer_list, FUN = function(x) {summary(x)$r.squared})
pupemer_p <- c(NA, unlist(sapply(pupemer_list, 
                                 FUN = function(x) { broom::tidy(x) %>%
                                     slice(4) %>% 
                                     pull(p.value) %>% 
                                     signif(4) })))

# summary table for pup emergence 
pupemer_modtable <- data.frame(variable = env_terms, 
                               AICc =  pupemer_AICc,
                               beta  = pupemer_beta, 
                               beta_se  = pupemer_beta_se,
                               gsbeta = pupemer_gsbeta, 
                               gsbeta_se = pupemer_gsbeta_se,
                               gs2beta = pupemer_gs2beta, 
                               gs2beta_se = pupemer_gs2beta_se,
                               r2 = pupemer_r2,
                               pval = pupemer_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se, gs2beta, gs2beta_se)

# Model output for the best model
summary(pupemer_list[[3]])

# Multiple regression with rainfall + temp + group size
pupemer_m8 <- lm(meanPupsEmerged  ~ MeanGroupSize_z + I(MeanGroupSize_z^2) + lograin_breedingseason_z + temp_summer_z, 
                 data = groupsize_change)
summary(pupemer_m8)
AICc(pupemer_m8) - AICc(pupemer_m2)

# Generate relevant plots for no. pups emerged -------------------

# plot 1: the effect of rainfall on pup emergence
pr13 <- predict_response(pupemer_m2, "lograin_breedingseason_z [all]")
  plot13 <- plot(pr13, show_residuals = TRUE) 
  plot13_qq <- ggplot_build(plot13) 
  plot13_qq1 <- plot13_qq$data[[1]] 
  plot13_qq2 <- plot13_qq$data[[2]]
  plot13_qq3 <- plot13_qq$data[[3]] 
  val13 <- groupsize_change$lograin_breedingseason
  plot13_qq1$x <- (plot13_qq1$x*sd(val13))+mean(val13)
  plot13_qq2$x <- (plot13_qq2$x*sd(val13))+mean(val13)
  plot13_qq3$x <- (plot13_qq3$x*sd(val13))+mean(val13)

plot13 <- ggplot(data = plot13_qq1, aes(x, y)) + 
  geom_ribbon(data = plot13_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot13_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "ln Rainfall (mm)", y = "Pups emerged per group") + 
  scale_y_continuous(breaks = seq(4, 20, 2), 
                     labels = c("", "6", "", "10", "", "14", "", "18", ""), 
                     limits = c(3.8, 17)) + 
  scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                     labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
  ggplot2::annotate("text", x = 5.5, y = 16.5, label = "β = 1.60 ± 0.35, p < 0.001", 
                    size = 3, colour = "darkblue")

# plot 2: the effect of group size on pup emergence
pr14 <- predict_response(pupemer_m2, "MeanGroupSize_z [all]")
  plot14 <- plot(pr14, show_residuals = TRUE) 
  plot14_qq <- ggplot_build(plot14) 
  plot14_qq1 <- plot14_qq$data[[1]] 
  plot14_qq2 <- plot14_qq$data[[2]]
  plot14_qq3 <- plot14_qq$data[[3]]
  val14 <- groupsize_change$MeanGroupSize
  # edit where I want ymax line to stop
  extra <- predict_response(pupemer_m2, "MeanGroupSize_z [-1.5745, -1.5745]")
  plot14_qq3 <- bind_rows(ggplot_build(plot(extra))$data[[2]][1,], plot14_qq3)
  # edit where I want main line to stop 
  extra <- predict_response(pupemer_m2, "MeanGroupSize_z [-1.8925, -1.8925]")
  plot14_qq2 <- bind_rows(ggplot_build(plot(extra))$data[[1]][1,], plot14_qq2[-1,])
  plot14_qq1$x <- (plot14_qq1$x*sd(val14))+mean(val14)
  plot14_qq2$x <- (plot14_qq2$x*sd(val14))+mean(val14)
  plot14_qq3$x <- (plot14_qq3$x*sd(val14))+mean(val14)

plot14 <- ggplot(data = plot14_qq1, aes(x, y)) + 
  geom_line(data = plot14_qq3, aes(y = ymin), colour = "black", linetype = 2) +
  geom_line(data = plot14_qq3, aes(y = ymax), colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot14_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Mean group size", y = "Pups emerged per group") + 
  scale_y_continuous(breaks = seq(4, 20, 2), 
                     labels = c("" ,"6", "", "10", "", "14", "", "18", ""), 
                     limits = c(3.8, 17)) + 
  scale_x_continuous(breaks = seq(8, 24, 2), 
                     labels = c("8", "", "12", "", "16", "", "20", "", "24")) + 
  ggplot2::annotate("text", x = 16, y = 16.5, 
                    label = expression(β[quad]~"= 1.25 ± 0.30, p < 0.001"),
                    size = 3, colour = "darkblue")

# plot 3: the effect of summer temperatures on pup emergence
pr15 <- predict_response(pupemer_m4, "temp_summer_z [all]")
  plot15 <- plot(pr15, show_residuals = TRUE) 
  plot15_qq <- ggplot_build(plot15) 
  plot15_qq1 <- plot15_qq$data[[1]] 
  plot15_qq2 <- plot15_qq$data[[2]]
  plot15_qq3 <- plot15_qq$data[[3]]
  val15 <- groupsize_change$temp_summer
  plot15_qq1$x <- (plot15_qq1$x*sd(val15))+mean(val15)
  plot15_qq2$x <- (plot15_qq2$x*sd(val15))+mean(val15)
  plot15_qq3$x <- (plot15_qq3$x*sd(val15))+mean(val15)

plot15 <- ggplot(data = plot15_qq1, aes(x, y)) + 
  geom_ribbon(data = plot15_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
              colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot15_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Average summer\ntemperature (°C)", y = "Pups emerged per group") + 
  scale_y_continuous(breaks = seq(4, 20, 2), 
                     labels = c("" ,"6", "", "10", "", "14", "", "18", ""), 
                     limits = c(3.8, 17)) + 
  scale_x_continuous(breaks = seq(30, 37, 1), 
                     labels = c("", "31", "", "33", "", "35", "", "37")) + 
  ggplot2::annotate("text", x = 33.5, y = 16.5, label = "β = -1.17 ± 0.45, p = 0.017",
                    size = 3, colour = "darkblue")

(plot13 | plot15 | plot14)

#--------------------
# Working with DETRENDED predictors and response
# set up the alternative models detrended no. pups emerged ~ detrended predictors
detrend_pupemer_m0 <- lm(resid_meanPupsEmerged  ~ resid_MeanGroupSize_z + I(resid_MeanGroupSize_z^2), data = groupsize_change)
detrend_pupemer_m1 <- lm(resid_meanPupsEmerged  ~ resid_MeanGroupSize_z + I(resid_MeanGroupSize_z^2) +  resid_lograin_earlysummer_z, data = groupsize_change)
detrend_pupemer_m2 <- update(detrend_pupemer_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_breedingseason_z)
detrend_pupemer_m3 <- update(detrend_pupemer_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_twoyear_z)
detrend_pupemer_m4 <- update(detrend_pupemer_m1, ~. -resid_lograin_earlysummer_z + resid_temp_summer_Z)
detrend_pupemer_m5 <- update(detrend_pupemer_m1, ~. -resid_lograin_earlysummer_z + resid_temp_allyear_z)
detrend_pupemer_m6 <- update(detrend_pupemer_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_allyear_z)
detrend_pupemer_m7 <- update(detrend_pupemer_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_twoyear_z)

detrend_pupemer_list <- list(detrend_pupemer_m0, 
                             detrend_pupemer_m1, detrend_pupemer_m2, detrend_pupemer_m3, 
                             detrend_pupemer_m4, detrend_pupemer_m5, detrend_pupemer_m6, detrend_pupemer_m7)

detrend_pupemer_AICc <- sapply(detrend_pupemer_list, FUN = AICc)
detrend_pupemer_beta <- as.numeric(sapply(detrend_pupemer_list, FUN = function(x) {signif(coef(x)[4], 3)}))
detrend_pupemer_beta_se <- as.numeric(sapply(detrend_pupemer_list, FUN = function(x) {signif(summary(x)$coef[,2][4], 3)}))
detrend_pupemer_gsbeta <- as.numeric(sapply(detrend_pupemer_list, FUN = function(x) {signif(coef(x)[2], 3)}))
detrend_pupemer_gsbeta_se <- as.numeric(sapply(detrend_pupemer_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
detrend_pupemer_gs2beta <- as.numeric(sapply(detrend_pupemer_list, FUN = function(x) {signif(coef(x)[3], 3)}))
detrend_pupemer_gs2beta_se <- as.numeric(sapply(detrend_pupemer_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
detrend_pupemer_r2 <- sapply(detrend_pupemer_list, FUN = function(x) {summary(x)$r.squared})
detrend_pupemer_p <- c(NA, unlist(sapply(detrend_pupemer_list, 
                                         FUN = function(x) { broom::tidy(x) %>%
                                             slice(4) %>% 
                                             pull(p.value) %>% 
                                             signif(4) })))

# summary table for population change 
detrend_pupemer_modtable <- data.frame(variable = env_terms, 
                                       AICc =  detrend_pupemer_AICc,
                                       beta  = detrend_pupemer_beta, 
                                       beta_se  = detrend_pupemer_beta_se,
                                       gsbeta = detrend_pupemer_gsbeta, 
                                       gsbeta_se = detrend_pupemer_gsbeta_se,
                                       gs2beta = detrend_pupemer_gs2beta, 
                                       gs2beta_se = detrend_pupemer_gs2beta_se,
                                       r2 = detrend_pupemer_r2,
                                       pval = detrend_pupemer_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se, gs2beta, gs2beta_se) 

# Model output for the best model
summary(detrend_pupemer_list[[3]])

#==========================================

# (iv) Model the  NO. OF PUPS RECRUITED 

#===========================================

  # set up the alternative models for pup recruitment (for groups that survive the year)
puprec_m0 <- lm(meanPupsRecruited  ~ MeanGroupSize_z + I(MeanGroupSize_z^2), data = groupsize_change)
puprec_m1 <- lm(meanPupsRecruited  ~ MeanGroupSize_z + I(MeanGroupSize_z^2) + lograin_earlysummer_z,data = groupsize_change)
puprec_m2 <- update(puprec_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
puprec_m3 <- update(puprec_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
puprec_m4 <- update(puprec_m1, ~. -lograin_earlysummer_z + temp_summer_z)
puprec_m5 <- update(puprec_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
puprec_m6 <- update(puprec_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
puprec_m7 <- update(puprec_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z)

puprec_list <- list(puprec_m0, 
                    puprec_m1, puprec_m2, puprec_m3, puprec_m4, puprec_m5,
                    puprec_m6, puprec_m7)

puprec_AICc <- sapply(puprec_list, FUN = AICc)
puprec_beta <- as.numeric(sapply(puprec_list, FUN = function(x) {signif(coef(x)[4], 3)}))
puprec_beta_se <- as.numeric(sapply(puprec_list, FUN = function(x) {signif(summary(x)$coef[,2][4], 3)}))
puprec_gsbeta <- as.numeric(sapply(puprec_list, FUN = function(x) {signif(coef(x)[2], 3)}))
puprec_gsbeta_se <- as.numeric(sapply(puprec_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
puprec_gs2beta <- as.numeric(sapply(puprec_list, FUN = function(x) {signif(coef(x)[3], 3)}))
puprec_gs2beta_se <- as.numeric(sapply(puprec_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
puprec_r2 <- sapply(puprec_list, FUN = function(x) {summary(x)$r.squared})
puprec_p <- c(NA, unlist(sapply(puprec_list, 
                                FUN = function(x) { broom::tidy(x) %>%
                                    slice(4) %>% 
                                    pull(p.value) %>% 
                                    signif(4) })))

# summary table for pup recruitment
puprec_modtable <- data.frame(variable = env_terms, 
                              AICc =  puprec_AICc,
                              beta  = puprec_beta, 
                              beta_se  = puprec_beta_se,
                              gsbeta = puprec_gsbeta, 
                              gsbeta_se = puprec_gsbeta_se,
                              gs2beta = puprec_gs2beta, 
                              gs2beta_se = puprec_gs2beta_se,
                              r2 = puprec_r2,
                              pval = puprec_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se, gs2beta, gs2beta_se)

# Model output for the best model
summary(puprec_list[[3]])

# Multiple regression with rainfall + temp + group size
puprec_m8 <- lm(meanPupsRecruited  ~ MeanGroupSize_z + I(MeanGroupSize_z^2) + lograin_breedingseason_z + temp_summer_z, data = groupsize_change)
summary(puprec_m8)
AICc(puprec_m8) - AICc(puprec_m2)

# Generate relevant plots for pup recruitment  -------------------

# plot 1: the effect of rainfall on pup recruitment
pr16 <- predict_response(puprec_m2, "lograin_breedingseason_z [all]")
  plot16 <- plot(pr16, show_residuals = TRUE) 
  plot16_qq <- ggplot_build(plot16) 
  plot16_qq1 <- plot16_qq$data[[1]] 
  plot16_qq2 <- plot16_qq$data[[2]]
  plot16_qq3 <- plot16_qq$data[[3]] 
  val16 <- groupsize_change$lograin_breedingseason
  plot16_qq1$x <- (plot16_qq1$x*sd(val16))+mean(val16)
  plot16_qq2$x <- (plot16_qq2$x*sd(val16))+mean(val16)
  plot16_qq3$x <- (plot16_qq3$x*sd(val16))+mean(val16)

plot16 <- ggplot(data = plot16_qq1, aes(x, y)) + 
  geom_ribbon(data = plot16_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot16_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "ln Rainfall (mm)", y = "Pups recruited per group") + 
  scale_y_continuous(breaks = seq(2, 16, 2), 
                     labels = c("", "4", "", "8", "", "12", "", "16"), 
                     limits = c(2, 16)) + 
  scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                     labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
  ggplot2::annotate("text", x = 5.5, y = 15.5, label = "β = 2.11 ± 0.32, p < 0.001", 
                    size = 3, colour = "darkblue")

# plot 2: the effect of group size on pup recruitment
pr17 <- predict_response(puprec_m2, "MeanGroupSize_z [all]")
  plot17 <- plot(pr17, show_residuals = TRUE) 
  plot17_qq <- ggplot_build(plot17) 
  plot17_qq1 <- plot17_qq$data[[1]] 
  plot17_qq2 <- plot17_qq$data[[2]]
  plot17_qq3 <- plot17_qq$data[[3]]
  val17 <- groupsize_change$MeanGroupSize
  # edit where I want ymax line to stop
  extra <- predict_response(puprec_m2, "MeanGroupSize_z [-1.875, -1.875]")
  plot17_qq3 <- bind_rows(ggplot_build(plot(extra))$data[[2]][1,], plot17_qq3)
  plot17_qq1$x <- (plot17_qq1$x*sd(val17))+mean(val17)
  plot17_qq2$x <- (plot17_qq2$x*sd(val17))+mean(val17)
  plot17_qq3$x <- (plot17_qq3$x*sd(val17))+mean(val17)

plot17 <- ggplot(data = plot17_qq1, aes(x, y)) + 
  geom_line(data = plot17_qq3, aes(y = ymin), colour = "black", linetype = 2) +
  geom_line(data = plot17_qq3, aes(y = ymax), colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot17_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Mean group size", y = "Pups recruited per group") + 
  scale_y_continuous(breaks = seq(2, 16, 2), 
                     labels = c("", "4", "", "8", "", "12", "", "16"), 
                     limits = c(2, 16)) + 
  scale_x_continuous(breaks = seq(8, 24, 2), 
                     labels = c("8", "", "12", "", "17", "", "20", "", "24")) + 
  ggplot2::annotate("text", x = 16, y = 15.5, 
                    label = expression(β[quad]~"= 1.10 ± 0.28, p < 0.001"),
                    size = 3, colour = "darkblue")

# plot 3: the effect of summer temperatures on pup recruitment
pr18 <- predict_response(puprec_m4, "temp_summer_z [all]")
  plot18 <- plot(pr18, show_residuals = TRUE) 
  plot18_qq <- ggplot_build(plot18) 
  plot18_qq1 <- plot18_qq$data[[1]] 
  plot18_qq2 <- plot18_qq$data[[2]]
  plot18_qq3 <- plot18_qq$data[[3]]
  val18 <- groupsize_change$temp_summer
  plot18_qq1$x <- (plot18_qq1$x*sd(val18))+mean(val18)
  plot18_qq2$x <- (plot18_qq2$x*sd(val18))+mean(val18)
  plot18_qq3$x <- (plot18_qq3$x*sd(val18))+mean(val18)

plot18 <- ggplot(data = plot18_qq1, aes(x, y)) + 
  geom_ribbon(data = plot18_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
              colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot18_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, 
       x = "Average summer\ntemperature (°C)", y = "Pups recruited per group") + 
  scale_y_continuous(breaks = seq(2, 16, 2), 
                     labels = c("", "4", "", "8", "", "12", "", "16"), 
                     limits = c(2, 16)) + 
  scale_x_continuous(breaks = seq(30, 37, 1), 
                     labels = c("", "31", "", "33", "", "35", "", "37")) + 
  ggplot2::annotate("text", x = 33.5, y = 15.5, label = "β = -1.66 ± 0.47, p = 0.002",
                    size = 3, colour = "darkblue")

(plot16 | plot18 | plot17)

#--------------------
# Working with DETRENDED predictors and response
# set up the alternative models detrended no. pups recruited ~ detrended predictors
# still need to scale to produce comparable coefficients. 
# still need to scale to produce comparable coefficients. 
detrend_puprec_m0 <- lm(resid_meanPupsRecruited  ~ resid_MeanGroupSize_z + I(resid_MeanGroupSize_z^2), 
                        data = groupsize_change)
detrend_puprec_m1 <- lm(resid_meanPupsRecruited  ~ resid_MeanGroupSize_z + I(resid_MeanGroupSize_z^2) + resid_lograin_earlysummer_z, data = groupsize_change)
detrend_puprec_m2 <- update(detrend_puprec_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_breedingseason_z)
detrend_puprec_m3 <- update(detrend_puprec_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_twoyear_z)
detrend_puprec_m4 <- update(detrend_puprec_m1, ~. -resid_lograin_earlysummer_z + resid_temp_summer_Z)
detrend_puprec_m5 <- update(detrend_puprec_m1, ~. -resid_lograin_earlysummer_z + resid_temp_allyear_z)
detrend_puprec_m6 <- update(detrend_puprec_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_allyear_z)
detrend_puprec_m7 <- update(detrend_puprec_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_twoyear_z)

detrend_puprec_list <- list(detrend_puprec_m0, 
                            detrend_puprec_m1, detrend_puprec_m2, detrend_puprec_m3, 
                            detrend_puprec_m4, detrend_puprec_m5, detrend_puprec_m6, detrend_puprec_m7)

detrend_puprec_AICc <- sapply(detrend_puprec_list, FUN = AICc)
detrend_puprec_beta <- as.numeric(sapply(detrend_puprec_list, FUN = function(x) {signif(coef(x)[4], 3)}))
detrend_puprec_beta_se <- as.numeric(sapply(detrend_puprec_list, FUN = function(x) {signif(summary(x)$coef[,2][4], 3)}))
detrend_puprec_gsbeta <- as.numeric(sapply(detrend_puprec_list, FUN = function(x) {signif(coef(x)[2], 3)}))
detrend_puprec_gsbeta_se <- as.numeric(sapply(detrend_puprec_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
detrend_puprec_gs2beta <- as.numeric(sapply(detrend_puprec_list, FUN = function(x) {signif(coef(x)[3], 3)}))
detrend_puprec_gs2beta_se <- as.numeric(sapply(detrend_puprec_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
detrend_puprec_r2 <- sapply(detrend_puprec_list, FUN = function(x) {summary(x)$r.squared})
detrend_puprec_p <- c(NA, unlist(sapply(detrend_puprec_list, 
                                        FUN = function(x) { broom::tidy(x) %>%
                                            slice(4) %>% 
                                            pull(p.value) %>% 
                                            signif(4) })))

# summary table for population change 
detrend_puprec_modtable <- data.frame(variable = env_terms, 
                                      AICc =  detrend_puprec_AICc,
                                      beta  = detrend_puprec_beta, 
                                      beta_se  = detrend_puprec_beta_se,
                                      gsbeta = detrend_puprec_gsbeta, 
                                      gsbeta_se = detrend_puprec_gsbeta_se,
                                      gs2beta = detrend_puprec_gs2beta, 
                                      gs2beta_se = detrend_puprec_gs2beta_se,
                                      r2 = detrend_puprec_r2,
                                      pval = detrend_puprec_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se, gs2beta, gs2beta_se)

# Model output for the best model
summary(detrend_puprec_list[[3]])

#==========================================

# (v) Model PUP SURVIVAL  

#===========================================

  # set up the alternative models pup survival
pupsurv_m0 <- lm(meanPupSurvival  ~ MeanGroupSize_z, data = groupsize_change)
pupsurv_m1 <- lm(meanPupSurvival  ~ MeanGroupSize_z + lograin_earlysummer_z, data = groupsize_change)
pupsurv_m2 <- update(pupsurv_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
pupsurv_m3 <- update(pupsurv_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
pupsurv_m4 <- update(pupsurv_m1, ~. -lograin_earlysummer_z + temp_summer_z)
pupsurv_m5 <- update(pupsurv_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
pupsurv_m6 <- update(pupsurv_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
pupsurv_m7 <- update(pupsurv_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z)

pupsurv_list <- list(pupsurv_m0, 
                     pupsurv_m1, pupsurv_m2, pupsurv_m3, pupsurv_m4, pupsurv_m5,
                     pupsurv_m6, pupsurv_m7)

pupsurv_AICc <- sapply(pupsurv_list, FUN = AICc)
pupsurv_beta <- as.numeric(sapply(pupsurv_list, FUN = function(x) {signif(coef(x)[3], 3)}))
pupsurv_beta_se <- as.numeric(sapply(pupsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
pupsurv_gsbeta <- as.numeric(sapply(pupsurv_list, FUN = function(x) {signif(coef(x)[2], 3)}))
pupsurv_gsbeta_se <- as.numeric(sapply(pupsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
pupsurv_r2 <- sapply(pupsurv_list, FUN = function(x) {summary(x)$r.squared})
pupsurv_p <- c(NA, unlist(sapply(pupsurv_list, 
                                 FUN = function(x) { broom::tidy(x) %>%
                                     slice(3) %>% 
                                     pull(p.value) %>% 
                                     signif(4) })))

# summary table for population change 
pupsurv_modtable <- data.frame(variable = env_terms, 
                               AICc =  pupsurv_AICc,
                               beta  = pupsurv_beta, 
                               beta_se  = pupsurv_beta_se,
                               gsbeta = pupsurv_gsbeta, 
                               gsbeta_se = pupsurv_gsbeta_se,
                               r2 = pupsurv_r2,
                               pval = pupsurv_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se)

# Model output for the best model
summary(pupsurv_list[[3]])

# Multiple regression with rainfall + temp + group size
pupsurv_m8 <- lm(meanPupSurvival  ~ MeanGroupSize_z + lograin_breedingseason_z + temp_summer_z, 
                 data = groupsize_change)
summary(pupsurv_m8)
AICc(pupsurv_m8) - AICc(pupsurv_m2)

# Generate relevant plots for pup survival -------------------

# plot 1: the effect of rainfall on pup survival
pr19 <- predict_response(pupsurv_m2, "lograin_breedingseason_z [all]")
  plot19 <- plot(pr19, show_residuals = TRUE) 
  plot19_qq <- ggplot_build(plot19) 
  plot19_qq1 <- plot19_qq$data[[1]] 
  plot19_qq2 <- plot19_qq$data[[2]]
  plot19_qq3 <- plot19_qq$data[[3]] 
  val19 <- groupsize_change$lograin_breedingseason
  plot19_qq1$x <- (plot19_qq1$x*sd(val19))+mean(val19)
  plot19_qq2$x <- (plot19_qq2$x*sd(val19))+mean(val19)
  plot19_qq3$x <- (plot19_qq3$x*sd(val19))+mean(val19)

plot19 <- ggplot(data = plot19_qq1, aes(x, y)) + 
  geom_ribbon(data = plot19_qq3, aes(ymin = ymin, ymax = ymax), 
              fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot19_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "ln Rainfall (mm)", y = "Pup survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                     labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
  ggplot2::annotate("text", x = 5.5, y = 1, label = "β = 0.079 ± 0.015, p < 0.001", 
                    size = 3, colour = "darkblue")

# plot 2: the effect of group size on pup survival
pr20 <- predict_response(pupsurv_m2, "MeanGroupSize_z [all]")
  plot20 <- plot(pr20, show_residuals = TRUE) 
  plot20_qq <- ggplot_build(plot20) 
  plot20_qq1 <- plot20_qq$data[[1]] 
  plot20_qq2 <- plot20_qq$data[[2]]
  plot20_qq3 <- plot20_qq$data[[3]]
  val20 <- groupsize_change$MeanGroupSize
  plot20_qq1$x <- (plot20_qq1$x*sd(val20))+mean(val20)
  plot20_qq2$x <- (plot20_qq2$x*sd(val20))+mean(val20)
  plot20_qq3$x <- (plot20_qq3$x*sd(val20))+mean(val20)

plot20 <- ggplot(data = plot20_qq1, aes(x, y)) + 
  geom_ribbon(data = plot20_qq3, aes(ymin = ymin, ymax = ymax), 
              fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot20_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Mean group size", y = "Pup survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(8, 24, 2), 
                     labels = c("8", "", "12", "", "16", "", "20", "", "24")) + 
  ggplot2::annotate("text", x = 16, y = 1, label = "β = 0.00 ± 0.02, p = 0.99", 
                    size = 3, colour = "darkblue")

# plot 3: the effect of summer temperatures on pup survival
pr21 <- predict_response(pupsurv_m4, "temp_summer_z [all]")
  plot21 <- plot(pr21, show_residuals = TRUE) 
  plot21_qq <- ggplot_build(plot21) 
  plot21_qq1 <- plot21_qq$data[[1]] 
  plot21_qq2 <- plot21_qq$data[[2]]
  plot21_qq3 <- plot21_qq$data[[3]] 
  val21 <- groupsize_change$temp_summer
  extra <- predict_response(pupsurv_m4, "temp_summer_z [-1.82, -1.82]")
  plot21_qq3 <- bind_rows(ggplot_build(plot(extra))$data[[2]][1,], plot21_qq3)
  plot21_qq1$x <- (plot21_qq1$x*sd(val21))+mean(val21)
  plot21_qq2$x <- (plot21_qq2$x*sd(val21))+mean(val21)
  plot21_qq3$x <- (plot21_qq3$x*sd(val21))+mean(val21)

plot21 <- ggplot(data = plot21_qq1, aes(x, y)) + 
  geom_line(data = plot21_qq3, aes(y = ymin), colour = "black", linetype = 2) +
  geom_line(data = plot21_qq3, aes(y = ymax), colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot21_qq2, linewidth = 1) + 
  coord_cartesian(expand = T) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, 
       x = "Average summer\ntemperature (°C)", y = "Pup survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(30, 37, 1), 
                     labels = c("", "31", "", "33", "", "35", "", "37")) + 
  ggplot2::annotate("text", x = 33.5, y = 1, label = "β = -0.065 ± 0.019, p = 0.003",
                    size = 3, colour = "darkblue")

(plot19 | plot21 | plot20)

#--------------------
# Working with DETRENDED predictors and response
# set up the alternative models detrended pup survival ~ detrended predictors
detrend_pupsurv_m0 <- lm(resid_meanPupSurvival  ~ resid_MeanGroupSize_z, data = groupsize_change)
detrend_pupsurv_m1 <- lm(resid_meanPupSurvival  ~ resid_MeanGroupSize_z + resid_lograin_earlysummer_z, data = groupsize_change)
detrend_pupsurv_m2 <- update(detrend_pupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_breedingseason_z)
detrend_pupsurv_m3 <- update(detrend_pupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_twoyear_z)
detrend_pupsurv_m4 <- update(detrend_pupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_temp_summer_Z)
detrend_pupsurv_m5 <- update(detrend_pupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_temp_allyear_z)
detrend_pupsurv_m6 <- update(detrend_pupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_allyear_z)
detrend_pupsurv_m7 <- update(detrend_pupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_twoyear_z)

detrend_pupsurv_list <- list(detrend_pupsurv_m0, 
                             detrend_pupsurv_m1, detrend_pupsurv_m2, detrend_pupsurv_m3, 
                             detrend_pupsurv_m4, detrend_pupsurv_m5, detrend_pupsurv_m6, detrend_pupsurv_m7)

detrend_pupsurv_AICc <- sapply(detrend_pupsurv_list, FUN = AICc)
detrend_pupsurv_beta <- as.numeric(sapply(detrend_pupsurv_list, FUN = function(x) {signif(coef(x)[3], 3)}))
detrend_pupsurv_beta_se <- as.numeric(sapply(detrend_pupsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
detrend_pupsurv_gsbeta <- as.numeric(sapply(detrend_pupsurv_list, FUN = function(x) {signif(coef(x)[2], 3)}))
detrend_pupsurv_gsbeta_se <- as.numeric(sapply(detrend_pupsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
detrend_pupsurv_r2 <- sapply(detrend_pupsurv_list, FUN = function(x) {summary(x)$r.squared})
detrend_pupsurv_p <- c(NA, unlist(sapply(detrend_pupsurv_list, 
                                         FUN = function(x) { broom::tidy(x) %>%
                                             slice(3) %>% 
                                             pull(p.value) %>% 
                                             signif(4) })))

# summary table for population change 
detrend_pupsurv_modtable <- data.frame(variable = env_terms, 
                                       AICc =  detrend_pupsurv_AICc,
                                       beta  = detrend_pupsurv_beta, 
                                       beta_se  = detrend_pupsurv_beta_se,
                                       gsbeta = detrend_pupsurv_gsbeta, 
                                       gsbeta_se = detrend_pupsurv_gsbeta_se,
                                       r2 = detrend_pupsurv_r2,
                                       pval = detrend_pupsurv_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se)

# Model output for the best model
summary(detrend_pupsurv_list[[3]])

#==========================================

# (vi) Model NON-PUP SURVIVAL (juv + subadult + adult)

#===========================================

# set up the alternative models non-pup survival
nonpupsurv_m0 <- lm(meanNonPupSurvival ~ MeanGroupSize_z, data = groupsize_change)
nonpupsurv_m1 <- lm(meanNonPupSurvival ~ MeanGroupSize_z + lograin_earlysummer_z, data = groupsize_change)
nonpupsurv_m2 <- update(nonpupsurv_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
nonpupsurv_m3 <- update(nonpupsurv_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
nonpupsurv_m4 <- update(nonpupsurv_m1, ~. -lograin_earlysummer_z + temp_summer_z)
nonpupsurv_m5 <- update(nonpupsurv_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
nonpupsurv_m6 <- update(nonpupsurv_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
nonpupsurv_m7 <- update(nonpupsurv_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z)

nonpupsurv_list <- list(nonpupsurv_m0, 
                        nonpupsurv_m1, nonpupsurv_m2, nonpupsurv_m3, nonpupsurv_m4, nonpupsurv_m5,
                        nonpupsurv_m6, nonpupsurv_m7)

nonpupsurv_AICc <- sapply(nonpupsurv_list, FUN = AICc)
nonpupsurv_beta <- as.numeric(sapply(nonpupsurv_list, FUN = function(x) {signif(coef(x)[3], 3)}))
nonpupsurv_beta_se <- as.numeric(sapply(nonpupsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
nonpupsurv_gsbeta <- as.numeric(sapply(nonpupsurv_list, FUN = function(x) {signif(coef(x)[2], 3)}))
nonpupsurv_gsbeta_se <- as.numeric(sapply(nonpupsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
nonpupsurv_r2 <- sapply(nonpupsurv_list, FUN = function(x) {summary(x)$r.squared})
nonpupsurv_p <- c(NA, unlist(sapply(nonpupsurv_list, 
                                    FUN = function(x) { broom::tidy(x) %>%
                                        slice(3) %>% 
                                        pull(p.value) %>% 
                                        signif(4) })))

# Summary table for population change 
nonpupsurv_modtable <- data.frame(variable = env_terms, 
                                  AICc =  nonpupsurv_AICc,
                                  beta  = nonpupsurv_beta, 
                                  beta_se  = nonpupsurv_beta_se,
                                  gsbeta = nonpupsurv_gsbeta, 
                                  gsbeta_se = nonpupsurv_gsbeta_se,
                                  r2 = nonpupsurv_r2,
                                  pval = nonpupsurv_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se)

# Model output for the best model
summary(nonpupsurv_list[[1]])

# plot 1: the effect of rainfall on non-pup survival
pr22 <- predict_response(nonpupsurv_m2, "lograin_breedingseason_z [all]")
  plot22 <- plot(pr22, show_residuals = TRUE) 
  plot22_qq <- ggplot_build(plot22) 
  plot22_qq1 <- plot22_qq$data[[1]] 
  plot22_qq2 <- plot22_qq$data[[2]]
  plot22_qq3 <- plot22_qq$data[[3]] 
  val22 <- groupsize_change$lograin_breedingseason
  plot22_qq1$x <- (plot22_qq1$x*sd(val22))+mean(val22)
  plot22_qq2$x <- (plot22_qq2$x*sd(val22))+mean(val22)
  plot22_qq3$x <- (plot22_qq3$x*sd(val22))+mean(val22)

plot22 <- ggplot(data = plot22_qq1, aes(x, y)) + 
  geom_ribbon(data = plot22_qq3, aes(ymin = ymin, ymax = ymax), 
              fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot22_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "ln Rainfall (mm)", y = "Non-pup survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                     labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
  ggplot2::annotate("text", x = 5.5, y = 1, label = "β = 0.017 ± 0.013, p = 0.20", 
                    size = 3, colour = "darkblue")

# plot 2: the effect of group size on non-pup survival
pr23 <- predict_response(nonpupsurv_m2, "MeanGroupSize_z [all]")
  plot23 <- plot(pr23, show_residuals = TRUE) 
  plot23_qq <- ggplot_build(plot23) 
  plot23_qq1 <- plot23_qq$data[[1]] 
  plot23_qq2 <- plot23_qq$data[[2]]
  plot23_qq3 <- plot23_qq$data[[3]]
  val23 <- groupsize_change$MeanGroupSize
  plot23_qq1$x <- (plot23_qq1$x*sd(val23))+mean(val23)
  plot23_qq2$x <- (plot23_qq2$x*sd(val23))+mean(val23)
  plot23_qq3$x <- (plot23_qq3$x*sd(val23))+mean(val23)

plot23 <- ggplot(data = plot23_qq1, aes(x, y)) + 
  geom_ribbon(data = plot23_qq3, aes(ymin = ymin, ymax = ymax), 
              fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot23_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Mean group size", y = "Non-pup survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(8, 24, 2), 
                     labels = c("8", "", "12", "", "16", "", "20", "", "24")) + 
  ggplot2::annotate("text", x = 16, y = 1, label = "β = 0.049 ± 0.013, p < 0.001", 
                    size = 3, colour = "darkblue")

# plot 3: the effect of summer temperatures on non-pup survival
pr24 <- predict_response(nonpupsurv_m4, "temp_summer_z [all]")
  plot24 <- plot(pr24, show_residuals = TRUE) 
  plot24_qq <- ggplot_build(plot24) 
  plot24_qq1 <- plot24_qq$data[[1]] 
  plot24_qq2 <- plot24_qq$data[[2]]
  plot24_qq3 <- plot24_qq$data[[3]] ;   plot24_qq3$ymax[plot24_qq3$ymax > 1] <- NA
  val24 <- groupsize_change$temp_summer
  plot24_qq1$x <- (plot24_qq1$x*sd(val24))+mean(val24)
  plot24_qq2$x <- (plot24_qq2$x*sd(val24))+mean(val24)
  plot24_qq3$x <- (plot24_qq3$x*sd(val24))+mean(val24)

plot24 <- ggplot(data = plot24_qq1, aes(x, y)) + 
  geom_ribbon(data = plot24_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
              colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot24_qq2, linewidth = 1) + 
  coord_cartesian(expand = T) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, 
       x = "Average summer\ntemperature (°C)", y = "Non-pup survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(30, 37, 1), 
                     labels = c("", "31", "", "33", "", "35", "", "37")) + 
  ggplot2::annotate("text", x = 33.5, y = 1, label = "β = -0.017 ± 0.013, p = 0.21",
                    size = 3, colour = "darkblue")

(plot22 | plot24 | plot23)

#--------------------
# Working with DETRENDED predictors and response
# set up the alternative models detrended non-pup survival ~ detrended predictors
# still need to scale to produce comparable coefficients. 
detrend_nonpupsurv_m0 <- lm(resid_meanNonPupSurvival  ~ resid_MeanGroupSize_z, data = groupsize_change)
detrend_nonpupsurv_m1 <- lm(resid_meanNonPupSurvival  ~ resid_MeanGroupSize_z + resid_lograin_earlysummer_z, data = groupsize_change)
detrend_nonpupsurv_m2 <- update(detrend_nonpupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_breedingseason_z)
detrend_nonpupsurv_m3 <- update(detrend_nonpupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_twoyear_z)
detrend_nonpupsurv_m4 <- update(detrend_nonpupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_temp_summer_Z)
detrend_nonpupsurv_m5 <- update(detrend_nonpupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_temp_allyear_z)
detrend_nonpupsurv_m6 <- update(detrend_nonpupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_allyear_z)
detrend_nonpupsurv_m7 <- update(detrend_nonpupsurv_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_twoyear_z)

detrend_nonpupsurv_list <- list(detrend_nonpupsurv_m0, 
                                detrend_nonpupsurv_m1, detrend_nonpupsurv_m2, detrend_nonpupsurv_m3, 
                                detrend_nonpupsurv_m4, detrend_nonpupsurv_m5, detrend_nonpupsurv_m6, detrend_nonpupsurv_m7)

detrend_nonpupsurv_AICc <- sapply(detrend_nonpupsurv_list, FUN = AICc)
detrend_nonpupsurv_beta <- as.numeric(sapply(detrend_nonpupsurv_list, FUN = function(x) {signif(coef(x)[3], 3)}))
detrend_nonpupsurv_beta_se <- as.numeric(sapply(detrend_nonpupsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
detrend_nonpupsurv_gsbeta <- as.numeric(sapply(detrend_nonpupsurv_list, FUN = function(x) {signif(coef(x)[2], 3)}))
detrend_nonpupsurv_gsbeta_se <- as.numeric(sapply(detrend_nonpupsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
detrend_nonpupsurv_r2 <- sapply(detrend_nonpupsurv_list, FUN = function(x) {summary(x)$r.squared})
detrend_nonpupsurv_p <- c(NA, unlist(sapply(detrend_nonpupsurv_list, 
                                            FUN = function(x) { broom::tidy(x) %>%
                                                slice(3) %>% 
                                                pull(p.value) %>% 
                                                signif(4) })))

# summary table for population change 
detrend_nonpupsurv_modtable <- data.frame(variable = env_terms, 
                                          AICc =  detrend_nonpupsurv_AICc,
                                          beta  = detrend_nonpupsurv_beta, 
                                          beta_se  = detrend_nonpupsurv_beta_se,
                                          gsbeta = detrend_nonpupsurv_gsbeta, 
                                          gsbeta_se = detrend_nonpupsurv_gsbeta_se,
                                          r2 = detrend_nonpupsurv_r2,
                                          pval = detrend_nonpupsurv_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se)

# Model output for the best model
summary(detrend_nonpupsurv_list[[1]])

#==========================================

# COMBINED ALL THE PLOTS TOGETHER 

#==========================================

plot13_final <- plot13 + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot14_final <- plot14 + 
  theme(axis.text = element_blank(), 
        axis.title= element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot15_final <- plot15 + 
  theme(axis.text = element_blank(), 
        axis.title= element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
(plot13_final | plot15_final | plot14_final)

plot16_final <- plot16 + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot17_final <- plot17 + 
  theme(axis.text = element_blank(), 
        axis.title= element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot18_final <- plot18 + 
  theme(axis.text = element_blank(), 
        axis.title= element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
(plot16_final | plot18_final | plot17_final)

plot19_final <- plot19 + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        plot.margin = margin(b = 0))
plot20_final <- plot20 + 
  theme(axis.text = element_blank(), 
        axis.title= element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot21_final <- plot21 + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
(plot19_final | plot21_final | plot20_final)

plot22_final <- plot22 + 
  theme(plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot23_final <- plot23 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot24_final <- plot24 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
(plot22 | plot24_final | plot23_final)

# and now add the four on top of each other
(plot13_final + labs(tag = "A") + theme(plot.tag = element_text(size = 14)) | plot15_final | plot14_final) /
  (plot16_final + labs(tag = "B") + theme(plot.tag = element_text(size = 14)) | plot18_final | plot17_final) /
  (plot19_final + labs(tag = "C") + theme(plot.tag = element_text(size = 14)) | plot21_final | plot20_final) / 
  (plot22_final + labs(tag = "D") + theme(plot.tag = element_text(size = 14)) | plot24_final | plot23_final)

#==========================================

# (vii) Model JUV + SUBADULT SURVIVAL 

#===========================================

# set up the alternative models subadult survival
juvsubadultsurv_m0 <- lm(meanJuvSubAdultSurvival ~ MeanGroupSize_z, data = groupsize_change)
juvsubadultsurv_m1 <- lm(meanJuvSubAdultSurvival ~ MeanGroupSize_z + lograin_earlysummer_z, data = groupsize_change)
juvsubadultsurv_m2 <- update(juvsubadultsurv_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
juvsubadultsurv_m3 <- update(juvsubadultsurv_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
juvsubadultsurv_m4 <- update(juvsubadultsurv_m1, ~. -lograin_earlysummer_z + temp_summer_z)
juvsubadultsurv_m5 <- update(juvsubadultsurv_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
juvsubadultsurv_m6 <- update(juvsubadultsurv_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
juvsubadultsurv_m7 <- update(juvsubadultsurv_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z)

juvsubadultsurv_list <- list(juvsubadultsurv_m0, juvsubadultsurv_m1, juvsubadultsurv_m2, 
                             juvsubadultsurv_m3, juvsubadultsurv_m4, juvsubadultsurv_m5,
                             juvsubadultsurv_m6, juvsubadultsurv_m7)

juvsubadultsurv_AICc <- sapply(juvsubadultsurv_list, FUN = AICc)
juvsubadultsurv_beta <- as.numeric(sapply(juvsubadultsurv_list, FUN = function(x) {signif(coef(x)[3], 3)}))
juvsubadultsurv_beta_se <- as.numeric(sapply(juvsubadultsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
juvsubadultsurv_gsbeta <- as.numeric(sapply(juvsubadultsurv_list, FUN = function(x) {signif(coef(x)[2], 3)}))
juvsubadultsurv_gsbeta_se <- as.numeric(sapply(juvsubadultsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
juvsubadultsurv_r2 <- sapply(juvsubadultsurv_list, FUN = function(x) {summary(x)$r.squared})
juvsubadultsurv_p <- c(NA, unlist(sapply(juvsubadultsurv_list, 
                                         FUN = function(x) { broom::tidy(x) %>%
                                             slice(3) %>% 
                                             pull(p.value) %>% 
                                             signif(4) })))

# Summary table for population change 
juvsubadultsurv_modtable <- data.frame(variable = env_terms, 
                                       AICc =  juvsubadultsurv_AICc,
                                       beta  = juvsubadultsurv_beta, 
                                       beta_se  = juvsubadultsurv_beta_se,
                                       gsbeta = juvsubadultsurv_gsbeta, 
                                       gsbeta_se = juvsubadultsurv_gsbeta_se,
                                       r2 = juvsubadultsurv_r2,
                                       pval = juvsubadultsurv_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se)

# Model output for the best mode
summary(juvsubadultsurv_list[[5]])

# Multiple regression with rainfall + temp + group size
juvsubadultsurv_m8 <- lm(meanJuvSubAdultSurvival  ~ MeanGroupSize_z + lograin_breedingseason_z + temp_summer_z, 
                         data = groupsize_change)
summary(juvsubadultsurv_m8)
AICc(juvsubadultsurv_m8) - AICc(juvsubadultsurv_m2)

# Generate relevant plots for juv + subadult survival  -------------------

# plot 1: the effect of rainfall on juv + subadult survival
pr25 <- predict_response(juvsubadultsurv_m2, "lograin_breedingseason_z [all]")
  plot25 <- plot(pr25, show_residuals = TRUE) 
  plot25_qq <- ggplot_build(plot25) 
  plot25_qq1 <- plot25_qq$data[[1]] 
  plot25_qq2 <- plot25_qq$data[[2]]
  plot25_qq3 <- plot25_qq$data[[3]] 
  val25 <- groupsize_change$lograin_breedingseason
  plot25_qq1$x <- (plot25_qq1$x*sd(val25))+mean(val25)
  plot25_qq2$x <- (plot25_qq2$x*sd(val25))+mean(val25)
  plot25_qq3$x <- (plot25_qq3$x*sd(val25))+mean(val25)

plot25 <- ggplot(data = plot25_qq1, aes(x, y)) + 
  geom_ribbon(data = plot25_qq3, aes(ymin = ymin, ymax = ymax), 
              fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot25_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "ln Rainfall (mm)", y = "Juvenile &\nsubadult survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                     labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
  ggplot2::annotate("text", x = 5.5, y = 1, label = "β = 0.031 ± 0.015, p = 0.055", 
                    size = 3, colour = "darkblue")

# plot 2: the effect of group size on juv + subadult survival
pr26 <- predict_response(juvsubadultsurv_m4, "MeanGroupSize_z [all]")
  plot26 <- plot(pr26, show_residuals = TRUE) 
  plot26_qq <- ggplot_build(plot26) 
  plot26_qq1 <- plot26_qq$data[[1]] 
  plot26_qq2 <- plot26_qq$data[[2]]
  plot26_qq3 <- plot26_qq$data[[3]]
  val26 <- groupsize_change$MeanGroupSize
  plot26_qq1$x <- (plot26_qq1$x*sd(val20))+mean(val26)
  plot26_qq2$x <- (plot26_qq2$x*sd(val20))+mean(val26)
  plot26_qq3$x <- (plot26_qq3$x*sd(val20))+mean(val26)

plot26 <- ggplot(data = plot26_qq1, aes(x, y)) + 
  geom_ribbon(data = plot26_qq3, aes(ymin = ymin, ymax = ymax), 
              fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot26_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Mean group size", y = "Juvenile &\nsubadult survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(8, 24, 2), 
                     labels = c("8", "", "12", "", "16", "", "20", "", "24")) + 
  ggplot2::annotate("text", x = 16, y = 1, label = "β = 0.043 ± 0.015, p = 0.010", 
                    size = 3, colour = "darkblue")

# plot 3: the effect of summer temperatures on  on juv + subadult survival
pr27 <- predict_response(juvsubadultsurv_m4, "temp_summer_z [all]")
  plot27 <- plot(pr27, show_residuals = TRUE) 
  plot27_qq <- ggplot_build(plot27) 
  plot27_qq1 <- plot27_qq$data[[1]] 
  plot27_qq2 <- plot27_qq$data[[2]]
  plot27_qq3 <- plot27_qq$data[[3]] 
  val27 <- groupsize_change$temp_summer
  plot27_qq1$x <- (plot27_qq1$x*sd(val27))+mean(val27)
  plot27_qq2$x <- (plot27_qq2$x*sd(val27))+mean(val27)
  plot27_qq3$x <- (plot27_qq3$x*sd(val27))+mean(val27)

plot27 <- ggplot(data = plot27_qq1, aes(x, y)) + 
  geom_ribbon(data = plot27_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
              colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot27_qq2, linewidth = 1) + 
  coord_cartesian(expand = T) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, 
       x = "Average summer\ntemperature (°C)", y = "Juvenile &\nsubadult survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(30, 37, 1), 
                     labels = c("", "31", "", "33", "", "35", "", "37")) + 
  ggplot2::annotate("text", x = 34, y = 1, label = "β = -0.039 ± 0.015, p = 0.017", 
                    size = 3, colour = "darkblue")

(plot25 | plot27 | plot26)

#--------------------
# Working with DETRENDED predictors and response
# set up the alternative models detrended juv + subadult surv ~ detrended predictors
detrend_juvsubadultsurv_m0 <- lm(resid_meanJuvSubAdultSurvival  ~ resid_MeanGroupSize_z, data = groupsize_change)
detrend_juvsubadultsurv_m1 <- lm(resid_meanJuvSubAdultSurvival  ~ resid_MeanGroupSize_z + resid_lograin_earlysummer_z, data = groupsize_change)
detrend_juvsubadultsurv_m2 <- update(detrend_juvsubadultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_breedingseason_z)
detrend_juvsubadultsurv_m3 <- update(detrend_juvsubadultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_twoyear_z)
detrend_juvsubadultsurv_m4 <- update(detrend_juvsubadultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_temp_summer_Z)
detrend_juvsubadultsurv_m5 <- update(detrend_juvsubadultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_temp_allyear_z)
detrend_juvsubadultsurv_m6 <- update(detrend_juvsubadultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_allyear_z)
detrend_juvsubadultsurv_m7 <- update(detrend_juvsubadultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_twoyear_z)

detrend_juvsubadultsurv_list <- list(detrend_juvsubadultsurv_m0, 
                                     detrend_juvsubadultsurv_m1, detrend_juvsubadultsurv_m2, detrend_juvsubadultsurv_m3, 
                                     detrend_juvsubadultsurv_m4, detrend_juvsubadultsurv_m5, detrend_juvsubadultsurv_m6, detrend_juvsubadultsurv_m7)

detrend_juvsubadultsurv_AICc <- sapply(detrend_juvsubadultsurv_list, FUN = AICc)
detrend_juvsubadultsurv_beta <- as.numeric(sapply(detrend_juvsubadultsurv_list, FUN = function(x) {signif(coef(x)[3], 3)}))
detrend_juvsubadultsurv_beta_se <- as.numeric(sapply(detrend_juvsubadultsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
detrend_juvsubadultsurv_densbeta <- as.numeric(sapply(detrend_juvsubadultsurv_list, FUN = function(x) {signif(coef(x)[2], 3)}))
detrend_juvsubadultsurv_densbeta_se <- as.numeric(sapply(detrend_juvsubadultsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
detrend_juvsubadultsurv_r2 <- sapply(detrend_juvsubadultsurv_list, FUN = function(x) {summary(x)$r.squared})
detrend_juvsubadultsurv_p <- c(NA, unlist(sapply(detrend_juvsubadultsurv_list, 
                                                 FUN = function(x) { broom::tidy(x) %>%
                                                     slice(3) %>% 
                                                     pull(p.value) %>% 
                                                     signif(4) })))

# summary table for population change 
detrend_juvsubadultsurv_modtable <- data.frame(variable = env_terms, 
                                               AICc =  detrend_juvsubadultsurv_AICc,
                                               beta  = detrend_juvsubadultsurv_beta, 
                                               beta_se  = detrend_juvsubadultsurv_beta_se,
                                               densbeta = detrend_juvsubadultsurv_densbeta, 
                                               densbeta_se = detrend_juvsubadultsurv_densbeta_se,
                                               r2 = detrend_juvsubadultsurv_r2,
                                               pval = detrend_juvsubadultsurv_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, densbeta, densbeta_se)

# Model output for the best model
summary(detrend_juvsubadultsurv_list[[6]])

#==========================================

# (viii) Model ADULT SURVIVAL 

#===========================================

  # set up the alternative models non-pup survival
adultsurv_m0 <- lm(meanAdultSurvival ~ MeanGroupSize_z, data = groupsize_change)
adultsurv_m1 <- lm(meanAdultSurvival ~ MeanGroupSize_z + lograin_earlysummer_z, data = groupsize_change)
adultsurv_m2 <- update(adultsurv_m1, ~. -lograin_earlysummer_z + lograin_breedingseason_z)
adultsurv_m3 <- update(adultsurv_m1, ~. -lograin_earlysummer_z + lograin_twoyear_z)
adultsurv_m4 <- update(adultsurv_m1, ~. -lograin_earlysummer_z + temp_summer_z)
adultsurv_m5 <- update(adultsurv_m1, ~. -lograin_earlysummer_z + temp_allyear_z)
adultsurv_m6 <- update(adultsurv_m1, ~. -lograin_earlysummer_z + spei6_allyear_z)
adultsurv_m7 <- update(adultsurv_m1, ~. -lograin_earlysummer_z + spei6_twoyear_z)

adultsurv_list <- list(adultsurv_m0, 
                       adultsurv_m1, adultsurv_m2, adultsurv_m3, adultsurv_m4, adultsurv_m5,
                       adultsurv_m6, adultsurv_m7)

adultsurv_AICc <- sapply(adultsurv_list, FUN = AICc)
adultsurv_beta <- as.numeric(sapply(adultsurv_list, FUN = function(x) {signif(coef(x)[3], 3)}))
adultsurv_beta_se <- as.numeric(sapply(adultsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
adultsurv_gsbeta <- as.numeric(sapply(adultsurv_list, FUN = function(x) {signif(coef(x)[2], 3)}))
adultsurv_gsbeta_se <- as.numeric(sapply(adultsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
adultsurv_r2 <- sapply(adultsurv_list, FUN = function(x) {summary(x)$r.squared})
adultsurv_p <- c(NA, unlist(sapply(adultsurv_list, 
                                   FUN = function(x) { broom::tidy(x) %>%
                                       slice(3) %>% 
                                       pull(p.value) %>% 
                                       signif(4) })))

# Summary table for population change 
adultsurv_modtable <- data.frame(variable = env_terms, 
                                 AICc =  adultsurv_AICc,
                                 beta  = adultsurv_beta, 
                                 beta_se  = adultsurv_beta_se,
                                 gsbeta = adultsurv_gsbeta, 
                                 gsbeta_se = adultsurv_gsbeta_se,
                                 r2 = adultsurv_r2,
                                 pval = adultsurv_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, gsbeta, gsbeta_se)

# Model output for the best model
summary(adultsurv_list[[1]])

# Multiple regression with rainfall + temp + group size
adultsurv_m8 <- lm(meanAdultSurvival  ~ MeanGroupSize_z + lograin_breedingseason_z + temp_allyear_z,
                   data = groupsize_change)
summary(adultsurv_m8)
AICc(adultsurv_m8) - AICc(adultsurv_m5)

# Generate relevant plots for adult survival -------------------

# plot 1: the effect of rainfall on adult survival
pr28 <- predict_response(adultsurv_m2, "lograin_breedingseason_z [all]")
  plot28 <- plot(pr28, show_residuals = TRUE) 
  plot28_qq <- ggplot_build(plot28) 
  plot28_qq1 <- plot28_qq$data[[1]] 
  plot28_qq2 <- plot28_qq$data[[2]]
  plot28_qq3 <- plot28_qq$data[[3]] 
  val28 <- groupsize_change$lograin_breedingseason
  plot28_qq1$x <- (plot28_qq1$x*sd(val28))+mean(val28)
  plot28_qq2$x <- (plot28_qq2$x*sd(val28))+mean(val28)
  plot28_qq3$x <- (plot28_qq3$x*sd(val28))+mean(val28)

plot28 <- ggplot(data = plot28_qq1, aes(x, y)) + 
  geom_ribbon(data = plot28_qq3, aes(ymin = ymin, ymax = ymax), 
              fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot28_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "ln Rainfall (mm)", y = "Adult survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(4.8, 6.2, 0.2), 
                     labels = c("", "5.0", "", "5.4", "", "5.8", "", "6.2")) + 
  ggplot2::annotate("text", x = 5.5, y = 1, label = "β = 0.005 ± 0.015, p = 0.72", 
                    size = 3, colour = "darkblue")

# plot 2: the effect of group size on adult survival 
pr29 <- predict_response(adultsurv_m4, "MeanGroupSize_z [all]")
  plot29 <- plot(pr29, show_residuals = TRUE) 
  plot29_qq <- ggplot_build(plot29) 
  plot29_qq1 <- plot29_qq$data[[1]] 
  plot29_qq2 <- plot29_qq$data[[2]]
  plot29_qq3 <- plot29_qq$data[[3]]
  val29 <- groupsize_change$MeanGroupSize
  plot29_qq1$x <- (plot29_qq1$x*sd(val23))+mean(val29)
  plot29_qq2$x <- (plot29_qq2$x*sd(val23))+mean(val29)
  plot29_qq3$x <- (plot29_qq3$x*sd(val23))+mean(val29)

plot29 <- ggplot(data = plot29_qq1, aes(x, y)) + 
  geom_ribbon(data = plot29_qq3, aes(ymin = ymin, ymax = ymax), 
              fill  = NA, colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot29_qq2, linewidth = 1) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, x = "Mean group size", y = "Adult survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(8, 24, 2), 
                     labels = c("8", "", "12", "", "16", "", "20", "", "24")) + 
  ggplot2::annotate("text", x = 16, y = 1, label = "β = 0.046 ± 0.015, p = 0.007", 
                    size = 3, colour = "darkblue")

# plot 3: the effect of summer temperatures on adult survival
pr30 <- predict_response(adultsurv_m4, "temp_summer_z [all]")
  plot30 <- plot(pr30, show_residuals = TRUE) 
  plot30_qq <- ggplot_build(plot30) 
  plot30_qq1 <- plot30_qq$data[[1]] 
  plot30_qq2 <- plot30_qq$data[[2]]
  plot30_qq3 <- plot30_qq$data[[3]]
  val30 <- groupsize_change$temp_summer
  plot30_qq1$x <- (plot30_qq1$x*sd(val30))+mean(val30)
  plot30_qq2$x <- (plot30_qq2$x*sd(val30))+mean(val30)
  plot30_qq3$x <- (plot30_qq3$x*sd(val30))+mean(val30)

plot30 <- ggplot(data = plot30_qq1, aes(x, y)) + 
  geom_ribbon(data = plot30_qq3, aes(ymin = ymin, ymax = ymax), fill  = NA, 
              colour = "black", linetype = 2) +
  geom_point(shape = 1, stroke = 0.8, size = 2) + 
  geom_line(data = plot30_qq2, linewidth = 1) + 
  coord_cartesian(expand = T) +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 9.5), 
        axis.title = element_text(colour = "black", size = 10.5)) +
  labs(title = NULL, 
       x = "Average summer\ntemperature (°C)", y = "Adult survival") + 
  scale_y_continuous(breaks = seq(0.5, 1, 0.1), limits = c(0.5, 1)) + 
  scale_x_continuous(breaks = seq(30, 37, 1), 
                     labels = c("", "31", "", "33", "", "35", "", "37")) + 
  ggplot2::annotate("text", x = 34, y = 1, label = "β = -0.007 ± 0.015, p = 0.67", 
                    size = 3, colour = "darkblue")

(plot28 | plot30 | plot29)

#--------------------
# Working with DETRENDED predictors and response
# set up the alternative models detrended subadult ~ detrended predictors
# still need to scale to produce comparable coefficients. 
detrend_adultsurv_m0 <- lm(resid_meanAdultSurvival  ~ resid_MeanGroupSize_z, data = groupsize_change)
detrend_adultsurv_m1 <- lm(resid_meanAdultSurvival  ~ resid_MeanGroupSize_z + resid_lograin_earlysummer_z, data = groupsize_change)
detrend_adultsurv_m2 <- update(detrend_adultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_breedingseason_z)
detrend_adultsurv_m3 <- update(detrend_adultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_lograin_twoyear_z)
detrend_adultsurv_m4 <- update(detrend_adultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_temp_summer_Z)
detrend_adultsurv_m5 <- update(detrend_adultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_temp_allyear_z)
detrend_adultsurv_m6 <- update(detrend_adultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_allyear_z)
detrend_adultsurv_m7 <- update(detrend_adultsurv_m1, ~. -resid_lograin_earlysummer_z + resid_spei6_twoyear_z)

detrend_adultsurv_list <- list(detrend_adultsurv_m0, 
                               detrend_adultsurv_m1, detrend_adultsurv_m2, detrend_adultsurv_m3, 
                               detrend_adultsurv_m4, detrend_adultsurv_m5, detrend_adultsurv_m6, detrend_adultsurv_m7)

detrend_adultsurv_AICc <- sapply(detrend_adultsurv_list, FUN = AICc)
detrend_adultsurv_beta <- as.numeric(sapply(detrend_adultsurv_list, FUN = function(x) {signif(coef(x)[3], 3)}))
detrend_adultsurv_beta_se <- as.numeric(sapply(detrend_adultsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][3], 3)}))
detrend_adultsurv_densbeta <- as.numeric(sapply(detrend_adultsurv_list, FUN = function(x) {signif(coef(x)[2], 3)}))
detrend_adultsurv_densbeta_se <- as.numeric(sapply(detrend_adultsurv_list, FUN = function(x) {signif(summary(x)$coef[,2][2], 3)}))
detrend_adultsurv_r2 <- sapply(detrend_adultsurv_list, FUN = function(x) {summary(x)$r.squared})
detrend_adultsurv_p <- c(NA, unlist(sapply(detrend_adultsurv_list, 
                                           FUN = function(x) { broom::tidy(x) %>%
                                               slice(3) %>% 
                                               pull(p.value) %>% 
                                               signif(4) })))

# summary table for population change 
detrend_adultsurv_modtable <- data.frame(variable = env_terms, 
                                         AICc =  detrend_adultsurv_AICc,
                                         beta  = detrend_adultsurv_beta, 
                                         beta_se  = detrend_adultsurv_beta_se,
                                         densbeta = detrend_adultsurv_densbeta, 
                                         densbeta_se = detrend_adultsurv_densbeta_se,
                                         r2 = detrend_adultsurv_r2,
                                         pval = detrend_adultsurv_p) %>% 
  mutate(deltaAICc = AICc - min(AICc), 
         rank = rank(AICc)) %>% 
  dplyr::select(variable, deltaAICc, rank, beta, beta_se, r2, pval, densbeta, densbeta_se)

# Model output for the best model
summary(detrend_adultsurv_list[[1]])

#-----------------------------------------------------------------

# combine the juv + sub survival & adult survival for the supp

# Make the plot for supplementary 
plot25_final <- plot25 + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot26_final <- plot26 + 
  theme(axis.text = element_blank(), 
        axis.title= element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot27_final <- plot27 + 
  theme(axis.text = element_blank(), 
        axis.title= element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
(plot25_final | plot27_final | plot26_final)

plot28_final <- plot28 + 
  theme(plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot29_final <- plot29 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
plot30_final <- plot30 + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        plot.margin = margin(t = 2, b = 1, l = 5, r = 1))
(plot28_final | plot30_final | plot29_final)

(plot25_final | plot27_final | plot26_final) /
  (plot28_final | plot30_final | plot29_final)

############################## END ###########################