################################################################

# R Script for "Environmental change and population regulation in Kalahari meerkats"

# ANALYIS 4: MODELLING GROUP EXTINCTIONS
# Author: Jack Thorley (jbt27@cam.ac.uk)
# Produced using R version 4.2.2

###############################################################

# The script carries out the analyses of group extinctions.

# load in the packages 
lapply(c("tidyverse", "ggplot2", "patchwork","glmmTMB", "DHARMa"), 
       FUN = library, character.only = TRUE)

# set the working directory (edit to location of data files on own machine)
setwd("INSERT FILE PATH")

# Packages I need
lapply(c("tidyverse", "ggplot2", "patchwork","glmmTMB", "DHARMa"), 
       FUN = library, character.only = TRUE)

# load in the required data set
df <- read.csv("MeerkatGroupExtinction.csv", header = TRUE) 

# filter out NA rows where the group's fate was uncertain
df <- filter(df, !is.na(Failed))

# Data info
nrow(df)
length(unique(df$GroupName))
length(unique(filter(df, Fate == "Current2023")$GroupName))   #

# Plot extinctions through time
  df_summ <- df %>% 
    group_by(Year, breeding_season) %>% 
    summarise(nExtinct = sum(Failed == 1, na.rm = T), 
              nAlive = sum(Failed == 0, na.rm = T), 
              nNA = sum(is.na(Failed))) %>% 
    ungroup() %>% 
    mutate(nTotal = (nExtinct + nAlive), 
           PropExtinct = nExtinct/(nExtinct + nAlive)) %>% 
    data.frame()

  plot(PropExtinct ~ Year, data = df_summ)
  plot(PropExtinct ~ log(lagrain_breedingseason), data = df_summ)

#---------------------

# Model the probability of group extinction ~ group size
# Note that Group size refers to the total size of the group in July of a given year

  # Fit the model
  m1 <- glmmTMB(Failed ~ GroupSize + (1|GroupName), 
                data = df, 
                family = "binomial") 
  summary(m1)
  #simulateResiduals(fittedModel = m1, plot = T)
  #testDispersion(m1)

  # predict the model output
  range(df$GroupSize)
  newdf <- data.frame(GroupSize = 4:47, GroupName = "Whiskers")
  newdf$Prob <- predict(m1, newdat = newdf, type = "link", re.form = ~0)
  newdf$SE <- predict(m1, newdat = newdf, type = "link", re.form = ~0, se.fit = T)$se.fit
  inv_logit <- function(x) { 1/(1 + exp(-x))}
  newdf$l95CI <- inv_logit(newdf$Prob - newdf$SE*1.96) 
  newdf$u95CI <- inv_logit(newdf$Prob + newdf$SE*1.96) 
  newdf$Prob <- inv_logit(newdf$Prob) 

# split raw group size data into intervals of 5 to get the sample sizes
  plotdata <- df %>% 
    mutate(n5 =  round(GroupSize / 5) * 5) %>% 
    group_by(n5) %>% 
    summarise(nExtinct = sum(Failed == 1, na.rm = T), 
              nAlive = sum(Failed == 0, na.rm = T), 
              nNA = sum(is.na(Failed))) %>% 
    ungroup() %>% 
    mutate(nTotal = (nExtinct + nAlive), 
           PropExtinct = nExtinct/(nExtinct + nAlive)) 

# plot output
  p1 <- ggplot(data = plotdata) + 
  geom_point(data = plotdata, aes(x = n5, y = PropExtinct, size = nTotal), 
             shape = 1, stroke = 1, colour = "black") + 
  geom_line(data = newdf, aes(x = GroupSize, y = Prob), lwd = 1) + 
  geom_line(data = newdf, aes(x = GroupSize, y = l95CI), lwd = 0.6, lty = 2) + 
  geom_line(data = newdf, aes(x = GroupSize, y = u95CI), lwd = 0.6, lty = 2) + 
  labs(x = "Group size", y = "Probability of group extinction", tag = "A") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black", size = 10), 
        axis.title = element_text(colour = "black", size = 11), 
        legend.position = c(0.85, 0.8), 
        legend.title = element_blank(), 
        legend.text = element_text(size = 11), 
        plot.tag = element_text(size = 16)) + 
  scale_x_continuous(breaks = seq(5, 50, 5), 
                     labels = c("", "10", "", "20", "", "30", "", "40", "", "50")) + 
  scale_y_continuous(breaks = seq(0, 0.4, 0.05), 
                     labels = c("0", "", "0.1", "", "0.2", "", "0.3", "", "0.4"), 
                     limits = c(0, 0.4))

#---------------------

# Compare the the annual probability of group extinction before and after 2012/2012
# Strictly, the post 2012/2013 period includes 2012/2013, the year in which group size and population feel steeply.
  
  # Set up the predictor variable for Pre and post 2012/2013
  df <- mutate(df, TimePeriod = if_else(Year < 2012, "Pre 2012/2013", "Post 2012/2013"))

  # Fir the model 
  m2 <- glmmTMB(Failed ~ TimePeriod + (1|GroupName),
          data = df, 
          family = "binomial") 
  summary(m2)
  res2 <- simulateResiduals(fittedModel = m1, plot = T)
  testDispersion(m2)
  #res2b <- recalculateResiduals(res2, group = df$TimePeriod)
  #plot(res2b)

  # predict the model output
  newdf <- data.frame(TimePeriod = c("Pre 2012/2013", "Post 2012/2013"), GroupName = "Whiskers")
  newdf$Prob <- predict(m2, newdat = newdf, type = "link", re.form = ~0)
  newdf$SE <- predict(m2, newdat = newdf, type = "link", re.form = ~0, se.fit = T)$se.fit
  inv_logit <- function(x) { 1/(1 + exp(-x))}
  newdf$l95CI <- inv_logit(newdf$Prob - newdf$SE*1.96) 
  newdf$u95CI <- inv_logit(newdf$Prob + newdf$SE*1.96) 
  newdf$Prob <- inv_logit(newdf$Prob) 
  newdf$TimePeriod <- factor(if_else(newdf$TimePeriod == "Pre 2012/2013", "Pre\n2012/2013", "Post\n2012/2013"), levels = c("Pre\n2012/2013", "Post\n2012/2013"))

  # produce the plot 
  p2 <- ggplot(data = newdf, aes(x = TimePeriod, y = Prob)) +
    geom_errorbar(aes(ymin = l95CI, ymax = u95CI), lwd = 0.6, width = 0) + 
    geom_point(shape = 21, size = 4, fill = "white", stroke =1) +
    labs(x = "Group size",  y = "Probability of group extinction", tag = "B") + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = "black", size = 10), 
          axis.title.y =  element_text(colour = "black", size = 11),
          axis.title.x = element_blank(), 
          legend.position = c(0.85, 0.8), 
          legend.title = element_blank(), 
          legend.text = element_text(size = 11), 
          plot.tag = element_text(size = 16)) + 
    scale_y_continuous(breaks = seq(0, 0.4, 0.05), 
                       labels = c("0", "", "0.1", "", "0.2", "", "0.3", "", "0.4"), 
                       limits = c(0, 0.4))

# combine the two plots for export
p1 + p2 + 
  plot_layout(widths = c(3,2))

##################################### END #####################################