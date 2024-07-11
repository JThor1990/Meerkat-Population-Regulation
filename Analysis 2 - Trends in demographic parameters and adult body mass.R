################################################################

# R Script for "Environmental change and population regulation in Kalahari meerkats"

# ANALYSIS 2: TRENDS IN DEMOGRAPHIC PARAMETERS AND ADULT BODY MASS
# Produced using R version 4.2.2

###############################################################

# The script characterises the long-term trends in multiple demographic parameters and plots them all together so that the flucutuations in density, group size, age structure, vital rates, and body mass can be viewed together. 
# It also statistically tests for the presence of directional trends and changepoints in all time series. 

# load in the packages 
lapply(c("ggplot2", "patchwork", "tidyverse", "trend"), FUN = library, character.only = TRUE)

# set the working directory (edit to location of data files on own machine)
setwd("INSERT FILE PATH")

# load in the required data sets
  # Density and average group sizes
  density_df <- read.csv("MeekatPopulationDensity.csv", header = TRUE) %>% 
    mutate(Date = as.Date(strptime(Date, format = "%d/%m/%Y")))
  # Population age structure over time 
  agestructure <-  read.csv("MeerkatAgeStructure.csv", header = TRUE) %>% 
    mutate(MonthStart = as.Date(MonthStart))
  # Demographic parameters
  annual_demog <-  read.csv("MeerkatTrends.csv", header = TRUE) %>% 
    mutate(MidYear =  as.Date(strptime(MidYear, format = "%d/%m/%Y")))
  # Body mass
  bodymass <- read.csv("MeekatPopulationAnnualBodyMass.csv", header = TRUE)
  bodymass_seasonal <- read.csv("MeekatPopulationSeasonalBodyMass.csv", header = TRUE)

#=====================================================================

# AVERAGE GROUP SIZE AND POPULATION DENSITY TRENDS

#=====================================================================

  # Note the date span of these data 
  range(density_df$Date[!is.na(density_df$MeanGroupSize)]) # Group size: April 1998 to February 2024
  range(density_df$Date[!is.na(density_df$IndDensity_KDE)])  # Individual density: August 2002 to February 2024
  range(density_df$Date[!is.na(density_df$GroupDensity_KDE)])  # Group density: August 2002 to February 2024
  # The density data starts later to coincide with when GPS data started being collected, which we used to 
  # estimate home ranges, and the from there, population density. 

  # Estimate the correlations between the measures, their directional trends, and test for the presence of a changepoint
  
  # Correlations among the measures
  # Group size vs pop density 
  cor.test(density_df$MeanGroupSize, density_df$IndDensity_KDE, method = "pearson")
  # Group size vs group density 
  cor.test(density_df$MeanGroupSize, density_df$GroupDensity_KDE, method = "pearson")
  # Pop density vs group density 
  cor.test(density_df$IndDensity_KDE, density_df$GroupDensity_KDE, method = "pearson")

  # Pettitt's test is a non-parametric test that looks for significant changes in the central tendency of the time series 
  # The null hypothesis is that there is no significant change
  # Group size Pettitt's test: 
  pettitt.test(density_df$MeanGroupSize)
  density_df$Date[176] # 2012-11-01

  # Population density Pettitt's test: 
  pettitt.test(density_df$IndDensity_KDE[!is.na(density_df$IndDensity_KDE)])
  density_df$Date[!is.na(density_df$IndDensity_KDE)][125] # 2012-12-01

  # Group Density Pettitt's test: 
  pettitt.test(density_df$GroupDensity_KDE[!is.na(density_df$GroupDensity_KDE)])
  density_df$Date[!is.na(density_df$GroupDensity_KDE)][125] # 2012-12-01
  # All aligned with one another

  # Mean average group size before this and after the changepoints
  mean(density_df$MeanGroupSize[density_df$Date <= as.Date("2012-11-01")]) # 15.82
  sd(density_df$MeanGroupSize[density_df$Date <= as.Date("2012-11-01")])   # 3.74
  mean(density_df$MeanGroupSize[density_df$Date > as.Date("2012-11-01")])  # 11.74
  sd(density_df$MeanGroupSize[density_df$Date > as.Date("2012-11-01")])    # 3.50
  t.test(density_df$MeanGroupSize[density_df$Date <= as.Date("2012-11-01")], 
         density_df$MeanGroupSize[density_df$Date > as.Date("2012-11-01")])

  # Mean population density before and after the changepoint
  mean(density_df$IndDensity_KDE[density_df$Date <=  as.Date("2012-12-01")], na.rm = TRUE) # 9.38
  sd(density_df$IndDensity_KDE[density_df$Date <= as.Date("2012-12-01")], na.rm = TRUE) # 2.25
  mean(density_df$IndDensity_KDE[density_df$Date >  as.Date("2012-12-01")]) # 5.25
  sd(density_df$IndDensity_KDE[density_df$Date > as.Date("2012-12-01")]) # 1.53
  t.test(density_df$IndDensity_KDE[density_df$Date <= as.Date("2012-11-01")], 
         density_df$IndDensity_KDE[density_df$Date > as.Date("2012-11-01")])

  # Run the Mann-Kendall tests (Kendall's tau), a non-parametric test for linear trends
  # MK group size
  mk.test(density_df$MeanGroupSize)
  groupsizebeta <- sens.slope(density_df$MeanGroupSize)
  groupsizebeta$estimates*12 ; groupsizebeta$conf.int*12 # convert slope to yearly rate of change
  # MK pop density 
  mk.test(density_df$IndDensity_KDE[!is.na(density_df$IndDensity_KDE)])
  popdensitybeta <- sens.slope(density_df$IndDensity_KDE[!is.na(density_df$IndDensity_KDE)])
  popdensitybeta$estimates*12 ; popdensitybeta$conf.int*12
  # MK group density 
  mk.test(density_df$GroupDensity_KDE[!is.na(density_df$GroupDensity_KDE)])
  groupdensitybeta <- sens.slope(density_df$GroupDensity_KDE[!is.na(density_df$GroupDensity_KDE)])
  groupdensitybeta$estimates*12 ; groupdensitybeta$conf.int*12

  # Identify the biggest single crashes in group size/pop density from July to July
  yearly_averages <- density_df %>% 
    filter(Month == 7) %>%
    mutate(RelChangeDensity = IndDensity_KDE/lag(IndDensity_KDE ), 
           RelChangeGroupSize = MeanGroupSize /lag(MeanGroupSize), 
           Season = paste0(Year - 1, "/", Year)) %>% 
    dplyr::select(Season, nGroups, PopulationSize, RelChangeDensity, RelChangeGroupSize) %>% 
    data.frame()
  arrange(yearly_averages, RelChangeDensity)    # 2012/2013, 2011/2012, 2003/2004
  arrange(yearly_averages, RelChangeGroupSize)  # 2015/2016, 2012/2013, 2007/2008       

  # Plot the change in group size, individual density and population density. 
  # Plot theme
  plot_theme <- theme_bw() + 
    theme(axis.title = element_text(size = 12, colour = "black"), 
          axis.text = element_text(size = 10, colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

  # create labels
  season_starts <- seq.Date(as.Date("1997-07-01"), as.Date("2024-07-01"), by = "year")

  # Group size plot
  p_groupsize <- ggplot(density_df, aes(x = Date, y = MeanGroupSize)) +
    geom_vline(xintercept = season_starts, linetype = 2, alpha = 0.05) +
    geom_vline(aes(xintercept =  as.Date("2012-11-01")), linetype = 2) +
    geom_ribbon(aes(ymin = l95GroupSize, 
                    ymax = u95GroupSize), 
                fill = "black", alpha = 0.1, linewidth = 0.8, colour = NA) + 
    geom_path(colour = "black", alpha = 1, linewidth = 0.8) + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.line = element_line(colour = "black"),
          plot.margin = margin(t = 0, b = 0)) +
    labs(y = "Group size", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1998-04-01"), as.Date("2024-03-01"))) +
    scale_y_continuous(breaks = seq(5, 30, 5), limits = c(4, 31))

  # Individual population density plot 
  p_inddensity <- ggplot(density_df, aes(x = Date, y = IndDensity_KDE)) +
    geom_vline(xintercept = season_starts, linetype = 2, alpha = 0.05) +
    geom_vline(aes(xintercept =  as.Date("2012-12-01")), linetype = 2) +
    geom_ribbon(aes(ymin = l95IndDensity_KDE, 
                    ymax = u95IndDensity_KDE), 
                fill = "black", alpha = 0.1, linewidth = 0.8, colour = NA) + 
    geom_path(colour = "black", alpha = 1, linewidth = 0.8) + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.line = element_line(colour = "black"), 
          plot.margin = margin(t = 0, b = 0)) +
    labs(y =  expression(atop(Population~density, (meerkats/km^2))), x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1998-04-01"), as.Date("2024-03-01"))) +
    scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 20))

  # Group density plot 
  #p_groupdensity <- ggplot(density_df, aes(x = Date, y = GroupDensity_KDE)) +
  #  geom_vline(xintercept = season_starts, linetype = 2, alpha = 0.05) +
  #  geom_vline(aes(xintercept =  as.Date("2012-12-01")), 
  #             linetype = 2) +
  #  geom_ribbon(aes(ymin = l95GroupDensity_KDE, 
  #                  ymax = u95GroupDensity_KDE), 
  #              fill = "black", alpha = 0.05, linewidth = 0.8, colour = NA) + 
  #  geom_path(colour = "black", alpha = 1, linewidth = 0.8) + 
  #  plot_theme +
  #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
  #        axis.line = element_line(colour = "black"), 
  #        plot.margin = margin(t = 0, b = 0)) +
  #  labs(y =  expression(atop(Group~density, (meerkat~groups/km^2))), x = "Year") +
  #  scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
  #               limits = c(as.Date("1998-04-01"), as.Date("2024-03-01"))) +
  #  scale_y_continuous(breaks = seq(0.25, 1.25, 0.25), limits = c(0.25, 1.25))

  # For now plot them on top of each other 
  p1 <- p_groupsize +
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(), 
          axis.text.y = element_text(size = 9), 
          axis.title.y = element_text(size = 11), 
          axis.ticks.x = element_blank(),
          axis.line = element_line(colour = "black")) 

  p2 <- p_inddensity + 
    theme(axis.text.x = element_text(size = 9), 
          axis.text.y = element_text(size = 9), 
          axis.title.y = element_text(size = 11))
  
  p1/p2 # look at the plot 

#=====================================================================
  
# PLOT THE AGE STRUCTURE OF THE POPULATION OVER TIME
  
#=====================================================================
  
  # Plot the age structure 
  # make breeding season notes for all plots
  season_starts <- seq.Date(as.Date("1997-07-01"), as.Date("2024-07-01"), by = "year")
  
  # Make sure the categorical levels are in the correct order 
  agestructure$AgeClass <- factor(agestructure$AgeClass , levels = c("Adults", "Subadults", "Juveniles", "Pups"))
  
  # Age-structure plot 
  p_agestructure <- ggplot(agestructure, aes(x = MonthStart, y = Proportion, fill = AgeClass)) +
    geom_area(colour = "#0000001A") + 
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    coord_cartesian(expand = F) + 
    plot_theme + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.title.x = element_blank(), 
          axis.title.y = element_text(size = 12),
          legend.title = element_blank(), 
          legend.text = element_text(size = 9)) +
    labs(y = "Population\nproportion") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01")))
  
  # Extract the color scale from the plot for use later
  col_scale <- dplyr::select(ggplot_build(p_agestructure)$data[[1]], fill, group) %>% 
    distinct() %>% 
    mutate(AgeClass = case_when(group == 1 ~ "Adult", 
                                group == 2 ~ "Subadult", 
                                group == 3 ~ "Juveniles", 
                                group == 4 ~ "Pups"))
  
#=====================================================================
  
# TRENDS IN DEMOGRAPHIC PARAMETERS: PLOTS AND STATISTICS 
  
#===================================================================== 
  
  # Plot the average number of dominant female pregnancies over time
  p_dompregnancies <- ggplot(annual_demog, 
                             aes(x = MidYear, y = mean_dompregnancies)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = mean_dompregnancies - sem_dompregnancies, 
                      ymax = mean_dompregnancies + sem_dompregnancies),
                  width = 0) +
    geom_point(pch = 21, fill = "black", size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black")) + 
    coord_cartesian(expand = F) +
    labs(y = "Number of\ndominant pregnancies", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) + 
    scale_y_continuous(breaks = seq(2, 5, 0.5), 
                       labels = c("2", "", "3", "", "4", "", "5"), 
                       limits = c(2, 4.7))

#-------------------------  
   
  # plot the average number of subordinate female pregnancies (absolute)
  p_subpregnancies_abs <- ggplot(annual_demog, 
                                 aes(x = MidYear, y = mean_subpregnancies)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = mean_subpregnancies - sem_subpregnancies, 
                      ymax = mean_subpregnancies + sem_subpregnancies),
                  width = 0) +
    geom_point(pch = 21, fill = "white", size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black")) + 
    coord_cartesian(expand = F) +
    labs(y = "Number of\nsubordinate pregnancies", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) + 
    scale_y_continuous(breaks = seq(2, 10, 1), 
                       labels = c("2", "", "4", "", "6", "", "8", "", "10"), 
                       limits = c(0.5, 8.5))

#-------------------------  
  
  # Plot the average number of subordinate female pregnancies per capita (absolute)
  p_subpregnancies_percapita <- ggplot(annual_demog, 
                                       aes(x = MidYear, y = mean_subpregnancies_percapita)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = mean_subpregnancies_percapita - sem_subpregnancies_percapita, 
                      ymax = mean_subpregnancies_percapita + sem_subpregnancies_percapita),
                  width = 0) +
    geom_point(pch = 21, fill = "white", size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black")) + 
    coord_cartesian(expand = F) +
    labs(y = "Subordinate pregnancies \n per capita", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) + 
    scale_y_continuous(breaks = seq(0, 1.2, 0.1), 
                       labels = c("0", "", "0.2", "", "0.4", "", "0.6", "", 
                                  "0.8", "", "1.0", "", "1.2"), 
                       limits = c(0.1, 1.2))

#-------------------------  
  
  # Plot the number of litters born to dominants and subordinates
  p_numlitters <- ggplot(annual_demog, 
                         aes(x = MidYear - months(1), y = mean_domlitters)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = mean_domlitters - sem_domlitters, 
                      ymax = mean_domlitters + sem_domlitters),
                  width = 0) +
    geom_point(pch = 21, fill = "black", size = 2, stroke = 1)  + 
    geom_path(aes(x = MidYear + months(1), 
                  y = mean_sublitters), colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(x = MidYear + months(1), 
                      ymin = mean_sublitters - sem_sublitters, 
                      ymax = mean_sublitters + sem_sublitters),
                  width = 0) +
    geom_point(aes(x = MidYear + months(1), 
                   y = mean_sublitters), pch = 21, fill = "white", size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black")) + 
    coord_cartesian(expand = F) +
    labs(y = "Number of\nlitters born", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) + 
    scale_y_continuous(breaks = seq(0, 3.5, 0.5), 
                       labels = c("0", "", "1", "", "2", "", "3", ""), 
                       limits = c(0, 3.5))

#-------------------------  
  
  # Create a combined plot for the supplementary
  p_subpregnancies_abs <- p_subpregnancies_abs + 
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank()) 
  p_subpregnancies_percapita <- p_subpregnancies_percapita + 
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank()) 
  p_dompregnancies <- p_dompregnancies + 
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank()) 
  
  p_dompregnancies / p_subpregnancies_percapita / p_numlitters
  
  # Changepoints and linear trends
  # Run Pettitt's test to identify possible changepoint in each time series
  # Dominant pregnancies/year
  pettitt.test(annual_demog$mean_dompregnancies) # n.s. (p = 0.74)
  # Subordiante pregnancies/year (absolute)
  pettitt.test(annual_demog$mean_subpregnancies) # n.s. (p = 0.018)
  # Subordinate pregnancies/year (per capita)
  pettitt.test(annual_demog$mean_subpregnancies_percapita) # n.s. (p = 0.53)
  # Dominant litters born
  pettitt.test(annual_demog$mean_domlitters) # n.s. (p = 0.08)
  # Subordinate litters born
  pettitt.test(annual_demog$mean_sublitters) # n.s. (p = 0.48)
  
  # Run Mann-Kendall test to test for linear trends in the time series
  #  Dominant pregnancies/year
  mk.test(annual_demog$mean_dompregnancies) # p = 0.47
  sens.slope(annual_demog$mean_dompregnancies)
  # Sub pregnancies/year (absolute)
  mk.test(annual_demog$mean_subpregnancies) # p = 0.0008
  sens.slope(annual_demog$mean_subpregnancies)
  # Sub pregnancies/year (per capita)
  mk.test(annual_demog$mean_subpregnancies_percapita) # p = 0.22
  sens.slope(annual_demog$mean_subpregnancies_percapita)
  #  Dominant litters born 
  mk.test(annual_demog$mean_domlitters) # p = 0.044
  sens.slope(annual_demog$mean_domlitters) 
  #  Subordinate litters born
  mk.test(annual_demog$mean_sublitters) # p = 0.33
  sens.slope(annual_demog$mean_sublitters)
  mean(annual_demog$mean_sublitters) ; sd(annual_demog$mean_sublitters) # 0.88 +- 0.34

#------------------------- 
    
  # Plot fecundity: average number of pups reaching emergence / year
  p_fecundity <- ggplot(annual_demog, 
                        aes(x = MidYear, y = mean_fecundity_pergroup)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = mean_fecundity_pergroup - sem_fecundity_pergroup, 
                      ymax = mean_fecundity_pergroup + sem_fecundity_pergroup),
                  width = 0) +
    geom_point(pch = 21, fill = "white", size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black")) + 
    coord_cartesian(expand = F) +
    labs(y = "Fecundity\n(pups to emergence)", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) +
    scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0, 18))
  
#------------------------- 
  
  # Plot recruitment: average number of pups reaching nutritional independence (90 days)
  p_recruitment <- ggplot(annual_demog, 
                          aes(x = MidYear, y = mean_recruitment_pergroup)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = mean_recruitment_pergroup - sem_recruitment_pergroup, 
                      ymax = mean_recruitment_pergroup + sem_recruitment_pergroup), 
                  width = 0) +
    geom_point(pch = 21, fill = "white", size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black")) +
    coord_cartesian(expand = F) +
    labs(y = "Recruitment \n (pups to independence)", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) +
    scale_y_continuous(breaks = seq(0, 16, 4), limits = c(0, 18))

#-------------------------  

    # Plot the survival of pups from emergence to nutritional independence (recruitment)  
  p_pupsurvival <- ggplot(annual_demog, 
                          aes(x = MidYear, y = mean_pupsurvival_pergroup)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = mean_pupsurvival_pergroup - sem_pupsurvival_pergroup, 
                      ymax = mean_pupsurvival_pergroup + sem_pupsurvival_pergroup), 
                  width = 0) +
    geom_point(pch = 21, fill = "white", size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black")) +
    coord_cartesian(expand = F) +
    labs(y = "Pup\nsurvival", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0.4, 1))
  
  # Changepoints and linear trends
  # Run Pettitt's test to identify possible changepoint in each time series
  # Fecundity
  pettitt.test(annual_demog$mean_fecundity_pergroup) # n.s. (p = 1)
  # Recruitment (to independence)
  pettitt.test(annual_demog$mean_recruitment_pergroup) # n.s. (p = 0.31)
  # Pup survival
  # Group-level average
  pettitt.test(annual_demog$mean_pupsurvival_pergroup) # sig (p = 0.0012)
  annual_demog$MidYear[13]  # 2011-01-01
  # Overall population level 
  pettitt.test(annual_demog$totalPupSurvival) # sig (p = 0.0012)
  annual_demog$MidYear[12]  # 2010-01-01
  
  p_pupsurvival <- p_pupsurvival + 
    geom_vline(aes(xintercept =  as.Date("2011-07-01")), linetype = 2) 
  
  # Run Mann-Kendall test for linear trends in the time series
  # Fecundity
  mk.test(annual_demog$mean_fecundity_pergroup) # n.s. (p = 0.76)
  sens.slope(annual_demog$mean_fecundity_pergroup)
  # Recruitment 
  mk.test(annual_demog$mean_recruitment_pergroup) # n.s. (p = 16)
  sens.slope(annual_demog$mean_recruitment_pergroup)
  # Pup survival 
  # Group-level average pup survival
  mk.test(annual_demog$mean_pupsurvival_pergroup) # n.s. (p = 0.006)
  pupsurv_groupslope <- sens.slope(annual_demog$mean_pupsurvival_pergroup)
  pupsurv_groupslope$estimates*100 # convert to percentage
  pupsurv_groupslope$conf.int*100
  # Population level overall pup survival
  mk.test(annual_demog$totalPupSurvival) # n.s. (p = 0.008)
  sens.slope(annual_demog$totalPupSurvival)
  pupsurv_groupslope2 <- sens.slope(annual_demog$totalPupSurvival)
  pupsurv_groupslope2$estimates*100 # convert to percentage
  pupsurv_groupslope2$conf.int*100
  
  # What is the average pup survival either side of the identified changepoint
  mean(annual_demog$totalPupSurvival[annual_demog$year <= 2009])
  sd(annual_demog$totalPupSurvival[annual_demog$year <= 2009])
  mean(annual_demog$totalPupSurvival[annual_demog$year > 2009])
  sd(annual_demog$totalPupSurvival[annual_demog$year > 2009])
  t.test(annual_demog$totalPupSurvival[annual_demog$year <= 2009], 
         annual_demog$totalPupSurvival[annual_demog$year > 2009])
  
  # I want to combine fecundity and recruitment into a single plot and overtop survival
  # (I will also edit the date slightly for each to offset the two)
  fecundity_df <- dplyr::select(annual_demog, breeding_season, MidYear, ngroups, 
                                mean_fecundity_pergroup:sem_fecundity_pergroup) %>% 
    mutate(MidYear = MidYear - months(2))
  
  recruitment_df <- dplyr::select(annual_demog, breeding_season, MidYear, ngroups, 
                                  mean_recruitment_pergroup:sem_recruitment_pergroup) %>% 
    mutate(MidYear = MidYear + months(2))
  
  # the combined plot 
  p_combo <-  ggplot(fecundity_df, aes(x = MidYear, y = mean_fecundity_pergroup)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.7, linewidth = 0.5) +  # fecundity
    geom_path(data = recruitment_df, aes(y = mean_recruitment_pergroup),  # recruitment
              colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = mean_fecundity_pergroup - sem_fecundity_pergroup, 
                      ymax = mean_fecundity_pergroup + sem_fecundity_pergroup), 
                  width = 0) +                                    # fecundity 
    geom_errorbar(data = recruitment_df,
                  aes(ymin = mean_recruitment_pergroup - sem_recruitment_pergroup, 
                      ymax = mean_recruitment_pergroup + sem_recruitment_pergroup, 
                      y = mean_recruitment_pergroup), 
                  width = 0) +                                    # recruitment
    geom_point(pch = 21, fill = "white", size = 2, stroke = 1) + # fecundity
    geom_point(data = recruitment_df, aes(y = mean_recruitment_pergroup), 
               pch = 21, fill = "lightgrey", size = 2, stroke = 1)  + # recruitment
    plot_theme +
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          axis.line = element_line(colour = "black")) +
    coord_cartesian(expand = F) +
    labs(y = "Pup\nrecruitment", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) +
    scale_y_continuous(breaks = seq(0, 20, 4), limits = c(0, 18))
  
  # Add the figure legend
  p_combo <- p_combo + 
    annotate("point", x = as.Date("2023-07-01"), y = 2.5, pch = 21, fill = "white", size = 2, stroke = 1) + 
    annotate("point", x = as.Date("2023-07-01"), y = 1, pch = 21, fill = "lightgray", size = 2, stroke = 1) + 
    annotate("text", x = as.Date("2021-02-15"), y = 2.7, label = "Emerged pups") + 
    annotate("text", x = as.Date("2021-01-01"), y = 1.2, label = "Recruited pups")

  # Start to put together the figure 
  # (I will tweak the aesthetics further later on. For now this is sufficient)
  p_agestructure <- p_agestructure + 
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank())
  
  p_pupsurvival <- p_pupsurvival + 
    theme(axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12), 
          axis.title.x = element_blank())
  
  p_agestructure / p_combo / p_pupsurvival
  
#-------------------------
  
  # Plot the survival of the juveniles, subadults, and adults
  p_survival <- ggplot(annual_demog, 
                       aes(x = MidYear, y = mean_adultsurvival_pergroup)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    # Adults
    geom_path(colour = "black", alpha = 1, linewidth = 0.6) + 
    geom_ribbon(aes(ymin = mean_adultsurvival_pergroup - sem_adultsurvival_pergroup, 
                    ymax = mean_adultsurvival_pergroup + sem_adultsurvival_pergroup), 
                alpha = 0.1) +
    geom_point(pch = 21, fill = col_scale$fill[1], size = 2, stroke = 1)  + 
    # Subadults
    geom_path(aes(y = mean_subadultsurvival_pergroup), colour = "black", 
              alpha = 1, linewidth = 0.6) + 
    geom_ribbon(aes(ymin = mean_subadultsurvival_pergroup - sem_subadultsurvival_pergroup, 
                    ymax = mean_subadultsurvival_pergroup + sem_subadultsurvival_pergroup), 
                alpha = 0.1) +
    geom_point(aes(y = mean_subadultsurvival_pergroup), 
               pch = 21, fill = col_scale$fill[2], size = 2, stroke = 1)  + 
    # Juveniles
    geom_path(aes(y = mean_juvenilesurvival_pergroup),
              colour = "black", alpha = 1, linewidth = 0.6) + 
    geom_ribbon(aes(ymin = mean_juvenilesurvival_pergroup - sem_juvenilesurvival_pergroup, 
                    ymax = mean_juvenilesurvival_pergroup + sem_juvenilesurvival_pergroup), 
                alpha = 0.1) +
    geom_point(aes(y = mean_juvenilesurvival_pergroup), 
               pch = 21, fill = col_scale$fill[3], size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black")) +
    coord_cartesian(expand = F) +
    labs(y = "Adult, SubAdult, Juvenile survival", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0.4, 1))
  
  # Combine the survival of all the 'non-pup' categories
  p_allsurvival <- ggplot(annual_demog, aes(x = MidYear, y = mean_allsurvival_pergroup)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.7, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = mean_allsurvival_pergroup - sem_allsurvival_pergroup, 
                      ymax = mean_allsurvival_pergroup + sem_allsurvival_pergroup), 
                  width = 0) +
    geom_point(pch = 21, fill = "white", size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black")) +
    coord_cartesian(expand = F) +
    labs(y = "Non-pup\nsurvival", x = "Year") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2024-02-01"))) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0.4, 1))
  
  # Changepoints and linear trends. 
  # Run Pettitt's test to identify possible changepoint in each time series
  # Survival of all non-pups (juvs, subs, & adults combined)
  # Group level average
  pettitt.test(annual_demog$mean_allsurvival_pergroup) # sig (p = 0.023)
  annual_demog$MidYear[14]
  # Overall population level
  pettitt.test(annual_demog$totalallsurvival) # sig (p = 0.023)
  annual_demog$MidYear[14]
  
  # Juveniles 
  # Group level average
  pettitt.test(annual_demog$mean_juvenilesurvival_pergroup
               [!is.na(annual_demog$mean_juvenilesurvival_pergroup)]) # n.s. (p = 0.12)
  # Overall popoulation level 
  pettitt.test(annual_demog$totaljuvenilesurvival
               [!is.na(annual_demog$totaljuvenilesurvival)]) # nearly sig (p = 0.063)
  
  # Subadults 
  # Group-level average
  pettitt.test(annual_demog$mean_subadultsurvival_pergroup) # n.s. (p = 0.53)
  # Overall population level 
  pettitt.test(annual_demog$totalsubadultsurvival) # n.s. (p = 0.65)
  
  # Adults 
  # Group-level average
  pettitt.test(annual_demog$mean_adultsurvival_pergroup) # n.s. (p = 0.10)
  # Overall population level 
  pettitt.test(annual_demog$totaladultsurvival) # n.s. (p = 0.12)
  
  # Linear trends Mann-Kendall test
  # Survival of all non-pups 
  # Group-level average 
  mk.test(annual_demog$mean_allsurvival_pergroup) # sig (p = 0.018)
  sens.slope(annual_demog$mean_allsurvival_pergroup)
  # Overall population level
  mk.test(annual_demog$totalallsurvival) # sig (p = 0.008)
  allsurvival_slope2 <- sens.slope(annual_demog$totalallsurvival)

  # Juveniles 
  # Group-level average 
  mk.test(annual_demog$mean_juvenilesurvival_pergroup
          [!is.na(annual_demog$mean_juvenilesurvival_pergroup)]) # n.s. (p = 0.044)
  sens.slope(annual_demog$mean_juvenilesurvival_pergroup
             [!is.na(annual_demog$mean_juvenilesurvival_pergroup)])
  # Overall population level
  mk.test(annual_demog$totaljuvenilesurvival
          [!is.na(annual_demog$totaljuvenilesurvival)]) # sig (p = 0.022)
  sens.slope(annual_demog$totaljuvenilesurvival
             [!is.na(annual_demog$totaljuvenilesurvival)])
  
  # Subadults 
  # Group-level average 
  mk.test(annual_demog$mean_subadultsurvival_pergroup) # sig (p = 0.17)
  sens.slope(annual_demog$mean_subadultsurvival_pergroup)
  # Overall population level
  mk.test(annual_demog$totalsubadultsurvival) # sig (p = 0.37)
  sens.slope(annual_demog$totalsubadultsurvival)
  
  # Adults
  # Group-level average 
  mk.test(annual_demog$mean_allsurvival_pergroup) # sig (p = 0.0182
  sens.slope(annual_demog$mean_allsurvival_pergroup)
  # Overall population level 
  mk.test(annual_demog$totaladultsurvival) # sig (p = 0.055)
  sens.slope(annual_demog$totaladultsurvival)
  
  # Add the significant changepoint to the non-pup survival plot
  p_allsurvival <- p_allsurvival + 
    geom_vline(aes(xintercept =  as.Date("2012-07-01")),linetype = 2) +
    theme(axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12), 
          axis.title.x = element_blank())
  
#=====================================================================
  
# TRENDS IN ADULT BODY MASS: PLOTS AND STATISTICS 

#===================================================================== 
  
  bodymass <- bodymass %>%  
    mutate(MidYear = as.Date(paste0(year + 1, "-01-01"))) # add date for plotting

  # Plot adult body mass over time
  p_bodymass <- ggplot(bodymass, aes(x = MidYear, y = avgMass)) +
    geom_vline(xintercept = season_starts,  linetype = 2, alpha = 0.05) +
    geom_path(colour = "black", alpha = 0.8, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = avgMass - SEM, 
                      ymax = avgMass + SEM), 
                  width = 0) +
    geom_point(pch = 21, fill = "white", size = 2, stroke = 1)  + 
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.text.y = element_text(size = 10),
          axis.line = element_line(colour = "black") ,
          plot.tag = element_text(size = 14),  
          plot.margin = margin(t = 0, b = 0)) +
    coord_cartesian(expand = F) +
    labs(y = "Adult\nbody mass", x = "Year", tag = "C") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2025-03-01"))) + 
    scale_y_continuous(breaks = seq(620, 720, 20), limits = c(610, 730))
  
  # Changepoints and linear trends. 
  # Average adult body mass at the annual level
  pettitt.test(bodymass$avgMass)
  bodymass$MidYear[8]
  mk.test(bodymass$avgMass) 
  sens.slope(bodymass$avgMass) 
  
  # Add the significant changepoint to the plot
  p_bodymass <- p_bodymass + 
    geom_vline(aes(xintercept =  as.Date("2006-07-01")),linetype = 2)
  
  # Repeat the analysis in each season of the year 
  # Average adult body mass in winter
  mk.test(bodymass_seasonal$avgMass[bodymass_seasonal$Season == "Winter"]) 
  sens.slope(bodymass_seasonal$avgMass[bodymass_seasonal$Season == "Winter"]) 
  
  # Average adult body mass in early summer
  mk.test(bodymass_seasonal$avgMass[bodymass_seasonal$Season == "EarlySummer"]) 
  sens.slope(bodymass_seasonal$avgMass[bodymass_seasonal$Season == "EarlySummer"]) 
  
  # Average adult body mass in mid-to-ate summer
  mk.test(bodymass_seasonal$avgMass[bodymass_seasonal$Season == "LateSummer"]) 
  sens.slope(bodymass_seasonal$avgMass[bodymass_seasonal$Season == "LateSummer"]) 
  
  # Plot the changes at different times of the year for the supporting information 
  bodymass_seasonal <- bodymass_seasonal %>% 
    mutate(Label = factor(case_when(Season == "Winter" ~ "Winter\n(May-Aug)", 
                                    Season == "EarlySummer" ~ "Early summer\n(Sep-Nov)", 
                                    Season == "LateSummer" ~ "Late summer\n(Dec-Apr)"), 
                          levels = c("Winter\n(May-Aug)", 
                                     "Early summer\n(Sep-Nov)",
                                     "Late summer\n(Dec-Apr)")))
  
  # Add text to give the linear rate of change predicted by Sen's slope.
  bodymass_seasonal_text <- data.frame(Label =  c("Winter\n(May-Aug)", 
                                                  "Early summer\n(Sep-Nov)",
                                                  "Late summer\n(Dec-Apr)"), 
                                       year = 2014, 
                                       avgMass = 750, 
                                       Text = c("β = -1.80g/year\np = 0.010", 
                                                "β = -2.48g/year\np = 0.008", 
                                                "β = -1.22g/year\np = 0.072"))
  
  # make the plot                              
  p_bodymass_seasonal <- ggplot(bodymass_seasonal, aes(x = year, y = avgMass)) +
    geom_path(colour = "black", alpha = 0.5, linewidth = 0.5) + 
    geom_errorbar(aes(ymin = avgMass - SEM, 
                      ymax = avgMass + SEM), 
                  width = 0) +
    geom_point(pch = 21, fill = "white", size = 2, stroke = 1)  + 
    geom_smooth(aes(group = Label), method = "lm", alpha = 0.3, fill = "lightblue") +
    geom_text(data = bodymass_seasonal_text, aes(label = Text), colour = "blue", 
              size = 3) +
    facet_wrap(~Label) +
    plot_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9), 
          axis.title.y = element_text(size = 11), 
          strip.text = element_text(size = 11)) +
    labs(y = "Adult body mass (g)", x = "Breeding season") +
    scale_x_continuous(breaks = 1998:2022, 
                       labels = c("98/99", "","","", 
                                  "02/03", "","","", 
                                  "06/07", "","","", 
                                  "10/11", "","","", 
                                  "14/15", "","","", 
                                  "18/19", "","","", 
                                  "22/23")) +
    scale_y_continuous(breaks = seq(575, 775, 25))
  
  
#=====================================================================
  
# PLOTTING ALL THE TIME SERIES TOGETHER TO PRODUCE THE FINAL FIGURE
  
#===================================================================== 
  
  # I need to ensure that all plots line up exactly, sharing the same x axis limits, 
  # and also to amend the plot margins so that it looks good aesthetically
  
  # PANEL A:
  
  # Update date the population density time series 
  p_A1 <- p_inddensity + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          plot.tag = element_text(size = 14)) + 
    labs(tag = "A", y = "Population\ndensity") + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1998-04-15"), as.Date("2023-11-15"))) 
  
  # Update the group size plot
  p_A2 <- p_groupsize +
    theme(plot.tag = element_blank(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12)) + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1998-04-15"), as.Date("2023-11-15"))) 
  
  # update the the age structure plot
  p_A3 <- p_agestructure + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2025-03-01"))) +
    theme(axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          plot.margin = margin(t = 0, b = 0))
  
  partA <- p_A1 / p_A2 / p_A3

#------------------------- 
  
  # PANEL B:

  # Update pup emergence/recruitment plot updated
  p_B1 <- p_combo + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2025-03-01"))) +
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          plot.margin = margin(t = 0, b = 0), 
          plot.tag = element_text(size = 14)) + 
    labs(tag = "B") 
  
  # Update the pup survival plot
  p_B2 <- p_pupsurvival + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2025-03-01"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          plot.margin = margin(t = 0, b = 0))
  
  # Update the non-pup survival plot
  p_B3 <- p_allsurvival + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", 
                 limits = c(as.Date("1997-01-01"), as.Date("2025-03-01"))) +
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          plot.margin = margin(t = 0, b = 0))
  
  partB <- p_B1 / p_B2 / p_B3
  
#------------------------- 
  
  # PANEL C: 
  
  # This should just be retained at the body mass plot
  p_C <- p_bodymass
  
#------------------------- 
  
  # Combine all the plots together
  
  # And combine all the plots for panels A, B and C
  p_final <- p_A1 / p_A2 / p_A3 / p_B1 / p_B2 / p_B3 / p_C
  # This figure is exported and the final small edits carried out in inkscape
  
################################ END ########################################