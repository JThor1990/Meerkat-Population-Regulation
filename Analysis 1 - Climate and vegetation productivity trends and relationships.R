################################################################

# R Script for "Environmental change and population regulation in Kalahari meerkats"

# ANALYSIS 1: CLIMATE AND VEGETATION PRODUCTIVITY TRENDS AND INTER-RELATIONSHIPS

# Produced using R version 4.2.2

###############################################################

# The script explores the inter and intrannual variation in temperature, rainfall and SPEI in the Kalahari, 
# using a combination of site-measured data and remotely sensed data. 
# As part of this analysis, it identifies the long-term trends in climate at the field site. 
# It also investigates the correlation between annual measures of rainfall and a measures of total growing season productivity, the SIV of NDVI

# load in the packages 
lapply(c("ggplot2", "ggrepel","lubridate", "mgcv", "patchwork", "SPEI", "tidyverse", "trend"), 
       FUN = library, character.only = TRUE)

# set the working directory (edit to location of data files on own machine)
setwd("INSERT FILE PATH")

# Load in the temperature and rainfall data 
  # Rainfall (monthly)
  rain <- read.csv("MeerkatMonthlyRain.csv", header = TRUE) 
  # Temperature (Daily)
  temp <- read.csv("MeerkatDailyTemp.csv", header = TRUE)
  # SPEI
  spei <- read.csv("MeerkatSPEI.csv", header= TRUE) %>% 
    mutate(date = as.Date(strptime(date, format = "%d/%m/%Y")))
  
# Wrangle the climate data a bit 
  # Aggregate temp by month 
  temp_monthly <- temp %>% 
    group_by(year, month, month_start) %>% 
    summarise(tmax = mean(tempmax_noaa, na.rm = TRUE), 
              tmin = mean(tempmin_noaa, na.rm = TRUE)) %>% 
    data.frame()

  # join the monthly rainfall and temp data together for the bivariate GAM to follow
  climate <- full_join(rain, temp_monthly, by = c("month_start", "month", "year")) %>% 
    rename(rainfall = rain_gpcp) %>% 
    mutate(monthabb = month.abb[month], 
           month_start = as.Date(strptime(month_start, format = "%d/%m/%Y"))) %>% 
    dplyr::select(year, month, month_start, rainfall, tmax, tmin, monthabb) %>% 
    data.frame()
  
  # Generate a custom SPEI infex for the reserve
  # To create our own index we need to join the NOAA temp data with the GPCP data, 
  # because this should be more accurate than the CPC which is used by the SPEI database
  speidat <- rain %>% 
    dplyr::select(year, month, rain_onsite) %>%
    left_join(temp_monthly) %>% 
    filter(year >= 1997) %>% 
    rename(rainfall = rain_onsite) %>% 
    na.omit()

  speidat$PET <- as.vector(hargreaves(Tmin = speidat$tmin, 
                                      Tmax = speidat$tmax, 
                                      Pre = speidat$rainfall, 
                                      lat = -26.98))
  water_bal <-  speidat$rainfall - speidat$PET
  speidat$SPEI_1 <- c(spei(water_bal, 1)$fitted)
  speidat$SPEI_3 <- c(spei(water_bal, 3)$fitted)
  speidat$SPEI_6 <- c(spei(water_bal, 6)$fitted)
  speidat$SPEI_12 <- c(spei(water_bal, 12)$fitted)
  
#=====================================================================
  
# GENERATE INITIAL PLOTS FOR TEMPERATURE, RAINFALL AND SPEI TIME SERIES

#=====================================================================
  
  # First plot the long-term remotely-sensed time series for rainfall and temperature by month 
  # Tweak so that the facets start with the breeding season
  climate <- mutate(climate, monthnum = if_else(month <= 6, month + 6, month - 6)) %>% 
    rename(temperature = tmax) # easier going forwards 

  #  Generate the labels for each plot: 
  month_lab <- climate %>%
     dplyr::select(monthabb, monthnum) %>% 
     distinct() %>% 
     mutate(year = 1983 + 20, temperature = 39, rainfall = 165)
    
  # temperature plot
  temperature_plot <- ggplot(climate, aes(x = year, y = temperature)) + 
    geom_vline(xintercept = 1993, linetype = 1, alpha = 0.2, colour = "gray") +
    geom_point(data = climate, shape = 1, size = 2, colour = "red", alpha = 0.5) + 
    geom_text(data = month_lab, aes(label = as.factor(monthabb)), size = 3.5) + 
    facet_wrap(~ monthnum) + 
    theme_bw() + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank(), 
          panel.grid = element_blank(), 
          axis.text.y = element_text(size = 9, colour = "black"), 
          axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, colour = "black"), 
          axis.title = element_text(size = 12, colour = "black")) + 
    scale_x_continuous(breaks = seq(1975, 2020, 5), 
                       labels = c("", "1980", "", "1990", "", "2000", "", "2010", "", "2020")) +
    ylim(c(18, 40)) + 
    labs(x = "Year", y = "Mean maximum daily\nair temperature (ºC)")
  
  temperature_plot # look at the plot 
  
  # rainfall plot
  rainfall_plot <- ggplot(climate, aes(x = year, y = rainfall)) + 
    geom_vline(xintercept = 1993, linetype = 1, alpha = 0.2, colour = "gray") +
    geom_point(data = climate, shape = 1, size = 2, colour = "blue", alpha = 0.5) + 
    geom_text(data = month_lab, aes(label = monthabb), size = 3.5) + 
    facet_wrap(~ monthnum) + 
    theme_bw() + 
    theme(strip.background = element_blank(), 
       strip.text = element_blank(), 
       panel.grid = element_blank(), 
       axis.text.y = element_text(size = 9, colour = "black"), 
       axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, colour = "black"), 
       axis.title = element_text(size = 12, colour = "black")) + 
    scale_x_continuous(breaks = seq(1975, 2020, 5), 
                      labels = c("", "1980", "", "1990", "", "2000", "", "2010", "", "2020")) +
    scale_y_continuous(breaks = seq(0, 150, 25), limits = c(0, 170)) +
    labs(x = "Year", y = "Total monthly rainfall (mm)")

  rainfall_plot # look at the plot 
  
  # now plot the long-term SPEI derived from remotely sensed data (SPEI database)
  # filter to align with the other weather data 
  spei <- filter(spei, between(date, as.Date("1979-01-01"), as.Date("2024-01-01"))) 
  
  # we choose 3 but realistically any window of 3, 6, 12 makes the same point
  spei_plot <- ggplot(mutate(spei, SPEI_3 = if_else(SPEI_3 < -3, -3, SPEI_3)), 
                      aes(x = date, y = SPEI_3, fill = SPEI_3)) + 
    geom_vline(xintercept = as.Date("1993-01-01"), linetype = 1, alpha = 0.2, colour = "gray") +
    geom_hline(yintercept = 0, linewidth = 0.5) +
    geom_hline(yintercept = -0.5, linetype = 2, linewidth = 0.2, colour = "orange1") +
    geom_hline(yintercept = -1, linetype = 2, linewidth = 0.2, colour = "firebrick1") +
    geom_hline(yintercept = -2, linetype = 2, linewidth = 0.2, colour = "darkred") +
    geom_hline(yintercept = 0.5, linetype = 2, linewidth = 0.2, colour = "lightblue") +
    geom_hline(yintercept = 1, linetype = 2, linewidth = 0.2, colour = "dodgerblue") +
    geom_hline(yintercept = 2, linetype = 2, linewidth = 0.2, colour = "darkblue") +
    geom_bar(stat = "identity", 
             position = "identity", 
             linewidth=0, 
             aes(fill = factor(SPEI_3 >= 0)), 
             alpha = 0.95)    +              
    theme_bw() + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank(), 
          panel.grid = element_blank(), 
          axis.text.y = element_text(size = 9, colour = "black"), 
          axis.text.x = element_text(size = 9, angle = 90, 
                                     vjust = 0.5, colour = "black"), 
          axis.title.y = element_text(size = 12, colour = "black"), 
          axis.title.x = element_blank(), 
          legend.position = "none") + 
      labs(y = "SPEI (3 month)") +
    scale_x_date(breaks = seq.Date(as.Date("1975-01-01"), 
                                   as.Date("2025-01-01"), "5 years"), 
                 labels = c("", "1980", "", "1990", "", "2000", 
                            "", "2010", "", "2020", "")) + 
      scale_y_continuous(breaks = seq(-4, 2, 0.5),
                         labels = c("-4", "", "-3", "", "-2", "", "-1", 
                                    "", "0", "", "1", "", "2"), 
                         limits = c(-3, 2.5)) + 
    scale_fill_manual(values = c("red", "blue")) 
  
  spei_plot # look at the plot

#=====================================================================
  
# ANALYSE THE RAINFALL AND TERMPATURE LONG-TERM TIME SERIES WITH A BIVARIATE GAM
  
#=====================================================================
  
  # Log transform rainfall 
  climate$rainfall_skew <- log(climate$rainfall +1)

  # Fit the bivariate model with factor-level smooths 
    # i.e. a specific smooth for each month
  climate_bi <- gam(list(temperature ~ as.factor(month) + 
                           s(year, by = as.factor(month), k = 4, bs = "tp"),
                         rainfall_skew ~ as.factor(month) + 
                           s(year, by = as.factor(month), k = 4, bs = "tp")),
                    data = climate, 
                    family = mvn(d = 2))
  summary(climate_bi)
  solve(crossprod(climate_bi$family$data$R))
  
  # Generate the model predictions 
  pred_df <- expand.grid(month = 1:12, year = seq(min(climate$year), max(climate$year), 1))
  pred_df <- bind_cols(pred_df, 
                       predict(climate_bi, newdata = pred_df) %>% 
                        as.data.frame() %>% 
                        rename(temperature = names(.)[1] , 
                               rainfall = names(.)[2])) %>% 
  bind_cols(predict(climate_bi, newdata = pred_df, se.fit = TRUE)$se.fit %>% 
  as.data.frame() %>% 
  rename(temperatureSE = names(.)[1] , 
         rainfallSE = names(.)[2])) %>% 
  mutate(monthabb = factor(month.abb[month], 
                           levels = month.abb[c(7:12, 1:6)]))

  # Get the confidence intervals and back-transform rainfall
  pred_df <- pred_df %>% 
    mutate(temperatureL95 = temperature - 1.96*temperatureSE, 
           temperatureU95 = temperature + 1.96*temperatureSE, 
           rainfallL95 = rainfall - 1.96*rainfallSE,
           rainfallU95 = rainfall + 1.96*rainfallSE) %>% 
    mutate(rainfall = exp(rainfall) - 1, 
           rainfallL95 = exp(rainfallL95) - 1, 
           rainfallU95 = exp(rainfallU95) - 1)

  # tweak it to get the facets working in order
  pred_df <- mutate(pred_df, monthnum = if_else(month <= 6, month + 6, month - 6))

  # Update the plots to include the trend lines
  temperature_plot <- temperature_plot +
    geom_ribbon(data = pred_df, aes(ymin = temperatureL95, ymax = temperatureU95), fill = "red", 
                alpha = 0.1, colour = NA) +
    geom_line(data = pred_df, colour = "red", linewidth = 0.7) 

  rainfall_plot <- rainfall_plot +  
    geom_ribbon(data = pred_df,aes(ymin = rainfallL95, ymax = rainfallU95), fill = "blue", 
                alpha = 0.1, colour = NA) +
    geom_line(data = pred_df, colour = "blue", linewidth = 0.7) 

  # Now add on the predicted mean change from 1993 to 2023
  temperature_change <- pred_df %>% 
    filter(year %in% c(1993, 2023)) %>% 
    dplyr::select(monthnum, month, monthabb, year, temperature) %>% 
    pivot_wider(id_cols = c("month", "monthnum", "monthabb"), 
                names_from = year, 
                values_from = temperature) %>% 
    mutate(change = paste0("+",sprintf("%.2f", round(`2023`-`1993`, digits = 2))," ºC"), 
           temperature = 35, 
             year = 1983 + 20) %>% 
    mutate(percentchange = `2023`/`1993`*100) %>% 
    arrange(monthnum) %>%  
    data.frame()  

  # set the positions manually by year to get it looking nice 
  temperature_change$temperature <- c(36, 36, 36, 36, 
                                      25, 25, 25, 25,
                                      36, 36, 36, 36)
  
  rainfall_change <- pred_df %>% 
    filter(year %in% c(1993, 2022)) %>% 
    dplyr::select(monthnum, month, monthabb, year, rainfall) %>% 
    pivot_wider(id_cols = c("month", "monthnum", "monthabb"), 
                names_from = year, 
                values_from = rainfall) %>% 
    mutate(change = sprintf("%.2f",round(`2022`-`1993`, digits = 2))) %>% 
    mutate(change = if_else(change < 0, 
                            paste0(change, " mm"), 
                            paste0("+", change, " mm")), 
           rainfall = 140,
           year = 1983+20) %>% 
    mutate(percentchange = `2022`/`1993`*100) %>% 
    arrange(monthnum) %>% 
    data.frame()
  
  temperature_plot <- temperature_plot +
    geom_text(data = temperature_change, aes(label = change), size = 3)
  
  rainfall_plot <- rainfall_plot + 
    geom_text(data = rainfall_change, aes(label = change), size = 3) 
  
  temperature_plot/rainfall_plot # look at the updated plots
  
# These plots are good, but I also want to identify the regions of significant change in each time series. We can do this using the finite differences method:
# https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
  
# It is easier to implement the method if the model is fitted separately for temp and rainfall 
# N.B. This does not change the estimation of each montly smooth cf. the bivariate response model
# The only difference is that no correlation in the residuals is estimated any more. 
  
  # Temperature GAM
  temp_gam <- gam(temperature ~ as.factor(month) + 
                           s(year, by = as.factor(month), k = 4, bs = "tp"),
                    data = climate)
  
  x.mesh <- seq(1983,2022,length=500) ## where to evaluate derivatives
  newd <- expand.grid(year = x.mesh, month = as.factor(1:12))
  X0 <- predict(temp_gam,newd,type="lpmatrix") 
  
  eps <- 1e-7 ## finite difference interval
  x.mesh <- x.mesh + eps ## shift the evaluation mesh
  newd <- expand.grid(year = x.mesh, month = as.factor(1:12))
  X1 <- predict(temp_gam,newd,type="lpmatrix")
  
  Xp <- (X1-X0)/eps ## maps coefficients to (fd approx.) derivatives
  colnames(Xp)      ## can check which cols relate to which smooth
  
  # Plot the derivatives and extract the regions of significant change
  par(mfrow=c(4,3))  # 12 months
  newd$deriv <- as.numeric(NA)
  newd$derivl95 <- as.numeric(NA)
  newd$derivu95 <- as.numeric(NA)
  newd$signifD <- as.numeric(NA)
  
  for (i in 1:12) {  # plot derivatives and corresponding CIs
    Xi <- Xp*0 
    Xi[,(13+(i-1)*3):((13+(i-1)*3)+2)] <- Xp[,(13+(i-1)*3):((13+(i-1)*3)+2)]  
    # this gets the appropriate rows
    # but also only want the appropriate columns or df will be too large (2000/month)
    Xi <- Xi[((i-1)*500 + 1):(i*500),] 
    ## Xi%*%coef(b) = smooth deriv i
    df <- Xi%*%coef(temp_gam)                       
    df.sd <- rowSums(Xi%*%temp_gam$Vp*Xi)^.5        
    plot(x.mesh,df,type="l",ylim=range(c(df+2*df.sd,df-2*df.sd)), col = "red", main = i)
    abline(h = 0, lty = 1, col = "gray")
    lines(x.mesh,df+1.96*df.sd,lty=2, col = "red");lines(x.mesh,df-1.96*df.sd,lty=2, col = "red")
    
    newd$deriv[((i-1)*500 + 1):(i*500)] <- as.vector(df)
    newd$derivl95[((i-1)*500 + 1):(i*500)] <- as.vector(df)-1.96*as.vector(df.sd)
    newd$derivu95[((i-1)*500 + 1):(i*500)] <- as.vector(df)+1.96*as.vector(df.sd)
    newd$signifD <- ifelse(sign(newd$derivl95) ==  sign(newd$derivu95), 1, 0)
  }
  
  temp_regions <- newd %>% 
    group_by(month) %>% 
    mutate(signifreg = data.table::rleid(signifD)) %>% 
    filter(signifD == 1) %>% 
    group_by(month, signifreg) %>% 
    summarise(start = min(year), end = max(year)) %>% 
    data.frame()
  
  # Add the regions of significant change as a thicker line. 
  pred_df$signifDtemp <- NA
  for (j in 1:nrow(temp_regions)) {
    pred_df$signifDtemp[which(pred_df$month == temp_regions$month[j] & 
                                between(pred_df$year, temp_regions$start[j], temp_regions$end[j]))] <- "Y"
  }
  pred_df$signifDtemp[is.na(pred_df$signifDtemp)] <- "N"
  pred_df <- pred_df %>% 
    group_by(month) %>% 
    mutate(signifregtemp = data.table::rleid(signifDtemp)) %>% 
    data.frame()
    
  # Now we can add on the significant regions to the plots
  temperature_plot <- ggplot(climate, aes(x = year, y = temperature)) + 
    geom_vline(xintercept = 1993, linetype = 1, alpha = 0.2, colour = "gray") +
    geom_point(data = climate, shape = 1, size = 2, colour = "red", alpha = 0.3) + 
    geom_text(data = month_lab, aes(label = as.factor(monthabb)), size = 3.5) + 
    geom_text(data = temperature_change, aes(label = change), size = 3) +
    geom_ribbon(data = pred_df, aes(ymin = temperatureL95, ymax = temperatureU95), fill = "red", 
                alpha = 0.1, colour = NA) +
    geom_line(data = pred_df, colour = "red", linewidth = 0.7) + 
                       #-----------------# (the new bit is here)
    geom_line(data = filter(pred_df, signifDtemp== "Y"), 
              aes(group = signifregtemp), 
              colour = "red4", linewidth = 0.9) + 
                      #-----------------#
    facet_wrap(~ monthnum) + 
    theme_bw() + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank(), 
          panel.grid = element_blank(), 
          axis.text.y = element_text(size = 9, colour = "black"), 
          axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, colour = "black"), 
          axis.title = element_text(size = 12, colour = "black")) + 
    ylim(c(18, 40)) + 
    labs(x = "Year", y = "Mean maximum\ndaily air temperature (ºC)")  
  
  # Rainfall GAM
  rainfall_gam <- gam(rainfall_skew ~ as.factor(month) + 
                    s(year, by = as.factor(month), k = 4, bs = "tp"),
                  data = climate)
  
  x.mesh <- seq(1983,2022,length=500) ## where to evaluate derivatives
  newd <- expand.grid(year = x.mesh, month = as.factor(1:12))
  X0 <- predict(temp_gam,newd,type="lpmatrix") 
  
  eps <- 1e-7 ## finite difference interval
  x.mesh <- x.mesh + eps ## shift the evaluation mesh
  newd <- expand.grid(year = x.mesh, month = as.factor(1:12))
  X1 <- predict(temp_gam,newd,type="lpmatrix")
  
  Xp <- (X1-X0)/eps ## maps coefficients to (fd approx.) derivatives
  colnames(Xp)      ## can check which cols relate to which smooth
  
  # Plot the derivatives and extract the regions of significant change
  par(mfrow=c(4,3))  # 12 months
  newd$deriv <- as.numeric(NA)
  newd$derivl95 <- as.numeric(NA)
  newd$derivu95 <- as.numeric(NA)
  newd$signifD <- as.numeric(NA)
  
  for (i in 1:12) {  ## plot derivatives and corresponding CIs
    Xi <- Xp*0 
    Xi[,(13+(i-1)*3):((13+(i-1)*3)+2)] <- Xp[,(13+(i-1)*3):((13+(i-1)*3)+2)]  
    # this get's the appropriate rows
    # but also only want the appropriate columns or df will be too large (2000/month)
    Xi <- Xi[((i-1)*500 + 1):(i*500),] 
    ## Xi%*%coef(b) = smooth deriv i
    df <- Xi%*%coef(rainfall_gam)                        ## ith smooth derivative 
    df.sd <- rowSums(Xi%*%rainfall_gam$Vp*Xi)^.5         ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5
    plot(x.mesh,df,type="l",ylim=range(c(df+2*df.sd,df-2*df.sd)), col = "red", main = i)
    abline(h = 0, lty = 1, col = "gray")
    lines(x.mesh,df+1.96*df.sd,lty=2, col = "red");lines(x.mesh,df-1.96*df.sd,lty=2, col = "red")
    
    newd$deriv[((i-1)*500 + 1):(i*500)] <- as.vector(df)
    newd$derivl95[((i-1)*500 + 1):(i*500)] <- as.vector(df)-1.96*as.vector(df.sd)
    newd$derivu95[((i-1)*500 + 1):(i*500)] <- as.vector(df)+1.96*as.vector(df.sd)
    newd$signifD <- ifelse(sign(newd$derivl95) ==  sign(newd$derivu95), 1, 0)
  }
  
  rainfall_regions <- newd %>% 
    group_by(month) %>% 
    mutate(signifreg = data.table::rleid(signifD)) %>% 
    filter(signifD == 1) %>% 
    group_by(month, signifreg) %>% 
    summarise(start = min(year), end = max(year)) %>% 
    data.frame() # very few periods of significant rainfall change 
  
  # Add the regions of significant change as a thicker line. 
  pred_df$signifDrain <- NA
  for (j in 1:nrow(rainfall_regions)) {
    pred_df$signifDrain[which(pred_df$month == rainfall_regions$month[j] & between(pred_df$year,  rainfall_regions$start[j],  rainfall_regions$end[j]))] <- "Y"
  }
  pred_df$signifDrain[is.na(pred_df$signifDtemp)] <- "N"
  pred_df <- pred_df %>% 
    group_by(month) %>% 
    mutate(signifregrain = data.table::rleid(signifDrain)) %>% 
    data.frame()
  
  # Now we can add on the significant regions to the plots
  rainfall_plot <- ggplot(climate, aes(x = year, y = rainfall)) + 
    geom_vline(xintercept = 1993, linetype = 1, alpha = 0.2, colour = "gray") +
    geom_point(data = climate, shape = 1, size = 2, colour = "blue", alpha = 0.5) + 
    geom_text(data = month_lab, aes(label = monthabb), size = 3.5) + 
    geom_text(data = rainfall_change, aes(label = change), size = 3) +
    geom_ribbon(data = pred_df, aes(ymin = rainfallL95, ymax = rainfallU95), fill = "blue", 
                alpha = 0.1, colour = NA) +
    geom_line(data = pred_df, colour = "blue", linewidth = 0.7) + 
    #-----------------# (the new bit is here)
    geom_line(data = filter(pred_df, signifDrain== "Y"), 
              aes(group = signifregrain), 
              colour = "darkblue", linewidth = 0.9) + 
    #-----------------#
    facet_wrap(~ monthnum) + 
    theme_bw() + 
    theme(strip.background = element_blank(), 
          strip.text = element_blank(), 
          panel.grid = element_blank(), 
          axis.text.y = element_text(size = 9, colour = "black"), 
          axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, colour = "black"), 
          axis.title = element_text(size = 12, colour = "black")) + 
    scale_y_continuous(breaks = seq(0, 150, 25), limits = c(0, 170)) + 
    labs(x = "Year", y = "Total monthly rainfall (mm)")
  
  # Now we can put the two plots together 
  temperature_plot <- temperature_plot + 
    labs(tag = "A") +
    theme(axis.title.x= element_blank(), 
        axis.text.x = element_blank(), 
        plot.tag = element_text(size = 18)) 
  rainfall_plot <- rainfall_plot + 
    theme(axis.title.x = element_blank())
  spei_plot <- spei_plot + 
    labs(tag = "B") +
    theme(plot.tag = element_text(size = 18), 
          axis.text.x = element_text(size = 9.5, angle = 0)) 

  # set the layout with the patchwork package to plot, temp + rainfall + SPEI
  layout <- "
  #AAAAAAAAAAAA##
  #AAAAAAAAAAAA## 
  #AAAAAAAAAAAA##
  #AAAAAAAAAAAA##
  #BBBBBBBBBBBB##
  #BBBBBBBBBBBB##
  #BBBBBBBBBBBB##
  #BBBBBBBBBBBB##
  CCCCCCCCCCCCCCC
  CCCCCCCCCCCCCCC
  "

  (temperature_plot + rainfall_plot + spei_plot) +
    plot_layout(design = layout) # the final plot 
  # export @ W6.5 inch by H9.5 inch

#=====================================================================
  
# FURTHER ASSESSING THE TRENDS IN RAINFALL, TEMPERATURE, AND SPEI
  
#=====================================================================
  
# Across the span of the project (1998-2023)
  # Has there been a long-term trend in the various climate variables 
  # (the above makes clear that for temperature there is, but it's good to test with a 
  # specific non-linear test for detecting linear trends in time series, the Mann-Kendall trend test) 
  
# Mann-Kendall trends tests 
  # Temperature (monthly resolution, NOAA data, after 1998)
  mk.test(climate$temperature[climate$year >= 1998]) 
  tempbeta <- sens.slope(climate$temperature[climate$year >= 1993])
  tempbeta$estimate*12  #convert slope to /year estimate
  tempbeta$conf.int*12  
  
  # rainfall (monthly resolution, GPCP, after 1993) 
  mk.test(climate$rainfall[climate$year >= 1993 & !is.na(climate$rainfall)]) 
  rainbeta <- sens.slope(climate$rainfall[climate$year >= 1993 & !is.na(climate$rainfall)])
  rainbeta$estimate*12  #/year 
  rainbeta$conf.int*12  
  
  # rainfall (monthly resolution, site-measured, after 1993) 
  mk.test(rain$rain_onsite[rain$year >= 1998]) 
  rainbeta2 <- sens.slope(rain$rain_onsite[rain$year >= 1998])
  rainbeta2$estimate*12  #/year 
  rainbeta2$conf.int*12  
  
  # Before getting the trends in annual rainfall I have to calculate the totals
  # Annual rainfall (by breeding season, Jul to Jul) 
  # For GPCP data (remotely sensed)
  rain_across_season_gpcp <- rain %>% 
    mutate(breeding_season = if_else(month %in% 1:6, paste0(year - 1, "/", year), 
                                     paste0(year, "/", year + 1))) %>% 
    group_by(breeding_season) %>% 
    summarise(AnnualRainfall_mm = sum(rain_gpcp, na.rm = T)) %>% 
    filter(breeding_season != "2023/2024") %>% 
    mutate(Year = as.numeric(substring(breeding_season, 1, 4))) %>% 
    data.frame()
  mean(rain_across_season_gpcp$AnnualRainfall_mm[rain_across_season_gpcp$Year > 1992])
  sd(rain_across_season_gpcp$AnnualRainfall_mm[rain_across_season_gpcp$Year > 1992])
  range(rain_across_season_gpcp$AnnualRainfall_mm[rain_across_season_gpcp$Year > 1992])
  
  # For site-measured rainfall 
  rain_across_season_reserve <- rain %>% 
    mutate(breeding_season = if_else(month %in% 1:6, paste0(year - 1, "/", year), 
                                     paste0(year, "/", year + 1))) %>% 
    group_by(breeding_season) %>% 
    summarise(AnnualRainfall_mm = sum(rain_onsite, na.rm = T)) %>% 
    filter(breeding_season != "2023/2024") %>% 
    mutate(Year = as.numeric(substring(breeding_season, 1, 4))) %>% 
    filter(Year >= 1998) %>% 
    data.frame()
  mean(rain_across_season_reserve$AnnualRainfall_mm)
  sd(rain_across_season_reserve$AnnualRainfall_mm)
  range(rain_across_season_reserve$AnnualRainfall_mm)
  
  # Mann-Kendall on annual rainfall
  # GPCP
  mk.test(rain_across_season_gpcp$AnnualRainfall_mm[rain_across_season_gpcp$Year >= 1993]) # over 30 years
  sens.slope(rain_across_season_gpcp$AnnualRainfall_mm[rain_across_season_gpcp$Year >= 1993]) # over 30 years
  mk.test(rain_across_season_reserve$AnnualRainfall_mm) # over 40 years
  sens.slope(rain_across_season_reserve$AnnualRainfall_mm) # over 40 years
  # Site-measured rainfall 
  mk.test(rain_across_season_reserve$AnnualRainfall_mm) 
  sens.slope(rain_across_season_reserve$AnnualRainfall_mm)
  
  # Also compute linear trends in each month which can be comapred to the GAM results 
  # GPCP
  for (k in 1:12) {
    # non-parametric
    res <- mk.test(climate$rainfall[climate$month == k & climate$year >= 1993 & !is.na(climate$rainfall)]) 
    print(paste0(k, ", ", signif(res$statistic,4), ", ", signif(res$p.value, 2)))
  }
  
  # Onsite rainfall
  for (k in 1:12) {
    # non-parametric
    res <- mk.test(rain$rain_onsite[rain$month == k & rain$year >= 1998]) 
    print(paste0(k, ", ", signif(res$statistic,4), ", ", signif(res$p.value, 2)))
  }
  
  # This indicates less rainfall in the early austral summer (Sep-Nov) which we can also test
  earlysummer_rain_gpcp <- rain %>% 
    filter(between(year, 1983, 2022)) %>% 
    filter(month %in% 9:11) %>% 
    group_by(year) %>% 
    summarise(season_rain = sum(rain_gpcp, na.rm = T)) %>% 
    data.frame()
  
  earlysummer_rain_reserve <- rain %>% 
    filter(year >= 1998) %>% 
    filter(month %in% 9:11) %>% 
    group_by(year) %>% 
    summarise(season_rain = sum(rain_onsite, na.rm = T), 
              season_over5mm = sum(rain_onsite >= 5, na.rm = T)) %>% 
    data.frame()
  
  # Mann-Kendall test on early summer rainfall 
  # GPCP 
  mk.test(earlysummer_rain_gpcp$season_rain) 
  sens.slope(earlysummer_rain_gpcp$season_rain)
  # Onsite  (total)
  mk.test(earlysummer_rain_reserve$season_rain) 
  sens.slope(earlysummer_rain_reserve$season_rain)
  # Onsite (rainfall events)
  mk.test(earlysummer_rain_reserve$season_over5mm) 
  sens.slope(earlysummer_rain_reserve$season_over5mm)
  
  # SPEI-6 trends (estimated using site-measured rainfall: the shorter time series)
  mk.test(speidat$SPEI_6[!is.na(speidat$SPEI_6)]) 
  # sens.slope(speidat$SPEI_6[!is.na(speidat$SPEI_6)])
  # Note that the slope here is somewhat arbitrary as it's a standardised measure.
  # Also note that the result is qualitatively the same using the 3-month SPEI
  mk.test(speidat$SPEI_3[!is.na(speidat$SPEI_3)]) 
  # sens.slope(speidat$SPEI_3[!is.na(speidat$SPEI_3)])
  # Note that the slope here is somewhat arbitrary as it's a standardised measure.
  
  # SPEI-6 (using SPEI database values: the longer time series)
  mk.test(spei$SPEI_6[!is.na(spei$SPEI_6)]) 
  # sens.slope(spei$SPEI_6[!is.na(spei$SPEI_6)])

  # What proportion of the decade from 2010 to 2020 were they in drought or moderate drought 
  # according to the site-derived SPEI
  speidat %>% 
    filter(year %in% 2010:2020) %>% 
    summarise(SPEI6_moderatedrought = sum(SPEI_6 < -1)/n()) # 37%
  
  # Compare this to the SPEI database data set 
  # Need to reload SPEI here to get the complete time series considered (from the 50s)
  # From this we can assess the background rate of moderate droughts since the 1950s
  spei <- read.csv("MeerkatSPEI.csv", header= TRUE) %>% 
    mutate(date = as.Date(strptime(date, format = "%d/%m/%Y")))

  # 2010-2020
  spei %>% 
    filter(!is.na(SPEI_6)) %>% 
    filter(between(date, as.Date("2010-07-01"), as.Date("2020-07-01"))) %>% 
    summarise(SPEI6_moderatedrought = sum(SPEI_6 < -1)/n()) # 29.8 %
  
  # Background rate
  spei %>% 
    filter(!is.na(SPEI_6)) %>% 
    summarise(SPEI6_moderatedrought = sum(SPEI_6 < -1)/n()) # 15.7 %
  
#=====================================================================
  
# EXTRACTING SOME ADDITIONAL WEATHER-RELATED INFORMATION  
  
#=====================================================================
  
  # monthly averages and variability for temperature and rainfall 
  # remotely sensed data (CPC temperature, GPCP rainfall)
  climate %>% 
    filter(year >= 1993) %>% 
    group_by(month) %>% 
    summarise(tempmean = mean(temperature), 
              tempCV =  sd(temperature)/mean(temperature), 
              rainmean = mean(rainfall, na.rm = T), 
              rainCV= sd(rainfall, na.rm = T)/mean(rainfall, na.rm = T)) %>% 
    arrange(month) %>% 
    data.frame() 
  
  # monthly rainfall averages and variability 
  # site-measured rainfall data
  rain %>% 
    filter(year >= 1998) %>% 
    group_by(month) %>% 
    summarise(rainmean = mean(rain_onsite, na.rm = T), 
              rainCV= sd(rain_onsite, na.rm = T)/mean(rain_onsite, na.rm = T)) %>% 
    arrange(month) %>% 
    data.frame() 
    
  # Get the average annual temperature for completeness
  annual_temp <- climate %>% 
    filter(year < 2024) %>% 
    group_by(year) %>% 
    summarise(averagemaxtemp = mean(temperature, na.rm = TRUE)) %>% 
    arrange(year) %>% 
    data.frame()

  # Mann-Kendall on annual temperature
  mk.test(annual_temp$averagemaxtemp[annual_temp$year >= 1993]) 
  sens.slope(annual_temp$averagemaxtemp[annual_temp$year >= 1993])
  mk.test(annual_temp$averagemaxtemp) 
  sens.slope(annual_temp$averagemaxtemp)

  # Based on the increasing temperatures, 
  # plot the increase in the number of hot days in the year/breeding season
  
  # Extreme hot days > 35, 37.5, 40
  hotdays <- temp %>% 
    mutate(breedingseason = if_else(month %in% 1:6, year - 1, year)) %>% 
    filter(!(breedingseason %in% c(1978, 2023))) %>%  # don't have full seasons
    group_by(breedingseason) %>% 
    summarise(n35 = sum(tempmax_noaa  > 35, na.rm = T),
              n37.5 = sum(tempmax_noaa  > 37.5, na.rm = T), 
              n40 = sum(tempmax_noaa  > 40, na.rm = T)) %>% 
    pivot_longer(cols = c(n35, n37.5, n40)) %>% 
    mutate(name = case_when(name == "n35" ~ "> 35°C", 
                            name == "n37.5" ~ "> 37.5°C", 
                            name == "n40" ~ "> 40°C")) %>% 
    data.frame()
    
  # This figure appears in the supporting information 
  p_hotdays <- ggplot(hotdays, aes(x = breedingseason, y= value, group = name, colour = name)) + 
    geom_path(alpha = 0.1) + 
    geom_point(size = 3, alpha  = 0.3) +
    geom_smooth(aes(fill = name), method = "gam", formula = y ~ s(x, k = 5), alpha  = 0.2) + 
    theme_bw() + 
    theme(legend.position = c(0.2, 0.9), 
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          strip.background = element_blank(), 
          strip.text = element_blank(), 
          panel.grid = element_blank(), 
          axis.text.y = element_text(size = 12, colour = "black"), 
          axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, colour = "black"), 
          axis.title = element_text(size = 13, colour = "black")) + 
    labs(x = "Year", y = "Number of days in year") + 
    scale_x_continuous(breaks = seq(1980, 2025, 5)) + 
    scale_y_continuous(breaks = seq(0, 150, 25)) +
    scale_colour_manual(values = c( "orange1", "firebrick1", "darkred")) +
    scale_fill_manual(values = c( "orange1", "firebrick1", "darkred"))

  p_hotdays # look at the figure
  
#=====================================================================
  
# PLOTTING THE CORRELATION BETWEEN ANNUAL RAINFALL AND VEGETATION PRODUCTIVITY 
  
#=====================================================================
  
  # Load in the vegetation productivity data
  acrosspixel_ndvi <- read.csv("MeerkatSIVofNDVI.csv", header = T)
  # The various metrics are outlined in the supporting information, and come from the TIMESAT software
  # Of interest here is the small integral values of  NDVI 
  
  # Before that, a few of the phenology metrics are worth being aware of
  
  # Start of the growing season 
  # (days after the start of the season, defined in the timesat SOFTWARE)
  # NDVI data are provided by MODIS every 16 days. 
  # The start of the growing seasons are provided relative to the 1st NDVI value to fall in July 
  # This was either on the 11th or 12th of July, depending on whether a leap year. 
  # Consequently, the typical start of the growing season can be considered as the mean number of days after this date that the TIMESAT software identified the upturn in NDVI. 
  # Note that the "growingseason_start_mean" is the yearly mean across pixels
  ymd(as.Date("2000-07-11") + mean(acrosspixel_ndvi$growingseason_start_mean, ra.rm = TRUE)) 
  # growing season starts on 14th January on average (mean, or 4th January median)
  
  # Similarly, the end of the growing season 
  # (days after the start of the season)
  ymd(as.Date("2000-07-11") + mean(acrosspixel_ndvi$growingseason_end_mean, ra.rm = TRUE)) 
  # And ends on the 31st July
  
  # Peak of the growing season 
  # (days after the start of the season)
  ymd(as.Date("2000-07-11") + mean(acrosspixel_ndvi$growingseason_peak_mean, ra.rm = TRUE)) 
  # And peaks on the 2nd April on average (mean, or 28th March median)
  
  mean(acrosspixel_ndvi$growingseason_length_mean)
  sd(acrosspixel_ndvi$growingseason_length_mean)   
  # The growing season is therefore 197 days on average (+- 33.4 days)
  
  # set new ggplot theme
  plot_theme <- theme_bw() +  
    theme(axis.title = element_text(size = 11.5), 
          axis.text = element_text(size = 11, colour = "black"), 
          legend.title = element_text(size = 11), 
          legend.text = element_text(size = 11), 
          strip.text = element_text(size = 12), 
          strip.background = element_rect(fill = "lightblue"), 
          panel.grid = element_blank())
  
  acrosspixel_ndvi$growthseason <- paste0(substring(acrosspixel_ndvi$Year, 3, 4), "/", 
                                          substring(acrosspixel_ndvi$Year + 1, 3, 4))
  
  # Plotting annual productivity (SIV of NDVI) against the site-measured annual rainfall
  pndvi_1 <- ggplot(acrosspixel_ndvi, aes(x = log(AnnualRainfall_Onsite), y = SI_NDVI_mean)) + 
    geom_point(size = 2.5, stroke = 1, pch = 1) + 
    geom_smooth(method = "lm", colour = "forestgreen") +
    geom_text_repel(aes(label = growthseason), size = 3) +
    plot_theme + 
    theme(axis.text = element_text(colour = "black", size = 11), 
          plot.tag = element_text(colour = "black", size = 14), 
          plot.title = element_text(hjust = 0.5, size = 12)) +
    labs(x = "log(Annual rainfall) (mm)", 
         y = "Vegetation productivity\n(SIV of NDVI)", 
         tag = "A", 
         title = "Onsite rainfall") + 
    scale_y_continuous(breaks = seq(0, 2.0, 0.2), limits = c(0, 1.7)) + 
    scale_x_continuous(breaks = seq(4.5, 6.5, 0.25), 
                       labels = c(4.5, "", 5, "", 5.5, "", 6, "", 6.5))
  
  cor.test(log(acrosspixel_ndvi$AnnualRainfall_Onsite), 
           acrosspixel_ndvi$SI_NDVI_mean, method = "pearson")
  
  pndvi_1 <- pndvi_1 + 
    ggplot2::annotate("text", x = 5.6, y = 1.65, label = "r = 0.72 [95%CI: 0.44, 0.87]\np < 0.001", 
                      size = 3, colour = "forestgreen")
  
  # Plotting annual productivity (SIV of NDVI) against NOAA CPC rainfall
  pndvi_2 <- ggplot(acrosspixel_ndvi, aes(x = log(AnnualRainfall_NOAA), y = SI_NDVI_mean)) + 
    geom_point(size = 2.5, stroke = 1, pch = 1) + 
    geom_smooth(method = "lm", colour = "forestgreen") +
    geom_text_repel(aes(label = growthseason), size = 3) +
    plot_theme + 
    theme(axis.text = element_text(colour = "black", size = 11), 
          plot.tag = element_text(colour = "black", size = 14), 
          plot.title = element_text(hjust = 0.5, size = 12)) +
    labs(x = "log(Annual rainfall) (mm)", 
         y = "Vegetation productivity\n(SIV of NDVI)", 
         tag = "B", 
         title = "NOAA CPC rainfall") + 
    scale_y_continuous(breaks = seq(0, 2.0, 0.2), limits = c(0, 1.7)) + 
    scale_x_continuous(breaks = seq(4.5, 6.5, 0.25), 
                       labels = c(4.5, "", 5, "", 5.5, "", 6, "", 6.5))
  
  cor.test(log(acrosspixel_ndvi$AnnualRainfall_NOAA), 
           acrosspixel_ndvi$SI_NDVI_mean, method = "pearson")
  
  pndvi_2 <- pndvi_2 + 
    ggplot2::annotate("text", x = 5.25, y = 1.65, label = "r = 0.45 [95%CI: 0.05, 0.73]\np = 0.04", 
                      size = 3, colour = "forestgreen")
  
  # Plotting annual productivity (SIV of NDVI) against NOAA CPC rainfall
  pndvi_3 <- ggplot(acrosspixel_ndvi, aes(x = log(AnnualRainfall_GPCP), y = SI_NDVI_mean)) + 
    geom_point(size = 2.5, stroke = 1, pch = 1) + 
    geom_smooth(method = "lm", colour = "forestgreen") +
    geom_text_repel(aes(label = growthseason), size = 3) +
    plot_theme + 
    theme(axis.text = element_text(colour = "black", size = 11), 
          plot.tag = element_text(colour = "black", size = 14), 
          plot.title = element_text(hjust = 0.5, size = 12)) +
    labs(x = "log(Annual rainfall) (mm)", 
         y = "Vegetation productivity\n(SIV of NDVI)", 
         tag = "C", 
         title = "GPCP rainfall") + 
    scale_y_continuous(breaks = seq(0, 2.0, 0.2), limits = c(0, 1.7)) + 
    scale_x_continuous(breaks = seq(4.5, 6.5, 0.25), 
                       labels = c(4.5, "", 5, "", 5.5, "", 6, "", 6.5))
  
  cor.test(log(acrosspixel_ndvi$AnnualRainfall_GPCP), 
           acrosspixel_ndvi$SI_NDVI_mean, method = "pearson")
  
  pndvi_3 <- pndvi_3 + 
    ggplot2::annotate("text", x = 5.4, y = 1.65, label = "r = 0.56 [95%CI: 0.20, 0.79]\np = 0.005", 
                      size = 3, colour = "forestgreen")
  
  
  ## Plotting annual productivity (SIV of NDVI) against year  
  pndvi_4 <- ggplot(acrosspixel_ndvi, aes(x = growthseason, y = SI_NDVI_mean)) + 
    geom_hline(yintercept = mean(acrosspixel_ndvi$SI_NDVI_mean) - 
                 sd(acrosspixel_ndvi$SI_NDVI_mean), linetype = 2, colour = "forestgreen") + 
    geom_hline(yintercept = mean(acrosspixel_ndvi$SI_NDVI_mean) + 
                 sd(acrosspixel_ndvi$SI_NDVI_mean), linetype = 2, colour = "forestgreen") +
    geom_hline(yintercept = mean(acrosspixel_ndvi$SI_NDVI_mean), 
               linetype = 1, colour = "forestgreen") +
    geom_errorbar(aes(ymin = SI_NDVI_mean - SI_NDVI_SD, 
                      ymax = SI_NDVI_mean + SI_NDVI_SD), width = 0) +
    geom_point(size = 2.5, stroke = 1, fill = "white", pch = 21) + 
    plot_theme + 
    theme(axis.text.x = element_text(colour = "black", size = 9, angle = 90, vjust = 0.5), 
          plot.tag = element_text(colour = "black", size = 14)) +
    labs(x = NULL, 
         y = "Primary productivity\n(SIV of NDVI)", 
         tag = "D") + 
    scale_y_continuous(breaks = seq(0, 2.0, 0.2))
  
  # final plots 
  p_final_main <- pndvi_1 + 
    theme(plot.title = element_blank(), 
          plot.tag = element_blank()) 
  
  p_upper <- pndvi_1 + pndvi_2 + pndvi_3
  p_lower <- (plot_spacer() | pndvi_4 | plot_spacer()) + 
    plot_layout(ncol = 3, widths = c(1, 8, 2)) 
  p_final <- (p_upper/p_lower) + 
    plot_layout(heights = c(3,2))
  
  # Has the SIV of NDVI changed changed over time?
  mk.test(acrosspixel_ndvi$SI_NDVI_mean) 
  sens.slope(acrosspixel_ndvi$SI_NDVI_mean)

################################ END ########################################