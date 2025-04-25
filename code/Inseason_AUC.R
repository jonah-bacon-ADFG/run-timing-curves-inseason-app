#####################################################################################################################################################
# 
# Title: Inseason_AUC.R Script
# Author(s): Tim Blackmon and Ted Otis
# Creation Date: 4/4/2024 (Tim Blackmon)
# Date Last Revised: 4/11/2024 (Ted Otis)
# Revision Notes: cleaned up comments and added header details.
#
# Custodian: Ted Otis, ADF&G LCI Area Finfish Research Biologist, Homer 
# Ph: (907) 235-8191, email: ted.otis@alaska.gov; 
# Mail: AK Dept. of Fish and Game, 3298 Douglas Place, Homer, AK 99603
# 
# Synopsis: This code uses aerial and ground survey data from the current year to produce inseason AUC escapement indices for each stock.
#  It produces a variety of plots, including one (run timing) that shows the current run timing and escapement relative to that stock's
#  historical average run timing and that stock's SEG or management objective, so managers can see if we're tracking to meet the SEG.
#  It also produces an "OutputAUC" table that provides the same information in tabular form for all surveyed stocks, which serves as 
#  documentation of how that year's escapement indices were derived. Post-season, these data can be uploaded to a table in the escapement
#  database.
# 
# Comments: This script is still a work in progress. It currently works for pink and chum salmon stocks with a 17.5 d streamlife (SL). 
#  Modifications are needed to accommodate McNeil River chums (13.8 d SL) and sockeye salmon, where peak counts are used, not AUC indices.
# 
#####################################################################################################################################################

install.packages("tidyverse")                                                   # packages only need to be installed 1x, libraries each time
install.packages("gridExtra")
install.packages("jpeg")
install.packages("dplyr")
install.packages("Rtools43")
install.packages("extrafont")

library(tidyverse)                                                              # tidyverse includes dplyr, ggplot, readr, tidyr, etc.
library(gridExtra)
library(jpeg)                                                                   # jpeg needed to read/write .jpg files?
library(dplyr)
library(extrafont)
library(RColorBrewer)                                                           # installs additional color palette options for plotting


projdir <- getwd()
setwd("4_Inseason_AUC")                                                         # defines root project directory for later use
rootdir <- getwd()

loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12, base_family='Times New Roman')+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

StreamLife <- 17.5                                                              ## estimated life in stream of fish
ObsEff <- 1.0                                                                   ## observer efficiency

AUCCURRENTYEAR.dat <- read.csv("input/CURRENTYEAR.csv")                         ## read in input data
head(AUCCURRENTYEAR.dat, 10) 
tail(AUCCURRENTYEAR.dat, 10)

AUCCURRENTYEAR.dat$Year <- as.integer(AUCCURRENTYEAR.dat$Year)                  ## converts character to integer
AUCCURRENTYEAR.dat$Date <- as.Date(AUCCURRENTYEAR.dat$Date)                     ## converts character to date
AUCCURRENTYEAR.dat <- arrange(AUCCURRENTYEAR.dat, Date)                         ## sorts dataframe by date


AUCGROUPS.dat<- AUCCURRENTYEAR.dat %>% group_by(Year, SPECIES,SURVEY_TYPE, 
                                  STOCK) %>% summarize(TotalCount = sum(COUNT)) ## Groups dataframe by categories


                                                                                ## nrow counts how many unique groups
                                                                                ## for loop runs the AUC calculations for
for (i in c(1:nrow(AUCGROUPS.dat))){                                            ## each group
  AUCCY.dat <- subset(AUCCURRENTYEAR.dat, Year == AUCGROUPS.dat$Year[i] &
                      SPECIES == AUCGROUPS.dat$SPECIES[i] &
                      STOCK == AUCGROUPS.dat$STOCK[i] &
                      SURVEY_TYPE == AUCGROUPS.dat$SURVEY_TYPE[i])
  
  AUCCY.dat$JulianDay <- as.POSIXlt(AUCCY.dat$Date)$yday                        ## converts date to Julian Date integer
  AUCCY.dat <- arrange(AUCCY.dat, JulianDay)                                    ## sorts by julian day
  print(i)                                                                             
  
  if (AUCCY.dat$COUNT[1] != 0) {                                                ## creates count of zero if first survey count is nonzero
    FirstRow <- head(AUCCY.dat,1)                                       
    FirstRow$JulianDay <- AUCCY.dat$JulianDay[1]-StreamLife
    FirstRow$Date <- as.Date(FirstRow$Date[1])-StreamLife
    FirstRow$COUNT <- 0
    AUCCY.dat <- rbind(FirstRow, AUCCY.dat)
  }
  
  if (AUCCY.dat$COUNT[nrow(AUCCY.dat)] != 0) {                                  ## creates count of zero if last survey count is nonzero
    LastRow <- tail(AUCCY.dat,1)
    LastRow$JulianDay <- LastRow$JulianDay +StreamLife
    LastRow$Date <- as.Date(LastRow$Date[1])+StreamLife
    LastRow$COUNT <- 0
    AUCCY.dat <- rbind(AUCCY.dat, LastRow)
  }
  
  
  for (j in c(1:nrow(AUCCY.dat))){                                              ## Calculates columns Fishdays, SumFishDays, EscInd, and SumEscape
    if (j>1){
      AUCCY.dat$Days[j]     <-  AUCCY.dat$JulianDay[j]-AUCCY.dat$JulianDay[j-1]
      AUCCY.dat$FishDays[j]    <- (AUCCY.dat$COUNT[j]+
                                    AUCCY.dat$COUNT[j-1])*(AUCCY.dat$Days[j])/2
      
  AUCCY.dat$SumFishDays[j] <- AUCCY.dat$FishDays[j] + AUCCY.dat$SumFishDays[j-1]
  AUCCY.dat$EscInd[j]      <- AUCCY.dat$FishDays[j]/(StreamLife*ObsEff)
  AUCCY.dat$SumEscape[j]   <- AUCCY.dat$EscInd[j] + AUCCY.dat$SumEscape[j-1]
    }
    else{
      AUCCY.dat$Days[j] <- 0
      AUCCY.dat$FishDays[j] <-0
      AUCCY.dat$SumFishDays[j] <-0
      AUCCY.dat$EscInd <- 0
      AUCCY.dat$SumEscape[j] <-0
    }
    
  }
  
  for (k in c(1:nrow(AUCCY.dat))){                                              ## Calculates PercentEscape Column.
    AUCCY.dat$PercentEscape[k] <- AUCCY.dat$SumEscape[k] / 
    AUCCY.dat$SumEscape[nrow(AUCCY.dat)]
  }
  
  
  
  p1  <-    ggplot(AUCCY.dat, aes(x=JulianDay, y=COUNT)) +                      ## AUC plot
    geom_line(color="blue") + geom_point() + theme_gray(base_size = 8)

  p2  <-    ggplot(AUCCY.dat, aes(x=JulianDay, y=SumEscape)) +                  
    geom_line(color="blue") + geom_point() + theme_gray(base_size = 8)          ## cumulative escapement plot
  
  logist <-   ggplot(AUCCY.dat, aes(x=JulianDay, y=PercentEscape)) + 
    geom_point() + theme_gray(base_size = 8)+stat_smooth(method="glm", 
                               method.args = list(family="binomial"), se=FALSE)

    ## run timing curve
  
  file <- paste("PLOTS",AUCCY.dat$STOCK, AUCCY.dat$SPECIES, AUCCY.dat$Year,
                AUCCY.dat$SURVEY_TYPE, sep = "/")                               ## creates title for each unique instance
  
  g <- grid.arrange(p1, p2, logist, top = file,  ncol = 2, nrow = 2)            ## combines all 3 plots into one grid
  
  
  ggsave(paste(file,".jpg"), device = "jpg", height = 4, width = 5, 
         units = "in", g)                                                       ## saves plot as jpg file
  
  if(exists('OUTPUTAUCCURRENTYEAR.dat') && 
     is.data.frame(get('OUTPUTAUCCURRENTYEAR.dat'))){                           ##checks to see if output dataframe exists
    OUTPUTAUCCURRENTYEAR.dat <- rbind(OUTPUTAUCCURRENTYEAR.dat,AUCCY.dat)       ##if it does, it appends new data
  }
  else {                                                                        ## if it doesn't creates new dataframe
    OUTPUTAUCCURRENTYEAR.dat <- AUCCY.dat
  }
  
  write.csv(OUTPUTAUCCURRENTYEAR.dat, "output/OutputAUC.csv", row.names=FALSE)  ## writes output dataframe to csv file
}



AUCALLYEARS.dat <- read.csv("input/InputRUN_TIMING.csv")                        ## read in input data
AUCALLYEARS.dat <- subset(AUCALLYEARS.dat, Year >=1976)                         ## only include years after 1976


AUCALLYEARS.dat$MMDD <- format(as.Date(AUCALLYEARS.dat$JulianDay, 
                               origin = as.Date("0000-01-01")), "%m-%d")        ## create a MMDD date without year


head(AUCALLYEARS.dat, 10) 

AUCALLYEARS.dat$Year <- as.integer(AUCALLYEARS.dat$Year)                        ## converts character to integer
AUCALLYEARS.dat$Date <- as.Date(AUCALLYEARS.dat$Date)                           ## converts character to date
AUCALLYEARS.dat$JulianDay <- as.POSIXlt(AUCALLYEARS.dat$Date)$yday              ## converts date to Julian Date integer


##AUCGROUPS.dat<- AUCALLYEARS.dat %>% group_by(SPECIES, 
##                          STOCK) %>% summarize(TotalCount = sum(COUNT))       ## Groups dataframe by categories

AUCGROUPS.dat <- read.csv("input/STOCKS.csv")                                   ## Imports list of stocks for run timing 
                                                                                ## curves

            
                                                                                
                                                                                ## for loop runs the AUC calculations for
for (i in c(1:nrow(AUCGROUPS.dat))){                                            ## each group
  AUC.dat <- subset(AUCALLYEARS.dat, 
                      SPECIES == AUCGROUPS.dat$SPECIES[i] &
                      STOCK == AUCGROUPS.dat$STOCK[i])
  model = glm(PercentEscape ~ JulianDay, family=binomial, data = AUC.dat)       ## fits glm logistic model to data
  
  RunTab <- data.frame(JulianDay = c(min(AUCALLYEARS.dat$JulianDay):
                                       max(AUCALLYEARS.dat$JulianDay)))         ## creates row from min to max date of data
  
  RunTab$MMDD <- format(as.Date(RunTab$JulianDay, 
                                origin = as.Date("0000-01-01")), "%m-%d")       ## espresses date in MMDD format
  
  RunTab$OriginDate <- as.Date(RunTab$JulianDay, 
                                origin = as.Date("0000-01-01"))
  RunTab$PercentEscape <- round(predict(model, RunTab, type = "response"),3)    ## uses model to predict run % based on date
  
  
  ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
  
  RunTab <- bind_cols(RunTab, setNames(as_tibble(predict(model, RunTab, 
                                                         se.fit = TRUE)[1:2]),
                                       c('fit_link', 'se_link')))
  
  RunTab <- mutate(RunTab, fit_resp = ilink(fit_link), 
                   se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
                   se_lwr = round(ilink(fit_link - (1.65 * se_link)),3))
  
  RunTab$LowerSum <- ceiling(RunTab$Percent*AUCGROUPS.dat$LOWER[i])             ## calculates # fish to meet LEG
  RunTab$UpperSum <- ceiling(RunTab$Percent*AUCGROUPS.dat$UPPER[i])             ## calculates # fish to meet UEG
  RunTab$LowerSum_se_upr <- ceiling(RunTab$se_upr*AUCGROUPS.dat$UPPER[i])
  RunTab$LowerSum_se_lwr <- ceiling(RunTab$se_lwr*AUCGROUPS.dat$LOWER[i])
  RunTab$UpperSum_se_upr <- ceiling(RunTab$se_upr*AUCGROUPS.dat$UPPER[i])
  RunTab$UpperSum_se_lwr <- ceiling(RunTab$se_lwr*AUCGROUPS.dat$LOWER[i])
  
  AUC.dat$OriginDate <- as.Date(AUC.dat$JulianDay, 
                        origin = as.Date("0000-01-01"))
  
 
                                                                                ## run timing curve with Julian date
  
  AUCCURRENTYEAR.dat <- subset(OUTPUTAUCCURRENTYEAR.dat, 
                        STOCK == AUCGROUPS.dat$STOCK[i] &
                        SPECIES == AUCGROUPS.dat$SPECIES[i])
  AUCCURRENTYEAR.dat$OriginDate <- 
    as.Date(as.POSIXlt(AUCCURRENTYEAR.dat$Date)$yday, 
                                            origin = as.Date("0000-01-01"))

    CurrentYearPlot <- ggplot() + xlab("Julian Day") +
                ylab("Total Fish") + 
                scale_x_date(breaks = "1 week", date_labels = "%b %d") +
     theme(axis.text.x = element_text(angle = 270,vjust=-0.1)) +
                geom_line(data=RunTab, aes(x=OriginDate, y=UpperSum), 
                          color ="steelblue") +
                geom_line(data=RunTab, aes(x=OriginDate, y=LowerSum), 
                          color = "steelblue")  +
                geom_ribbon(data=RunTab, aes(x=OriginDate, 
                                             ymin = LowerSum_se_lwr,
                                             ymax = UpperSum_se_upr), 
                            alpha = 0.1)  +
                geom_point(data=AUCCURRENTYEAR.dat, 
               aes(x=OriginDate, y=SumEscape), 
               color = "darkred") +
                geom_line(data=AUCCURRENTYEAR.dat, 
                          aes(x=OriginDate, y=SumEscape), 
                          color = "darkred") +
                
              ggtitle(paste(AUCCURRENTYEAR.dat$Year, AUCGROUPS.dat$STOCK[i], 
                            AUCGROUPS.dat$SPECIES[i], 
                            "Run Timing", sep = " ")) +
              theme(plot.title = element_text(hjust = 0.5))
              
  newfile <- paste("PLOTS",AUCGROUPS.dat$STOCK[i], AUCGROUPS.dat$SPECIES[i], 
                AUCCURRENTYEAR.dat$Year, "Run Timing", sep = "/") 
    
  ggsave(paste(newfile,".jpg"), device = "jpg", height = 4, width = 5, 
         units = "in", CurrentYearPlot)                                         ## saves plot as jpg file


## RunTabExport <- select(RunTab, c(JulianDay, MMDD, se_lwr, PercentEscape, 
##                                  se_upr, LowerSum, UpperSum))  
 
##  write.csv(RunTabExport, paste(file, "RunTable.csv"), row.names=FALSE)       ## writes dataframe to csv file
}

##  if(exists('OutputAllAUC.dat') && is.data.frame(get('OutputAllAUC.dat'))){   ## checks to see if output dataframe exists
##          OutputAllAUC.dat <- rbind(OutputAllAUC.dat,AUC.dat)                 ## if it does, it appends new data
##  }
##  else {                                                                      ## if it doesn't creates new dataframe
##    OutputAllAUC.dat <- AUC.dat
##  }
  
##  write.csv(OutputAllAUC.dat, "OUTPUT/OutputAUC.csv", row.names=FALSE)        ## writes output dataframe to csv file

setwd(projdir)