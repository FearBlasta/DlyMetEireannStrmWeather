#Install Packages if necessary
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("lubridate")
#devtools::install_github("dkahle/ggmap")

#devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)

# Libraries Used
library(lubridate)
library(dplyr, warn.conflicts = FALSE)
#suppressPackageStartupMessages(library(dplyr))

library(DT) #Nice Tables
#library(stringr) #String Mainpulatiion

library(httr) #Ensure URL exists
library(ggmap) #Nice GIS Graphing Maps
library(ggplot2) #Nice Graphs

library(ggradar) #Radar Graph
library(scales) #Scaling values to fit Radar graph

library(cowplot) #combine ggplot graphs in a grid


# Weather Stations # https://www.met.ie/climate/weather-observing-stations
wthObsrStns = c('Athenry','Ballyhaise','Belmullet','Carlow Oakpark','Claremorris','Dunsany','Fermoy Moorepark','Finner','Gurteen','Johnstown Castle','Mace Head','Malin Head','Markree','Mount Dillon','Mullingar','Newport','Dublin (Phoenix Park)','Roches Point','Sherkin Island','Valentia')

#This Station name don't match file
#wthObsrStns


# From www.met.ie
# Sample dPth="http://cli.fusio.net/cli/climate_data/webdata/dly1075.csv"

# #############################################
### Function to exclude header rows from CSV file
# #############################################
readMetCSV <- function(dPth) {
  #dPth='https://cli.fusio.net/cli/climate_data/webdata/dly5602.csv'
  #Read www.Met.ie data from file
  #Ignore header information
  #library(httr)
  
  if(http_status(GET(dPth))$reason=="Not Found") {
    return (list("Error",http_status(GET(dPth))$reason))
  } else {
    f = file(dPth, 'r')
  }
  
  i=0
  hdrText=""
  dfMet=""
  #skip all schema rows until the csv header
  while ( TRUE ) {
    #Read a line at a time
    line = readLines(f, n = 1)
    
    #File doesn't have 5 columns in the first 40 rows; exit loop
    if(i>40) {
      break
    }
    
    #If the row contains more than 5 comma's this is the first row of the header file
    if (length(strsplit(line,',')[[1]])>5) {
      if(i==0){i=1}
      dfMet=read.csv(dPth, skip=i)
      
      break #Exit loop
    }
    hdrText=paste(hdrText,"\n",line) #Capture Header Text for Info
    i=i+1
    
    #print(paste(i,hdrText))
  }
  
  close(f) #Close file
  return(list(hdrText, dfMet))
}

# #############################################
### Verify fields exist, otherwise return Error message
# #############################################
checkAttributes<-function(dfCSV){
  #Hard Coded list of field names (I failed to find a correct list on www.met.ie)
  reqFldNms<-list("date", "ind", "maxtp", "ind.1", "mintp", "igmin", "gmin", "ind.2", "rain", "cbl", "wdsp",  "ind.3","hm", "ind.4", "ddhm",  "ind.5", "hg", "soil", "pe", "evap", "smd_wd", "smd_md", "smd_pd", "glorad", "sun")
  
  for(nm in reqFldNms) {
    
    #Check name does not exist
    if(!(nm %in% names(dfCSV))) {
      cat(nm," ",!(nm %in% names(dfCSV)),"\n")
      #Check is abbreviated name exists
      if(!(substr(nm,1,4) %in% names(dfCSV))) {
        
        #Change Column Name
        colnames(dfCSV)[which(names(dfCSV) == substr(nm,1,4))] <- nm
      } else {
        cat(nm, "\n\n")
        #Add Column Name
        dfCSV[nm]=NA
      }
    }
  }
  
  return(dfCSV)
  
}

# #############################################
### Format Data in CSV
# #############################################
formatMetCSV <- function(dfMet) {
  #Read www.Met.ie data from file
  dfMet$date1 <- parse_date_time(dfMet$date, orders = "dmy")
  
  dfMet$Year <- as.numeric(substr(dfMet$date1, 1, 4))
  dfMet$Month <- as.numeric(substr(dfMet$date1, 6, 7))
  dfMet$Day <- as.numeric(substr(dfMet$date1, 9, 10))
  dfMet$abbrDay <- format(strptime(dfMet$date, format = "%d-%b-%Y"), '%a')
  
  dfMet$Month <- factor(dfMet$Month,
                        levels = seq(1,12,by=1),
                        labels = month.name)
  
  dfMet$Season <- dfMet$Month
  levels(dfMet$Season) <- list("Winter" = c("December", "January", "February"), 
                               "Spring" = c("March","April","May"),
                               "Summer" = c("June","July","August"),
                               "Autumn" = c("September","October","November"))
  
  #RainType summary(dfMet$maxtp) 
  #summary(dfMet$wdsp)
  #str(dfMet$abbrDay)
  
  dfMet$"rainType"[is.na(dfMet$maxtp) | is.na(dfMet$rain) & is.na(dfMet$rain)] = ''
  dfMet$"rainType"[dfMet$maxtp < 4 & dfMet$rain > 2 & dfMet$rain < 4] = 'Frigid Rain'
  dfMet$"rainType"[dfMet$maxtp > 4 & dfMet$rain > 4 & dfMet$rain < 8 
                   & dfMet$cbl < summary(dfMet$cbl)[[2]]] = 'Raining Cats & Dogs' #cbl less than 1st Quartile
  dfMet$"rainType"[dfMet$mintp > 10 & dfMet$maxtp < 20 & dfMet$rain > 2] = 'Drizzle'
  dfMet$"rainType"[dfMet$maxtp > 10 & dfMet$maxtp < 20 & dfMet$rain < 2] = 'Sprinkle'
  dfMet$"rainType"[dfMet$maxtp > 20 & dfMet$rain > 0] = 'Soft Rain'
  dfMet$"rainType"[dfMet$wdsp > 15.5 & dfMet$rain > 4] = 'Horizontal Rain'
  dfMet$"rainType"[dfMet$maxtp > 20] = 'Scorcher'
  dfMet$"rainType"[is.na(dfMet$"rainType") & dfMet$rain > 1] = '4 Seasons is one Day'
  dfMet$"rainType"[is.na(dfMet$"rainType") & dfMet$rain > 8] = 'A Torrent'
  
  dfMet
}


# #############################################
### Display Text if text exist
# #############################################
printDsc<- function(dsc){
  #If Description is not an empty string print description
  if(nchar(dsc[[1]])>1) {
    print(strsplit(dsc[[1]],"\n "),sep="\n")
  } else {
    print("File has no description")
  }
}


# #############################################
### Major Weather Events
# #############################################
#List of [Major Weather Events](https://www.met.ie/climate/major-weather-events) on [www.met.ie]:
getMajorWeatherEvents <- function() {
  #CSV hardcoded from text on page https://www.met.ie/climate/major-weather-events
  strmPth="Data/met climate major weather events.csv"
  dfMetEvnts=read.csv(strmPth,skip=0)
  
  dfMetEvnts$Date<-parse_date_time(dfMetEvnts$Date, 'Ymd') #strptime(dfMetEvnts$Date, format = "%Y-%m-%d")
  
  return(dfMetEvnts)
}

# #############################################
### Major Storms
# #############################################
getMajorStormEvents <- function() {
  dfMetEvents<-getMajorWeatherEvents()
  
  #Get Storms Only
  dfMetStrms<-dfMetEvnts %>% filter(substr(Event,1,5)=="Storm")
  
  return(dfMetStrms)
}


# #############################################
### Miscellaneous Roughwork
# #############################################
#dfCork = dfMetStns %>%
#  filter(County=='Cork',is.na(Close.Year))  %>%
#  group_by(Open.Year) %>%
#  summarise(n=n()) %>%
#  arrange(desc(as.numeric(Open.Year)))

#unique(dfCork$Open.Year)
#distinct(dfCork)
#summary(dfCork)



# #############################################
### www.Met.ie Station Details
# #############################################
getStationDetails <- function() {
  #library('dplyr')
  
  stnPth="https://cli.fusio.net/cli/climate_data/webdata/StationDetails.csv"
  metStn <- readMetCSV(stnPth)
  
  metStn[[2]] <- metStn[[2]] %>%
    mutate(Status = ifelse(is.na(`Close.Year`),"Open","Closed"))
  
  #Only Get Open Stations for further use
  dfMetStns <- metStn[[2]]
  
  #dfMetOpenCorkStns <- metStn[[2]] %>%
  #  filter(Status=="Open" & County=='Cork') # in Cork Only
  
  return(dfMetStns)
}



# #############################################
### Histogram of Open and Shut Weather Stations
# #############################################
dispHistStations <- function(dfMetStns) {
  gHst<-ggplot(dfMetStns, aes(County, fill=Status)) +
    geom_histogram(bins=30, stat="count") +
    theme(axis.text.x=element_text(angle=60, size=8), legend.position="bottom")
  
  #ggsave("Ireland Weather Stations.png", width=600, height=400, units="mm", plot=gHst)
  
  return(gHst)
}


# #############################################
### Histogram of Open and Shut Weather Stations
# #############################################
dispMapStations <- function(dfMetStns) {
  ### Map showing stations
  #devtools::install_github("dkahle/ggmap")
  #library(ggmap)
  
  gHst<-dispHistStations(dfMetStns)
  #get_googlemap("ireland", zoom = 12) %>% ggmap()
  
  gMp1<-qmplot(Longitude, Latitude, data = dfMetStns, maptype = "toner-lite", color=Status) +
    theme(legend.position="bottom")
  
  #ggsave("Ireland Weather Stations.png", width=300, height=400, units="mm", plot=gMpl)
  #qmplot(Longitude, Latitude, data = metStn[[2]], maptype = "toner-lite", geom = "density2d", color = I("red"))
  
  return(plot_grid(gHst, gMp1, nrow=1, align="h")) #Unhappy with alignment
}




# #############################################
### Get All daily weather data for all stations
# #############################################
getDlyStationData <- function(thrshld=24) {
  #Using Cork Stations as a subset for faster development
  #This should be parameterised
  dfMetOpenCorkStns <- getStationDetails() %>%
      filter(Status=="Open" & County=='Cork') # in Cork Only
  
  for(sNm in unique(dfMetOpenCorkStns$name)) {
    stnPth=paste("https://cli.fusio.net/cli/climate_data/webdata/dly", trimws(dfMetOpenCorkStns[["Station.Number"]][dfMetOpenCorkStns["name"]==sNm]),".csv",sep="")
    
    metWthr<-readMetCSV(stnPth)
    
    #If readMetCSV is a string (i.e. empty) Then do nothing #Otherwise is is a dataframe
    if(typeof(metWthr[[2]])=="character") {
      #print(paste(sNm,":",stnPth,"\n"))
    } else {
      cat("[",sNm,"] : ",stnPth," [",typeof(metWthr[[2]]),"]\n\t"
          , length(names(metWthr[[2]]))," Columns: ", paste(names(metWthr[[2]]), collapse=","),"\n"
          #, shape(metWthr[[2]]),"Observations\n"
          ,sep="")
      
      #Only select weather stations with 24+ attributes
      #This should be parameterised as a threshold
      if(length(names(metWthr[[2]]))>=thrshld) {
        
        #Ensure all the attributes are correct
        metWthr[[2]]<-checkAttributes(metWthr[[2]])
        
        #Add Station Name to the Dataset
        metWthr[[2]]["Station"]<-toupper(sNm)
        
        #Combine all station datasets
        if(!exists("allStnData")) {
          allStnData <- metWthr[[2]]
        } else {
          allStnData <- allStnData %>%
            bind_rows(metWthr[[2]])
        }
        
        #cat(length(names(metWthr[[2]]))," Columns: ", paste(names(metWthr[[2]]), collapse=","),"\n"
        #    , length(metWthr[[2]])," Observations\n"
        #    ,sep="")
        #cat(!("sun" %in% names(metWthr[[2]])),"\n\n")
      }
    }
    
    #checkAttributes()
    #print(typeof(metWthr[[2]]))
    #printDsc(metWthr[[1]])
  }
  
  return(allStnData)
}



# #############################################
### Get All daily weather data for all stations
# #############################################
updateWthrStationStatus <- function() {
  dfMetStns<-getStationDetails()
  allStnData<-getDlyStationData(24)
  
  dfMetStns$Status[toupper(dfMetStns$name) %in% unique(allStnData$Station)] = "Report"
  
  return(dfMetStns)
}

# #############################################
### Get All daily weather data for all stations
# #############################################
dispUpdWthrStat <- function(dfMetStns) {
  datatable(dfMetStns, editable=TRUE, options=list(scrollX='400px'))
}

#dfMetStns %>%
#  group_by(Status) %>%
#  tally()



# #############################################
### Get All daily weather data for all stations
# #############################################
dispUpdWthrGrph <- function(allStnData) {
  #library(ggmap)
  
  dfMetStns<-getStationDetails()
  
  gHst<-dispHistStations(dfMetStns)
  #get_googlemap("ireland", zoom = 12) %>% ggmap()
  
  gMp1<-qmplot(Longitude, Latitude, data = dfMetStns, maptype = "toner-lite", color=Status) +
    theme(legend.position="bottom")
  
  dfMetStns$Status[toupper(dfMetStns$name) %in% unique(allStnData$Station)] = "Report"
  
  gMp2<-qmplot(Longitude, Latitude, data = dfMetStns %>% filter(Status=="Report"), maptype = "toner-lite", color=Status) +
    theme(legend.position="bottom")
  
  plot_grid(gHst, gMp1, gMp2, nrow=1, align="h") #Unhappy with alignment
}



# #############################################
### Default Weather Station
# #############################################
frmtWthr <- function(dfFmtWthr) {
  ### Load Weather of Station data for "Cork Airport"
  #unique(allStnData$Station) = "CORK_AIRPORT"  "MOORE_PARK"    "ROCHES_POINT"  "SHERKINISLAND"
  dfFmtWthr <- formatMetCSV(allStnData %>% filter(Station==unique(allStnData$Station)[1]))
}



# #############################################
### Display boxplot of Yearly Weather by Season
# #############################################
dispBoxPltBySeason <- function(dfFmtWthr) {
  pSes <- ggplot(dfFmtWthr, aes(Year, maxtp, group=Year, fill=Season), na.rm=TRUE) +
    geom_boxplot() +
    facet_wrap(~Season)
  
  return(pSes)
}


  

#datatable(dfFmtWthr, options=list(scrollX='400px'))
#summary(dfFmtWthr)


# #############################################
### Get Map of Ireland with longitude and latitude
# #############################################
dispIrlMap <- function() {
  ### Get Staten Map of Ireland
  
  #Attempt to get map of Ireland from Weather Station: Min and Max, Longitude and Latitude values
  #irl<-c(top=floor(summary(metStn[[2]]$Longitude)[[1]]*1.05), bottom=ceiling(summary(metStn[[2]]$Longitude)[[6]]/1.05),
  #       left=ceiling(summary(metStn[[2]]$Latitude)[[1]]/1.05), right=floor(summary(metStn[[2]]$Latitude)[[6]]*1.05))
  #irl
  
  irl <- c(left = -11, bottom = 51, right = -5, top = 55.5)
  get_stamenmap(irl, zoom = 8, maptype = "toner-lite") %>% ggmap() 
  #?get_googlemap("ireland", zoom = 12) %>% ggmap()
  #  Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
}



# #############################################
### Use Linear Regression lines to Check for Globabl Warming
# #############################################
dispLmTempYrly <- function(dfFmtWthr) {
  #Group and summarise data by yrMthDt
  dfWthrTemp <- dfFmtWthr %>%
    mutate(yrMthDt = as.numeric(sub('-', '', substr(strptime(date, format = "%d-%b-%Y"), 1, 7)))) %>%
    group_by(yrMthDt) %>%
    summarise(meanTp = mean((maxtp+mintp)/2, na.rm = TRUE), maxTp = max(maxtp, na.rm = TRUE), minTp = min(mintp, na.rm = TRUE), numRcrds = n()) %>%
    select(`yrMthDt`, `meanTp`, `maxTp`, `minTp`, `numRcrds`) %>%
    arrange(yrMthDt)
  
  #library(ggplot2)
  
  g1 <- ggplot(dfWthrTemp, aes(yrMthDt,minTp)) + 
    geom_line(aes(y=minTp, colour=minTp)) + 
    geom_smooth(method="lm", se=F) +
    labs(x="", y="Mean Temp (°C)") +
    scale_x_continuous(breaks=seq(195000, 202000, 330)) +
    theme(axis.text.x =element_text(angle=45))
  
  g2 <- ggplot(dfWthrTemp, aes(yrMthDt,meanTp)) + 
    geom_line(aes(y=meanTp, colour=meanTp)) +
    geom_smooth(method="lm", se=F) +
    labs(x="", y="Mean Temp (°C)",colour="Temp") +
    scale_x_continuous(breaks=seq(195000, 202000, 330)) +
    theme(axis.text.x =element_text(angle=45)) +
    scale_color_gradient(low="greenyellow",high="forestgreen")
  
  g3 <- ggplot(dfWthrTemp, aes(yrMthDt,maxTp)) + 
    geom_line(aes(y=maxTp, colour=maxTp)) + 
    geom_smooth(method="lm", se=F) +
    labs(x="", y="Max Temp (°C)")  +
    labs(title="Temperature by Year and Month",subtitle="Month in format: YYYYMM") +
    scale_x_continuous(breaks=seq(195000, 202000, 330)) +
    theme(axis.text.x =element_text(angle=45)) +
    scale_color_gradient(low="orange",high="red")
  
  
  #install.packages("reshape2")
  #library(reshape2)
  
  #install.packages("cowplot")
  #library(cowplot)
  return(plot_grid(g3,g2,g1,nrow=3,rel_heights=c(2,1.5,1.5)))
}


# #############################################
### Summarise Globabl Warming Linear Regression
### subFunctions
# #############################################
dispHyp <- function(smryLM, engh, chrg) {
  msg<-paste("At a confidence Interval of 95%, there is", engh
             , "enough evidence to REJECT H0, the Null Hypothesis, in favour of the alternative hypothesis that there is a"
             , chrg, "difference in the mean temperatures.<br/>And", engh, "support the claim, Global warming is"
             , if(chrg==" positive "){" NOT "} else {" "}
             , "fake news! i.e. Pr(>|t|) = ", smryLM$coefficients[2,4], " for ", attr(smryLM$terms,"term.labels"),"<br/><br/>", sep="")
  
  return(msg)
}

hpoth <- function(smryLM) {
  #Does the probability meet the Threshold
  if(smryLM$coefficients[2,4] < 0.05) {
    sTh = " " #is enough
  } else {
    sTh = " NOT " #is not enough
  }
  
  #Is slope positive or negative
  if(smryLM$coefficients[2,1] > 0) {
    sSlp = " positive "
  } else {
    sSlp = " negative "
  }
  
  return(dispHyp(smryLM,sTh,sSlp))
}


# #############################################
### Summarise Globabl Warming Linear Regression
# #############################################
smryTempYrly <- function(dfFmtWthr) {
  #Group and summarise data by yrMthDt
  dfWthrTemp <- dfFmtWthr %>%
    mutate(yrMthDt = as.numeric(sub('-', '', substr(strptime(date, format = "%d-%b-%Y"), 1, 7)))) %>%
    group_by(yrMthDt) %>%
    summarise(meanTp = mean((maxtp+mintp)/2, na.rm = TRUE), maxTp = max(maxtp, na.rm = TRUE), minTp = min(mintp, na.rm = TRUE), numRcrds = n()) %>%
    select(`yrMthDt`, `meanTp`, `maxTp`, `minTp`, `numRcrds`) %>%
    arrange(yrMthDt)
  
  #### Usinga Confidence Interval of 95%
  
  lmMinTemp<-summary(lm(minTp ~ yrMthDt, data = dfWthrTemp))
  lmMeanTemp<-summary(lm(meanTp ~ yrMthDt, data = dfWthrTemp))
  lmMaxTemp<-summary(lm(maxTp ~ yrMthDt, data = dfWthrTemp))

  msg<-paste("The slope of the 3 linear regression lines is:",
      "<br/>    Slope of line lm(Min Temperature ~ Date(YYYYMM)):", substr(as.character(lmMinTemp$coefficients[2,1]),1,8),
      "<br/>    Slope of line lm(Mean Temperature ~ Date(YYYYMM)):", substr(as.character(lmMeanTemp$coefficients[2,1]),1,8),
      "<br/>    Slope of line lm(Max Temperature ~ Date(YYYYMM)):", substr(as.character(lmMaxTemp$coefficients[2,1]),1,8)
      ,"<br/><br/>"
      , hpoth(lmMinTemp)
      , hpoth(lmMeanTemp)
      , hpoth(lmMaxTemp))
  
  return(msg)
}

#cat("However, only Null Hypothesis for lm(Min Temperature ~ Date(YYYYMM)) can be rejected in favour of the alternative hypothesis that there is a positive difference in the mean temperatures. And support the claim Global warming is not fake news!")



# #############################################
### Summary Join to Storms and Weather
# #############################################
smryStrmWthr <- function(dfMetStrms, dfFmtWthr) {
  #Join Storms to Station Weather Data
  dfStrmStn<-right_join(dfMetStrms,	dfFmtWthr, by=c("Date"="date1"))
  dfStrmStn<-dfStrmStn %>%
    mutate(Event = ifelse(is.na(Event) & (Season=="Winter" | Season=="Spring"),"Normal (Cool)",
                          ifelse(is.na(Event) & (Season=="Summer" | Season=="Autumn"),"Normal (Warm)",Event)))
  
  #dfStrmStn<-dfStrmStn %>%
  #  mutate(Event = ifelse(is.na(Event) & Season=="Winter","Normal (Win)",
  #                        ifelse(is.na(Event) & Season=="Spring","Normal (Spr)",
  #                        ifelse(is.na(Event) & Season=="Summer","Normal (Sum)",
  #                        ifelse(is.na(Event) & Season=="Autumn","Normal (Aut)",Event)))))
  
  #dfMetStrms$Date #"2018-03-04 GMT" "2018-03-03 GMT" "2018-03-02 GMT" "2018-03-01 GMT" "2018-02-28 GMT"
  #[6] "2017-10-16 IST" "2014-02-12 GMT" "1986-08-15 IST" "1961-09-16 IST"
  
  #dfStrmStn %>%
  #  group_by(Event) %>%
  #  summarise(n=n())
  
  #head(dfStrmStn, 20)

  #Get Mean Weather values for Storm events
  eventWthr<-dfStrmStn %>%
    #select(Event, maxtp, mintp, rain, cbl, wdsp, hg, glorad) %>%
    group_by(Event) %>%
    summarise("Max Temp (C)"=mean(maxtp, na.rm=TRUE),
              "Min\nTemp (C)"=mean(mintp, na.rm=TRUE),
              "Rain (mm)"=mean(rain, na.rm=TRUE),
              "Pressure (hpa)"=mean(cbl, na.rm=TRUE),
              "Wind\nSpeed (kt)"=mean(wdsp, na.rm=TRUE),
              "Highest\nGust (kt)"=mean(hg, na.rm=TRUE),
              #"Global Radiation (J/cm sq.)"=mean(glorad, na.rm=TRUE)
    )
  
  
  #is.nan(1/0-1/0)
  
  #eventWthr %>%
  #  mutate(`Global Radiation (J/cm sq.)`=
  #           if(is.nan(eventWthr$`Global Radiation (J/cm sq.)` ) ==TRUE){0}else{eventWthr$`Global Radiation (J/cm sq.)` })
  
  #              #"Global Radiation (J/cm sq.)"=if(is.na(glorad)==TRUE){0}else{mean(glorad, na.rm=TRUE)}) #%>%
  #  #mutate("Global Radiation (J/cm sq.)"=if(is.na("Global Radiation (J/cm sq.)")==TRUE){0}else{"Global Radiation (J/cm sq.)"}) #%>%
  
  return(eventWthr)
}


# #############################################
### Plot Storm Temp, Pressure & Rain
# #############################################
dispScttrPlt <- function(eventWthr) {
  dPlt<-ggplot(eventWthr, aes(`Max Temp (C)`, `Rain (mm)`, colour=Event, size=`Pressure (hpa)`)) + 
    geom_point()
  
  return(ggplotly(dPlt))
}



# #############################################
### Radar of Storms & Normal
# #############################################
dispRdrStrmPlt <- function(eventWthr) {
  #https://www.rdocumentation.org/packages/ggradar/versions/0.2/topics/ggradar-package
  
  #install.packages("devtools")
  #devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
  #library(ggradar)
  #suppressPackageStartupMessages(library(dplyr))
  #library(scales)
  
  ggWthr<-eventWthr%>%
    mutate_each(funs(rescale), -Event)
  
  ggradar(ggWthr
          , axis.label.size=3
          , group.line.width=1, group.point.size=2
          #, plot.title="Storm Attributes to Norm")
          , legend.text.size=10) +
    labs(title="Major Storm Events",subtitle="Comparison of Storm Attributes to Norm") +
    theme(text=element_text(size=10))
  #, legend.title="Event Names")
  
  #add_rownames(var="group") %>%
  #mutate(eventDate = paste(Event," (",substr(date1,1,10),")",sep="")) %>%
}
