library(reshape2)
library(dplyr)
library(lubridate)
library(rCharts)

###############
#  Constants
###############
outputChoice_pctChange <- "%-change"
outputChoice_raw <- "Raw"

#################
# Utilities
#################

# Given UNIX timestamp, gets date in EST using standard default
unix_getdate <- function(val) { as.POSIXct(val, origin="1970-01-01", tz="US/Eastern") }

# Given date in google finance string format, gets data in EST 
gf_getdate <- function(sval) { unix_getdate(as.numeric(substr(sval,2,nchar(sval)))) }

# Fixes up the raw column of dates
gf_fixdates <- function(dates, interval) {
  begin <- NA
  retval <- NA
  # By inspection, GF intraday dates are all relative to a beginning record 
  # with Unix timestamp and subsequent records offset from the block head.
  for (idx in seq_along(dates)) {
    if (substr(dates[idx],1,1) == "a" ) {
      begin <- gf_getdate(dates[idx]) 
      dates[idx] <- "0"
    }
    convdt <- begin + as.numeric(dates[idx]) * interval
    if ( is.na(retval)[1] ) {
      retval <- convdt
    }
    else {
      retval <- append(retval,convdt)
    }
  }
  attr(retval,"tzone") <- "US/Eastern"      # Sigh... have to set it again 
  retval    
}

#################
# Data Retrieval
#################

# Creates URL for Google Finance intraday price api
gf_makeurl <- function(sym, days, interval) {
  paste0("http://www.google.com/finance/getprices?i=",interval,
         "&p=",days,"d&f=d,o,c,v&df=cpct&q=",sym)
}

# Get raw data from the Google Finance getprice API
gf_fetchdata <- function(url) {
  urlconn <- url(url)
  lines=readLines(urlconn)
  close(urlconn)
  lines
}

# Validates GF data from the header
gf_validate <- function(lines) {

  # Are there enough lines  
  linesok <- (length(lines) > 7)
  
  # Is it NYSE or NASDAQ
  exch<-lines[grep("EXCHANGE", lines)]
  exchok <- ( substr(exch, nchar(exch)-3, nchar(exch)) == "NYSE" ) ||
    ( substr(exch, nchar(exch)-5, nchar(exch)) == "NASDAQ" )
  
  # Does it open at 9:30AM Eastern and close at 4:00PM Eastern 
  opn <- lines[grep("MARKET_OPEN_MINUTE", lines)]
  opnok <- (substr(opn, 20, nchar(opn)) == "570")
  cls <- lines[grep("MARKET_CLOSE_MINUTE", lines)]
  clsok <- (substr(cls, 21, nchar(cls)) == "960")
  
  if ( exchok && linesok && opnok && clsok ) {
    lines
  } else {
    # Symbol not found, data not there, data is non-standard somehow or doesn't trade
    # So pass up an NA
    NA
  }  
}

# Gets raw intraday Google Finance price data
gf_getdata <- function(sym, days, interval) {
  # Make url
  urlg <- gf_makeurl(sym,days, interval)

  # Get raw data
  lines <- gf_fetchdata(urlg)
  
  # Validate the data
  lines <- gf_validate(lines)

  if ( is.na(lines)[1] ) {
    # Symbol not found, data not there
    NA
  }
  else {
    # Parse data excluding 7 header lines  
    tconn = textConnection(lines[-(1:7)])
    df <- read.csv(tconn, header=FALSE, stringsAsFactors=FALSE)
    close(tconn) 
    names(df) <- c("DATE","OPEN","CLOSE","VOLUME")
    df
  }
}

###############################
#  Data Cleaning and Analysis
###############################

# Fixes the offset dates in the raw output, adds DAY, PERIOD
# columns, filters out the first bar. and returns a melted data frame
gf_prepdata <- function(df, interval) {
  # Convert dates from offsets 
  df$DATE <- gf_fixdates(df$DATE, interval)
    
  today <- sprintf("%02d/%02d", month(Sys.Date()), day(Sys.Date()))
  interval_min <- interval / 60

  dfp <- df %>%     
    mutate(DAY=sprintf("%02d/%02d",month(DATE),day(DATE)), 
           PERIOD=(hour(DATE) * 60 + minute(DATE) - 570)) %>% 
    filter(PERIOD != 0) %>%  # Markets open at 9:30, so exclude this bar (see below)
    filter(DAY != today) %>%  # Exclude the current day to avoid partial data
    melt(id=c("DATE","DAY","PERIOD"), 
         measure.vars=c("OPEN","CLOSE","VOLUME"))
  dfp$DATE <- NULL

  # Note: Bars cover the period immediately before the time listed on the bar.
  # Therefore a 9:30 bar reflects activity right at the open or during pre-open, and 
  # so we want to exclude it because it's not normal market activity.
  
  # Find prices 
  # During initial inspection of the data, the opening prices of bars usually did 
  # not match the closing prices of the previous bar.  This reflects some issue with 
  # the google API and their data handling practices, so we will just take the 
  # average.
  popen <- gf_castdata(dfp, "OPEN", 1)
  popen$PERIOD <- popen$PERIOD - interval_min 
  names(popen) <- gsub("_OPEN","",names(popen))
  pclose <- gf_castdata(dfp, "CLOSE", 1)
  names(pclose) <- gsub("_CLOSE","",names(pclose))
  ptmp <- rbind(popen[1,])
  for (i in 2:nrow(popen)) {
    ptmp <- rbind(ptmp, (pclose[i-1,] + popen[i,])/2)
  }
  ptmp <- rbind(ptmp, pclose[nrow(pclose),])
  # Add back in to prepped data as APRICE
  mtmp <- melt(ptmp, id.vars = "PERIOD")
  mtmp$DAY <- mtmp$variable
  mtmp$variable <- "APRICE"
  rbind(dfp, mtmp[,c("DAY","PERIOD","variable","value")])
}

# casts a melted gf data frame, desired data vs DAY (orient=0) or PERIOD (orient=1)
gf_castdata <- function(df, what, orient=0) {
  if (orient==0) {
    df %>% filter(variable==what) %>% dcast(DAY ~ PERIOD + variable)   
  }
  else {
    df %>% filter(variable==what) %>% dcast(PERIOD ~ DAY + variable)   
  }
}

# Computes the difference in open/close prices for each bar period (sense=0)
# or the difference between closing price and open price of a reference bar (sense=1)
# Usually the reference bar here will be the first bar, so The latter sense gives
# the intraday price movement from the daily open.
gf_diffdata <- function(dfall, sense = 0, refcol=2) {
  popen <- gf_castdata(dfall, "OPEN", 0)
  pclose <- gf_castdata(dfall, "CLOSE", 0)
  tmp <- data.frame(DAY = popen$DAY)
  if ( sense == 0 ) {
    for ( i in 2:length(popen) ) {
      tmp <- cbind(tmp, pclose[,i] - popen[,i])
    }
  } else {
    for ( i in 2:length(popen) ) {
      tmp <- cbind(tmp, pclose[,i] - popen[,refcol])
    }
  }
  names(tmp) <- gsub("_OPEN","",names(popen))
  tmp
}

# Computes a list of correlations of data frame columns wth respect to 
# a reference column.  The first column is used to exclude any leading 
# index columns in the given data frame.
gf_corrcompute <- function(dfdiff, refcol=2, firstcol = 2) {
  res <- sapply(firstcol:length(dfdiff), 
                function(i){cor(dfdiff[,refcol], dfdiff[,i],
                                use="complete.obs")})
  names(res) <- names(dfdiff)[firstcol:length(dfdiff)]
  res
}

# Uses gf_corrcompute twice to compute correlations between prices in 
# the reference bar period and other bars.  The outputs are as follows.
#    Fixed: Column differences are within each bar.  The correlation is 
#           how well does the price movement in the reference bar predict
#           price moves in other bar periods
#    Rolling: Column differences are relative to the daily open.  The 
#           correlation is effectively how well does the initial price
#           move predict the closing prices of subsequent bars.
gf_correlationData <- function(tmp, refcol=2) {
  n<-as.numeric(refcol)
  df1 <- gf_diffdata(tmp)
  y1 <- gf_corrcompute(df1,n)
  df2 <- gf_diffdata(tmp,1)
  y2 <- gf_corrcompute(df2,n)
  x<-as.numeric(names(y1))
  ret <- data.frame(x,y1,y2)
  names(ret) <- c("Minutes", "RollingCorr", "FixedCorr")
  ret
}

##########################
#  Graphics Functions
##########################

# Uses nplot to create the price chart
gf_priceChart <- function(px, option="") {
  n1 <- nPlot(data=px, 
              x="PERIOD", y="value", group="DAY", 
              type="lineChart")
  n1$yAxis(tickFormat = "#! d3.format(',.3f') !#") 
  n1$xAxis(axisLabel="Minutes after Daily Open")
  n1
}

# Uses nplot to create the volume chart
gf_volumeChart <- function(vol, option="") {
  vol$value <- vol$value/1000
  n1 <- nPlot(data=vol, 
              x="PERIOD", y="value", group="DAY", 
              type="multiBarChart")
  if ( option == outputChoice_pctChange ) {
    n1$yAxis(tickFormat = "#! d3.format(',.3f') !#") 
  } else {
    n1$yAxis(tickFormat = "#! d3.format(',.0f') !#") 
  }
  n1$xAxis(axisLabel="Minutes after Daily Open")
  n1$chart(showControls = FALSE)
  n1$chart(stacked = TRUE)
  n1
}

# Uses base graphics to produce the correlation plots
gf_correlationPlots <- function(plotdata) {
  par(mfrow=c(1,2))
  with(plotdata, plot(RollingCorr~Minutes), xlab="Minutes After Open", ylab="Rolling Difference Correlation")
  with(plotdata, plot(FixedCorr~Minutes), xlab="Minutes After Open", ylab="Fixed Difference Correlation")
  par(mfrow=c(1,1))
}
