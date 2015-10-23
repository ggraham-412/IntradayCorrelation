library(reshape2)
library(dplyr)
library(lubridate)
library(rCharts)

# Given UNIX timestamp, gets date in EST using standard default
unix_getdate <- function(val) { as.POSIXct(val, origin="1970-01-01", tz="US/Eastern") }

# Given date in google finance string format, gets data in EST 
gf_getdate <- function(sval) { unix_getdate(as.numeric(substr(sval,2,nchar(sval)))) }

# Validates data in the header
gf_validate <- function(url) {
  
  # Read data as text
  urlconn <- url(url)
  lines=readLines(urlconn)
  close(urlconn)
  
  linesok <- (length(lines) > 7)
  
  exch<-lines[grep("EXCHANGE", lines)]
  exchok <- ( substr(exch, nchar(exch)-3, nchar(exch)) == "NYSE" ) ||
    ( substr(exch, nchar(exch)-5, nchar(exch)) == "NASDAQ" )
  
  opn <- lines[grep("MARKET_OPEN_MINUTE", lines)]
  opnok <- (substr(opn, 20, nchar(opn)) == "570")
  cls <- lines[grep("MARKET_CLOSE_MINUTE", lines)]
  clsok <- (substr(cls, 21, nchar(cls)) == "960")
  
  if ( exchok && linesok && opnok && clsok ) {
    lines
  } else {
    # Symbol not found, data not there
    NA
  }  
}

# Creates URL for Google Finance intraday price api
gf_makeurl <- function(sym, days, interval) {
  paste0("http://www.google.com/finance/getprices?i=",interval,
         "&p=",days,"d&f=d,o,h,l,c,v&df=cpct&q=",sym)
}

# GF intraday dates are given in a block format with each block 
# starting with a record with Unix timestamp and subsequent records 
# offst from the block head.
gf_fixdates <- function(dates, interval) {
  begin <- NA
  retval <- NA
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
  attr(retval,"tzone") <- "US/Eastern"      # Sigh...
  retval    
}

# Gets raw intraday Google Finance price data
gf_getdata <- function(sym, days, interval) {
  # Make url
  urlg <- gf_makeurl(sym,days, interval)
  
  lines <- gf_validate(urlg)
  
  if ( is.na(lines)[1] ) {
    # Symbol not found, data not there
    NA
  }
  else {
    # Parse data excluding 7 header lines  
    tconn = textConnection(lines[-(1:7)])
    df <- read.csv(tconn, header=FALSE, stringsAsFactors=FALSE)
    close(tconn) 
    
    # Get the "schema" line describing columns in output
    schema <- lines[grep("COLUMNS=", lines)]
    schema <- strsplit(substr(schema,9,nchar(schema)),",")
    names(df) <- unlist(schema)
    
    df
  }
}

# Fixes the offset dates in the raw output, adds DAY, PERIOD, and TR
# columns, filters out the first bar. and returns a melted data frame
gf_prepdata <- function(df, interval) {
  # Convert dates from offsets 
  df$DATE <- gf_fixdates(df$DATE, interval)
  
  today <- sprintf("%02d/%02d", month(Sys.Date()), day(Sys.Date()))
  interval_min <- interval / 60
  
  dfp <- df %>%     
    mutate(DAY=sprintf("%02d/%02d",month(DATE),day(DATE)), 
           PERIOD=(hour(DATE) * 60 +minute(DATE) - 570),
           PERIODS=sprintf("%02d:%02d",hour(DATE),minute(DATE)),
           TR=HIGH-LOW) %>% 
    filter(PERIODS != "09:30") %>%  # Markets open at 9:30, so exclude this bar
    filter(DAY != today) %>%  # Markets open at 9:30, so exclude this bar
    melt(id=c("DATE", "DAY","PERIOD"), 
         measure.vars=c("OPEN","HIGH","LOW","CLOSE","VOLUME","TR"))
  dfp$DATE <- NULL
  
  # Find prices 
  popen <- gf_castdata(dfp, "OPEN", 1)
  popen$PERIOD <- popen$PERIOD - interval_min # Bars reported at end of bar!
  names(popen) <- gsub("_OPEN","",names(popen))
  pclose <- gf_castdata(dfp, "CLOSE", 1)
  names(pclose) <- gsub("_CLOSE","",names(pclose))
  ptmp <- rbind(popen[1,])
  for (i in 2:nrow(popen)) {
    ptmp <- rbind(ptmp, (pclose[i-1,] + popen[i,])/2)
  }
  ptmp <- rbind(ptmp, pclose[nrow(pclose),])
  mtmp <- melt(ptmp, id.vars = "PERIOD")
  mtmp$DAY <- mtmp$variable
  mtmp$variable <- "APRICE"
  rbind(dfp, mtmp[,c("DAY","PERIOD","variable","value")])
}

# casts a melted gf data frame, desired data vs DAY
gf_castdata <- function(df, what, orient=0) {
  if (orient==0) {
    df %>% filter(variable==what) %>% dcast(DAY ~ PERIOD + variable)   
  }
  else {
    df %>% filter(variable==what) %>% dcast(PERIOD ~ DAY + variable)   
  }
}

gf_filterdata <- function(df, what, option="") {
  tmp <- df %>% filter(variable==what)
  if ( option == "%-change" ) {
    tmp <- gf_normdata(tmp)
  }
  tmp
}

gf_normdata <- function(df) {
  ltmp <- split(df, df$DAY)  
  ctmp <- NA
  for ( l in ltmp ) { 
    l$value <- l$value / l$value[1]
    if ( is.na(ctmp)[1] ) {
      ctmp<-l
    } else {
      ctmp <- rbind(ctmp,l)
    }
  }
  ctmp
}



# computes the difference in open/close for each Period
gf_diffdata <- function(dfall, sense = 0) {
  popen <- gf_castdata(dfall, "OPEN", 0)
  pclose <- gf_castdata(dfall, "CLOSE", 0)
  tmp <- data.frame(DAY = popen$DAY)
  if ( sense == 0 ) {
    for ( i in 2:length(popen) ) {
      tmp <- cbind(tmp, pclose[,i] - popen[,i])
    }
  } else {
    for ( i in 2:length(popen) ) {
      tmp <- cbind(tmp, pclose[,i] - popen[,2])
    }
  }
  names(tmp) <- gsub("_OPEN","",names(popen))
  tmp
}

gf_corrcompute <- function(dfdiff, refcol=2, firstcol = 2) {
  res <- sapply(firstcol:length(dfdiff), 
                function(i){cor(dfdiff[,refcol], dfdiff[,i],
                                use="complete.obs")})
  names(res) <- names(dfdiff)[firstcol:length(dfdiff)]
  res
}

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

gf_priceChart <- function(px, option="") {
  n1 <- nPlot(data=px, 
              x="PERIOD", y="value", group="DAY", 
              type="lineChart")
  n1$yAxis(tickFormat = "#! d3.format(',.3f') !#") 
  n1$xAxis(axisLabel="Minutes after Daily Open")
  n1
}

gf_volumeChart <- function(vol, option="") {
  vol$value <- vol$value/1000
  n1 <- nPlot(data=vol, 
              x="PERIOD", y="value", group="DAY", 
              type="multiBarChart")
  if ( option == "%-change" ) {
    n1$yAxis(tickFormat = "#! d3.format(',.3f') !#") 
  } else {
    n1$yAxis(tickFormat = "#! d3.format(',.0f') !#") 
  }
  n1$xAxis(axisLabel="Minutes after Daily Open")
  n1$chart(showControls = FALSE)
  n1$chart(stacked = TRUE)
  n1
}

gf_correlationPlots <- function(plotdata) {
  par(mfrow=c(1,2))
  with(plotdata, plot(RollingCorr~Minutes), xlab="Minutes After Open", ylab="Rolling Difference Correlation")
  with(plotdata, plot(FixedCorr~Minutes), xlab="Minutes After Open", ylab="Fixed Difference Correlation")
  par(mfrow=c(1,1))
}
