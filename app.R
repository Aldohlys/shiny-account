library(shiny)
#### library(shinyWidgets) ## for radioGroupButtons widget
library(dplyr)
library(lubridate) #### For today() function
library(DT)  ### Data table
library(quantmod)  ### Retrieve last market values
library(scales) ### label_percent function
library(readr) ## for read_delim function
library(magrittr) ### for %<>% pipe

source("C:\\Users\\martinale\\Documents\\RProjects\\Trading\\helpersv2.R")

getLastTickerData = function(ticker) {
  if (is.null(ticker)) return(list(last=NA,change=NA))
  
  ### Retrieve data from Yahoo Finance - no need to launch IBKR TWS
  ### Get last price and last change (J/J-1)
  tryCatch({
    ticker=getSymbols(ticker,auto.assign=FALSE,from=today()-10,warnings=FALSE) ## Case Tuesday morning and US market not yet opened + Monday and Friday were off -> Get Wed and THur data
    names(ticker)[length(names(ticker))]="Adjusted" ### Last column is "ticker.Adjusted" -> to be renamed as Adjusted
    last_data=as.numeric(ticker[[nrow(ticker),"Adjusted"]])
    p_last_data=as.numeric(ticker[[nrow(ticker)-1,"Adjusted"]])
    return(list(
      last=round(last_data,2),
      change=label_percent(accuracy=0.01)(last_data/p_last_data-1)
    ))
  }, error = function(e) {
    print(paste("Error:", e))
    return(list(last="Non disponible",change=NA))
  })
}

lastSPY=getLastTickerData("SPY")  ### Mkt value

source("C:\\Users\\martinale\\Documents\\RProjects\\Trading\\server.R")
source("C:\\Users\\martinale\\Documents\\RProjects\\Trading\\ui.R")


options(encoding = "UTF-8")

shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE, width=1000))
