#### helpers.R

library(tidyr) #FOr pivot_longer
library(dplyr)
library(ggplot2)
library(DescTools) #for GCD function
#library(xlsx)
library(lubridate)
library(scales) #FOr labels functions
library(purrr) #FOr map2
library(quantmod) ### TO get EUR/USD/CHF conversion rates
library(magrittr) ## for %<>% operator


### For BS computations
#### source("C:\\Users\\aldoh\\Documents\\R\\Trading\\ROptionsLibrary.R")
##############################
### ACCOUNT 
##############################

readAccount = function(accountnr) {
  account_data = read.csv("C:\\Users\\aldoh\\Documents\\NewTrading\\NewAccount.csv", sep=";")
  ### account	date	heure	
  ### NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq	
  ### FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
  ### CashFlow
  ### Starts on Oct 4th, 2022 for IBKR, on June 1st for Gonet
  
  ### Convert from European date format to internal R date format
  account_data$date=as.Date(account_data$date,format="%d.%m.%Y")
  filter(account_data,account==accountnr)
}

plt_account = function(data,typePlot) {
  data=mutate(data,datetime=ymd_hms(paste(date,heure)))
  if (typePlot=="percent") {
    ggplot(data,aes(x=datetime))+
      scale_y_continuous(name="Value",labels=scales::percent,n.breaks=20)+
      scale_x_datetime(name="Value date", date_labels = "%b %d", guide = guide_axis(n.dodge = 2),date_breaks = "5 days")+
      geom_line(aes(y=metrics/first(metrics)-1),color="purple")+
      geom_hline(aes(yintercept=0))+
      ggtitle("Account evolution in %")
  }
  else {
    ggplot(data,aes(x=datetime))+
      scale_y_continuous(name="Value",n.breaks=20)+
      scale_x_datetime(name="Value date", date_labels = "%b %d", guide = guide_axis(n.dodge = 2),date_breaks = "5 days")+
      geom_line(aes(y=metrics),color="darkgreen")+
      {if (max(data$metrics) >0 & min(data$metrics) <0) {geom_hline(aes(yintercept=0))} else NULL}+
      ggtitle("Account evolution in value")
  }
}


############ PORTFOLIO #####################
### Test purpose: account="U1804173"
readPortfolio = function(account) {
  
  portf= data.frame(read.csv(paste0("C:\\Users\\aldoh\\Documents\\NewTrading\\",account,".csv"),sep=";"))
  ### Convert from European date format to internal R date format
  portf$date=as.Date(portf$date,format="%d.%m.%Y") 
  ### Convert position into an integer (this is not a float)
  portf$position=as.integer(portf$position)
  ## Remove all CASH positions that are virtual
  portf %<>% filter(secType!="CASH") 
  
  ## For nicer output on screen -makes column names shorter
  
  ### Case where there are options in the portfolio
  if ("lastTradeDateOrContractMonth" %in% colnames(portf)) portf %<>% rename(expiration=lastTradeDateOrContractMonth)
  if ("right" %in% colnames(portf)) portf %<>% rename(`Put/Call`=right)
  if ("undPrice" %in% colnames(portf)) portf %<>% rename(uPrice=undPrice)
  if ("impliedVol" %in% colnames(portf)) portf %<>% rename(IV=impliedVol)
  
  portf %<>% rename(type=secType,pos=position,
               mktPrice=marketPrice, mktValue=marketValue, 
               avgCost=averageCost,uPnL=unrealizedPnL)
  print(portf)
  return(portf)
}


#### Nice plot for greeks display over time
plt_portfolio_greeks= function(greeks) {
  greeks=pivot_longer(greeks,cols=c("delta", "gamma","theta","vega"))
  cols= c("delta" = "red", "gamma" = "blue", "vega" = "yellow", "theta" = "orange")
  ggplot(na.omit(greeks),aes(x=datetime,y=value))+
    scale_y_continuous(name="Greek total value",n.breaks=20)+
    scale_x_datetime(name="Value date", date_labels = "%b %d", guide = guide_axis(n.dodge = 2),date_breaks = "2 days")+
    geom_line(aes(color=name))+
    scale_color_manual(values=cols,breaks=c("delta","gamma","vega","theta"),
                       labels=c("delta","gamma","vega","theta"))+
    #scale_colour_viridis_d()+
    ggtitle("Greeks evolution")
}

currency_format = function(amount,currency){
  euro <- label_dollar(
    prefix = "",
    suffix = " \u20ac",
    accuracy=0.01
  )
  chf <- label_dollar(
    prefix = "",
    suffix = " CHF",
    accuracy=0.01
    )
  dollar <- label_dollar(
    prefix = "",
    suffix = " $",
    accuracy=0.01
  )
  
  switch(currency, "EUR"=euro(amount),
         "CHF"=chf(amount),
         "USD"=dollar(amount))
}

### The 2 lines below will not work with "from=today()" between 00:00 and 6:00am 
### as Europe is one day after the US between this time period and Yahoo server is located in the US
suppressWarnings({
  euro=getSymbols("EURUSD=X",from=today()-1,warnings=FALSE, auto.assign = FALSE)[[4]]
  chf=getSymbols("CHFUSD=X",from=today()-1,warnings=FALSE, auto.assign = FALSE)[[4]]
})

currency_convert = function(amount,currency) {
  ### Suppress warning that close is only current close and not final one for today

    cur_convert= switch(currency, "EUR"=euro,
           "CHF"=chf,
           "USD"=1)
  
  as.numeric(cur_convert*amount)
}

# currency_convert(100,"CHF")
# currency_convert(110,"EUR")
# currency_convert(120,"USD")
# as.numeric(map2(c(100,110,120),c("CHF","EUR","USD"),currency_convert))

#### If one of the position delta cannot be computed (NA - not available) -
#### then the whole Greek is equal to NA
#### If there are multiple currencies then they all become dollars
greeksNet = function(portf) {
  portf=mutate(portf,uPrice=as.numeric(map2(uPrice,currency,currency_convert)))
  mutate(portf,
          dnet=case_when(
          type=="STK" ~ 1*eval(pos),
          type=="OPT" ~ 100*eval(delta)*eval(pos),
          TRUE ~ 0),
         ddnet=case_when(
           type=="STK" ~ 1*eval(pos)*eval(uPrice),
           type=="OPT" ~ 100*eval(delta)*eval(pos)*eval(uPrice),
           TRUE ~ 0),
         gnet=if_else(type=="OPT",
                      100*gamma*pos,
                      0),
         tnet=if_else(type=="OPT",
                      100*theta*pos,
                      0),
         vnet=if_else(type=="OPT",
                      100*vega*pos,
                      0)) %>% summarize(delta=sum(dnet,na.rm=FALSE), 
                                        deltadollars=currency_format(sum(ddnet,na.rm=FALSE),"USD"),
                                        gamma=sum(gnet,na.rm=FALSE), 
                                        theta=sum(tnet,na.rm=FALSE),
                                        vega=sum(vnet,na.rm=FALSE))
}



####################  SYMBOL
plt_sym_metrics = function(data,metrics,typePlot) {
  data= mutate(data,datetime=ymd_hms(paste(date,heure)))
  data = group_by(data,datetime)
  
  if (metrics %in% c("delta","gamma","theta","vega")) {
    data = greeksNet(data)
    dplot = filter(mutate(data, yplot=.data[[metrics]]),!is.na(yplot))
  }
  else {    
    #### Notice that sum must be done for each datetime
    dplot = summarize(data, yplot=sum(.data[[metrics]],na.rm=TRUE))
  }
  
  if (typePlot=="percent") {
  #####  DATA TO REWORK - equal to Value
  ### DPLYR Tidy evaluation => usage de .data
      ### dplot should not be grouped at this level (grouping removed by summarize)
      dplot = dplot %>% mutate(yplot=(yplot-first(yplot))/abs(first(yplot)))
      ggplot(dplot,aes(x=datetime))+
      scale_y_continuous(name="Value",labels=scales::percent,n.breaks=20)+
      scale_x_datetime(name="Value date", date_labels = "%b %d", 
                     guide = guide_axis(n.dodge = 2),breaks = date_breaks("2 days"))+
      geom_line(aes(y=yplot),color="purple")+
      {if (max(dplot$yplot,na.rm=TRUE) >0 & min(dplot$yplot,na.rm=TRUE) <0) {geom_hline(aes(yintercept=0))} else NULL}+
      ggtitle(paste(metrics,"evolution in % !!! Position may vary"))
  }
  
  else {
    ### DPLYR Tidy evaluation => usage de .data
    #### Market price is wrong as must be ratioed with position
      ggplot(dplot,aes(x=datetime))+
      scale_y_continuous(name=metrics,n.breaks=20)+
      scale_x_datetime(name="Value date", date_labels = "%b %d", 
                       guide = guide_axis(n.dodge = 2),breaks = date_breaks("2 days"))+
      geom_line(aes(y=yplot),color="darkgreen")+
      {if (max(dplot$yplot,na.rm=TRUE) >0 & min(dplot$yplot,na.rm=TRUE) <0) {geom_hline(aes(yintercept=0))} else NULL}+
      ggtitle(paste(metrics,"evolution in value  !!! Position may vary"))    
    
  }
  
}

# positions=data.frame(type="P",expiration=ymd("2023-02-17"),strike=80.0)
# positions=add_row(positions,type="P",expiration=ymd("2023-02-17"),strike=90.0)
# positions=add_row(positions,type="P",expiration=ymd("2023-04-21"),strike=100.0)
# positions=add_row(positions,type="C",expiration=ymd("2023-03-21"),strike=100.0)

# compute_opt_price = function(positions, price, vol,days){
#   mutate(positions,p_price= if_else(
#     type=="P", getBSPutPrice(S=price,K=positions$strike,r=interest_rate,
#                              DTE=as.numeric(ymd(positions$expiration)-today()-days),
#                                  sig=vol),
#     getBSCallPrice(S=price,K=positions$strike,r=interest_rate,
#                    DTE=as.numeric(ymd(positions$expiration)-today()-days),sig=vol)))
# }
