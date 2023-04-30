library(shiny)
library(DT)

source(paste0(NewTrading,"helpers.R"))

# Define server logic ----
server <- function(input, output,session){  
  
  ##############################  ACCOUNT ################################
  ### account	date	heure	
  ### NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq	
  ### FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
  
  accountAllData = reactive({
    input$go
    return(readAccount(input$account))
  })
  
  accountOneData = reactive({
    req(input$dateAccount)
    
    data=filter(accountAllData(),date == input$dateAccount)
    if (nrow(data)==0) {
      ### Nearest recorded date 
      list_dates= accountAllData()$date
      nearDate=list_dates[which.min(abs(list_dates-input$dateAccount))]
      data=filter(accountAllData(),date == nearDate)
      #freezeReactiveValue(input, "dateAccount")
      updateDateInput(session,"dateAccount",value=nearDate)
    }
    filter(data,heure==max(heure))  
  }) 
  
  output$oDate <- renderText({
    format(input$dateAccount,"%A %d %B %Y")
  })

  output$accountMetrics = renderTable({
    data=select(accountOneData(),UnrealizedPnL,RealizedPnL,OptionMarketValue,StockMarketValue)
    data.frame(
      UnrealizedPnL=label_dollar(accuracy=1)(data$UnrealizedPnL),
      RealizedPnL=label_dollar(accuracy=1)(data$RealizedPnL),
      OptionMarketValue=label_dollar(accuracy=1)(data$OptionMarketValue),
      StockMarketValue=label_dollar(accuracy=1)(data$StockMarketValue)
    )
  })

  output$account = renderTable({
    data=select(accountOneData(),NetLiquidation,TotalCashBalance,FullMaintMarginReq)
    
    data.frame(
      NetLiquidationValue=label_dollar(accuracy=1)(data$NetLiquidation),
      TotalCash=label_dollar(accuracy=1)(data$TotalCashBalance),
      MaintenanceMargin=label_dollar(accuracy=1)(data$FullMaintMarginReq),
      #EquityNLRatio=label_percent(accuracy=0.01)((NetLiquidation-TotalCashBalance)/NetLiquidation),
      `Margin to Cash Ratio`=label_percent(accuracy=0.1)(data$FullMaintMarginReq/data$TotalCashBalance)
    )
  })
  
  output$eqcurve = renderPlot({
    data=select(accountAllData(),date,heure,
                metrics=input$accountPlotChoice)
    
    ### Keep only data with non-NA values
    data=data[!is.na(data['metrics']),]
    #data=filter(data,(date>= input$rangeDate[1] & date<= input$rangeDate[2])) Removed that filter - never used
    plt_account(data,input$PlotType)
  })

  ###########################################  PORTFOLIO ################################
  # date;heure;secType;symbol;lastTradeDateOrContractMonth;strike;right;position;marketPrice;optPrice;
  # marketValue;averageCost;unrealizedPnL;impliedVol;pvDividend;delta;gamma;vega;theta;undPrice;currency
  portfolio = reactive({
    message(sys.call())
    portf=readPortfolio(input$account)
    portf$date=as.Date(portf$date,format="%d.%m.%Y") 
    portf$heure=parse_hms(portf$heure)
    portf
  })

  portfolioDate = reactive({
    message(sys.call())
    data=filter(portfolio(),date == ymd(input$dateAccount))

    if (nrow(data)==0) {
      ### Nearest recorded date 
      list_dates= portfolio()$date
      nearDate=list_dates[which.min(abs(list_dates-input$dateAccount))]
      data=filter(portfolio(),date == nearDate)
      #freezeReactiveValue(input, "dateAccount")
      updateDateInput(session,"dateAccount",value=nearDate)
    }
    data
    ####  ?aggregate.zoo
  })
  
  portfolioDateTime = reactive({
    message(sys.call())
    data=filter(portfolioDate(),heure== parse_hms(input$timePortfolio))
    if (nrow(data)==0) {
      list_times=unique(portfolioDate()$heure)
      nearTime=list_times[which.min(abs(list_times-parse_hms(input$timePortfolio)))]
      data=filter(portfolioDate(),heure == nearTime)
      ### freezeReactiveValue(input,"timePortfolio")
      updateSelectInput(session,"timePortfolio",choices=list_times)
    }
    data
  })
  
  
  portfolioOne = reactive({
     message(sys.call())
     data= portfolioDateTime() %>%
      group_by(symbol,currency) %>%
      summarise(uPnL=sum(uPnL))
    
    ### Convert in USD if necessary
    data=mutate(data,uPnL=as.numeric(map2(uPnL,currency,currency_convert)))
    message(sys.call()," ",data)
    data
  })
  
  observeEvent(input$timePortfolio, {
    list_symbols=unique(portfolioDateTime()$symbol)
    updateSelectInput(session,"symbolsPortfolio",choices=c("All",list_symbols))
  }, ignoreInit=T)
 
  output$oPortDate <- renderText({
    format(input$dateAccount,"%A %d %B %Y")
  })
  
  output$Gainers <-renderPlot({
    data=filter(portfolioOne(),uPnL>=0)
    if (nrow(data)!=0) {
      ggplot(data, aes(x=reorder(symbol,uPnL), y=uPnL,fill=symbol)) +
        geom_col(width=0.5) +
        scale_y_continuous("Unrealized PnL",labels = label_dollar(),n.breaks = 10)+
        scale_x_discrete("Underlying")+
        ggtitle("Gainers")
    }
  })
  
  output$Decliners <-renderPlot({
    data=filter(portfolioOne(),uPnL<0)
    if (nrow(data)!=0) {
      ggplot(data, aes(x=reorder(symbol,uPnL), y=uPnL,fill=symbol)) +
      geom_col(width=0.5) +
      scale_y_continuous("Unrealized PnL",labels = label_dollar(),n.breaks = 10)+
      scale_x_discrete("Underlying")+
      ggtitle("Decliners")
    }
  })
  
   output$portfolioMetrics = renderTable({
     portfolioDateTime() %>% greeksNet()
   })

   output$greeksPortfolio = renderPlot({
     data=portfolio()
     data=filter(data,(date>= ymd(input$dateAccount) - 30 & date<= ymd(input$dateAccount)))
     plt_portfolio_greeks(group_by(mutate(data,datetime=ymd_hms(paste(date,heure))),datetime)
                          %>% greeksNet())
   })
  
  ############################### SYMBOL #######################################
  # date                         
  # heure
  # type - STK, OPT, CFD, CASH
  # symbol - SPY, SLV,...
  # expiration - ex: "20221210"
  # Put/Call - P ou C
  # pos 1,-1,2,-2,...
  # mktPrice  as per real-time market data               
  # optPrice option price per IBKR propritary model
  # mktValue mktPrice*pos 
  # avgCost initial position cost - per contract
  # uPnL unrealized P and L
  # IV
  # pvDividend ?? not usable
  # delta, gamma, vega, theta
  # uPrice - Underlying price
  # currency - USD, CHF, EUR      
  
 ##### Get position history for either one symbol or all symbols
  #### -- depending upon input$symbolsPortfolio value


  symAllTable = reactive({
    portf=portfolio() 
    # %>% filter(date>= input$rangeDate[1] & date<= input$rangeDate[2])
    if (input$symbolsPortfolio!="All") {
      portf = portf %>% filter(symbol==input$symbolsPortfolio) %>%
        select(-symbol)
    }
    portf
  })
  
  ##### Get current position for one or all symbols
  symOneTable = reactive({
        filter(symAllTable(),date==input$dateAccount,
               heure==parse_hms(input$timePortfolio)) %>% select(-c(date,heure,pvDividend))
  })
  
  ##### Select -if wanted- individual positions in current portfolio
  ##### By default all positions are selected for a single symbol selection and none for "All" selection
  output$symPositions <- renderDT({
      dt=mutate(symOneTable(),
                strike=as.character(map2(strike,currency,currency_format)),
                mktPrice=as.character(map2(round(mktPrice,2),currency,currency_format)),
                expiration=format(ymd(expiration),"%d %b %Y"))
      datatable(dt,
                options=list(paging=FALSE,searching=FALSE,
                             info=FALSE,ordering=FALSE,autowidth=TRUE,
                             columnDefs = list(list(visible=FALSE, 
                                                    targets=c("delta","gamma","theta","vega","uPrice","optPrice",
                                                              "avgCost","mktValue","uPnL","currency")))),
                selection = list(mode='multiple', 
                                 selected = {if (input$symbolsPortfolio != "All") rownames(dt) else NULL}),
                rownames=FALSE) %>% formatPercentage('IV',2)
      },server = FALSE)
  
  ## Compute Greeks for selected rows/positions
  output$symGreeks <- renderTable({
    greeksNet(symOneTable()[input$symPositions_rows_selected,,drop=FALSE])
  })
  
  ### Plot 
  output$symMetrics = renderPlot({
    expiration_selected=symOneTable()[input$symPositions_rows_selected,]$expiration
    strike_selected=symOneTable()[input$symPositions_rows_selected,]$strike
    putcall_selected=symOneTable()[input$symPositions_rows_selected,]$`Put/Call`
    
    report= filter(symAllTable(),expiration %in% expiration_selected, 
                   strike %in% strike_selected, `Put/Call` %in% putcall_selected)
   
    ## Execute only if at least one line is selected
    if (nrow(report) != 0) {
      ### Normalize mktPrice so the sum of the normalized prices gives the total position price
      if (nrow(report)==1) {
        nbPos=report$pos 
      } else 
        { nbPos= GCD(report$pos,na.rm=TRUE) }
      report=mutate(report,mktPrice=(pos/nbPos)*mktPrice)
    
      plt_sym_metrics(
        report,
        input$symPlotChoice,input$PlotType)
    }
   })
  
  output$oProjDate <- renderText({
    if(input$p_days != 0) {
      format(today()+input$p_days,"%A %d %B %Y")  
    }
  })
  
  output$symReporting <- renderTable({
    req(input$timePortfolio,input$symbolsPortfolio)
    
    report=select(symOneTable()[input$symPositions_rows_selected,,drop=FALSE],
                  expiration,strike,type=`Put/Call`,IV,
                  pos,mktPrice,mktValue,avgCost,uPnL,uPrice,currency)
    #If stock and options are mixed -- for stocks uPrice=0
    nb_currencies = report %>% summarize(n_distinct(currency))
    
    ## Produce table only if same currency for all selected positions
    ### This will always be the case if only one symbol
    ### But the table can also be produced if no symbol selected but selected lines all share the same currency
    if (nb_currencies == 1) {
      curr=report$currency[1]
      ### If only one row then no GCD computation, just report$pos
      if (nrow(report)==1) {
        nbPos=report$pos 
      }
      else {
        nbPos = GCD(report$pos,na.rm=TRUE)
      }
      table=data.frame(
        #### uPrice may not exist or equal to 0 if position on the underlying exists also
        Date=format(today(),"%d %b %Y"),
        uPrice=currency_format(max(report$uPrice),curr),
        
        Cost=currency_format(sum(report$pos*report$avgCost),curr),
        ## To take into account ratioed positions
        NbPos= nbPos,
        MarketPrice=currency_format(sum(report$pos/nbPos*report$mktPrice),curr),
        MarketValue=currency_format(sum(report$mktValue),curr),
        UnrealizedPnL=currency_format(sum(report$uPnL),curr),
        Return=label_percent(accuracy=0.1)(sum(report$uPnL)/abs(sum(report$pos*report$avgCost)))
      ) 
      
      if(input$p_u_price != 0) {
        report=compute_opt_price(positions=report,days=input$p_days,
                          vol=input$p_vol/100,price=input$p_u_price)
        add_row(table, 
                Date=format(today()+input$p_days,"%d %b %Y"),
                uPrice=currency_format(input$p_u_price,curr),
                Cost=currency_format(sum(report$pos*report$avgCost),curr),
                NbPos=nbPos,
                MarketPrice=currency_format(sum(report$pos/nbPos*report$p_price),curr),
                MarketValue=currency_format(sum(100*report$pos*report$p_price),curr),
                UnrealizedPnL=currency_format(sum(report$pos*(100*report$p_price-report$avgCost)),curr),
                Return=label_percent(accuracy=0.1)(sum(report$pos*(100*report$p_price-report$avgCost))
                                                   /abs(sum(report$pos*report$avgCost)))
                )
      }
      else return(table)
    }
  })
  
  output$symReporting2 <- renderTable({
    report=select(symOneTable()[input$symPositions_rows_selected,,drop=FALSE],
                  pos,mktPrice,mktValue,avgCost,uPnL,expiration,currency)
    
    expiration_selected=symOneTable()[input$symPositions_rows_selected,]$expiration
    strike_selected=symOneTable()[input$symPositions_rows_selected,]$strike
    putcall_selected=symOneTable()[input$symPositions_rows_selected,]$`Put/Call`

    report= filter(symAllTable(),expiration %in% expiration_selected, 
                   strike %in% strike_selected, `Put/Call` %in% putcall_selected)
    
    ## Produce table only if same currency for all selected positions
    ## Produce able for only one symbol
    nb_currencies = report %>% summarize(n_distinct(currency))
    if ((nb_currencies == 1) && (input$symbolsPortfolio!="All") ){
      
      exp_date= ymd(min(report$expiration))
      first_date= ymd(min(report$date))
      DaysToGo= as.numeric(exp_date-today())
      DaysPassed= as.numeric(today()-first_date)
      DaysTotal=as.numeric(exp_date-first_date)
      
      table=data.frame(
        InitialDate=format(first_date,"%d %b %Y"),
        StartDate=format(today(),"%d %b %Y"),
        DaysToGo=DaysToGo,
        TimePassed=label_percent(accuracy=0.1)(DaysPassed/DaysTotal),
        TimeRemaining=label_percent(accuracy=0.1)(DaysToGo/DaysTotal)
      )
      
      if (input$p_days != 0) {
        DaysToGo=DaysToGo-input$p_days
        add_row(table,
             InitialDate=format(first_date,"%d %b %Y"),
             StartDate=format(today()+input$p_days,"%d %b %Y"),
             DaysToGo=DaysToGo,
             TimePassed=label_percent(accuracy=0.1)((DaysPassed+input$p_days)/DaysTotal),
             TimeRemaining=label_percent(accuracy=0.1)(DaysToGo/DaysTotal))
      }
      else return(table)
    }
    })
  
  
  # If you want the application (and the Rscript) to stop after the user exited the Shiny application 
  # (closed the browser tab in which the application was running), 
  # add the following line to the "server" function
  # session is an input argument to server function
  session$onSessionEnded(stopApp)
}