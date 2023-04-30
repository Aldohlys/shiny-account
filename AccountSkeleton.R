# Define UI ----
library(shiny)
library(dplyr)
library(lubridate) #### For today() function
library(DT)

list_heures=parse_hms(sprintf("%02d:%02d:%02d",(9:17),0,0))
NewTrading="C:\\Users\\martinale\\Documents\\RProjects\\Trading\\"

###source(paste0(NewTrading,"helpers.R"))

### Define UI 
#################################
ui <- fluidPage(
        titlePanel("Etat des comptes"),
  
        fluidRow(
          column(width = 4,
                 selectInput("account", 
                             label = "Choose an account:",
                             choices = list("U1804173","DU5221795","Gonet"), selected = "U1804173"),
                 dateInput("dateAccount",label= "Choose a date:", value=today()),
                 selectInput("timePortfolio",label="Choose a time: ",list_heures),  ### default values
                 actionButton("go", label="Reload!",class = "btn-success"),
                 selectInput("symbolsPortfolio",label="Choose a symbol: ","All")
          ),
          
          column(width=8,
                 tabsetPanel(type = "tabs",
                             tabPanel("Account", 
                                      h4("Valeur du compte au ",textOutput("acc_date",inline=TRUE)),
                                      tableOutput("accountOneData")
                             ),
                             tabPanel("Portfolio", 
                                      fluidRow(
                                        h4("Valeur du portefeuille au ",textOutput("oPortDate",inline=TRUE))
                                      ),
                                      tableOutput("portfolioMetrics")
                             ),
                             tabPanel("Symbol",
                                      fluidRow(
                                        tableOutput("symMetrics")           
                                      )
                             )
                 )
          )
        )
)

###########################
### Define server

server <- function(input, output, session) {
  ###################  ACCOUNT ###########################################################    
  output$acc_date=renderText(paste0(input$dateAccount))
      
  accountAllData= reactive({
      input$go
      account_data = read.csv(paste0(NewTrading, "NewAccount.csv"), sep=";")
      account_data$date=as.Date(account_data$date,format="%d.%m.%Y")
      account_data$heure=parse_hms(account_data$heure)
      account_data=filter(account_data,account==input$account)
      account_data
  })
      

  output$accountOneData = renderTable({
      data=filter(accountAllData(),date == input$dateAccount)
      if (nrow(data)==0) {
        ### Nearest recorded date 
        list_dates= accountAllData()$date
        nearDate=list_dates[which.min(abs(list_dates-input$dateAccount))]
        data=filter(accountAllData(),date == nearDate)
        #freezeReactiveValue(input, "dateAccount")
        updateDateInput(session,"dateAccount",value=nearDate)
        }
      data=filter(data,heure==max(heure))
      data$date=format(data$date, "%d.%m.%Y")
      data$heure=format(data$heure, "%H%M%S") 
      data
  }) 
  
  ###########################  PORTFOLIO ###################################
  
  portfolio = reactive({
    input$go
    portf= data.frame(read.csv(paste0(NewTrading,input$account,".csv"),sep=";"))
    ### Convert from European date format to internal R date format
    portf$date=as.Date(portf$date,format="%d.%m.%Y") 
    portf$heure=parse_hms(portf$heure)
    portf
  })
  
  portfolioDate = reactive({
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
      
  output$oPortDate = renderText(paste(input$dateAccount,input$timePortfolio))
  output$portfolioMetrics = renderTable({
    portf=portfolioDateTime()
    portf$date=format(portf$date, "%d.%m.%Y")
    portf$heure=format(portf$heure, "%H%M%S")
    portf
   })
  
  observeEvent(input$timePortfolio, {
    list_symbols=unique(portfolioDateTime()$symbol)
    updateSelectInput(session,"symbolsPortfolio",choices=c("All",list_symbols))
  })
  
  #########################  SYMBOL ###########
  
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
  
  output$symMetrics= renderTable(symOneTable())
   
}
shinyApp(ui, server)




