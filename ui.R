
# Define UI ----
library(shiny)
library(dplyr)
library(lubridate) #### For today() function
library(hms)
library(DT)

list_heures=parse_hms(sprintf("%02d:%02d:%02d",(9:17),0,0))

# Define UI ----
ui <- fluidPage(
  titlePanel("Etat des comptes"),
 
  fluidRow(
    column(width = 4,
           selectInput("account", 
                       label = "Choose an account:",
                       choices = list("Live","U1804173","DU5221795","Gonet"), selected = "U1804173"),
           dateInput("dateAccount", 
                     label= "Choose a date:", value=today()),
           actionButton("go", label="Reload!",class = "btn-success"),
           selectInput("timePortfolio",label="Choose a time: ",choices = list_heures),  ### default values
           selectInput("symbolsPortfolio", label = "Choose a symbol:", choices = "All"),
           radioButtons("PlotType", label="Graphique: ", choices = c("Value"="value","Percentage"="percent"),
                        inline=TRUE),
           # dateRangeInput("rangeDate", label="Choose interval date:",
           #                start = "2022-10-04", end = today(),
           #                min = "2022-10-04",
           #                max = today(),
           #                format = "yyyy-mm-dd",
           #                startview = "month",
           #                weekstart = 1, ## Debut de semaine = lundi
           #                language = "fr",
           #                separator = " à ",
           #                width = NULL,
           #                autoclose = TRUE)
          
           h4("Projection: ",textOutput("oProjDate",inline=TRUE)),
           numericInput("p_days",label="Number of days: ",value=0),
           numericInput("p_u_price",label="Projected underlying price: ",value=0),
           numericInput("p_vol",label="Projected volatility (%): ",value=30.0)

    ),
    
    column(width=8,
           tabsetPanel(type = "tabs",
                       tabPanel("Account", 
                                h2("Valeur du compte au",textOutput("oDate",inline=TRUE)),
                                
                                tableOutput("account"),
                                tableOutput("accountMetrics"),
                                radioButtons("accountPlotChoice",
                                             label="Graphique: ",
                                             choices=c("NetLiquidation","TotalCashBalance","FullMaintMarginReq",
                                                       "UnrealizedPnL","RealizedPnL","OptionMarketValue",
                                                       "StockMarketValue"),
                                             inline=TRUE),
                                plotOutput("eqcurve")
                                ),
                        tabPanel("Portfolio", 
                                 fluidRow(
                                  h4("Valeur du portefeuille au",textOutput("oPortDate",inline=TRUE)),
                                  column(width=6,
                                         plotOutput("Gainers")),
                                  column(width=6,
                                         plotOutput("Decliners"))
                                  ),
                                 tableOutput("portfolioMetrics"),
                                 plotOutput("greeksPortfolio")
                                ),
                       tabPanel("Symbol", 
                                DTOutput("symPositions"),
                                tableOutput("symReporting"),
                                tableOutput("symReporting2"),
                                tableOutput("symGreeks"),
                                radioButtons("symPlotChoice",
                                             label="Graphique: ",
                                             choices=c("UnrealizedPnL"="uPnL","Market Price"="mktPrice",
                                             # "Option Price"="optPrice",
                                             "Underlying Price"="uPrice",
                                             "Market Value"="mktValue",
                                             # "IV"="IV",
                                             "delta"="delta", "gamma"="gamma",
                                             "vega"="vega","theta"="theta"),
                                             inline=TRUE),
                                plotOutput("symMetrics")
                                )
           ),
    )
  )
)
