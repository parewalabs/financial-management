library("shiny")
library("ggplot2")

capital_investment <- 40000

cash_flow <- "
Year,CashFlow
1,-17500
2,20000
3,25000
4,25000
5,25000"

discountRate <- 10

ui <- fluidPage(titlePanel("Payback Period Example"),
                a("Source Code", href="https://github.com/parewalabs/financial-management/blob/master/PaybackPeriod/app.R"),
                br(), br(),
                sidebarLayout(
                  sidebarPanel(sliderInput("capitalInput", "Initial Capital", min = 0, max = 100000,
                                           value = capital_investment, pre = "$"),
                               sliderInput("discountRate", "Discount Rate", min=0, max=30,
                                           value=discountRate, post = "%"),
                               textAreaInput("cashflowInput", "Cash flows", value = cash_flow, width = NULL, height = NULL,
                                             cols = NULL, rows = 10, placeholder = NULL, resize = NULL)
                  ),
                  mainPanel(
                    plotOutput("cashflowsBarChart"),
                    br(), br(),
                    h4("Normal payback period"), 
                    h2(textOutput("paybackPeriod")),
                    br(),
                    h4("Discounted payback period"),
                    h2(textOutput("discountedPaybackPeriod"))
                    
                  )))
server <- function(input, output) {
  cashflow_df <- reactive({
    read.table(text = input$cashflowInput, sep =",", header = TRUE, stringsAsFactors = FALSE) 
  })
  
  output$paybackPeriod <- renderText({
    capital <- input$capitalInput
    
    sum <- 0
    index <- 1
    for(i in cashflow_df()$CashFlow) {
      sum <- sum + i
      if(sum >= capital) {
        break
      }
      index <- index + 1
    }
    lastYearCashFlow <- cashflow_df()$CashFlow[index]
    
    amountExceedingCapital <- sum - capital
    
    index <- index - amountExceedingCapital/lastYearCashFlow
    
    index <- round(index, digits=2)
    return(index)
  })
  
  output$discountedPaybackPeriod <- renderText({
    capital <- input$capitalInput
    discountRate <- input$discountRate
    
    sum <- 0
    index <- 1
    for(i in cashflow_df()$CashFlow) {
      sum <- sum + i/((1+discountRate/100)^index)
      if(sum >= capital) {
        break
      }
      index <- index + 1
    }
    
    lastYearCashFlow <- cashflow_df()$CashFlow[index]
    amountExceedingCapital <- sum - capital
    index <- index - amountExceedingCapital/lastYearCashFlow
    index <- round(index, digits=2)
    
    return(index)
  })
  
  output$cashflowsBarChart <- renderPlot({
    ggplot(cashflow_df(), aes(Year, CashFlow)) + geom_bar(stat = "identity")
  })
}
shinyApp(ui = ui, server = server)