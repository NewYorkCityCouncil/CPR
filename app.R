# test app for CPR. Brook Frye. Began 9/26/2018
# load dependancies

library(readr)
library(data.table)
library(shiny)
library(rsconnect)
library(dplyr)
library(ggplot2)
library(magrittr)
library(zoo)
library(plotly)
library(DT)
library(extrafont)
#loadfonts(device = "win")
library(readr)
library(cowplot)
library(gridExtra)
library(shiny)
library(data.table)


#dt <- fread("shiny_cpr.csv")[, -1]
dt <- fread("cpr_w_anomolies.csv")[, -1]
# datsub <- read_csv("shiny_cpr.csv", col_types = cols(remainder = col_double()))[, -1]
# setDT(datsub)
dt[, MY := as.yearmon(MY)]
dt <- dt[N >= 10, ] # subset down to indicators that have more than 10 data points 

# test app 
ui <- fluidPage(
  
  titlePanel("CPR Data"), 
  sidebarLayout(    
  sidebarPanel(
    selectInput("input_type", "Agency", choices = sort(unique(dt$Agency))), 
    # radio button or just print the arima 
    uiOutput("ui"), 
    textOutput("direction")
    
  ), 
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotlyOutput("plot"),
               textOutput("about")),
      tabPanel("Table", dataTableOutput("table"), width = 8
    

    )
  )

)

))

server <- function(input, output) {
  
  output$ui <- renderUI({
    # req(input$input_type)
    if (is.null(input$input_type))
      return()

    switch(input$input_type,
           
           "ACS" = selectInput("ind", "Indicator",
                                choices = dt[Agency %in% "ACS", unique(ind)]),
           "BIC" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "BIC", unique(ind)]),
           "BOE" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "BOE", unique(ind)]), 
           "BPL" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "BPL", unique(ind)]), 
           "CCHR" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "CCHR", unique(ind)]), 
           "CCRB" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "CCRB", unique(ind)]), 
           "CUNY" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "CUNY", unique(ind)]), 
           "DCA" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DCA", unique(ind)]), 
           "DCAS" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DCAS", unique(ind)]), 
           "DCLA" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DCLA", unique(ind)]), 
           "DCP" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DCP", unique(ind)]), 
           "DDC" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DDC", unique(ind)]), 
           "DEP" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DEP", unique(ind)]), 
           "DFTA" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DFTA", unique(ind)]), 
           "DHS" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DHS", unique(ind)]), 
           "DOB" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOB", unique(ind)]),
           "DOC" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOC", unique(ind)]),
           "DOE" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOE", unique(ind)]),
           "DOF" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOF", unique(ind)]),
           "DOHMH" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOHMH", unique(ind)]),
           "DOI" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOI", unique(ind)]),
           "DOITT" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOITT", unique(ind)]),
           "DOP" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOP", unique(ind)]), 
           "DORIS" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DORIS", unique(ind)]),
           "DOT" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOT", unique(ind)]),
           "DOP" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DOP", unique(ind)]),
           "DPR" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DPR", unique(ind)]),
           "DSNY" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DSNY", unique(ind)]),
           "DYCD" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "DYCD", unique(ind)]),
           "EDC" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "EDC", unique(ind)]),
           "FDNY" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "FDNY", unique(ind)]),
           "HPD" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "HPD", unique(ind)]),
           "HRA" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "HRA", unique(ind)]),
           "LAW" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "LAW", unique(ind)]),
           "LPC" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "LPC", unique(ind)]),
           "NYCEM" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "NYCEM", unique(ind)]),
           "NYCHA" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "NYCHA", unique(ind)]), 
           "NYCHH" = selectInput("ind", "Indicator",
                                 choices = dt[Agency %in% "NYCHH", unique(ind)]), 
           "NYCHA" = selectInput("ind", "Indicator",
                                 choices = dt[Agency %in% "NYCHA", unique(ind)]), 
           "NYPD" = selectInput("ind", "Indicator",
                                 choices = dt[Agency %in% "NYPD", unique(ind)]), 
           "NYPL" = selectInput("ind", "Indicator",
                                choices = dt[Agency %in% "NYPL", unique(ind)]),
           "OATH" = selectInput("ind", "Indicator",
                                choices = dt[Agency %in% "OATH", unique(ind)]),
           "OCME" = selectInput("ind", "Indicator",
                                choices = dt[Agency %in% "OCME", unique(ind)]),
           "QPL" = selectInput("ind", "Indicator",
                                choices = dt[Agency %in% "QPL", unique(ind)]),
           "SBS" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "SPS", unique(ind)]),
           "TLC" = selectInput("ind", "Indicator",
                               choices = dt[Agency %in% "TLC", unique(ind)])
           
    )
           
  })
  

    output$plot <- renderPlotly({
      dat <- dt[Agency %in% input$input_type, ][ind %in% input$ind, ]
      if(length(unique(dat$anomaly))>1) {
      p <- ggplot(dat, aes(x = MY, y = val2, colour = anomaly)) + 
        geom_point() + 
       # geom_smooth(span = .25, se=F) +
        theme_bw() + labs(x = "Date", y = "Indicator") + 
        theme(axis.title=element_text(size=14, family = "Arial"))
      }
      
      else{
        p <- ggplot(dat, aes(x = MY, y = val2)) + 
          geom_point() + 
          geom_smooth(span = .25, se=F) +
          theme_bw() + labs(x = "Date", y = "Indicator") + 
          theme(axis.title=element_text(size=14, family = "Arial"))
  
      }
      ggplotly(p)
  })
  
 
  output$table <- renderDataTable({
    req(input$input_type)
    ag <- input$input_type
    indc <- input$ind
    dt <- dt[Agency %in% ag, ]
    dt <- dt[ind %in% indc, .(Date = as.Date(as.yearmon(MY)), Agency = Agency, Indicator = ind, Value = val2)]
    datatable(dt, options = list(paging=T))
    })

  output$about <- renderText({
    dat <- dt[Agency %in% input$input_type, ][ind %in% input$ind, ]
    if(length(unique(dat$anomaly))>1)
    paste("This plot shows how the indicator --", input$ind,  "-- changes over time. To determine anomalies, we used a time series decomposion and flagged 
          any points that, after subtracting the time and overall trends, were larger or smaller than 3 times the IQR.",
           sep = " ")
    else{
    paste("This plot shows how the indicator --", input$ind,  "-- changes over time. For this plot, there is either too little data to do anamoly detection or we did not find any anamolies. 
               The loess curve shows how the data are changing over time.",
               sep = " ")}
  })
  
output$direction <- renderText({ 
  ag <- input$input_type
  indc <- input$ind
  dt <- dt[Agency %in% ag, ]
  dt <- dt[ind %in% indc, ]
  dt$Direction <- ifelse(dt$Direction=="Down", "down", ifelse(dt$Direction=="Up", "up", "*"))
  paste("The desired direction of change for this metric\n is", " ", unique(dt$Direction))
  
})

}
# change aesthetics and maybe add a NYC graphic header 

shinyApp(ui=ui, server=server)













