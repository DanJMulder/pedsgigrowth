## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##   Shiny App for Quickly Calculating Pediatric Growth                                      ##
##   Written by Daniel Mulder, March 2022                                                    ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## Preamble
# This script does not constitute medical advice and is only to be used for the purposes of learning or preparing personal templates
# This script contains no real medical information, any information contained within is fictional example information

# Load required packages ----
library(shiny) # for interactive web application framework
library(tidyverse) # for basic data organization
library(zscorer) # for calculating zscores from WHO data
library(glue) # for gluing together text


ui <- fluidPage(
  mainPanel(
    br(),
    tags$b("Growth Parameters Calculator (for copy/paste into clinic note)"),
    br(),
    br(),
    dateInput("date_of_birth", width = '50%', "Date of Birth (YYYY-MM-DD):", value = "2010-01-01"),
    selectInput("sex", width = '50%', "Sex:", choices = c("female", "male")),
    numericInput("height", width = '50%', "Height (in cm):", tags$span(style = "font-weight: 400", "Height (cm)")),
    numericInput("weight", width = '50%', "Weight (in kg):", tags$span(style = "font-weight: 400", "Weight (kg)")),
    tagAppendAttributes(textOutput("full_text"), style = "white-space:pre-wrap;"),
    br(),
    br(),
    paste("References: zscorer package: https://cran.r-project.org/web/packages/zscorer/index.html")
    )
  )

server <- function(input, output, session) {

    formData <- reactive({
    
    # patient_df <- as.data.frame(
    #   date_of_birth <- input$date_of_birth,
    #   sex <- input$sex,
    #   height <- input$height,
    #   weight <- input$weight
    # )
    # 
    height_text <- input$height
    height_percentile_text <- input$height
    height_zscore_text <- input$height
    # height_zscore_text <- addWGSR(data = "patient_df", sex = "sex", firstPart = "height", secondPart = "weight", index = "hfa")
    
    return(glue("Growth - ",
                paste0("height ",
                       height_text,
                       " cm (percentile = ",
                       height_percentile_text,
                       ", z-score = ",
                       height_zscore_text,
                       ")")
                )
           )
    })
  
  output$full_text <- renderPrint(formData())
  
}

shinyApp(ui, server)