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
library(lubridate) # for creating/parsing date objects


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
      
      age_in_days <- as.numeric(today() - ymd(input$date_of_birth))
      
      sex_coded <- if (input$sex == "male"){
        1
      } else if (input$sex == "female"){
        2
      }
      
      height_text <- input$height

      patient_df <- data.frame(
        age = age_in_days,
        sex = sex_coded,
        height = input$height,
        weight = input$weight
        )

      patient_df_w_hfa <- addWGSR(data = patient_df,
                                  sex = "sex",
                                  firstPart = "height",
                                  secondPart = "age",
                                  index = "hfa")
    
      height_zscore_text <- patient_df_w_hfa[[1, 5]]
      height_percentile_text <- round(pnorm(height_zscore_text)*100,0)
    
      patient_df_w_wfa <- addWGSR(data = patient_df,
                                sex = "sex",
                                firstPart = "weight",
                                secondPart = "age",
                                index = "wfa")
    
      height_percentile_text <- height_percentile_text
      
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