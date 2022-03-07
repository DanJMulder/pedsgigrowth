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
    tags$b(h1(strong("Growth Parameters Calculator"), style = "font-size:30px;")),
    tags$b(strong("(for copy/paste into clinic note)")),
    br(),
    br(),
    dateInput("date_of_birth", width = '50%', "Date of Birth (YYYY-MM-DD):", value = "2013-01-01"),
    selectInput("sex", width = '50%', "Sex:", choices = c("female", "male")),
    numericInput("height", width = '50%', "Height (in cm):", tags$span(style = "font-weight: 400", "Height (cm)")),
    numericInput("weight", width = '50%', "Weight (in kg):", tags$span(style = "font-weight: 400", "Weight (kg)")),
    tagAppendAttributes(textOutput("full_text"), style = "white-space:pre-wrap;"),
    br(),
    br(),
    paste("References:"),
    br(),
    paste("World Health Organization. WHO Child Growth Standards: Length/Height-for-age, Weight-for- age, Weight-for-length, Weight-for-height, and Body Mass Index-for age: Methods and Development. 1st ed. World Health Organization; 2006."),
    br(),
    paste("zscorer package: https://cran.r-project.org/web/packages/zscorer/index.html")
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
      
      patient_df <- data.frame(
        age = age_in_days,
        sex = sex_coded,
        height = input$height,
        weight = input$weight
        )

      # height
      capture.output(patient_df_w_hfa <- addWGSR(data = patient_df,
                                                 sex = "sex",
                                                 firstPart = "height",
                                                 secondPart = "age",
                                                 index = "hfa")
                     )

      height_zscore_text <- patient_df_w_hfa[[1, 5]]
      height_percentile_text <- round(pnorm(height_zscore_text)*100, 0)

      # weight
      capture.output(patient_df_w_wfa <- addWGSR(data = patient_df,
                                sex = "sex",
                                firstPart = "weight",
                                secondPart = "age",
                                index = "wfa")
                     )
      
      weight_zscore_text <- patient_df_w_wfa[[1, 5]]
      weight_percentile_text <- round(pnorm(weight_zscore_text)*100, 0)
      
      # BMI
      capture.output(patient_df_w_bmia <- addWGSR(data = patient_df,
                                                 sex = "sex",
                                                 firstPart = "weight",
                                                 secondPart = "height",
                                                 thirdPart = "age",
                                                 index = "bfa")
      )
      
      bmi <- round(input$weight / (((input$height)/100)^2), digits = 1)
      
      bmi_zscore_text <- patient_df_w_bmia[[1, 5]]
      bmi_percentile_text <- round(pnorm(bmi_zscore_text)*100, 0)
      
      if (age_in_days < 3685){
      return(glue("Growth (by WHO) - ",
                  paste0("height ",
                         input$height,
                         " cm (percentile = ",
                         height_percentile_text,
                         ", z-score = ",
                         height_zscore_text,
                         "), ",
                         "weight ",
                         input$weight,
                         " kg (percentile = ",
                         weight_percentile_text,
                         ", z-score = ",
                         weight_zscore_text,
                         "), BMI ",
                         bmi,
                         " kg/m^2 (percentile = ",
                         bmi_percentile_text,
                         ", z-score = ",
                         bmi_zscore_text,
                         ")")
                  )
             )
      } else if (age_in_days >= 3685){
        return(glue("Growth (by WHO) - ",
                    paste0("height ",
                           input$height,
                           " cm (percentile = ",
                           height_percentile_text,
                           ", z-score = ",
                           height_zscore_text,
                           "), ",
                           "weight ",
                           input$weight,
                           " kg (no WHO weight percentile/z-score for this age), BMI ",
                           bmi,
                           " kg/m^2 (percentile = ",
                           bmi_percentile_text,
                           ", z-score = ",
                           bmi_zscore_text,
                           ")")
                    )
               )
        }
    })
  
  output$full_text <- renderPrint(formData())
  
}

shinyApp(ui, server)