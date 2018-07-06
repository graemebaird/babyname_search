library(shiny)
library(stringr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  "USA SSA name browser",
  # Application title
  
  
  tabPanel(
    "Name searcher",
    fluidRow(
      column(
        4,
        wellPanel(
          selectInput("gender", "Gender", c("any", "boy", "girl")),
          sliderInput("year", "Year sampled", 1880, 2016, value = c(1880, 2016)),
          sliderInput("length", "Number of letters", 0, 12, value = c(0, 12)),
          actionButton("reset_input", "Reset inputs")
        )
      ),
      column(4,
             wellPanel(
               sliderInput("vowels", "Number of vowels", 0, 6, value = c(0, 6)),
               sliderInput("consonants", "Number of consonants", 0, 8, value = c(0, 8)),
               sliderInput("rarity", "Rarity (yearly)", 0, .082, value = c(.00001, .1))
             )),
      column(4, wellPanel(
        textInput("name_cont", "Name contains this word (e.g., \"der\")"),
        textInput("name_end", "Name ends with (e.g., \"aura\")"),
        textInput("name_start", "Name starts with (e.g., \"Lau\")"),
        textInput("name_letters", "Name contains these letters (e.g., \"abcd\")")
      ))
    ),
    fluidRow(column(12, wellPanel(
      DT::dataTableOutput(outputId = "table_boy")
    )))
  ),
  tabPanel("Name plotter",
           fluidRow(
             column(
               3,
               wellPanel(
                 textInput("name1", "Names:"),
                 textInput("name2", ""),
                 textInput("name3", ""),
                 textInput("name4", ""),
                 textInput("name5", ""),
                 textInput("name6", ""),
                 checkboxInput("log_plot", "Log scale ", FALSE),
                 actionButton("reset_plot", "Reset")
               )
             ),
             column(6,
                    plotOutput(
                      "plot1", width = 500, height = 500
                    ))
           )),
  
  tabPanel(
    "Search and plot",
    fluidRow(
      column(4,
             wellPanel(
               selectInput(
                 "sort_plot",
                 "Sort by:",
                 choices = c("Top10", "Bottom10", "Random10")),
                 checkboxInput("log_splot", "Log scale ", FALSE),
                 br(),
                 actionButton("reset_input_plot", "Reset inputs")
               
             )),
      column(4,
             wellPanel(
               textInput("name_cont_plot", "Name contains this word (e.g., \"der\")"),
               textInput("name_end_plot", "Name ends with (e.g., \"aura\")")
             )),
      column(4,
             wellPanel(
               textInput("name_start_plot", "Name starts with (e.g., \"Lau\")"),
               textInput(
                 "name_letters_plot",
                 "Name contains these letters (e.g., \"abcd\")"
               )
             ))
    ),
    fluidRow(column(
      6,
      plotOutput("plot2", width = 800, height = 500)
    ))
  )
))