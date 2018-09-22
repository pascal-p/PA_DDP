#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

selList = list("Boosting C5.0" = "C5.0",
               "Stochastic Gradient Boosting" = "GBM",
               "Bagged CART" = "BCART",
               "Bagged RF" = "RF")

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("slate"),

  # Application title
  titlePanel("AToShiDe"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="dataSet", label="Select DataSet",
        list("ionosphere", "phishing")
      ),

      sliderInput(inputId="trainSetPerc", label="Percentage for Training set?",
                  min=0.5, max=1.0, value=0.7, step=0.1),

      checkboxGroupInput(
        inputId="algorithms", label="List of Algorithms [none]",
        choices=selList,
        selected=NULL
      ),

      radioButtons(
        inputId="cvType", label="Cross validation type [cv]",
        choices=list("Simple cv" = "cv", "Repeated cv" = "repeatedcv")
      ),

      radioButtons(
        inputId="metric", label="Metric for model selection [Accuracy]",
        choices=list("Accuracy" = "Accuracy", "Kappa" = "Kappa")
      ),

      conditionalPanel(
        condition = "input.algorithms != ''",
        radioButtons(
          inputId="withPlots", label="Plot statistics [no]",
          choices=list("yes", "no"),
          selected="no"
        )
      )

      # submitButton("Submit")
    ),

    mainPanel(
      tabsetPanel(
        id="theTabs",

        tabPanel("Main",
                 h3("Summary"),
                 htmlOutput("dataset"),

                 htmlOutput("resultTable"),
                 tableOutput("result"),

                 htmlOutput("legendtitle"),
                 tableOutput("legend"),

                 htmlOutput("selection"),
                 htmlOutput("bestSelection"),
                 value="main"),

        tabPanel("DataSet",
                 h3("Selected Dataset"),
                 dataTableOutput("datasetHead")),

        tabPanel("Plots",
                 h3("Main characteristics of the results"),
                 plotOutput("plot1"),
                 value="plots"),

        tabPanel("About",
                 htmlOutput("about"),
                 value="about")
      )
    )
  )

))
