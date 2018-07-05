library(shiny)
library(visNetwork)
library(dplyr)
library(igraph)
library(DT)
library(shinythemes)
header <- headerPanel("KarthikReddyParla_Social_Network_Analysis")
header[[2]]$attribs$id = "header"
ui <-(fluidPage(theme = shinytheme("cerulean"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  header,
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1", label = "upload email-Eu-core file",accept =  'text/plain'),
      fileInput(inputId = "file2", label = "upload email-Eu-core-department-labels file",accept =  'text/plain'),
   numericInput("count","Enter the number of connection to be displayed",18,max=30)
  
    )
  ,
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel("networkPlot",visNetworkOutput("plottest")),
                  tabPanel("# Mails Sent",   DT::dataTableOutput('Senddata') ),
                  tabPanel("# Mails Recived",     DT::dataTableOutput('ReciveData')),
                  tabPanel("2-hop neighbors",  fluidRow(verbatimTextOutput("Summary1"),br(),uiOutput('Group'),br(),uiOutput('columns'),br(),uiOutput('ViewChange'),br(),visNetworkOutput('twohopPlot1'))),
                  tabPanel("TOP 10 centrality",   fluidRow( DT::dataTableOutput('dept1'),br(),DT::dataTableOutput('centrality1')) ),
                  tabPanel("Across Department",    DT::dataTableOutput('dept') )
                   )

    )
  )
)
)