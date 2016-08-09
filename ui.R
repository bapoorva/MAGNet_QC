library(shinydashboard)
library(shinyIncubator)
library(shiny)
library(plotly)
library(d3heatmap)
library(shinyjs)

dashboardPage(
  dashboardHeader(title = "MAGnet RNA-Seq",titleWidth = 500),
  dashboardSidebar(width = 500,
                   div(style="overflow-y: scroll"),
                   tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }" ))),
                   uiOutput("projects"),
                   checkboxGroupInput("stats",label="Mapped Statistics",choices=list("Unique Reads"='unique',"Multi-Mapping Reads"='multi',"Unmapped Reads"='unmapped'),selected = 'unique'),
                   hr(),
                   actionButton(inputId = 'barplotop', label = 'Click to view Bar Graph')
),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    useShinyjs(),
    #uiOutput("plotUI"),
    tabsetPanel(type="tabs", id = "tabvalue",
                tabPanel(title = "Mapped stats", value = 'tab',uiOutput("plotUI")),
                tabPanel(title = "Bar Graph", value = 'tab1',
                         fluidRow(
                           column(6,uiOutput("baroptions")),
                          column(6,uiOutput("laneoptions"))),
                         plotlyOutput("barplot_out",width = 1800, height = 900))
    )
)
  )
