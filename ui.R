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
                   fileInput("file", label = h3("Upload Input csv file")),
                   checkboxGroupInput("stats",label="Mapped Statistics",choices=list("Unique Reads"='unique',"Multi-Mapping Reads"='multi',"Unmapped Reads"='unmapped'),selected = 'unique')
),

  dashboardBody(
#     tags$head(
#       tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
#     ),
    uiOutput("plotUI")
)
  )
