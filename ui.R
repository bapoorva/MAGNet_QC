library(shinydashboard)
#library(shinyIncubator)
library(shiny)
library(plotly)
library(d3heatmap)
library(shinyjs)

dashboardPage(
  dashboardHeader(title = "STAR summary",titleWidth = 300),
  dashboardSidebar(width = 300,
                   div(style="overflow-y: scroll"),
                   tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }" ))),
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))),
                   uiOutput("projects"),
                   sidebarMenu(
                     menuItemOutput("menuitem_loaddata"),
                     menuItem("PhenoData", tabName = "pheno", icon = icon("hand-o-right")),
                     menuItem("FastQC Report", tabName = "fastqc", icon = icon("hand-o-right")),
                     menuItem('PCA-Plot', tabName = 'pcaplot', icon = icon('hand-o-right')), 
                     menuItem("Bargraph-Samples", tabName = "bargraph", icon = icon("bar-chart")),
                     menuItem("Library Complexity Summary", tabName = "libcomp", icon = icon("hand-o-right")),
                     menuItem("Metrics", tabName = "Metrics", icon = icon("hand-o-right")),
                     menuItem("Mark Duplicates", tabName = "markdup", icon = icon("hand-o-right"))
                   )#end of sidebar menu

),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    useShinyjs(),
     tabItems(
       tabItem(tabName = "dashboard",
               fluidRow(
               box(
                 width = 8, status = "primary",solidHeader = TRUE,
                 title = "Mapped Summary",
                 uiOutput("plotUI")
               ),
               box(width = 4, status = "primary",solidHeader = TRUE,title = "Controls",
                   selectInput("mapsumoptions","Select Group", c('Tissue_Source'='Tissue_Source','CHF_Etiology'='CHF_Etiology','Gender'='Gender','Race'='Race','Library_Pool'='Library_Pool')),
                   uiOutput("mapsum")
       ))),
       tabItem(tabName = "pheno",DT::dataTableOutput('anno')),
       tabItem(tabName = "fastqc",htmlOutput("fastqc",height=800,width=1100)),
       tabItem(tabName = "pcaplot",
               fluidRow(
                 box(
                   width = 8, status = "primary",solidHeader = TRUE,
                   title = "PCA Plot",
                   fluidRow(
                     column(6,plotOutput("biplot",width=750,height=600))
                   )
                 ),br(),
               box(
                 width = 4, status = "primary",solidHeader = TRUE,
                 title = "Select Options",
                 fluidRow(
                   column(6,uiOutput("pcaxoptions")),
                   column(6,uiOutput("pcayoptions"))
                 ),
                 br(),textOutput("biplottitle"),br(),
                 fluidRow(
                   column(6,uiOutput("pcipslide")),
                   column(6,uiOutput("pcslide"))
                 ),
                 fluidRow(
                   column(6,uiOutput("maineffect")),
                   column(6,hr())
                 ),
                 br()),
               
               fluidRow(
                 column(6,uiOutput("dwldbiplot")))
               
       )),
       
       tabItem(tabName = "bargraph",uiOutput("indoptions"),uiOutput("allsamp"),plotlyOutput("barplotsind_out",width = 1100, height = 800)),
  tabItem(tabName = "libcomp",
          fluidRow(
            box(
              width = 8, status = "primary",solidHeader = TRUE,
              title = "Mapped Summary",
              DT::dataTableOutput('libcomplex'),hr(),
              plotlyOutput("libc_bplot",width = 1200, height = 500)
            ),
            box(width = 4, status = "primary",solidHeader = TRUE,title = "Controls",
                fluidRow(
                  column(6,uiOutput("xoptions")),
                  column(6,uiOutput("yoptions"))),
                fluidRow(
                  column(6,uiOutput("xop2")),
                  column(6,hr()))
            ))),
  tabItem(tabName = "Metrics",
          fluidRow(
            box(
              width = 8, status = "primary",solidHeader = TRUE,
              title = "Mapped Summary",
              DT::dataTableOutput('metrics'),hr(),
              plotlyOutput("metr_bplot",width = 1200, height = 500)
            ),
            box(width = 4, status = "primary",solidHeader = TRUE,title = "Controls",
                fluidRow(
                  column(6,uiOutput("mxoptions")),
                  column(6,uiOutput("myoptions")))
            ))),
  tabItem(tabName = "markdup",
          fluidRow(
            box(
              width = 8, status = "primary",solidHeader = TRUE,
              title = "Mapped Summary",
              DT::dataTableOutput('mrkdup'),hr(),
              plotlyOutput("mrkdup_bplot",width = 1200, height = 500)
            ),
            box(width = 4, status = "primary",solidHeader = TRUE,title = "Controls",
                fluidRow(
                  column(6,uiOutput("dxoptions")),
                  column(6,uiOutput("dyoptions")))
            )))
  
  )))
