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
                     menuItem("Full Unmerged PhenoData", tabName = "pheno2", icon = icon("hand-o-right")),
                     menuItem("FastQC Report", tabName = "fastqc", icon = icon("hand-o-right")),
                     menuItem('PCA-Plot', tabName = 'pcaplot', icon = icon('hand-o-right')), 
                     menuItem("Bargraph-Samples", tabName = "bargraph", icon = icon("bar-chart")),
                     menuItem("Library Complexity Summary", tabName = "libcomp", icon = icon("hand-o-right")),
                     menuItem("Metrics", tabName = "Metrics", icon = icon("hand-o-right")),
                     menuItem("Mark Duplicates", tabName = "markdup", icon = icon("hand-o-right")),
                     menuItem("TIN", tabName = "tin", icon = icon("hand-o-right")),
                     menuItem('Differential gene Expression', icon = icon('hand-o-right'),
                              menuSubItem(icon=NULL,tabName = 'dge',uiOutput("limmalist")))
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
       tabItem(tabName = "pheno2",DT::dataTableOutput('anno_full')),
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
              plotlyOutput("libc_bplot",width = 1000, height = 500)
            ),
            box(width = 4, status = "primary",solidHeader = TRUE,title = "Controls",
                fluidRow(
                  column(6,uiOutput("xoptions")),
                  column(6,uiOutput("yoptions"))),
                uiOutput("xopsub")
            ))
          ),
  tabItem(tabName = "Metrics",
          fluidRow(
            box(width = 8, status = "primary",solidHeader = TRUE,
              title = "Mapped Summary",
              DT::dataTableOutput('metrics'),hr(),
              plotlyOutput("metr_bplot",width = 1000, height = 500)
            ),
            box(width = 4, status = "primary",solidHeader = TRUE,title = "Controls",
                fluidRow(
                  column(6,uiOutput("mxoptions")),
                  column(6,uiOutput("myoptions"))),
                uiOutput("mxopsub")
            ))),
  tabItem(tabName = "markdup",
          fluidRow(
            box(
              width = 8, status = "primary",solidHeader = TRUE,
              title = "Mapped Summary",
              DT::dataTableOutput('mrkdup'),hr(),
              plotlyOutput("mrkdup_bplot",width = 1000, height = 500)
            ),
            box(width = 4, status = "primary",solidHeader = TRUE,title = "Controls",
                fluidRow(
                  column(6,uiOutput("dxoptions")),
                  column(6,uiOutput("dyoptions"))),
                uiOutput("dxopsub")
            ))),
  tabItem(tabName = "tin",
          fluidRow(
            box(
              width = 8, status = "primary",solidHeader = TRUE,
              title = "Tin results",
              DT::dataTableOutput('tin')
            ))),
  tabItem(tabName = "dge",
          fluidRow(
            box(width = 8, status = "primary",solidHeader = TRUE,title = "Dot Plot of the gene of interest",
                fluidRow(
                  column(6,uiOutput("boxplotcol"))
                ),
                fluidRow(
                  column(6,plotlyOutput('dotplot',width = 800))
                )),
            box(width = 4, status = "primary",solidHeader = TRUE,title = "Gene Selection",
                radioButtons("radio", label = h4("Gene Selection"),
                             choices = c("None" = 'none',"Upregulated" = 'up', "Downregulated" = 'down', "Both" = 'both'),
                             selected = 'none'),
                
                sliderInput("lfc", label = h4("Fold Change"), min = 0.5,max = 6, value = 2),
                sliderInput("apval", label = h4("P. Value"), min = 0.01,max = 0.2, value =0.05),br(),
                fluidRow(
                  column(6,downloadButton('dwld','Download results table')),
                  column(6,downloadButton('downloaddotplot', 'Download Dot plot')))
            )),
          
          box(width = 12, status = "primary",solidHeader = TRUE,title = "Limma data",
              br(),textOutput("contrdesc"),br(),DT::dataTableOutput('limma')))
  )))
