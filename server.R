library(shinydashboard)
library(shinyIncubator)
library(shiny)
library(plotly)
library(shinyjs)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

shinyServer(function(input, output,session) {

  #Read and cleanupthe results file
  readfile = reactive({
    ipfile=input$file 
    file <- fread(ipfile$datapath) %>% mutate(library= gsub('star_hg19/','',library))  %>%
      separate(library,c('run','lane','barcode'),sep='_') %>% mutate(pool=paste(run,lane,sep='_')) %>%
      select(-Startedjobon:-MappingspeedMillionofreadsperhour)
    colnames(file)=c("run","lane","barcode","Numberofinputreads","Averageinputreadlength","Uniquelymappedreads_number","Uniquelymappedreads_percentage","Averagemappedlength","Numberofsplices_Total","Numberofsplices_Annotated_sjdb","Numberofsplices_GT_AG","Numberofsplices_GC_AG","Numberofsplices_AT_AC","Numberofsplices:Non-canonical","Mismatchrateperbase","Deletionrateperbase","Deletionaveragelength","Insertionrateperbase","Insertionaveragelength","Numberofreadsmappedtomultipleloci","percentageofreadsmappedtomultipleloci","Numberofreadsmappedtotoomanyloci","percentageofreadsmappedtotoomanyloci","percentageofreadsunmapped_toomanymismatches","percentageofreadsunmapped_tooshort","percentageofreadsunmapped_other","Numberofchimericreads","ofchimericreads","pool")
    return(file)
  })
  
  #Create table for uniquely mapped reads with link to FASTQC html files
  table_unique = reactive({
    validate(
      need(input$file, "Upload input file")
    )
    dt = readfile()
    run=dt$run
    lane=dt$lane
    barcode=dt$barcode
    link1_name=paste0(run,"_s_",lane,"_1_",barcode,"_fastqc.html")
    link2_name=paste0(run,"_s_",lane,"_2_",barcode,"_fastqc.html")
#     url1= paste0("file:///Users/bapoorva/Desktop/Shiny/MAGnet_RNAseq/fastQC/",link1_name)
#     url2= paste0("file:///Users/bapoorva/Desktop/Shiny/MAGnet_RNAseq/fastQC/",link2_name)
    dt$link1=paste0("<a href='",link1_name,"'target='_blank'>","Link to FASTQC1","</a>")
    dt$link2=paste0("<a href='",link2_name,"'target='_blank'>","Link to FASTQC2","</a>")
    dt=as.data.frame(dt)
    dt=data.frame(dt[,1:19],dt[,29:31])
    return(dt)
  })
  #Create table for multi-mapped reads with link to FASTQC html files
  table_multi = reactive({
    validate(
      need(input$file, "Upload input file")
    )
    dt = readfile()
    
    run=dt$run
    lane=dt$lane
    barcode=dt$barcode
    link1_name=paste0(run,"_s_",lane,"_1_",barcode,"_fastqc.html")
    link2_name=paste0(run,"_s_",lane,"_2_",barcode,"_fastqc.html")
    #     url1= paste0("file:///Users/bapoorva/Desktop/Shiny/MAGnet_RNAseq/fastQC/",link1_name)
    #     url2= paste0("file:///Users/bapoorva/Desktop/Shiny/MAGnet_RNAseq/fastQC/",link2_name)
    dt$link1=paste0("<a href='",link1_name,"'target='_blank'>","Link to FASTQC1","</a>")
    dt$link2=paste0("<a href='",link2_name,"'target='_blank'>","Link to FASTQC2","</a>")
    dt=as.data.frame(dt)
    dt=data.frame(dt[,1:4],dt[,20:23],dt[,29:31])
    return(dt)
  })
  #Create table for unmapped reads with link to FASTQC html files
  table_unmapped = reactive({
    validate(
      need(input$file, "Upload input file")
    )
    dt = readfile()
    run=dt$run
    lane=dt$lane
    barcode=dt$barcode
    link1_name=paste0(run,"_s_",lane,"_1_",barcode,"_fastqc.html")
    link2_name=paste0(run,"_s_",lane,"_2_",barcode,"_fastqc.html")
    #     url1= paste0("file:///Users/bapoorva/Desktop/Shiny/MAGnet_RNAseq/fastQC/",link1_name)
    #     url2= paste0("file:///Users/bapoorva/Desktop/Shiny/MAGnet_RNAseq/fastQC/",link2_name)
    dt$link1=paste0("<a href='",link1_name,"'target='_blank'>","Link to FASTQC1","</a>")
    dt$link2=paste0("<a href='",link2_name,"'target='_blank'>","Link to FASTQC2","</a>")
    dt=as.data.frame(dt)
    dt=data.frame(dt[,1:4],dt[,24:31])
    return(dt)
  })
  #~~~~~~~~~~~~~~~~~~~~
  output$table_unique = DT::renderDataTable({
            DT::datatable(table_unique(),
                          extensions = c('Buttons','Scroller'),
                          options = list(dom = 'Bfrtip',
                            searchHighlight = TRUE,
                            pageLength = 10,
                            lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                            scrollX = TRUE,
                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                          ),selection = list(mode = 'single', selected =1),caption= "Uniquely mapped Reads",escape=FALSE)
          })
          
          output$table_unmapped = DT::renderDataTable({
            DT::datatable(table_unmapped(),
                          extensions =  c('Buttons','Scroller'),
                          options = list(dom = 'Bfrtip',
                            searchHighlight = TRUE,
                            pageLength = 10,
                            lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                            scrollX = TRUE,
                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                          ),selection = list(mode = 'single', selected =1),caption= "Unmapped Reads",escape=FALSE)
          })
          
          output$table_multi = DT::renderDataTable({
            DT::datatable(table_multi(),
                          extensions =  c('Buttons','Scroller'),
                          options = list(dom = 'Bfrtip',
                            searchHighlight = TRUE,
                            pageLength = 10,
                            lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                            scrollX = TRUE,
                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                          ),selection = list(mode = 'single', selected =1),caption= "Multi-Mapped Reads",escape=FALSE)
          })
  #~~~~~~~~~~~~~~~~~~~~
  #Get column list and populate drop-down for each tab
  output$ui_unique = renderUI({
    cols1=c("Uniquelymappedreads_percentage","Uniquelymappedreads_number","Numberofsplices_Total","Numberofsplices_GT_AG","Numberofsplices_GC_AG","Numberofsplices_AT_AC")
    selectInput("attr1","Select an attribute for the boxplot",as.list(as.character(cols1)))
  })
  
  output$ui_multi = renderUI({
    cols2=c("Numberofreadsmappedtomultipleloci", "percentageofreadsmappedtomultipleloci","Numberofreadsmappedtotoomanyloci","percentageofreadsmappedtotoomanyloci")
    selectInput("attr2","Select an attribute for the boxplot",as.list(as.character(cols2)))
  })
  
  output$ui_unmapped = renderUI({
    cols3=c("percentageofreadsunmapped_toomanymismatches","percentageofreadsunmapped_tooshort","percentageofreadsunmapped_other")
    selectInput("attr3","Select an attribute for the boxplot",as.list(as.character(cols3)))
  })
  #~~~~~~~~~~~~~~~~~~~~
  #Generate box-plot (uniquely mapped)
  boxplot1_out = reactive({
    validate(
      need(input$file, "Upload input file")
    )
    d=readfile()
    attr1=input$attr1
    v=paste("d$",attr1,sep="")
    v=eval(parse(text=v))
    p <- plot_ly(d,x=pool,y=v,color=pool,xlab=attr1, type = "box") %>%
      layout(title = "BOX PLOT",xaxis = list(title ="pool"),yaxis = list(title = as.character(attr1)))
  })
  #Generate box-plot (multi-mapped)
  boxplot2_out = reactive({
    validate(
      need(input$file, "Upload input file")
    )
    d=readfile()
    attr2=input$attr2
    v=paste("d$",attr2,sep="")
    v=eval(parse(text=v))
    p <- plot_ly(d,x=pool,y=v,color=pool,xlab=attr2, type = "box") %>%
      layout(title = "BOX PLOT",xaxis = list(title ="pool"),yaxis = list(title = as.character(attr2)))
  })
  #Generate box-plot (unmapped)
  boxplot3_out = reactive({
    validate(
      need(input$file, "Upload input file")
    )
    d=readfile()
    attr3=input$attr3
    v=paste("d$",attr3,sep="")
    v=eval(parse(text=v))
    p <- plot_ly(d,x=pool,y=v,color=pool,xlab=attr3, type = "box") %>%
      layout(title = "BOX PLOT",xaxis = list(title ="pool"),yaxis = list(title = as.character(attr3)))
  })


#~~~~~~~~~~~~~~~~~~~~
  #User-Interface
  output$plotUI = renderUI({
    do.call(tabsetPanel,
            lapply(input$stats,function(s){ #for either upregulated/downregulated selected
              call("tabPanel",s,
                   call('dataTableOutput',paste0("table_",s)),
                   call('uiOutput',paste0("ui_",s)),
                   call("plotlyOutput",paste0("boxplot_",s))
              )
            })
    )
  })
  
  observe({
    lapply(input$stats, function(s){
      
      ## Add a DataTable 
      output[['table_unique']] <- DT::renderDataTable(table_unique(),options=list(iDisplayLength=10))
      output[['table_multi']] <- DT::renderDataTable(table_multi(),options=list(iDisplayLength=10))
      output[['table_unmapped']] <- DT::renderDataTable(table_unmapped(),options=list(iDisplayLength=10))
      
      
      output$table_unique = DT::renderDataTable({
        DT::datatable(table_unique(),
                      extensions = c('TableTools','ColVis','Scroller'),
                      options = list(
                        searchHighlight = TRUE,
                        pageLength = 10,
                        lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                        scrollX = TRUE
                      ),selection = list(mode = 'single', selected =1),caption= "Uniquely mapped Reads",escape=FALSE)
      })
      
      output$table_unmapped = DT::renderDataTable({
        DT::datatable(table_unmapped(),
                      extensions = c('TableTools','ColVis','Scroller'),
                      options = list(
                        searchHighlight = TRUE,
                        pageLength = 10,
                        lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                        scrollX = TRUE
                      ),selection = list(mode = 'single', selected =1),caption= "Unmapped Reads",escape=FALSE)
      })
      
      output$table_multi = DT::renderDataTable({
        DT::datatable(table_multi(),
                      extensions = c('TableTools','ColVis','Scroller'),
                      options = list(
                        searchHighlight = TRUE,
                        pageLength = 10,
                        lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                        scrollX = TRUE
                      ),selection = list(mode = 'single', selected =1),caption= "Multi-Mapped Reads",escape=FALSE)
      })
      
      ## Add the boxplots
      output[['boxplot_unique']] = renderPlotly({boxplot1_out()})
      output[['boxplot_multi']] = renderPlotly({boxplot2_out()})
      output[['boxplot_unmapped']] = renderPlotly({boxplot3_out()})

      return(s)
    })
  })
  
  
  
})
