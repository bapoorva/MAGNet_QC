library(shinydashboard)
library(shinyIncubator)
library(shiny)
library(plotly)
library(shinyjs)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(DT)

shinyServer(function(input, output,session) {
  
  #Read the parameter file
  readexcel = reactive({
    file = read.csv("data/param.csv")
  })
  
  #Get Project list and populate drop-down
  output$projects = renderUI({
    excel=readexcel()
    prj=excel$projects
    selectInput("projects","Select a project",as.list(as.character(prj)))
  })
  
  
  #Read and cleanupthe results file
  readfile = reactive({
    inFile = paste('data/',as.character(input$projects),'/STAR_summary.csv',sep = '')
    file <- fread(inFile) %>% mutate(library= gsub('.*STAR/','',library))  %>%
      separate(library,c('id','run','lane','barcode'),sep='_') %>% mutate(pool=paste(run,lane,sep='_')) %>%
      select(-Startedjobon:-MappingspeedMillionofreadsperhour)
    #rownames(file)=file$id
    #file=as.data.frame(file[,-1])
    colnames(file)=c("id","run","lane","barcode","Numberofinputreads","Averageinputreadlength","Uniquelymappedreads_number","Uniquelymappedreads_percentage","Averagemappedlength","Numberofsplices_Total","Numberofsplices_Annotated_sjdb","Numberofsplices_GT_AG","Numberofsplices_GC_AG","Numberofsplices_AT_AC","Numberofsplices:Non-canonical","Mismatchrateperbase","Deletionrateperbase","Deletionaveragelength","Insertionrateperbase","Insertionaveragelength","Numberofreadsmappedtomultipleloci","percentageofreadsmappedtomultipleloci","Numberofreadsmappedtotoomanyloci","percentageofreadsmappedtotoomanyloci","percentageofreadsunmapped_toomanymismatches","percentageofreadsunmapped_tooshort","percentageofreadsunmapped_other","Numberofchimericreads","ofchimericreads","pool")
    return(file)
  })
  
  barplot = reactive({
    inFile = paste('data/',as.character(input$projects),'/STAR_summary.csv',sep = '')
    d<- fread(inFile) %>% mutate(library= gsub('.*STAR/','',library))  %>%
      separate(library,c('id','run','lane','barcode'),sep='_') %>% mutate(pool=paste(run,lane,sep='_')) %>%
      select(-Startedjobon:-MappingspeedMillionofreadsperhour)
    d=d[d$run==input$run,]
    d=d[d$lane==input$lane,]
    #d$lib=paste0(d$run,"_",d$lane)
    #d$library=d$lib
    gg=d %>% select(id,`Uniquelymappedreads`,`ofreadsmappedtomultipleloci`,`ofreadsmappedtotoomanyloci`,`ofreadsunmapped:toomanymismatches`,`ofreadsunmapped:tooshort`,`ofreadsunmapped:other`) %>%
      gather("maptype","perc",-id) %>% ggplot(.,aes(x=id,y=perc,fill=maptype))+geom_bar(stat="identity", colour="white")+  scale_fill_brewer(palette="Dark2")+
      theme_bw()+theme(
        plot.margin=unit(x=c(0,0,0,0),units="mm"),
        legend.position="top",
        axis.text.x  = element_text(angle=40, vjust=0.5, size=9)
      ) + ylab('% Reads') 
    (gg <- ggplotly(gg))
    #return(gg)
  })
  
  output$barplot_out = renderPlotly({
    barplot()
  })
  
  output$baroptions <- renderUI({
    inFile = paste('data/',as.character(input$projects),'/STAR_summary.csv',sep = '')
    d<- fread(inFile) %>% mutate(library= gsub('.*STAR/','',library))  %>%
      separate(library,c('id','run','lane','barcode'),sep='_') %>% mutate(pool=paste(run,lane,sep='_')) %>%
      select(-Startedjobon:-MappingspeedMillionofreadsperhour)
    run=unique(d$run)
    selectInput("run", "Select run",as.list(as.character(run)))
  })
  
  output$laneoptions <- renderUI({
    inFile = paste('data/',as.character(input$projects),'/STAR_summary.csv',sep = '')
    d<- fread(inFile) %>% mutate(library= gsub('.*STAR/','',library))  %>%
      separate(library,c('id','run','lane','barcode'),sep='_') %>% mutate(pool=paste(run,lane,sep='_')) %>%
      select(-Startedjobon:-MappingspeedMillionofreadsperhour)
    d=d[d$run==input$run,]
    lane=sort(as.numeric(unique(d$lane)))
    selectInput("lane", "Select lane",as.list(as.character(lane)))
  })
  
  observe({
    if(input$barplotop>0){
      updateTabsetPanel(session = session, inputId = 'tabvalue', selected = 'tab1')}
    #toggle(condition =input$barplot,selector = "#tabvalue li a[data-value=tab1]")
  })
  
  
  #Create table for uniquely mapped reads with link to FASTQC html files
  table_unique = reactive({
    dt = readfile()
    run=dt$run
    lane=dt$lane
    barcode=dt$barcode
    link1_name=paste0(run,"_s_",lane,"_1_",barcode,"_fastqc.html")
    link2_name=paste0(run,"_s_",lane,"_2_",barcode,"_fastqc.html")
    dt$link1=paste0("<a href='",link1_name,"'target='_blank'>","Link to FASTQC1","</a>")
    dt$link2=paste0("<a href='",link2_name,"'target='_blank'>","Link to FASTQC2","</a>")
    dt=as.data.frame(dt)
    dt=data.frame(dt[,1:20],dt[,30:32])
    return(dt)
  })
  #Create table for multi-mapped reads with link to FASTQC html files
  table_multi = reactive({
    dt = readfile()
    run=dt$run
    lane=dt$lane
    barcode=dt$barcode
    link1_name=paste0("/fujfs/d3/MAGnet_RNAseq_v2/fastQC/",run,"_s_",lane,"_1_",barcode,"_fastqc.html")
    link2_name=paste0("/fujfs/d3/MAGnet_RNAseq_v2/fastQC/",run,"_s_",lane,"_2_",barcode,"_fastqc.html")
    dt$link1=paste0("<a href='",link1_name,"'target='_blank'>","Link to FASTQC1","</a>")
    dt$link2=paste0("<a href='",link2_name,"'target='_blank'>","Link to FASTQC2","</a>")
    dt=as.data.frame(dt)
    dt=data.frame(dt[,1:5],dt[,21:24],dt[,30:32])
    return(dt)
  })
  #Create table for unmapped reads with link to FASTQC html files
  table_unmapped = reactive({
    dt = readfile()
    run=dt$run
    lane=dt$lane
    barcode=dt$barcode
    link1_name=paste0("/fujfs/d3/MAGnet_RNAseq_v2/fastQC/",run,"_s_",lane,"_1_",barcode,"_fastqc.html")
    link2_name=paste0("/fujfs/d3/MAGnet_RNAseq_v2/fastQC/",run,"_s_",lane,"_2_",barcode,"_fastqc.html")
    dt$link1=paste0("<a href='",link1_name,"'target='_blank'>","Link to FASTQC1","</a>")
    dt$link2=paste0("<a href='",link2_name,"'target='_blank'>","Link to FASTQC2","</a>")
    dt=as.data.frame(dt)
    dt=data.frame(dt[,1:5],dt[,25:32])
    return(dt)
  })
  #~~~~~~~~~~~~~~~~~~~~
  output$table_unique = DT::renderDataTable({
    DT::datatable(table_unique(),
                  extensions =  c('Buttons','Scroller'),
                  options = list(dom = 'Bfrtip',
                                 searchHighlight = TRUE,
                                 pageLength = 10,
                                 lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                                 scrollX = TRUE,
                                 buttons = c('copy', 'print')
                  ),selection = list(mode = 'single', selected =1),caption= "Unique Reads",escape=FALSE)
  })
          
          output$table_unmapped = DT::renderDataTable({
            DT::datatable(table_unmapped(),
                          extensions =  c('Buttons','Scroller'),
                          options = list(dom = 'Bfrtip',
                            searchHighlight = TRUE,
                            pageLength = 10,
                            lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                            scrollX = TRUE,
                            buttons = c('copy','print')
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
                            buttons = c('copy', 'print')
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
    d=readfile()
    attr1=input$attr1
    v=paste("d$",attr1,sep="")
    v=eval(parse(text=v))
    p <- plot_ly(d,x=pool,y=v,color=pool,xlab=attr1, type = "box") %>%
      layout(title = "BOX PLOT",xaxis = list(title ="pool"),yaxis = list(title = as.character(attr1)))
  })
  #Generate box-plot (multi-mapped)
  boxplot2_out = reactive({
    d=readfile()
    attr2=input$attr2
    v=paste("d$",attr2,sep="")
    v=eval(parse(text=v))
    p <- plot_ly(d,x=pool,y=v,color=pool,xlab=attr2, type = "box") %>%
      layout(title = "BOX PLOT",xaxis = list(title ="pool"),yaxis = list(title = as.character(attr2)))
  })
  #Generate box-plot (unmapped)
  boxplot3_out = reactive({
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
                   call("downloadButton",paste0("save_",s),'Save as csv'),
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
                      extensions = c('Buttons','Scroller'),
                      options = list(dom = 'Bfrtip',
                                     searchHighlight = TRUE,
                                     pageLength = 10,
                                     lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                                     scrollX = TRUE,
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ),rownames=TRUE,selection = list(mode = 'single', selected =1),escape=FALSE,caption= "Unique Reads")
      })
      
      output$table_unmapped = DT::renderDataTable({
        DT::datatable(table_unmapped(),
                      extensions = c('Buttons','Scroller'),
                      options = list(dom = 'Bfrtip',
                                     searchHighlight = TRUE,
                                     pageLength = 10,
                                     lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                                     scrollX = TRUE,
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ),rownames=TRUE,selection = list(mode = 'single', selected =1),escape=FALSE,caption= "Unmapped Reads")
      })
      
      output$table_multi = DT::renderDataTable({
        DT::datatable(table_multi(),
                      extensions = c('Buttons','Scroller'),
                      options = list(dom = 'Bfrtip',
                                     searchHighlight = TRUE,
                                     pageLength = 10,
                                     lengthMenu = list(c(30, 50, 100, 150, 200, -1), c('30', '50', '100', '150', '200', 'All')),
                                     scrollX = TRUE,
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ),selection = list(mode = 'single', selected =1),caption= "Multi-Mapped Reads",escape=FALSE)
      })
      
      ## Add the boxplots
      output[['boxplot_unique']] = renderPlotly({boxplot1_out()})
      output[['boxplot_multi']] = renderPlotly({boxplot2_out()})
      output[['boxplot_unmapped']] = renderPlotly({boxplot3_out()})
      
      
      output[['save_unique']] <- downloadHandler(
        filename = function() { paste0("unique",".csv") },
        content = function(file) { write.csv(table_unique(),file=file,row.names=FALSE) }
      )
      output[['save_unmapped']] <- downloadHandler(
        filename = function() { paste0("unmapped",".csv") },
        content = function(file) { write.csv(table_unmapped(),file=file,row.names=FALSE) }
      )
      output[['save_multi']] <- downloadHandler(
        filename = function() { paste0("multi",".csv") },
        content = function(file) { write.csv(table_multi(),file=file,row.names=FALSE) }
      )
      
      return(s)
    })
  })
  
  
  
})
