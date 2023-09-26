options(shiny.maxRequestSize=3000*1024^2)
library(shiny)
library(tidyverse)
library(fields)
library(ncdf4)
library(dipsaus)
library(data.table)
library(DT)
library(plotly)
library(shinyWidgets)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("journal"),
                titlePanel("Open and Write Data from NetCDF Files"),
                sidebarLayout(
                  sidebarPanel(width = 3,
                               fancyFileInput('file', "Please upload"),
                               uiOutput("UI_variables"),
                               uiOutput("UI_date"),
                               conditionalPanel(condition = "input$file != NULL", uiOutput("UI_depth")),
                               radioButtons("data_choice", "Choose",
                                            choices = c("All", "current Date"), inline = T),
                               downloadButton("downloadData", "Download"),
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Plot",
                               fluidRow(
                                 column(6,
                                        switchInput("flip", "Flip")),
                                 column(6,
                                        radioButtons("plot_switch", " ",
                                                     choices = c("static"), selected = "static", inline = T))),
                               uiOutput("plot")),
                      tabPanel("Data", dataTableOutput("table"))
                    )
                  )
                )
)

server <- function(input, output,session) {
  session$onSessionEnded({
    stopApp
  })
  
  nc <- reactive({
    file <- input$file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "nc" | ext == "nc4", "Please upload a NetCDF file"))
    
    
    shiny::withProgress(
      message = 'Shiny is busy...',
      detail = 'Please wait...',
      value = 0,
      expr = {nc <- ncdf4::nc_open(file$datapath)}
    )
    
    
    return(nc)
  })
  
  
  output$downloadData <- shiny::withProgress(
    message = 'Downloading Data...',
    detail = 'Please wait...',
    value = 0,
    expr = {
      downloadHandler(
        filename = function() {
          # Generate a unique filename based on user input
          if (input$data_choice == "All") {
            filename <- paste("All_data_", Sys.time(), ".csv", sep = "")
          } else {
            filename <- paste("data_", Sys.Date(), "_", input$nc_variables, ".csv", sep = "")
          }
          return(filename)
        },
        content = function(file) {
          tryCatch({
            # Generate and save the data
            if (input$data_choice == "All") {
              write.csv(Data_All(), file, row.names = F)
            } else {
              write.csv(Data(), file)
            }
          }, error = function(e) {
            # Handle errors (e.g., data generation or writing failed)
            shiny::showNotification("Error downloading data.", type = "error")
          })
        }
      )
    }
  )
  
  
  output$plot <- renderUI({
    switch (input$plot_switch,
            "static" = renderPlot(plot_fields(), width = 1000, height = 700),
            "interactive" = renderPlotly(plot_ggplot())
    )
  })
  
  output$UI_variables <-  renderUI({
    selectInput("nc_variables", "Variables",
                choices =names(nc()$var))
  })
  
  Time <- reactive({
    Time <- ncvar_get(nc(), "time")
    startDate <- ncatt_get(nc(), "time", "units")$value
    if(str_detect(startDate, "seconds")){Units <- "seconds"; Modyfier <- 1}
    if(str_detect(startDate, "hours")){Units <- "hours"; Modyfier <- 3600}
    if(str_detect(startDate, "days")){Units <- "days"; Modyfier <- 60*60*24}
    if(str_detect(startDate, "months")){Units <- "months"; Modyfier <- 720}
    
    startDate <- str_remove_all(startDate, Units)
    startDate <- str_remove_all(startDate, "since")
    if(all(Time == 0)){Time<-c(1:length(Time))}
    Time <- as.POSIXct(Time*Modyfier, cal = "gregorian", origin = startDate)
    Time <- as.Date(Time)
    
    return(Time)
  })
  
  output$UI_date <- renderUI({
    if(is.null(nc())){return(NULL)}
    sliderTextInput("date",
                    "Dates:",
                    choices = unique(Time()),
                    selected =  unique(Time())[1],
                    animate = TRUE)
    
  })
  
  output$UI_depth <- renderUI({
    if(!is.null(nc()$dim$depth) & length(nc()$dim$depth$vals) > 1 ){
      unit <- ncatt_get(nc(), "depth", "units")$value
      vals <- round(ncvar_get(nc(), "depth"),2)
      sliderTextInput("depth", paste("Depth", unit),
                      choices = vals, selected = vals[1], grid = TRUE)
    }else{return(NULL)}
    
  })
  
  
  Depth <- reactive({
    depth <- round(ncvar_get(nc(), "depth"),2)
    val <- which(input$depth == depth)
    observe(print(val))
    val
  })
  
  Date <- reactive({
    which(input$date == Time())
  })
  
  Variable_NC <- reactive({
    var <- ncvar_get(nc(), as.character(input$nc_variables))
    if(length(dim(var)) == 3){var <- var[,,Date()]}
    if(length(dim(var)) == 4){var <- var[,,Depth(),Date()]}
    return(var)
  })
  
  
  Latitude <- reactive({
    try(Lat <- ncvar_get(nc(), "latitude"))
    try(Lat <- ncvar_get(nc(), "lat"))
    try(LON <- nc()$dim$lat$vals)
    return(sort(Lat))
  })
  
  Longitude <- reactive({
    if(is.null(nc())){NULL}
    try(Lon <- ncvar_get(nc(), "longitude"))
    try(Lon <- ncvar_get(nc(), "long"))
    try(Lon <- ncvar_get(nc(), "lng"))
    try(Lon <- ncvar_get(nc(), "lon"))
    try(LON <- nc()$dim$lon$vals)
    return(sort(Lon))
  })
  
  Data_All <- reactive({
    
    shiny::withProgress(
      message = 'Shiny is busy...',
      detail = 'Please wait...',
      value = 0,
      expr = {
        Data <- expand.grid("Lon" = Longitude(), "Lat" = Latitude(), "Time" = Time())
        Data <- cbind(Data, "Var" = as.vector(Variable_NC()))
      }
    )
  })
  
  
  Data <- shiny::withProgress(
    message = 'Shiny is busy...',
    detail = 'Please wait...',
    value = 0,
    expr = {
      reactive({
     Data <- expand.grid("Lon" = Longitude(), "Lat" = Latitude(), "Time" = Time()[Date()])
      Data <- cbind(Data, "Var" = as.vector(Variable_NC()))
      })
  })
  
  Unit <- reactive({
    ncatt_get(nc(), input$nc_variables, "units")[2]
  })
  
  plot_ggplot <- reactive({
    ggplot(Data(), aes(y=Lat, x=Lon))+
      geom_point(aes(color = Var))+
      coord_quickmap()+
      theme_bw()+
      scale_color_viridis_c()
  })
  
  plot_fields <- reactive({
    shiny::withProgress(
      message = 'Shiny is busy...',
      detail = 'Please wait...',
      value = 0,
      expr = {  var <- Variable_NC()
      if(length(Variable_NC())==0){return(NULL)}
      if(input$flip){var <- var[,ncol(var):1]}
      fields::image.plot(Longitude(),
                         Latitude(), 
                         var,
                         main = paste(input$nc_variables),
                         #sub = paste("Mean temp:", list2[i]),
                         xlab = "Longitude",
                         ylab = "Latitude",
                         legend.lab = Unit(),
                         legend.line = 2.5
      )}
    )
    
  })
  
  output$table <- shiny::withProgress(
    message = 'Shiny is busy...',
    detail = 'Please wait...',
    value = 0,
    expr = {renderDataTable(
    Data(),rownames= FALSE,filter = 'top',
      extensions = c('Buttons',"Scroller","FixedColumns"), options = list(
        dom = 'BRfrltpi',
        buttons =
          list('copy', 'print', list(
            extend = 'collection',
            buttons = list(
              list(extend = 'csv', filename = "NC File", 
                   exportOptions = list(modifier = list(page = "all"))),
              list(extend = 'excel', filename = "NC File"),
              list(extend = 'pdf', filename = "NC File")),
            text = 'Download'
          )),
        columnDefs = list(list(className = 'dt-center', targets="_all")),
        autoWidth=TRUE,
        rownames= FALSE,
        pageLength = 1000,
        deferRender = TRUE,
        scrollY = 600,
        scroller = TRUE
      )
    )}
   )
}

# Run the application 
shinyApp(ui = ui, server = server)
