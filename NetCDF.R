#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize=3000*1024^2)
library(shiny)
library(tidyverse)
library(fields)
library(ncdf4)
library(dipsaus)
#library(PCICt)
#library(RNetCDF)
#library(dplyr)
library(data.table)
library(DT)
library(plotly)
#library(sp)
library(shinyWidgets)
library(shinythemes)
#library(shinyFiles)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("journal"),

    # Application title
    titlePanel("Open and Write Data from NetCDF Files"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
          #shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE),
          #actionButton("btn_file", "Choose a NetCDF file"),
          fancyFileInput('file', "Please upload"),
          uiOutput("UI_variables"),
          uiOutput("UI_date"),
          uiOutput("UI_depth"),
          radioButtons("data_choice", "Choose",
                       choices = c("All", "current Date"), inline = T),
          downloadButton("downloadData", "Download"),
          #noUiSliderInput("lat", "Latitude", max = 90, min = -90, value = c(0,0),
          #            orientation = "vertical",width = "80px", height = "100px"),
          #noUiSliderInput("lon", "Longitude", max = 180, min = -180, value = c(0,0)),
          #plotlyOutput("scatterPlot"),
          #verbatimTextOutput("selectedValues")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
           #tabPanel("Plot", plotOutput("plot", width = 700, height = 900)),
           tabPanel("Plot",
                    radioButtons("plot_switch", " ",
                                 choices = c("static"), selected = "static", inline = T),
                    uiOutput("plot")),
           tabPanel("Data", dataTableOutput("table"))
          )
        )
    ),tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               bottom: 0px;
               right: 0px;
               width: 20%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage"))
    

)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  session$onSessionEnded({
    #print("Stop!")
    stopApp
  })
  

  nc <- reactive({
    file <- input$file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "nc", "Please upload a NetCDF file"))
    
    nc <- ncdf4::nc_open(file$datapath)
  })

  
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$data_choice == "All"){paste("All data-", input$nc_variables, ".csv", sep="")}else{paste("data-", Time()[Date()], input$nc_variables, ".csv", sep="")}
    },
    content = function(file) {
      if(input$data_choice == "All"){write.csv(Data_All(), file)}else{write.csv(Data(), file)}
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
    
    #startDate <- "1950-01-01 00:00:0.0"
    Time <- as.POSIXct(Time*Modyfier, cal = "gregorian", origin = startDate)
    Time <- as.Date(Time)
    #observe(print(Time))
  })
  
  output$UI_date <- renderUI({
    if(!is.null(file())){
      Time <- ncvar_get(nc(), "time")
      startDate <- ncatt_get(nc(), "time", "units")$value
      if(str_detect(startDate, "seconds")){Units <- "day"}
      if(str_detect(startDate, "hours")){Units <- "day"}
      if(str_detect(startDate, "days")){Units <- "month"}
      #if(str_detect(startDate, "months")){Units <- "month"}
      
      selection <- format(seq.Date(from = as.Date(Time()[1]), 
                                   to = as.Date(Time()[length(Time())]), by = Units))
      
      #dateInput("date", "Date", min = as.Date(Time()[1]), max = as.Date(Time()[length(Time())]))
      sliderTextInput("date",
                  "Dates:",
                  choices = selection,
                  #min = as.Date(selection[1]),
                  #max = as.Date(selection[length(selection)]),
                  selected = as.Date(selection[1]),
                  #timeFormat="%Y-%m-%d",
                  animate = TRUE)

    }
  })
  
  output$UI_depth <- renderUI({
    if(!is.null(nc()$dim$depth) & length(nc()$dim$depth$vals) > 1 ){
      #depth <- ncvar_get(nc(), "depth");
      #unit <- nc()$dim$depth$units
      unit <- ncatt_get(nc(), "depth", "units")$value
      #vals <- round(nc()$dim$depth$vals, 2)
      vals <- round(ncvar_get(nc(), "depth"),2)
      #sliderInput("depth","Depth",
      #            min = 1, max = length(depth),
      #            value = 1, step = 1, labels)
      sliderTextInput("depth", paste("Depth", unit),
                      choices = vals, selected = vals[1], grid = TRUE)
    }
    
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
    return(Lat)
  })
  
  Longitude <- reactive({
    if(is.null(nc())){NULL}
    try(Lon <- ncvar_get(nc(), "longitude"))
    try(Lon <- ncvar_get(nc(), "long"))
    try(Lon <- ncvar_get(nc(), "lng"))
    try(Lon <- ncvar_get(nc(), "lon"))
    return(Lon)
  })
  
  Data_All <- reactive({
    Data <- expand.grid("Lon" = Longitude(), "Lat" = Latitude(), "Time" = Time())
    #Data <- cbind(Data, "Var" = as.vector(ncvar_get(nc(), as.character(input$nc_variables))))
    Data <- cbind(Data, "Var" = as.vector(Variable_NC()))
  })
  
  
  Data <- reactive({
    Data <- expand.grid("Lon" = Longitude(), "Lat" = Latitude(), "Time" = Time()[Date()])
    Data <- cbind(Data, "Var" = as.vector(Variable_NC()))
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
    #if(is.null(Longitude()) | is.null(Latitude()) | is.null(Variable_NC())){return(NULL)}else{
    fields::image.plot(Longitude(),
                        Latitude(), 
                       Variable_NC(),
                       main = paste(input$nc_variables),
                       #sub = paste("Mean temp:", list2[i]),
                       xlab = "Longitude",
                       ylab = "Latitude",
                       legend.lab = Unit(),
                       legend.line = 2.5
                       #col = magma(200)
                       ) 
    #}
  })
  
  output$table <- renderDataTable(Data(),rownames= FALSE,filter = 'top',
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
                                  ))
  
  
  #output$scatterPlot <- renderPlotly({
  #  # Sample data (you can replace this with your actual data)
  #  x <- seq(-180,180, length.out = 100)
  #  y <- seq(-90,90,  length.out = 100)
  #  df <- expand.grid(x=x,y=y)
  #  # Create the scatter plot using plot_ly from the plotly package
  #  plot_ly(data = df, x = df$x, y = df$y, mode = "markers", opacity = 0) %>%
  #    layout(dragmode = "select") # Use "select" for rectangular selection
  #})
  #
  ## Observe changes in the selected data points
  #observeEvent(event_data("plotly_selected"), {
  #  selected_data <- event_data("plotly_selected")
  #  
  #  if (!is.null(selected_data)) {
  #    x_range <- range(selected_data$x)
  #    y_range <- range(selected_data$y)
  #    
  #    output$selectedValues <- renderPrint({
  #      paste("Min X:", x_range[1], "Max X:", x_range[2], "Min Y:", y_range[1], "Max Y:", y_range[2])
  #    })
  #  } else {
  #    output$selectedValues <- renderPrint({
  #      "No points selected."
  #    })
  #  }
  #})
  

}

# Run the application 
shinyApp(ui = ui, server = server)
