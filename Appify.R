## The Module script
# A zoon output model that builds a Shiny app to explore
# input data, model results and predictions

Appify <- function(.model, .ras, dir = tempdir()){
  
  zoon::GetPackage('shiny')
  zoon::GetPackage('DT')
  zoon::GetPackage('leaflet')
  zoon:::GetPackage('htmlwidgets')
  zoon:::GetPackage('viridis')
  zoon:::GetPackage('rgdal')
  zoon:::GetPackage("googleVis")
  
  # Check dir exists
  if(!dir.exists(dir)){
    warning(paste('Directory', dir, 'does not exist. Appify will attempt to create it.'))
    dir.create(dir)
  } 
  
  # Create the data object to be used in the app
  input_data <- list(model = .model,
                     raster = .ras)
  
  # create server code
  server <- 'palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
            
            shinyServer(function(input, output, session) {
            
              # Combine the selected variables into a new data frame
              selectedData <- reactive({
                iris[, c(input$xcol, input$ycol)]
              })
            
              clusters <- reactive({
                kmeans(selectedData(), input$clusters)
              })
            
              output$plot1 <- renderPlot({
                par(mar = c(5.1, 4.1, 0, 1))
                plot(selectedData(),
                     col = clusters()$cluster,
                     pch = 20, cex = 3)
                points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
              })
            
            })'
  
  # create UI code
  ui <- "shinyUI(pageWithSidebar(
                  headerPanel('Iris k-means clustering'),
                  sidebarPanel(
                    selectInput('xcol', 'X Variable', names(iris)),
                    selectInput('ycol', 'Y Variable', names(iris),
                                selected=names(iris)[[2]]),
                    numericInput('clusters', 'Cluster count', 3,
                                 min = 1, max = 9)
                  ),
                  mainPanel(
                    plotOutput('plot1')
                  )
                ))"
  
  
  # Create the file structure
  dir.create(file.path(dir, 'Appify'))
  dir.create(file.path(dir, 'Appify', 'data'), showWarnings = FALSE)
  save(input_data, file.path(dir, 'Appify', 'data', 'input_data.rdata'))
  write.table(ui, file = file.path(dir, 'Appify', 'ui.R'),
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
  write.table(server, file = file.path(dir, 'Appify', 'server.R'),
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
  
  shiny::runApp(appDir = file.path(dir, 'Appify'))
    
}