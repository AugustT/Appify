## server script

# load in the data
load(file = 'data/input_data.rdata')

shinyServer(function(input, output, session) {
  
  ################
  ## Occurrence ##
  ################
  
  # Create the occurrence data tab
  output$occurrence_text <- renderUI({
    
    occ_mods <- unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$occurrence)))
    proc_mods <- unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$process)))
    
    div(
      p('Here is the occurrence data that was used in the workflow.',
        'This occurrence data is generated from', length(occ_mods),
        'occurrence module(s)', paste0('(', paste(occ_mods, collapse = ', '), ')'),
        'and', length(proc_mods), 'process module(s)',
        paste0('(', paste(proc_mods, collapse = ', '), ')')),
      p('Use the download button to download this data as an R object')
    )
  }) 
  
  # Download occ data
  output$downloadOccData <- downloadHandler(
    filename = function() {
      paste('occ-data-', Sys.Date(), '.rdata', sep='')
    },
    content = function(file) {
      occ_data <- lapply(input_data$model, function(x){
        occ_data <- x$data
        attr(occ_data, 'call_path') <- attr(x, 'call_path')
        occ_data
      })
      save(occ_data, file = file)
    }
  )

  # Create the occ tabs in main panel
  output$occurrence_tabs <- renderUI({
    
    html <- list()
    
    for(i in 1:length(input_data$model)){
      
       tab <- tabPanel(title = paste(i, attr(input_data$model[[i]], 'call_path')$occurrence),
                       div(br(),
                           leafletOutput(paste('occurrence_map', i, sep = '_')),
                           br(),
                           DT::dataTableOutput(paste('occurrence_table', i, sep = '_'))
                           )
                      )
      
       html <- append(html, list(tab))
       
    }
    
    do.call(tabsetPanel, html)
    
  })
  
  # Loop through the occurence elements and create data tables
  for(j in 1:length(input_data$model)){
    local({ # There is some strange environment stuff that goes on here
      my_j <- j
      output[[paste('occurrence_table', my_j, sep = '_')]] <- DT::renderDataTable({
        DT::datatable(input_data$model[[my_j]]$data)
      })
    })
  }

  # Loop through the occurence elements and create maps
  for(f in 1:length(input_data$model)){
    local({ # There is some strange environment stuff that goes on here
      my_j <- f
      output[[paste('occurrence_map', my_j, sep = '_')]] <- leaflet::renderLeaflet({

        m <- leaflet::leaflet()
        m <- leaflet::addTiles(map = m, group = 'OpenStreetMap')
        m <- leaflet::addProviderTiles(map = m,
                                       provider = 'Esri.WorldImagery',
                                       group = 'Esri.WorldImagery')
        
        # add training data
        df <- input_data$model[[my_j]]$data
        
        # color palettes for circles
        fill_pal <- colorFactor(grey(c(1, 0, 0.5)),
                                domain = c('presence',
                                           'absence',
                                           'background'),
                                ordered = TRUE)
        
        border_pal <- colorFactor(grey(c(0, 1, 1)),
                                  domain = c('absence',
                                             'presence',
                                             'background'),
                                  ordered = TRUE)
        
        overlay_groups <- NULL 
        
        for (type in c('absence', 'background', 'presence')) {
          if (any(df$type == type)) {
            idx <- df$type == type
            group_name <- paste(type, 'data')
            overlay_groups <- c(overlay_groups, group_name)
            m <- leaflet::addCircleMarkers(map = m,
                                           lng = df$lon[idx],
                                           lat = df$lat[idx],
                                           color = grey(0.4),
                                           fillColor = fill_pal(type),
                                           weight = 1,
                                           opacity = 1,
                                           fillOpacity = 1,
                                           radius = 5,
                                           group = group_name,
                                           popup = paste('<b>',
                                                         paste(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), sep=""),
                                                         '</b>',
                                                         '<br>Longitude:', df$lon[idx],
                                                         '<br>Latitude:', df$lat[idx],
                                                         '<br>Fold:', df$fold[idx],
                                                         '<br>Value:', df$value[idx]))
            
          }
        }
        
        # add points legend
        m <- leaflet::addLegend(map = m,
                                pal = fill_pal,
                                opacity = 0.8, 
                                values = factor(c('presence', 'absence', 'background'),
                                                levels = c('presence', 'absence', 'background'),
                                                ordered = TRUE),
                                title = 'Data points')
        
        # add toggle for the layers
        m <- leaflet::addLayersControl(map = m,
                                       position = "topleft",
                                       baseGroups = c('OpenStreetMap',
                                                      'Esri.WorldImagery'),
                                       overlayGroups = overlay_groups)
        
        m
        
      })
    })
  }
  
  
  ###############
  ## Covariate ##
  ###############
  
  # Create the covariate text
  output$covariate_text <- renderUI({
    
    cov_mods <- unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$covariate)))
    proc_mods <- unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$process)))
    
    div(
      p('Here is the covariate data that was used in the workflow.',
        'This data is generated from', length(cov_mods),
        'covariate module(s)', paste0('(', paste(cov_mods, collapse = ', '), ')'),
        'and', length(proc_mods), 'process module(s)',
        paste0('(', paste(proc_mods, collapse = ', '), ')')),
      p('Use the download button to download this data as an R object')
    )
  }) 
  
  # Download Covariate data
  output$downloadCovData <- downloadHandler(
    filename = function() {
      paste('cov-data-', Sys.Date(), '.rdata', sep='')
    },
    content = function(file) {
      cov_data <- input_data$raster
      save(cov_data, file = file)
    }
  )
  
  # Create the occ tabs in main panel
  output$covariate_tabs <- renderUI({
    
    html <- list()
    
    for(i in 1:length(input_data$raster)){
      
      tab <- tabPanel(title = paste(i, attr(input_data$raster[[i]], 'call_path')$covariate),
                      div(br(),
                          radioButtons(paste0('ras_layer', i),
                                       label = 'Choose layer to display',
                                       choices = names(input_data$raster[[i]])),
                          br(),
                          leafletOutput(paste('covariate_map', i, sep = '_'),
                                        height = '700px')
                          )
      )
      
      html <- append(html, list(tab))
      
    }
    
    do.call(tabsetPanel, html)
    
  })

  # Loop through the covariate elements and create maps
  for(f in 1:length(input_data$raster)){
    local({ # There is some strange environment stuff that goes on here
      my_j <- f
      output[[paste('covariate_map', my_j, sep = '_')]] <- leaflet::renderLeaflet({
        
        m <- leaflet::leaflet()
        m <- leaflet::addTiles(map = m, group = 'OpenStreetMap')
        m <- leaflet::addProviderTiles(map = m,
                                       provider = 'Esri.WorldImagery',
                                       group = 'Esri.WorldImagery')
        
        # add training data
        .ras <- input_data$raster[[my_j]][[input[[paste0('ras_layer', my_j)]]]]

        # get covariates colour palette
        cov_pal <- leaflet::colorNumeric(viridis::viridis(10), 
                                         domain = c(minValue(.ras),
                                                    maxValue(.ras)), 
                                         na.color = 'transparent')
        
        # reproject pred_ras, suppressing warnings
        suppressWarnings(ext <- raster::projectExtent(.ras,
                                                      crs = sp::CRS('+init=epsg:3857')))
        suppressWarnings(.ras <- raster::projectRaster(.ras,
                                                       ext))
        
        m <- leaflet::addRasterImage(map = m,
                                     x = .ras,
                                     colors = cov_pal,
                                     project = FALSE,
                                     opacity = 0.6,
                                     group = names(.ras))
        
        # add to list of overlay layers
        overlay_groups <- names(.ras)
        
        
        # get legend values
        legend_values <- round(seq(minValue(.ras),
                                   maxValue(.ras),
                                   length.out = 10), 3)
        
        # add legend
        m <- leaflet::addLegend(map = m,
                                pal = cov_pal,
                                opacity = 0.8, 
                                values = legend_values, 
                                title = names(.ras))
        
        # add toggle for the layers
        m <- leaflet::addLayersControl(map = m,
                                       position = "topleft",
                                       baseGroups = c('OpenStreetMap',
                                                      'Esri.WorldImagery'),
                                       overlayGroups = overlay_groups)
        m
        
      })
    })
  }
  
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
})