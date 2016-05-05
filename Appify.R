## The Module script
# A zoon output model that builds a Shiny app to explore
# input data, model results and predictions
rm(list = ls())

Appify <- function(.model, .ras, directory = tempdir(), run_App = FALSE){
  
  zoon::GetPackage('shiny')
  zoon::GetPackage('DT')
  zoon::GetPackage('leaflet')
  zoon:::GetPackage('htmlwidgets')
  zoon:::GetPackage('viridis')
  zoon:::GetPackage('rgdal')
  zoon:::GetPackage("googleVis")
  
  # Check dir exists
  if(!dir.exists(directory)){
    stop(paste('Directory', directory, 'does not exist. Please create this first so that Appify can write to it'))
  } 
  
  # Create the data object to be used in the app
  input_data <- list(model = list(.model),
                     raster = list(.ras))
  
  ##########################
  ### create Server code ###
  ##########################
  
  server <- deparse(substitute(env = new.env(), expr = {

    ## server script
    
    # load in the data
    load(file = "data/input_data.rdata")
    # get list of modules once
    modules <- zoon::GetModuleList()
    
    # This function creates links of of modules that are on the repo
    
    linkModules <- function(names, modules, type){
      
      urlBase <- "https://github.com/zoonproject/modules/blob/master/R/"
      isChain <- FALSE
      
      if(grepl("^Chain\\(", names)){
        
        isChain <- TRUE
        names <- gsub("^Chain\\(", "", names)
        names <- gsub("\\)$", "", names)
        names <- trimws(strsplit(names, split = ",")[[1]])
      }
      
      linked <- sapply(X = names, FUN = function(x, type){
        if(x %in% modules[[type]]){
          x <- paste0('<a href="', urlBase, x, '.R" target="_blank">', x, '</a>')    
        }
        x
      }, type = type, USE.NAMES = FALSE)
      
      if(isChain){
        
        linked <- paste0('Chain(', paste(linked, collapse = ', '), ')')
        
      }
      
      return(linked)
      
    }
    
    shinyServer(function(input, output, session) {
      
      ##################
      ## Landing Page ##
      ##################
      
      output$org_chart <- googleVis::renderGvis({
        
        org_data <- lapply(1:length(input_data$model), function(j){
          
          x <- input_data$model[[j]]
          
          call_path <- as.character(unlist(attr(x, 'call_path')))
          
        })
        
        org_df <- as.data.frame(do.call(rbind, org_data), stringsAsFactors = FALSE)
        
        for(i in 1:4){
          
          mods <- as.data.frame(unique(org_df[,1:i]))
          
          if(nrow(mods)>1){
            
            modules_tab <- table(mods[,i])
            remods_tab <- names(modules_tab)[modules_tab>1]
            
            if(length(remods_tab)>0){
              
              for(n in remods_tab){
                
                org_df[,i][org_df[,i] == n] <- paste(org_df[,i][org_df[,i] == n], 1:length(org_df[,i][org_df[,i] == n]))
                
              }
              
            }
          }
        }
        
        org_df
        
        fixed <- apply(org_df, MARGIN = 1, FUN = function(x){
          
          mat <- matrix(data = NA, nrow = 4, ncol = 3, 
                        dimnames = list(NULL, c('module', 'parent', 'level')))
          
          for(i in 1:4){
            
            mat[,'module'][i] <- as.character(x[i])
            ifelse(i == 1,
                   mat[,'parent'][i] <- NA,
                   mat[,'parent'][i] <- as.character(x[i-1]))
            mat[,'level'][i] <- i
            
          }
          return(as.data.frame(mat, stringsAsFactors = FALSE))  
        })
        
        chart_data <- unique(do.call(rbind, fixed))
        chart_data$parent[is.na(chart_data$parent)] <- 'Zoon Workflow'
        
        zoon_wf <- data.frame(module = 'Zoon Workflow',
                              parent = NA,
                              level = 0)
        
        chart_data <- rbind(zoon_wf, chart_data)
        
        org_chart <- googleVis::gvisOrgChart(data = chart_data,
                                             idvar = 'module',
                                             parentvar = 'parent',
                                             options = list(size = 'large'))
      })
      
      ################
      ## Occurrence ##
      ################
      
      # Create the occurrence text
      output$occurrence_text <- renderUI({
        
        occ_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$occurrence))), modules = modules, type = 'occurrence')
        proc_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$process))), modules = modules, type = 'process')
        
        div(
          p(HTML('Here is the occurrence data that was used in the workflow.',
                 'This occurrence data is generated from', length(occ_mods),
                 'occurrence module(s)', paste0('(', paste(occ_mods, collapse = ', '), ')'),
                 'and', length(proc_mods), 'process module(s)',
                 paste0('(', paste(proc_mods, collapse = ', '), ')'))),
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
                              p(code(paste('Covariate module:', attr(input_data$model[[i]], 'call_path')$covariate)),
                                code(paste('Process module:', attr(input_data$model[[i]], 'call_path')$process))),
                              leaflet::leafletOutput(paste('occurrence_map', i, sep = '_'),
                                                     height = '500px'),
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
            input_data$model[[my_j]]$data[,'type'] <- as.character(input_data$model[[my_j]]$data[,'type'])
            DT::datatable(input_data$model[[my_j]]$data[,c('value','type','fold','longitude','latitude')],
                          rownames = FALSE,
                          filter = 'top')
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
            fill_pal <- leaflet::colorFactor(grey(c(1, 0, 0.5)),
                                             domain = c('presence',
                                                        'absence',
                                                        'background'),
                                             ordered = TRUE)
            
            border_pal <- leaflet::colorFactor(grey(c(0, 1, 1)),
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
        
        cov_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$covariate))), modules = modules, type = 'covariate')
        proc_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$process))), modules = modules, type = 'process')
        
        div(
          p(HTML('Here is the covariate data that was used in the workflow.',
                 'This data is generated from', length(cov_mods),
                 'covariate module(s)', paste0('(', paste(cov_mods, collapse = ', '), ')'),
                 'and', length(proc_mods), 'process module(s)',
                 paste0('(', paste(proc_mods, collapse = ', '), ')'))),
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
      
      # Create the cov tabs in main panel
      output$covariate_tabs <- renderUI({
        
        html <- list()
        
        for(i in 1:length(input_data$raster)){
          
          tab <- tabPanel(title = paste(i, attr(input_data$raster[[i]], 'call_path')$covariate),
                          div(br(),
                              radioButtons(paste0('ras_layer', i),
                                           label = 'Choose layer to display',
                                           choices = names(input_data$raster[[i]]),
                                           inline = TRUE),
                              br(),
                              leaflet::leafletOutput(paste('covariate_map', i, sep = '_'),
                                                     height = '500px')
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
                                             domain = c(raster::minValue(.ras),
                                                        raster::maxValue(.ras)), 
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
            legend_values <- round(seq(raster::minValue(.ras),
                                       raster::maxValue(.ras),
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
      
      
      ###########
      ## Model ##
      ###########
      
      # Create the model text
      output$model_text <- renderUI({
        
        occ_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$occurrence))), modules = modules, type = 'occurrence')
        cov_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$covariate))), modules = modules, type = 'covariate')
        proc_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$process))), modules = modules, type = 'process')
        model_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$model))), modules = modules, type = 'model')
        
        div(
          p(HTML('Here is the model summary from the workflow.',
                 'This model was generated from', length(occ_mods),
                 'occurrence module(s)', paste0('(', paste(occ_mods, collapse = ', '), '),'),
                 length(cov_mods), 'covariate module(s)',
                 paste0('(', paste(cov_mods, collapse = ', '), '),'),
                 length(proc_mods), 'process module(s)',
                 paste0('(', paste(proc_mods, collapse = ', '), '),'),
                 'and', length(model_mods), 'model module(s)',
                 paste0('(', paste(model_mods, collapse = ', '), ').')
          )),
          p('Use the download button to download this data as an R object')
        )
      }) 
      
      # Download model data
      output$downloadModelData <- downloadHandler(
        filename = function() {
          paste('model-data-', Sys.Date(), '.rdata', sep='')
        },
        content = function(file) {
          model_data <- lapply(input_data$model, function(x){
            model_data <- x$model
            attr(model_data, 'call_path') <- attr(x, 'call_path')
            model_data
          })
          save(model_data, file = file)
        }
      )
      
      # Create the model tabs in main panel
      output$model_tabs <- renderUI({
        
        html <- list()
        
        for(i in 1:length(input_data$model)){
          
          tab <- tabPanel(title = paste(i, attr(input_data$model[[i]], 'call_path')$model),
                          div(br(),
                              uiOutput(paste('model_print', i, sep = '_'))
                          )
          )
          
          html <- append(html, list(tab))
          
        }
        
        do.call(tabsetPanel, html)
        
      })
      
      # Loop through the models and create print summaries
      for(j in 1:length(input_data$model)){
        local({ # There is some strange environment stuff that goes on here
          my_j <- j
          output[[paste('model_print', my_j, sep = '_')]] <- renderUI({
            
            # Load in the packages used to create the model as they
            # will have special print methods
            # IDEALLY USE ZOON::GETPACKAGE HERE #
            for(package in input_data$model[[my_j]]$model$packages){
              require(package, character.only = TRUE)
            }
            
            x <- capture.output(print(input_data$model[[my_j]]$model$model))
            div(br(),
                p(code(paste('Occurrence module:', attr(input_data$model[[my_j]], 'call_path')$occurrence)),
                  code(paste('Covariate module:', attr(input_data$model[[my_j]], 'call_path')$covariate)),
                  code(paste('Process module:', attr(input_data$model[[my_j]], 'call_path')$process))),
                h3('Model print summary'),
                div(id = 'model_out',
                    HTML(paste(x, collapse = '<br/>'))))
          })
        })
      }
      
      #################
      ## Predictions ##
      #################
      
      # Create the model text
      output$pred_text <- renderUI({
        
        occ_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$occurrence))), modules = modules, type = 'occurrence')
        cov_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$covariate))), modules = modules, type = 'covariate')
        proc_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$process))), modules = modules, type = 'process')
        model_mods <- linkModules(unique(unlist(lapply(input_data$model, function(x) attr(x, 'call_path')$model))), modules = modules, type = 'model')
        
        div(
          p(HTML('Here we use the model and to create a prediction surface',
                 'using the original covariate data.',
                 'The model was generated from', length(occ_mods),
                 'occurrence module(s)', paste0('(', paste(occ_mods, collapse = ', '), '),'),
                 length(cov_mods), 'covariate module(s)',
                 paste0('(', paste(cov_mods, collapse = ', '), '),'),
                 length(proc_mods), 'process module(s)',
                 paste0('(', paste(proc_mods, collapse = ', '), '),'),
                 'and', length(model_mods), 'model module(s)',
                 paste0('(', paste(model_mods, collapse = ', '), ').')
          )))
      }) 
      
      # Create the predictions tabs in main panel
      output$pred_tabs <- renderUI({
        
        html <- list()
        
        for(i in 1:length(input_data$model)){
          
          tab <- tabPanel(title = paste(i, attr(input_data$model[[i]], 'call_path')$model),
                          div(br(),
                              p(code(paste('Occurrence module:', attr(input_data$model[[i]], 'call_path')$occurrence)),
                                code(paste('Covariate module:', attr(input_data$model[[i]], 'call_path')$covariate)),
                                code(paste('Process module:', attr(input_data$model[[i]], 'call_path')$process))),
                              leaflet::leafletOutput(paste('pred_map', i, sep = '_'),
                                                     height = '500px')
                          )
          )
          
          html <- append(html, list(tab))
          
        }
        
        do.call(tabsetPanel, html)
        
      })
      
      # Loop through the models and create maps
      for(f in 1:length(input_data$model)){
        local({ # There is some strange environment stuff that goes on here
          my_j <- f
          output[[paste('pred_map', my_j, sep = '_')]] <- leaflet::renderLeaflet({
            
            m <- leaflet::leaflet()
            m <- leaflet::addTiles(map = m, group = 'OpenStreetMap')
            m <- leaflet::addProviderTiles(map = m,
                                           provider = 'Esri.WorldImagery',
                                           group = 'Esri.WorldImagery')
            
            # add training data
            # which cov layer matches this model?
            cov_ras <- attr(input_data$model[[my_j]], 'call_path')$covariate
            ras_mods <- sapply(input_data$raster, function(x) attr(x, 'call_path')$covariate)
            .ras <- input_data$raster[[grep(cov_ras, ras_mods)]]
            
            # Make the prediction
            vals <- data.frame(raster::getValues(.ras))
            colnames(vals) <- names(.ras)
            
            pred <- zoon::ZoonPredict(input_data$model[[my_j]]$model,
                                      newdata = vals)
            
            pred_ras <- .ras[[1]]
            
            # pred is rounded so that very slight minus values become 0
            # this is matched by the legend
            pred_ras <- raster::setValues(pred_ras, 
                                          round(pred, 2))
            
            # get legend values
            legend_values <- round(seq(0, 1, length.out = 10), 2)
            
            # get prediction colour palette
            pred_pal <- leaflet::colorNumeric(viridis::viridis(10), 
                                              domain = legend_values, 
                                              na.color = 'transparent')
            
            # reproject pred_ras, suppressing warnings
            suppressWarnings(ext <- raster::projectExtent(pred_ras,
                                                          crs = sp::CRS('+init=epsg:3857')))
            suppressWarnings(pred_ras <- raster::projectRaster(pred_ras,
                                                               ext))
            
            # add the prediction raster
            m <- leaflet::addRasterImage(map = m,
                                         x = pred_ras,
                                         colors = pred_pal,
                                         project = FALSE,
                                         opacity = 0.8,
                                         group = 'predicted distribution')
            
            # add to the overlay groups list
            overlay_groups <- 'predicted distribution'
            
            
            # add predicted distribution legend
            m <- leaflet::addLegend(map = m,
                                    pal = pred_pal,
                                    opacity = 0.8, 
                                    values = legend_values, 
                                    title = 'Predicted distribution')
            
            # add training data
            df <- input_data$model[[my_j]]$data
            
            # color palettes for circles
            fill_pal <- leaflet::colorFactor(grey(c(1, 0, 0.5)),
                                             domain = c('presence',
                                                        'absence',
                                                        'background'),
                                             ordered = TRUE)
            
            border_pal <- leaflet::colorFactor(grey(c(0, 1, 1)),
                                               domain = c('absence',
                                                          'presence',
                                                          'background'),
                                               ordered = TRUE)
            
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
      
    })
  }))
  
  
  ######################
  ### create UI code ###
  ######################
  
  ui <- deparse(substitute(env = new.env(), expr = {
    
    # ui script
    
    library(markdown)
    
    shinyUI(navbarPage("Appify",
                       tabPanel("Overview",
                                sidebarLayout(
                                  sidebarPanel(
                                    width = 3,
                                    h2("Overview"),
                                    p(HTML('These pages show you the output from a ZOON workflow. <a href="https://zoonproject.wordpress.com/" target="_blank">Zoon</a> is a tool for reproducible species distribution modelling in R. Here you can find the results for the workflow detailed to the right. You will find the occurrence data used as well as the covariate data and the model output. Enjoy!'))
                                  ),
                                  mainPanel(
                                    div(img(src = 'https://github.com/zoonproject/blog/raw/master/zoon_top.png',
                                            width = '100%',
                                            alt = 'ZOON')),
                                    #h4('Workflow Structure'),
                                    br(),
                                    div(htmlOutput('org_chart'))
                                  )
                                )
                       ),
                       tabPanel("Occurrence data",
                                sidebarLayout(
                                  sidebarPanel(
                                    width = 3,
                                    h2("Occurrences"),
                                    uiOutput('occurrence_text'),
                                    br(),
                                    downloadButton('downloadOccData', 'Download')
                                  ),
                                  mainPanel(
                                    uiOutput("occurrence_tabs"),
                                    br()
                                  )
                                )
                       ),
                       tabPanel("Covariate data",
                                sidebarLayout(
                                  sidebarPanel(
                                    h2("Covariates"),
                                    uiOutput('covariate_text'),
                                    br(),
                                    downloadButton('downloadCovData', 'Download')
                                  ),
                                  mainPanel(
                                    uiOutput("covariate_tabs"),
                                    br()
                                  )
                                )
                       ),
                       tabPanel("Model",
                                tags$head(
                                  tags$style(HTML("
                                                  #model_out {
                                                  padding: 19px;
                                                  background-color: #f5f5f5;
                                                  border: 1px solid #e3e3e3;
                                                  border-radius: 4px;
                                                  }
                                                  "))
                                  ),
                                sidebarLayout(
                                  sidebarPanel(
                                    h2("Model"),
                                    uiOutput('model_text'),
                                    br(),
                                    downloadButton('downloadModelData', 'Download')
                                  ),
                                  mainPanel(
                                    uiOutput("model_tabs"),
                                    br()
                                  )
                                )
                                  ),
                       tabPanel("Predictions",
                                sidebarLayout(
                                  sidebarPanel(
                                    h2("Predictions"),
                                    uiOutput('pred_text')
                                  ),
                                  mainPanel(
                                    uiOutput("pred_tabs"),
                                    br()
                                  )
                                )
                       )
                                ))

  }))
  
  #########################
  #########################
  
  ## remove the curly barckets
  ui[[1]] <- ''
  ui[[length(ui)]] <- ''
  server[[1]] <- ''
  server[[length(server)]] <- ''
  
  # replace escaped double quotes with single quotes
#   ui <- gsub('\\\\\\"', "'", ui)
#   server <- gsub('\\\\\\"', "'", server)
  
  # Create the file structure
  subDir <- paste0('Appify_', format(Sys.time(), "%d_%m_%Y_%H%M%S"))
  
  # if in a fast loop it might write over the previous version so waith for the 
  # clock to tick over
  while(dir.exists(file.path(directory, subDir))){
    Sys.sleep(time = 1)
    subDir <- paste0('Appify_', format(Sys.time(), "%d_%m_%Y_%H%M%S"))
  }
  
  dir.create(file.path(directory, subDir))
  dir.create(file.path(directory, subDir, 'data'), showWarnings = FALSE)
  save(input_data, file = file.path(directory, subDir, 'data', 'input_data.rdata'))
  write.table(ui, file = file.path(directory, subDir, 'ui.R'),
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
  write.table(server, file = file.path(directory, subDir, 'server.R'),
              row.names = FALSE,
              col.names = FALSE,
              quote = FALSE)
  
  message('Your shiny app can be found here: ', file.path(directory, subDir), 
          '. For information on how to share this app visit http://shiny.rstudio.com/tutorial/lesson7/')
  
  if(run_App){
    shiny::runApp(appDir = file.path(directory, subDir))
  } else {
    message('To run this App use - shiny::runApp(appDir = "', gsub('\\\\', '/', file.path(directory, subDir)),'")')
  }

  return(file.path(directory, subDir))
    
}

library(zoon)

dir.create('module')

BuildModule(object = Appify,
            dir = 'module',
            type = 'output',
            title = 'Appify',
            description = 'The Appify module builds a shiny app that displays dynamic graphical and tabular representations of the input data (occurrence and covarite) as well as model predictions.',
            author = 'ZOON Developers, Tom August',
            email = 'zoonproject@gmail.com',
            details = 'Appify will return the path to the shiny application. For details on how to share the application visit http://shiny.rstudio.com/tutorial/lesson7/. You can run the app locally using shiny::runApp(appDir = <YOUR_PATH>)', 
            para = list(directory = '(Optional) a string giving the path to a directory within which to save the shiny app. This directory must exist.', 
                        run_App = "Logical, should the app be run automatically. Not recommended for workflows containing lists. Default is FALSE"),
            dataType = c('presence-only', 'presence/absence', 'presence/background', 'abundance', 'proportion')
            )

rm(list = 'Appify')

LoadModule('module/Appify.R')

work1 <- workflow(occurrence = UKAnophelesPlumbeus, 
                  covariate  = list(UKBioclim, UKAir),
                  process    = Chain(NoProcess, OneHundredBackground),
                  model      = RandomForest,
                  output     = Appify(run_App = TRUE))