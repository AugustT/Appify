
library(zoon)

work1 <- workflow(occurrence = UKAnophelesPlumbeus, 
                  covariate  = list(UKBioclim, UKAir),
                  process    = Chain(NoProcess, OneHundredBackground),
                  model      = RandomForest,
                  output     = PrintMap)

input_data <- list(model = work1$model.output,
                   raster = work1$covariate.output)

dir.create('data', showWarnings = FALSE)

save(input_data, file = 'data/input_data.rdata')

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

org_chart <- gvisOrgChart(data = chart_data, idvar = 'module', parentvar = 'parent')
plot(org_chart)
