
library(zoon)

work1 <- workflow(occurrence = UKAnophelesPlumbeus, 
                  covariate  = list(UKAir, UKBioclim),
                  process    = OneHundredBackground,
                  model      = RandomForest,
                  output     = PrintMap)

input_data <- list(model = work1$model.output,
                   raster = work1$covariate.output)

dir.create('data', showWarnings = FALSE)

save(input_data, file = 'data/input_data.rdata')
