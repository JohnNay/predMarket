## File Download and File
## GISS monthly data import script develodped by http://LearnR.wordpress.com   

giss_url <- c(land.sea = "http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts+dSST.txt",
              land = "http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts.txt")
giss_data.source.text <- c( land.sea = paste0("Data source: NASA, ", giss_url['land.sea']),
                            land = paste0("Data source: NASA, ", giss_url['land']))

giss_file <- c(land.sea = file.path('data',"GLB.Ts+dSST.txt"), 
               land = file.path('data','GLB.Ts.txt'))

update.data <- function() {
  for (loc in c('land.sea','land'))
    download.file(giss_url[loc], giss_file[loc])
}
