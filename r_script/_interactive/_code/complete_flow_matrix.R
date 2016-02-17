# Construct complete flow matrix
# H. Achicanoy & C. Khoury
# CIAT, 2016

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Load information
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

macro <- list.files(path=paste(work_dir, '/_macro/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)
region <- list.files(path=paste(work_dir, '/_regions/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)
macro_to_region <- list.files(path=paste(work_dir, '/_macro_to_region/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)
region_to_macro <- list.files(path=paste(work_dir, '/_region_to_macro/_outputs/circos/FS', sep=''), pattern='^interchange', full.names=TRUE)

# Macro files
macroFiles <- lapply(macro, function(x){z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]; colnames(z) <- rownames(z); return(z)})
# Region files
regionFiles <- lapply(region, function(x)
{
  z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]
  rownames(z) <- paste(rownames(z), '_reg', sep='')
  colnames(z) <- rownames(z)
  return(z)
})
# Macro to region files
macro_to_regionFiles <- lapply(macro_to_region, function(x)
{
  z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]
  colnames(z) <- c("N\nAmerica_reg","C\nAmerica_reg","Caribbean_reg","Andes_reg","Trop. S.\nAmerica_reg","Temp. S.\nAmerica_reg","W\nAfrica_reg","C\nAfrica_reg","E\nAfrica_reg",
                   "S\nAfrica_reg","IOI_reg","NW\nEurope_reg","SW\nEurope_reg","NE\nEurope_reg","SE\nEurope_reg","SE\nMediterranean_reg","W\nAsia_reg","C\nAsia_reg","S\nAsia_reg",
                   "E\nAsia_reg","SE\nAsia_reg","Pacific_reg","ANZ_reg")
  return(z)
})
# Region to macro files
region_to_macroFiles <- lapply(region_to_macro, function(x)
{
  z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]
  rownames(z) <- paste(rownames(z), '_reg', sep='')
  colnames(z) <- c("North\nAmerica","Latin\nAmerica","Africa","Europe","W Asia and\nSE Mediterranean","Central\nAsia",
                   "South\nAsia","East\nAsia","Southeast\nAsia","Pacific")
  return(z)
})

matrixNames <- c("North\nAmerica", "N\nAmerica_reg",
                 "Latin\nAmerica", "C\nAmerica_reg", "Caribbean_reg", "Andes_reg", "Trop. S.\nAmerica_reg", "Temp. S.\nAmerica_reg",
                 "Africa", "W\nAfrica_reg", "C\nAfrica_reg", "E\nAfrica_reg", "S\nAfrica_reg", "IOI_reg",
                 "Europe", "NW\nEurope_reg", "SW\nEurope_reg", "NE\nEurope_reg", "SE\nEurope_reg",
                 "W Asia and\nSE Mediterranean", "SE\nMediterranean_reg", "W\nAsia_reg",
                 "Central\nAsia", "C\nAsia_reg",
                 "South\nAsia", "S\nAsia_reg",
                 "East\nAsia", "E\nAsia_reg",
                 "Southeast\nAsia", "SE\nAsia_reg",
                 "Pacific", "Pacific_reg", "ANZ_reg")

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Create matrix
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Remember: rows are origins and columns are recipients

elements <- c('calories','fat','food_weight','protein')
lapply(1:length(macroFiles), function(i)
{
  # Create empty matrix
  flows_matrix <- matrix(data=NA, nrow=length(matrixNames), ncol=length(matrixNames))
  rownames(flows_matrix) <- colnames(flows_matrix) <- matrixNames
  flows_matrix <- as.data.frame(flows_matrix)
  
  # Define names of groups and elements
  macro_names <- rownames(macroFiles[[1]])
  region_names <- rownames(regionFiles[[1]])
  
  flows_matrix[macro_names, macro_names] <- macroFiles[[i]]; flows_matrix <- as.data.frame(flows_matrix) # macro to macro
  flows_matrix[region_names, region_names] <- regionFiles[[i]]; flows_matrix <- as.data.frame(flows_matrix) # region to region
  flows_matrix[macro_names, region_names] <- macro_to_regionFiles[[i]]; flows_matrix <- as.data.frame(flows_matrix) # macro to region
  flows_matrix[region_names, macro_names] <- region_to_macroFiles[[i]]; flows_matrix <- as.data.frame(flows_matrix) # region to macro
  
  write.csv(flows_matrix, paste(work_dir, '/_interactive/_flows_matrix/interchange_complete_flows_', elements[[i]], '.csv', sep=''), row.names=TRUE)
  
  return(cat('Done!\n'))
})
