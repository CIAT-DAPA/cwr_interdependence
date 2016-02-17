# Matrix to JSON format using R
# H. Achicanoy & C. Khoury
# CIAT, 2016

library(jsonlite)
library(readxl)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Load flow matrix
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Matrix should have macro regions and regions. All flows should be in the same table.
work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
flow_matrix_calories <- read_excel(path=paste(work_dir, '/_interactive/macro_micro_regions_to_JSON.xlsx', sep=''), sheet=2, col_names=TRUE)
flow_matrix_calories <- as.data.frame(flow_matrix_calories)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Define elements to construct JSON file
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# {names} for JSON file
mat.labels <- flow_matrix_calories[,1]

# {matrix} for JSON file
flow_matrix_calories <- flow_matrix_calories[,-1]
flow_matrix_calories <- as.matrix(flow_matrix_calories)

# {regions} for JSON file
macroNM <- c('North America', 'Latin America', 'Africa', 'Europe', 'W Asia and SE Mediterranean', 'Central Asia', 'South Asia', 'East Asia', 'Southeast Asia', 'Pacific')
grep2 <- Vectorize(grep, vectorize.args='pattern')
mtch <- grep2(pattern=macroNM, x=mat.labels, fixed=TRUE); mtch <- unlist(lapply(1:length(mtch), function(i){z<-mtch[[i]][1]; return(z)}))
regions <- mtch-1

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Making JSON file
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Put all elements together in a list, after that apply toJSON function
# Sublist can contain different type of information to show
json.test <- list(names=mat.labels,
                  regions=regions,
                  matrix=list('1990'=flow_matrix_calories,
                              '1995'=flow_matrix_calories,
                              '2000'=flow_matrix_calories,
                              '2005'=flow_matrix_calories)
                  )

sink(paste(work_dir, '/_interactive/_json/interdependence_calories.json', sep='')) # redirect console output to a file
  toJSON(json.test, pretty=TRUE)
sink()

