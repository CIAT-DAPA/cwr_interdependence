# Flow matrix by regions using information about crops - Production systems
# H. Achicanoy & C. Khoury
# CIAT, 2016

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
prod_data_elements <- list.dirs(path=paste(work_dir, '/_production_systems/_interdependence_analysis/_outputs', sep=''), full.names=FALSE, recursive=FALSE)
prod_data_elements <- paste(work_dir, '/_production_systems/_interdependence_analysis/_outputs/', prod_data_elements, '/prod_countries_regions_all_merge.csv', sep='')
prod_data_elements <- lapply(prod_data_elements, read.csv)
prod_data_elements <- Reduce(function(...) rbind(..., deparse.level=1), prod_data_elements)

prod_data_elements <- prod_data_elements[,c("Element", "Item", "Country", 'Average', 'Region')]
prod_data_elements <- unique(prod_data_elements)

fmeas <- as.character(unique(prod_data_elements$Element))
fitem <- as.character(unique(prod_data_elements$Item))
fcoun <- as.character(unique(prod_data_elements$Country))

library(dplyr)

# Step 1: Average of population by country (in production case it is not necessary only sum through countries for each region)

all_elements <- as.data.frame(dplyr::summarise(group_by(prod_data_elements, Region, Item, Element), sum(Average)))
names(all_elements)[4] <- "Average"
write.csv(all_elements, paste('C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_interactive/_useful_info/prod_sum_regions.csv', sep=''), row.names=FALSE)

# Step 2: Include crop origin regions

work_dir <-'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
all_elements <- read.csv(paste(work_dir, '/_interactive/_useful_info/prod_sum_regions.csv', sep=''))
food_regions <- read.csv(paste(work_dir, '/_production_systems/_circos/Prod/Prod_items_country_regions_2016_2_8.csv', sep=''))
food_regions <- food_regions[,c('Item', 'Region_crops')]
food_regions <- unique(food_regions); rownames(food_regions) <- 1:nrow(food_regions)

all_elements <- merge(all_elements, food_regions, by='Item')
write.csv(all_elements, paste(work_dir, '/_interactive/_useful_info/prod_sum_regions_and_origins.csv', sep=''), row.names=FALSE)
