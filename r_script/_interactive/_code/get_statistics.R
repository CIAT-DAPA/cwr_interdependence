# Get statistics from region to country
# H. Achicanoy & C. Khoury
# CIAT, 2016

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Food supplies
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

fs_region_country <- read.csv(paste(work_dir, '/_regions/_outputs/circos/FS/FS_sum_country_regions_cropregions.csv', sep=''))
fs_region_country$Average <- as.vector(rowMeans(fs_region_country[,paste('Y',2009:2011,sep='')]))
fs_region_country$Average <- round(fs_region_country$Average, 2)

elements <- sort(as.character(unique(fs_region_country$Element)))
fs_region_country_percent <- lapply(1:length(elements), function(i)
{
  fs_data <- subset(fs_region_country, fs_region_country$Element==elements[i])
  fs_data <- fs_data[,c("Country", "Region_crops", "Average")]
  rownames(fs_data) <- 1:nrow(fs_data)
  countries <- sort(as.character(unique(fs_data$Country)))
  countriesFiles <- lapply(1:length(countries), function(j)
  {
    fs_country <- subset(fs_data, fs_data$Country==countries[j]); rownames(fs_country) <- 1:nrow(fs_country)
    fs_country <- fs_country[order(fs_country$Average, decreasing=TRUE),]; rownames(fs_country) <- 1:nrow(fs_country)
    fs_country$Percentage <- fs_country$Average/sum(fs_country$Average)
    return(fs_country)
  })
  countriesFiles <- Reduce(function(...) rbind(..., deparse.level=1), countriesFiles)
  countriesFiles$Element <- paste(elements[i])
  
  return(countriesFiles)
})
fs_region_country_percent <- Reduce(function(...) rbind(..., deparse.level=1), fs_region_country_percent)
write.csv(fs_region_country_percent, paste(work_dir, '/_interactive/_useful_info/fs_regioncrops_to_country.csv', sep=''), row.names=FALSE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Production systems
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

prod_region_country <- read.csv(paste(work_dir, '/_production_systems/_circos/Prod/Prod_sum_country_regions_cropregions.csv', sep=''))
prod_region_country$Average <- as.vector(rowMeans(prod_region_country[,paste('Y',2009:2011,sep='')]))
prod_region_country$Average <- round(prod_region_country$Average, 2)

elements <- sort(as.character(unique(prod_region_country$Element)))
prod_region_country_percent <- lapply(1:length(elements), function(i)
{
  prod_data <- subset(prod_region_country, prod_region_country$Element==elements[i])
  prod_data <- prod_data[,c("Country", "Region_crops", "Average")]
  rownames(prod_data) <- 1:nrow(prod_data)
  countries <- sort(as.character(unique(prod_data$Country)))
  countriesFiles <- lapply(1:length(countries), function(j)
  {
    prod_country <- subset(prod_data, prod_data$Country==countries[j]); rownames(prod_country) <- 1:nrow(prod_country)
    prod_country <- prod_country[order(prod_country$Average, decreasing=TRUE),]; rownames(prod_country) <- 1:nrow(prod_country)
    prod_country$Percentage <- prod_country$Average/sum(prod_country$Average)
    return(prod_country)
  })
  countriesFiles <- Reduce(function(...) rbind(..., deparse.level=1), countriesFiles)
  countriesFiles$Element <- paste(elements[i])
  
  return(countriesFiles)
})
prod_region_country_percent <- Reduce(function(...) rbind(..., deparse.level=1), prod_region_country_percent)
write.csv(prod_region_country_percent, paste(work_dir, '/_interactive/_useful_info/prod_regioncrops_to_country.csv', sep=''), row.names=FALSE)
