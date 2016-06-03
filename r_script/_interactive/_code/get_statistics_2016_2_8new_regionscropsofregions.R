# Get statistics from region to country
# H. Achicanoy & C. Khoury
# CIAT, 2016

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Food supplies
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

fs_region_country <- read.csv(paste(work_dir, '/_regions/_outputs/circos/FS/FS_sum_country_regions_cropregions_2016_2_8.csv', sep=''))
fs_region_country$Average <- as.vector(rowMeans(fs_region_country[,paste('Y',2009:2011,sep='')]))
fs_region_country$Average <- round(fs_region_country$Average, 2)

elements <- sort(as.character(unique(fs_region_country$Element)))
fs_region_country_percent <- lapply(1:length(elements), function(i)
{
  fs_data <- subset(fs_region_country, fs_region_country$Element==elements[i])
  fs_data <- fs_data[,c("Country", "Region_crops", "Average")]
  rownames(fs_data) <- 1:nrow(fs_data)
  
  correct_total <- read.csv(list.files(paste(work_dir, '/_regions/_outputs/total_db_summary', sep=''), pattern='.csv', full.names=TRUE)[i])
  correct_total$Average <- as.vector(rowMeans(correct_total[,paste('Y',2009:2011,sep='')]))
  correct_total$Average <- round(correct_total$Average, 2)
  correct_total <- correct_total[,c("Country","Element","Average")]
  correct_total$Country <- as.character(correct_total$Country)
  
  countries <- sort(as.character(unique(fs_data$Country)))
  countriesFiles <- lapply(1:length(countries), function(j)
  {
    fs_country <- subset(fs_data, fs_data$Country==countries[j]); rownames(fs_country) <- 1:nrow(fs_country)
    fs_country <- fs_country[order(fs_country$Average, decreasing=TRUE),]; rownames(fs_country) <- 1:nrow(fs_country)
    fs_country$Percentage <- fs_country$Average/correct_total$Average[correct_total$Country==countries[j]]
    return(fs_country)
  })
  countriesFiles <- Reduce(function(...) rbind(..., deparse.level=1), countriesFiles)
  countriesFiles$Element <- paste(elements[i])
  
  return(countriesFiles)
})
fs_region_country_percent <- Reduce(function(...) rbind(..., deparse.level=1), fs_region_country_percent)
write.csv(fs_region_country_percent, paste(work_dir, '/_interactive/_useful_info/fs_regioncrops_to_country_2016_5_13.csv', sep=''), row.names=FALSE)

### Horizontal way

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
fs_region_country_percent <- read.csv(paste(work_dir, '/_interactive/_useful_info/fs_regioncrops_to_country_2016_5_13.csv', sep=''))

library(dplyr)
library(tidyr)

test.ave <- fs_region_country_percent[,c("Country","Region_crops","Average",'Element')]; test.ave <- unique(test.ave); rownames(test.ave) <- 1:nrow(test.ave)
test.ave <- test.ave %>% spread(Element, 'Average')
test.per <- fs_region_country_percent[,c("Country","Region_crops","Percentage",'Element')]; test.per <- unique(test.per); rownames(test.per) <- 1:nrow(test.per)
test.per <- test.per %>% spread(Element, 'Percentage')

colnames(test.ave)[3:ncol(test.ave)] <- c(paste(c('Fat', 'Calories', 'Food_weight', 'Protein'), '_value', sep=''))
colnames(test.per)[3:ncol(test.per)] <- c(paste(c('Fat', 'Calories', 'Food_weight', 'Protein'), '_percentage', sep=''))

fs_region_country_percent <- merge(test.ave, test.per, by=c('Country','Region_crops'))
rm(test.ave, test.per)

write.csv(fs_region_country_percent, paste(work_dir, '/_interactive/_useful_info/fs_regioncrops_to_country_matrix_2016_5_13.csv', sep=''), row.names=FALSE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Production systems
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
g=gc(); rm(list=ls())

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

prod_region_country <- read.csv(paste(work_dir, '/_production_systems/_circos/Prod/Prod_sum_country_regions_cropregions_2016_2_8.csv', sep=''))
prod_region_country$Average <- as.vector(rowMeans(prod_region_country[,paste('Y',2009:2011,sep='')]))
prod_region_country$Average <- round(prod_region_country$Average, 2)

elements <- sort(as.character(unique(prod_region_country$Element)))
prod_region_country_percent <- lapply(1:length(elements), function(i)
{
  prod_data <- subset(prod_region_country, prod_region_country$Element==elements[i])
  prod_data <- prod_data[,c("Country", "Region_crops", "Average")]
  rownames(prod_data) <- 1:nrow(prod_data)
  
  
  grep(pattern=tolower(elements[i]), x=list.dirs(paste(work_dir, '/_production_systems/_interdependence_analysis/_outputs', sep=''), full.names=FALSE, recursive=FALSE))
  
  if(elements[i]=="Area harvested"){
    correct_total <- read.csv(paste(work_dir, '/_production_systems/_interdependence_analysis/_outputs/by_area/results_dependence/Total_summary_db.csv', sep=''))
  } else {
    if(elements[i]=="Gross Production Value (current million US$)"){
      correct_total <- read.csv(paste(work_dir, '/_production_systems/_interdependence_analysis/_outputs/by_value/results_dependence/Total_summary_db.csv', sep=''))
    } else {
      correct_total <- read.csv(paste(work_dir, '/_production_systems/_interdependence_analysis/_outputs/by_quantity/results_dependence/Total_summary_db.csv', sep=''))
    }
  }
  correct_total$Average <- as.vector(rowMeans(correct_total[,paste('Y',2009:2011,sep='')]))
  correct_total$Average <- round(correct_total$Average, 2)
  correct_total <- correct_total[,c("Country","Element","Average")]
  correct_total$Country <- as.character(correct_total$Country)
  
  countries <- sort(as.character(unique(prod_data$Country)))
  countriesFiles <- lapply(1:length(countries), function(j)
  {
    prod_country <- subset(prod_data, prod_data$Country==countries[j]); rownames(prod_country) <- 1:nrow(prod_country)
    prod_country <- prod_country[order(prod_country$Average, decreasing=TRUE),]; rownames(prod_country) <- 1:nrow(prod_country)
    prod_country$Percentage <- prod_country$Average/correct_total$Average[correct_total$Country==countries[j]]
    return(prod_country)
  })
  countriesFiles <- Reduce(function(...) rbind(..., deparse.level=1), countriesFiles)
  countriesFiles$Element <- paste(elements[i])
  
  return(countriesFiles)
})
prod_region_country_percent <- Reduce(function(...) rbind(..., deparse.level=1), prod_region_country_percent)
write.csv(prod_region_country_percent, paste(work_dir, '/_interactive/_useful_info/prod_regioncrops_to_country_2016_5_13.csv', sep=''), row.names=FALSE)

### Horizontal way

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
prod_region_country_percent <- read.csv(paste(work_dir, '/_interactive/_useful_info/prod_regioncrops_to_country_2016_5_13.csv', sep=''))

library(dplyr)
library(tidyr)

test.ave <- prod_region_country_percent[,c("Country","Region_crops","Average",'Element')]; test.ave <- unique(test.ave); rownames(test.ave) <- 1:nrow(test.ave)
test.ave <- test.ave %>% spread(Element, 'Average')
test.per <- prod_region_country_percent[,c("Country","Region_crops","Percentage",'Element')]; test.per <- unique(test.per); rownames(test.per) <- 1:nrow(test.per)
test.per <- test.per %>% spread(Element, 'Percentage')

colnames(test.ave)[3:ncol(test.ave)] <- c(paste(c('HA', 'PV', 'PQ'), '_value', sep=''))
colnames(test.per)[3:ncol(test.per)] <- c(paste(c('HA', 'PV', 'PQ'), '_percentage', sep=''))

prod_region_country_percent <- merge(test.ave, test.per, by=c('Country','Region_crops'))
rm(test.ave, test.per)

write.csv(prod_region_country_percent, paste(work_dir, '/_interactive/_useful_info/prod_regioncrops_to_country_matrix_2016_5_13.csv', sep=''), row.names=FALSE)






