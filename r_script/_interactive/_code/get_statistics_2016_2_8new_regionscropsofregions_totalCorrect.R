# Get statistics from region to country
# H. Achicanoy & C. Khoury
# CIAT, 2016

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Food supplies
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

fs_region_country <- read.csv(paste(work_dir, '/_regions/_outputs/circos/FS/regions_sourceofdiets_weighted.csv', sep=''))
# fs_region_country$Average <- as.vector(rowMeans(fs_region_country[,paste('Y',2009:2011,sep='')]))
fs_region_country$Average <- round(fs_region_country$Average, 2)

correct_total <- read.csv(paste(work_dir, '/_interactive/_useful_info/fs_average_diet_regions.csv', sep=''))
correct_total <- as.data.frame(dplyr::summarise(group_by(correct_total, Region, Element), sum(Average)))
colnames(correct_total)[3] <- 'Average'
correct_total$Average <- round(correct_total$Average, 2)

elements <- sort(as.character(unique(fs_region_country$Element)))
fs_region_country_percent <- lapply(1:length(elements), function(i)
{
  fs_data <- subset(fs_region_country, fs_region_country$Element==elements[i])
  fs_data <- fs_data[,c("Region", "Region_crops", "Average")]
  rownames(fs_data) <- 1:nrow(fs_data)
  countries <- sort(as.character(unique(fs_data$Region)))
  countriesFiles <- lapply(1:length(countries), function(j)
  {
    fs_country <- subset(fs_data, fs_data$Region==countries[j]); rownames(fs_country) <- 1:nrow(fs_country)
    fs_country <- fs_country[order(fs_country$Average, decreasing=TRUE),]; rownames(fs_country) <- 1:nrow(fs_country)
    fs_country$Percentage <- fs_country$Average/correct_total$Average[correct_total$Region==countries[j] & correct_total$Element==elements[i]]
    return(fs_country)
  })
  countriesFiles <- Reduce(function(...) rbind(..., deparse.level=1), countriesFiles)
  countriesFiles$Element <- paste(elements[i])
  
  return(countriesFiles)
})
fs_region_country_percent <- Reduce(function(...) rbind(..., deparse.level=1), fs_region_country_percent)
write.csv(fs_region_country_percent, paste(work_dir, '/_interactive/_useful_info/fs_regioncrops_to_region_2016_5_17.csv', sep=''), row.names=FALSE)

### Horizontal way

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
fs_region_country_percent <- read.csv(paste(work_dir, '/_interactive/_useful_info/fs_regioncrops_to_region_2016_5_17.csv', sep=''))

library(dplyr)
library(tidyr)

test.ave <- fs_region_country_percent[,c("Region","Region_crops","Average",'Element')]; test.ave <- unique(test.ave); rownames(test.ave) <- 1:nrow(test.ave)
test.ave <- test.ave %>% spread(Element, 'Average')
test.per <- fs_region_country_percent[,c("Region","Region_crops","Percentage",'Element')]; test.per <- unique(test.per); rownames(test.per) <- 1:nrow(test.per)
test.per <- test.per %>% spread(Element, 'Percentage')

colnames(test.ave)[3:ncol(test.ave)] <- c(paste(c('Fat', 'Calories', 'Food_weight', 'Protein'), '_value', sep=''))
colnames(test.per)[3:ncol(test.per)] <- c(paste(c('Fat', 'Calories', 'Food_weight', 'Protein'), '_percentage', sep=''))

fs_region_country_percent <- merge(test.ave, test.per, by=c('Region','Region_crops'))
rm(test.ave, test.per)

write.csv(fs_region_country_percent, paste(work_dir, '/_interactive/_useful_info/fs_regioncrops_to_region_matrix_2016_5_17.csv', sep=''), row.names=FALSE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Production systems
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

g=gc(); rm(list=ls())

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'

fs_region_country <- read.csv(paste(work_dir, '/_production_systems/_circos/Prod/regions_sourceofprod_weighted.csv', sep=''))
fs_region_country$Average <- round(fs_region_country$Average, 2)

library(dplyr)

correct_total <- read.csv(paste(work_dir, '/_interactive/_useful_info/prod_sum_regions.csv', sep=''))
correct_total <- as.data.frame(dplyr::summarise(group_by(correct_total, Region, Element), sum(Average)))
colnames(correct_total)[3] <- 'Average'
correct_total$Average <- round(correct_total$Average, 2)

elements <- sort(as.character(unique(fs_region_country$Element)))
fs_region_country_percent <- lapply(1:length(elements), function(i)
{
  fs_data <- subset(fs_region_country, fs_region_country$Element==elements[i])
  fs_data <- fs_data[,c("Region", "Region_crops", "Average")]
  rownames(fs_data) <- 1:nrow(fs_data)
  countries <- sort(as.character(unique(fs_data$Region)))
  countriesFiles <- lapply(1:length(countries), function(j)
  {
    fs_country <- subset(fs_data, fs_data$Region==countries[j]); rownames(fs_country) <- 1:nrow(fs_country)
    fs_country <- fs_country[order(fs_country$Average, decreasing=TRUE),]; rownames(fs_country) <- 1:nrow(fs_country)
    if(elements[i] == 'Area harvested'){
      fs_country$Percentage <- fs_country$Average/correct_total$Average[correct_total$Region==countries[j] & correct_total$Element=='Area harvested']
    } else {
      if(elements[i] == 'Gross Production Value (current million US$)'){
        fs_country$Percentage <- fs_country$Average/correct_total$Average[correct_total$Region==countries[j] & correct_total$Element=='Gross Production Value (current million US$)']
      } else {
        if(elements[i] == 'Production'){
          fs_country$Percentage <- fs_country$Average/correct_total$Average[correct_total$Region==countries[j] & correct_total$Element=='Production']
        }
      }
    }
    return(fs_country)
  })
  countriesFiles <- Reduce(function(...) rbind(..., deparse.level=1), countriesFiles)
  countriesFiles$Element <- paste(elements[i])
  
  return(countriesFiles)
})
fs_region_country_percent <- Reduce(function(...) rbind(..., deparse.level=1), fs_region_country_percent)
write.csv(fs_region_country_percent, paste(work_dir, '/_interactive/_useful_info/prod_regioncrops_to_region_2016_5_17.csv', sep=''), row.names=FALSE)

### Horizontal way

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
fs_region_country_percent <- read.csv(paste(work_dir, '/_interactive/_useful_info/prod_regioncrops_to_region_2016_5_17.csv', sep=''))

library(dplyr)
library(tidyr)

test.ave <- fs_region_country_percent[,c("Region","Region_crops","Average",'Element')]; test.ave <- unique(test.ave); rownames(test.ave) <- 1:nrow(test.ave)
test.ave <- test.ave %>% spread(Element, 'Average')
test.per <- fs_region_country_percent[,c("Region","Region_crops","Percentage",'Element')]; test.per <- unique(test.per); rownames(test.per) <- 1:nrow(test.per)
test.per <- test.per %>% spread(Element, 'Percentage')

colnames(test.ave)[3:ncol(test.ave)] <- c(paste(c('HA', 'PV', 'PQ'), '_value', sep=''))
colnames(test.per)[3:ncol(test.per)] <- c(paste(c('HA', 'PV', 'PQ'), '_percentage', sep=''))

fs_region_country_percent <- merge(test.ave, test.per, by=c('Region','Region_crops'))
rm(test.ave, test.per)

write.csv(fs_region_country_percent, paste(work_dir, '/_interactive/_useful_info/prod_regioncrops_to_region_matrix_2016_5_17.csv', sep=''), row.names=FALSE)





