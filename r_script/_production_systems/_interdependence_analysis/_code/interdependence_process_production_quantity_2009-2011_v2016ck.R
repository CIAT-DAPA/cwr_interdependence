# Interdependence study
# H. Achicanoy & C. Khoury
# CIAT, 2014

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# INTERDEPENDENCE STUDY - PROCESSING DATA - Production quantity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load data
work_dir <- "D:/Tobackup/_hmcfw/PGR_interdependence/analysis/interdependence_circos/_production_systems/_interdependence_analysis"
main_data_set <- read.csv(paste(work_dir, "/_inputs/quantity_2009-2011_mean_mc_fm_selcom_selcoun.csv", sep=""), header=T)
ctry_data_set <- read.csv(paste(work_dir, "/_inputs/Countries_FS_2009-2011_data_regions_2016_1_20.csv", sep=""), header=T)
comm_data_set <- read.csv(paste(work_dir, "/_inputs/Item_region_production_2016_2_8.csv", sep=''), header=T)

dim(main_data_set)
dim(ctry_data_set)

comm_data_set <- comm_data_set[,c("Item_final", "Region")]
names(comm_data_set) <- c("Item", "Region_crops")

# Main data set replace NA's with 0's and delete Items not to use (test0)

test0 <- main_data_set
test0[is.na(test0)] <- 0
test0 <- test0[!(test0$Item=="Animal Products (Total)") & !(test0$Item=="Grand Total") & !(test0$Item=="Vegetal Products (Total)"),]

element <- sort(as.character(unique(test0$Element)))
country <- sort(as.character(unique(test0$Country)))

summary_results <- list(0)
for(i in 1:length(element)){
  
  for(j in 1:length(country)){
    
    data <- test0[which(test0$Element==element[i] & test0$Country==country[j]),]
    data <- data[, paste("Y", 2009:2011, sep="")]
    sumData <- colSums(data)
    sumData <- as.data.frame(t(sumData))
    sumData$Country <- paste(country[j])
    sumData$Element <- paste(element[i])
    
    if(j==1){
      db_summary <- sumData
    } else {
      db_summary <- rbind(db_summary, sumData)
    }
    
  }
  
  summary_results[[i]] <- db_summary
  
}

write.csv(summary_results[[1]], paste(work_dir, "/_outputs/by_quantity/results_dependence/Total_summary_db.csv", sep=""), row.names=F)

# Merge information of Main data set and Regions and crops (test1)
test <- merge(main_data_set, ctry_data_set, by=c("Country"))
test1 <- merge(test, comm_data_set, by=c("Item"))
rownames(test1) <- 1:nrow(test1)

test1[is.na(test1)] <- 0

write.csv(test1, paste(work_dir, "/_outputs/by_quantity/prod_countries_regions_all_merge.csv",sep=""), row.names=F)

# Remove data not for use (test2)

test2 <- test1[!(test1$Region_crops=="DO NOT INCLUDE"),]
#write.csv(test2, paste(work_dir, "/food_supplies_countries_regions_items4analysis.csv",sep=""), row.names=F)

rownames(test2) <- 1:nrow(test2)

## by country/commodity
# country, region = region_crops (Done)
# element
# sum of crops native to country/region

# Identify crop quantities native to country (test3)

test2$Region <- as.character(test2$Region)
test2$Region_crops <- as.character(test2$Region_crops)

test3 <- test2[which(test2$Region==test2$Region_crops),]
rownames(test3) <- 1:nrow(test3)
write.csv(test3, paste(work_dir, "/_outputs/by_quantity/prod_countries_regions_dupregionscrops.csv",sep=""), row.names=F)

# Calculate sums of crop quantities native to country

element <- sort(as.character(unique(test3$Element)))
country <- sort(as.character(unique(test3$Country)))
item <- sort(as.character(unique(test3$Item)))

dependence_results <- list(0)
for(i in 1:length(element)){
  
  for(j in 1:length(country)){
    
    total <- test3[test3$Country==country[j] & test3$Element==element[i],]
    rownames(total) <- 1:nrow(total)
    items <- sort(as.character(unique(total$Item)))
    mtch <- match(items, total$Item)
    total <- total[mtch,]
    rownames(total) <- 1:nrow(total)
    total <- total[paste("Y",2009:2011,sep="")]
    total <- colSums(total)
    total <- as.data.frame(t(total))
    total$Country <- paste(country[j])
    total$Element <- paste(element[i])
    
    if(j==1){
      db_results <- total
    } else {
      db_results <- rbind(db_results, total)
    }
    
  }
  
  dependence_results[[i]] <- db_results
  
}

write.csv(dependence_results[[1]], paste(work_dir, "/_outputs/by_quantity/results_dependence/Total_native.csv", sep=""), row.names=F)

## Read db summary
summary_results <- read.csv(paste(work_dir, "/_outputs/by_quantity/results_dependence/Total_summary_db.csv", sep=""))
country <- sort(as.character(unique(summary_results$Country)))

## Read maximum dependence data
dependence_results <- read.csv(paste(work_dir, "/_outputs/by_quantity/results_dependence/Total_native.csv", sep=""))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
### Calculate maximum dependence
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

max_dependence_results <- list(0)
for(i in 1:length(element)){
  
  for(j in 1:length(country)){
    
    if(country[j] %in% as.character(dependence_results$Country)){
      
      native <- dependence_results[which(dependence_results$Country == country[j]),]
      total <- summary_results[summary_results$Country == country[j],]
      native <- native[paste("Y",2009:2011,sep="")]
      total <- total[paste("Y",2009:2011,sep="")]
      
      max_dependence <- 1-(native/total)
      max_dependence <- as.data.frame(max_dependence)
      
    } else {
      
      max_dependence <- rep(1, 3)
      names(max_dependence) <- paste("Y",2009:2011,sep="")
      max_dependence <- as.data.frame(t(max_dependence))
      
    }
    
    max_dependence$Country <- paste(country[j])
    
    if(j==1){
      db_max_dependence <- max_dependence
    } else {
      db_max_dependence <- rbind(db_max_dependence, max_dependence)
    }
    
  }
  
  max_dependence_results[[i]] <- db_max_dependence
  
}

write.csv(max_dependence_results[[1]], paste(work_dir, "/_outputs/by_quantity/results_dependence/Maximum_dependence.csv", sep=""), row.names=F)

# Calculate contribution of all commodities where region = "Not_Specified"

test2 <- read.csv(paste(work_dir, "/_outputs/by_quantity/prod_countries_regions_all_merge.csv",sep=""), header=T)

test4 <- test2[test2$Region_crops=="Not_Specified",]
rownames(test4) <- 1:nrow(test4)
write.csv(test4, paste(work_dir, "/_outputs/by_quantity/prod_countries_notspecified.csv",sep=""), row.names=F)

nspecified_results <- list(0)
for(i in 1:length(element)){
  
  for(j in 1:length(country)){
    
    total_ns <- test2[test2$Region_crops=="Not_Specified" & test2$Country==country[j] & test2$Element==element[i],]
    rownames(total_ns) <- 1:nrow(total_ns)
    items <- sort(as.character(unique(total_ns$Item)))
    mtch <- match(items, total_ns$Item)
    total_ns <- total_ns[mtch,]
    rownames(total_ns) <- 1:nrow(total_ns)
    total_ns <- total_ns[paste("Y",2009:2011,sep="")]
    total_ns <- colSums(total_ns)
    total_ns <- as.data.frame(t(total_ns))
    total_ns$Country <- paste(country[j])
    total_ns$Element <- paste(element[i])
    
    if(j==1){
      db_nspecified <- total_ns
    } else {
      db_nspecified <- rbind(db_nspecified, total_ns)
    }
    
  }
  
  nspecified_results[[i]] <- db_nspecified
  
}

write.csv(nspecified_results[[1]], paste(work_dir, "/_outputs/by_quantity/results_dependence/Total_nspecified.csv", sep=""), row.names=F)

# Read Not specified data

nspecified_results <- read.csv(paste(work_dir, "/_outputs/by_quantity/results_dependence/Total_nspecified.csv", sep=""))

# Calculate percent of Not_specify

percent_nspecified_results <- list(0)
for(i in 1:length(element)){
  
  for(j in 1:length(country)){
    
    if(country[j] %in% as.character(nspecified_results$Country)){
      
      nspecified <- nspecified_results[nspecified_results$Country == country[j],]
      total <- summary_results[summary_results$Country == country[j],]
      nspecified <- nspecified[paste("Y",2009:2011,sep="")]
      total <- total[paste("Y",2009:2011,sep="")]
      
      per_nspecified <- nspecified/total
      per_nspecified <- as.data.frame(per_nspecified)
      
    } else {
      
      per_nspecified <- rep(1, 3)
      names(per_nspecified) <- paste("Y",2009:2011,sep="")
      per_nspecified <- as.data.frame(t(per_nspecified))
      
    }
    
    per_nspecified$Country <- paste(country[j])
    
    if(j==1){
      db_per_nspecified <- per_nspecified
    } else {
      db_per_nspecified <- rbind(db_per_nspecified, per_nspecified)
    }
    
  }
  
  percent_nspecified_results[[i]] <- db_per_nspecified
  
}

write.csv(percent_nspecified_results[[1]], paste(work_dir, "/_outputs/by_quantity/results_dependence/Nspecified_percent.csv", sep=""), row.names=F)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
### Calculate minimum dependence
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Read Not_specified percent data

percent_nspecified_results <- read.csv(paste(work_dir, "/_outputs/by_quantity/results_dependence/Nspecified_percent.csv", sep=""))

# Calculate percent of Not_specify

min_dependence_results <- list(0)
for(i in 1:length(element)){
  
  for(j in 1:length(country)){
    
    if(country[j] %in% as.character(percent_nspecified_results$Country)){
      
      nspecified <- percent_nspecified_results[percent_nspecified_results$Country == country[j],]
      max_dependence <- max_dependence_results[[i]][max_dependence_results[[i]]$Country == country[j],]
      nspecified <- nspecified[paste("Y",2009:2011,sep="")]
      max_dependence <- max_dependence[paste("Y",2009:2011,sep="")]
      
      min_dependence <- max_dependence-nspecified
      min_dependence <- as.data.frame(min_dependence)
      
    }
    
    min_dependence$Country <- paste(country[j])
    
    if(j==1){
      db_min_dependence <- min_dependence
    } else {
      db_min_dependence <- rbind(db_min_dependence, min_dependence)
    }
    
  }
  
  min_dependence_results[[i]] <- db_min_dependence
  
}

write.csv(min_dependence_results[[1]], paste(work_dir, "/_outputs/by_quantity/results_dependence/Minimum_dependence.csv", sep=""), row.names=F)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Put maximum and minimum together
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

max_dep_files <- read.csv(paste(work_dir, "/_outputs/by_quantity/results_dependence/Maximum_dependence.csv", sep=""))
min_dep_files <- read.csv(paste(work_dir, "/_outputs/by_quantity/results_dependence/Minimum_dependence.csv", sep=""))

names(max_dep_files)[1:3] <- paste0("max_",names(max_dep_files)[1:3])
names(min_dep_files)[1:3] <- paste0("min_",names(min_dep_files)[1:3])

combined <- merge(max_dep_files, min_dep_files, by="Country")

write.csv(combined, paste(work_dir, "/_outputs/by_quantity/results_dependence/combined_dependence_prodqty.csv", sep=''), row.names=F)
