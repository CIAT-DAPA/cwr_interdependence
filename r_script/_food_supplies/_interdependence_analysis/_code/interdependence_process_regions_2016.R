# Interdependence study
# H. Achicanoy & C. Khoury
# CIAT, 2015

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# INTERDEPENDENCE STUDY - PROCESSING DATA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load data
work_dir <- "C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_regions"
main_data_set <- read.csv(paste(work_dir, "/_inputs/FS_2009-2011mean_mc_fm_selcom_selcoun.csv", sep=""), header=T)
ctry_data_set <- read.csv(paste(work_dir, "/_inputs/Countries_FS_2009-2011_data_regions_2016_1_20.csv", sep=""), header=T)
comm_data_set <- read.csv(paste(work_dir, "/_inputs/FS_Items_regions_2015_4_15.csv", sep=""), header=T)

dim(main_data_set)
dim(ctry_data_set)

comm_data_set <- comm_data_set[,-c(4,5)]
names(comm_data_set) <- c("Item", "food_group", "Region_crops")

# Main data set replace NA's with 0's and delete Items not to use (test0)

test0 <- main_data_set
test0[is.na(test0)] <- 0
test0 <- test0[!(test0$Item=="Animal Products (Total)") & !(test0$Item=="Grand Total") & !(test0$Item=="Vegetal Products (Total)"),]

#write.csv(test0, paste("F:/CIAT/Interdependence/dependence_2009-2011/food_supplies_countries_regions_all.csv",sep=""), row.names=F)
#test0 <- read.csv(paste(work_dir, "/food_supplies_countries_regions_all.csv",sep=""), header=T)

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

elements <- c("Fat_supply_quantity", "Food_supply", "Food_supply_quantity", "Protein_supply_quantity")
mapply(summary_results, elements, FUN=function(x,y){write.csv(x, paste(work_dir, "/_outputs/total_db_summary/Total_summary_db_", y, ".csv", sep=""), row.names=F)})


# Merge information of Main data set and Regions and crops (test1)
test <- merge(main_data_set, ctry_data_set, by=c("Country"))
test1 <- merge(test, comm_data_set, by=c("Item"))
rownames(test1) <- 1:nrow(test1)

test1[is.na(test1)] <- 0

write.csv(test1, paste(work_dir, "/_outputs/food_supplies_countries_regions_all_merge.csv",sep=""), row.names=F)

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
write.csv(test3, paste(work_dir, "/_outputs/food_supplies_countries_regions_dupregionscrops.csv",sep=""), row.names=F)

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

elements <- c("Fat_supply_quantity", "Food_supply", "Food_supply_quantity", "Protein_supply_quantity")
mapply(dependence_results, elements, FUN=function(x,y){write.csv(x, paste(work_dir, "/_outputs/maximum_dependence/total_native/Total_native_", y, ".csv", sep=""), row.names=F)})

## Read db summary
total_db <- list.files(paste(work_dir, "/_outputs/total_db_summary", sep=""), full.names=T)
summary_results <- lapply(total_db, read.csv)
names(summary_results) <- elements
rm(total_db)

country <- sort(as.character(unique(summary_results[[1]]$Country)))

## Read maximum dependence data
max_db <- list.files(paste(work_dir, "/_outputs/maximum_dependence/total_native", sep=""), full.names=T)
dependence_results <- lapply(max_db, read.csv)
names(dependence_results) <- elements
rm(max_db)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
### Calculate maximum dependence
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

max_dependence_results <- list(0)
for(i in 1:length(element)){
  
  for(j in 1:length(country)){
    
    if(country[j] %in% as.character(dependence_results[[i]]$Country)){
      
      native <- dependence_results[[i]][dependence_results[[i]]$Country == country[j],]
      total <- summary_results[[i]][summary_results[[i]]$Country == country[j],]
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

names(max_dependence_results) <- elements
elements <- c("Fat_supply_quantity", "Food_supply", "Food_supply_quantity", "Protein_supply_quantity")
mapply(max_dependence_results, elements, FUN=function(x,y){write.csv(x, paste(work_dir, "/_outputs/maximum_dependence/Maximum_dependence_", y, ".csv", sep=""), row.names=F)})

# Calculate contribution of all commodities where region = "Not_Specified"

test2 <- read.csv(paste(work_dir, "/_outputs/food_supplies_countries_regions_all_merge.csv",sep=""), header=T)

test4 <- test2[test2$Region_crops=="Not_Specified",]
rownames(test4) <- 1:nrow(test4)
write.csv(test4, paste(work_dir, "/_outputs/food_supplies_countries_notspecified.csv",sep=""), row.names=F)

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

elements <- c("Fat_supply_quantity", "Food_supply", "Food_supply_quantity", "Protein_supply_quantity")
mapply(nspecified_results, elements, FUN=function(x,y){write.csv(x, paste(work_dir, "/_outputs/minimum_dependence/total_nspecified/Total_nspecified_", y, ".csv", sep=""), row.names=F)})

# Read Not specified data

nspecified_db <- list.files(paste(work_dir, "/_outputs/minimum_dependence/total_nspecified", sep=""), full.names=T)
nspecified_results <- lapply(nspecified_db, read.csv)
names(nspecified_results) <- elements
rm(nspecified_db)

# Calculate percent of Not_specify

percent_nspecified_results <- list(0)
for(i in 1:length(element)){
  
  for(j in 1:length(country)){
    
    if(country[j] %in% as.character(nspecified_results[[i]]$Country)){
      
      nspecified <- nspecified_results[[i]][nspecified_results[[i]]$Country == country[j],]
      total <- summary_results[[i]][summary_results[[i]]$Country == country[j],]
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

names(percent_nspecified_results) <- elements
elements <- c("Fat_supply_quantity", "Food_supply", "Food_supply_quantity", "Protein_supply_quantity")
mapply(percent_nspecified_results, elements, FUN=function(x,y){write.csv(x, paste(work_dir, "/_outputs/minimum_dependence/percent_nspecified/Nspecified_percent_", y, ".csv", sep=""), row.names=F)})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
### Calculate minimum dependence
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Read Not_specified percent data

nspecified_per_db <- list.files(paste(work_dir, "/_outputs/minimum_dependence/percent_nspecified", sep=""), full.names=T)
percent_nspecified_results <- lapply(nspecified_per_db, read.csv)
names(percent_nspecified_results) <- elements
rm(nspecified_per_db)

# Calculate percent of Not_specify

min_dependence_results <- list(0)
for(i in 1:length(element)){
  
  for(j in 1:length(country)){
    
    if(country[j] %in% as.character(percent_nspecified_results[[i]]$Country)){
      
      nspecified <- percent_nspecified_results[[i]][percent_nspecified_results[[i]]$Country == country[j],]
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

names(min_dependence_results) <- elements
elements <- c("Fat_supply_quantity", "Food_supply", "Food_supply_quantity", "Protein_supply_quantity")
mapply(min_dependence_results, elements, FUN=function(x,y){write.csv(x, paste(work_dir, "/_outputs/minimum_dependence/Minimum_dependence_", y, ".csv", sep=""), row.names=F)})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Put maximum and minimum together
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

max_dir <- paste(work_dir, "/_outputs/maximum_dependence", sep='')
min_dir <- paste(work_dir, "/_outputs/minimum_dependence", sep='')

labels <- list.files(max_dir, pattern=".csv")
labels <- gsub(pattern="Maximum_dependence_", replacement="", labels)
labels <- gsub(pattern=".csv", replacement="", labels)

max_dep_files <- list.files(max_dir, pattern=".csv", full.names=T)
min_dep_files <- list.files(min_dir, pattern=".csv", full.names=T)

max_dep_files <- lapply(max_dep_files, read.csv); lapply(max_dep_files, dim)
min_dep_files <- lapply(min_dep_files, read.csv); lapply(min_dep_files, dim)

for(i in 1:length(max_dep_files))
{
  names(max_dep_files[[i]])[1:3] <- paste0("max_",names(max_dep_files[[i]])[1:3])
  names(min_dep_files[[i]])[1:3] <- paste0("min_",names(min_dep_files[[i]])[1:3])
}; rm(i)

combined <- lapply(1:length(max_dep_files), function(i){x <- max_dep_files[[i]]; y <- min_dep_files[[i]]; z <- merge(x,y,by="Country"); return(z)})
mapply(combined, labels, FUN=function(x,y){write.csv(x, paste(work_dir, "/_outputs/combined_dependence/combined_dependence_", y, ".csv", sep=""), row.names=F)})
