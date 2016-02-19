# Matrix to JSON format using R: Extract top 5 of important crops by flow - Food supplies
# H. Achicanoy & C. Khoury
# CIAT, 2016

# Loading information
work_dir <-'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
all_elements <- read.csv(paste(work_dir, '/_regions/_outputs/circos/FS/fs_sum_items_region.csv', sep=''))
####
region_crops <- read.csv(paste(work_dir, "/_regions/_outputs/food_supplies_countries_regions_all_merge.csv", sep=''))
region_crops <- region_crops[,c('Item','Region_crops')]
region_crops <- unique(region_crops); rownames(region_crops) <- 1:nrow(region_crops)
####
all_elements <- merge(all_elements, region_crops, by='Item')
all_elements <- unique(all_elements); rownames(all_elements) <- 1:nrow(all_elements); rm(region_crops)
all_elements <- all_elements[,c('Element','Item','Average','Region_crops','Region')]
names(all_elements)[4:5] <- c('R_origin', 'R_recipients')
all_elements <- all_elements[all_elements$R_origin!='Not_Specified',]; all_elements <- all_elements[all_elements$R_recipients!='Not_Specified',]

# All posible flows
regions <- sort(unique(as.character(all_elements$R_recipients)))
all.combinations <- as.data.frame(expand.grid(regions, regions))
colnames(all.combinations) <- c('R_origin', 'R_recipients')
all.combinations$R_origin <- as.character(all.combinations$R_origin)
all.combinations$R_recipients <- as.character(all.combinations$R_recipients)

# Extract most important crops by flow
fmeas <- sort(unique(as.character(all_elements$Element)))
top5_element <- lapply(1:length(fmeas), function(i)
{
  # Subsetting by measure element
  measData <- all_elements[which(all_elements$Element==fmeas[[i]]),]; rownames(measData) <- 1:nrow(measData)
  
  library(dplyr)
  empty.combinations <- measData[,c('R_origin', 'R_recipients')]; empty.combinations <- unique(empty.combinations); rownames(empty.combinations) <- 1:nrow(empty.combinations)
  empty.combinations$R_origin <- as.character(empty.combinations$R_origin)
  empty.combinations$R_recipients <- as.character(empty.combinations$R_recipients)
  empty.combinations <- anti_join(all.combinations, empty.combinations)
  
  empty.combinations$Element <- as.character(unique(measData$Element))
  empty.combinations$Item <- 'NA'
  empty.combinations$Average <- 'NA'
  
  measData <- rbind(measData, empty.combinations); rm(empty.combinations)
  measData$Average <- as.numeric(measData$Average)
  rownames(measData) <- 1:nrow(measData)
  
  top5 <- lapply(1:nrow(all.combinations), function(j)
  {
    subData <- measData[measData$R_origin==all.combinations$R_origin[j] & measData$R_recipients==all.combinations$R_recipients[j],]
    subData <- subData[order(subData$Average, decreasing=TRUE),]; rownames(subData) <- 1:nrow(subData)
    
    if(nrow(subData)>1) # nrow(subData)!=0
    {
      if(nrow(subData)>5){
        subData <- subData[subData$Average>=1,] # subData$Average>0
        if(nrow(subData)!=0){
          subData <- subData[1:5,]; subData <- subData[complete.cases(subData),]
          rownames(subData) <- 1:nrow(subData)
        } else {
          subData <- data.frame(Element=measData$Element[i], Item=NA, Average=NA, R_origin=all.combinations$R_origin[j],  R_recipients=all.combinations$R_recipients[j])
        }
      } else {
        subData <- subData[subData$Average>=1,] # subData$Average>0
        if(nrow(subData)!=0){
          subData <- subData[1:5,]; subData <- subData[complete.cases(subData),]
          rownames(subData) <- 1:nrow(subData)
        } else {
          subData <- data.frame(Element=measData$Element[i], Item=NA, Average=NA, R_origin=all.combinations$R_origin[j],  R_recipients=all.combinations$R_recipients[j])
        }
      }
      return(subData)
    } else {
      cat('Combination from origin region:', as.character(all.combinations$R_origin[j]), 'to recipient region:', as.character(all.combinations$R_recipients[j]), 'has not flows\n')
      subData <- subData
      return(subData)
    }
  })
  top5 <- Reduce(function(...) rbind(..., deparse.level=1), top5)
  rownames(top5) <- 1:nrow(top5)
  return(top5)
})
top5_element <- Reduce(function(...) rbind(..., deparse.level=1), top5_element)

write.csv(top5_element, paste(work_dir, '/_interactive/_useful_info/most_important_fs_crop_flows_weighted.csv', sep=''), row.names=FALSE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Food supplies without hierarchical level
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

options(warn=-1)
library(jsonlite)

### Load flow matrix

# Matrix should have macro regions and regions. All flows should be in the same table.
work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
flows <- list.files(path=paste(work_dir, '/_interactive/_flows_matrix/fs_without_hierarchical_level', sep=''), pattern='*_weighted.csv$', full.names=TRUE)
flows <- flows[c(1,4,2,3)]
flowsFiles <- lapply(flows, function(x)
{
  z <- read.csv(x); rownames(z) <- z[,1]; z <- z[,-1]
  colnames(z) <- rownames(z)
  z <- round(z, 1)
  z2 <- z
  rownames(z2) <- paste(rownames(z2), '_rep', sep='')
  colnames(z2) <- rownames(z2)
  pos <- seq(1, 46, 2)
  pos2 <- seq(2, 46, 2)
  zFinal <- matrix(data=NA, nrow=46, ncol=46); zFinal <- as.data.frame(zFinal)
  rownames(zFinal)[pos] <- colnames(zFinal)[pos] <- rownames(z)
  rownames(zFinal)[pos2] <- colnames(zFinal)[pos2] <- rownames(z2)
  zFinal[pos, pos] <- z
  zFinal[pos2, pos2] <- z2
  zFinal[pos, pos2] <- z
  zFinal[pos2, pos] <- z
  return(zFinal)
})

### Support information files

# Data Frame with all posible combinations

all_elements <- read.csv(paste(work_dir, '/_regions/_outputs/circos/FS/fs_sum_items_region.csv', sep=''))
region_crops <- read.csv(paste(work_dir, "/_regions/_outputs/food_supplies_countries_regions_all_merge.csv", sep=''))
region_crops <- region_crops[,c('Item','Region_crops')]
region_crops <- unique(region_crops); rownames(region_crops) <- 1:nrow(region_crops)
all_elements <- merge(all_elements, region_crops, by='Item')
all_elements <- unique(all_elements); rownames(all_elements) <- 1:nrow(all_elements); rm(region_crops)
all_elements <- all_elements[,c('Element','Item','Average','Region_crops','Region')]
all_elements <- all_elements[all_elements$Region_crops!='Not_Specified',]; all_elements <- all_elements[all_elements$Region!='Not_Specified',]
all_elements <- all_elements[,c('Region_crops','Region')]
colnames(all_elements) <- c('R_origin', 'R_recipients')
regions <- sort(unique(as.character(all_elements$R_recipients)))
all.combinations <- as.data.frame(expand.grid(regions, regions)); colnames(all.combinations) <- c('R_origin', 'R_recipients')
rownames(all.combinations) <- 1:nrow(all.combinations)
all.combinations <- data.frame(R_origin=all.combinations$R_origin, R_recipients=all.combinations$R_recipients)
all.combinations$R_origin <- as.character(all.combinations$R_origin)
all.combinations$R_recipients <- as.character(all.combinations$R_recipients)
rm(all_elements, regions)

### Load information by flow and bring list format

top5_info <- read.csv(paste(work_dir, '/_interactive/_useful_info/most_important_fs_crop_flows_weighted.csv', sep=''))
top5_info <- top5_info[top5_info$R_origin!='Not_Specified',]; top5_info <- top5_info[top5_info$R_recipients!='Not_Specified',]
top5_info$Item <- tolower(gsub(pattern='&', replacement='and', x=gsub(pattern=',', replacement='', x=gsub(pattern=' ', replacement='_', x=top5_info$Item))))

### Fix crop list

cropList <- sort(as.character(unique(top5_info$Item)))
cropList <- tolower(gsub(pattern='&', replacement='and', x=gsub(pattern=',', replacement='', x=gsub(pattern=' ', replacement='_', x=cropList))))
cropListDF <- data.frame(crop=cropList, position=1:length(cropList))

elements <- sort(as.character(unique(top5_info$Element)))
elements <- elements[c(2, 4, 1, 3)]
top5_info <- lapply(1:length(elements), function(i)
{
  # Subsetting by element
  sub_data <- top5_info[top5_info$Element==elements[[i]],]; rownames(sub_data) <- 1:nrow(sub_data)
  combinations <- sub_data[,c('R_origin', 'R_recipients')]; combinations <- unique(combinations)
  combinations$R_origin <- as.character(combinations$R_origin); combinations$R_recipients <- as.character(combinations$R_recipients)
  rownames(combinations) <- 1:nrow(combinations)
  
  # Completing all flows
  library(dplyr)
  if(dim(combinations)[1] < 529){
    all.combinations <- anti_join(all.combinations, combinations)
    all.combinations$Element <- unique(as.character(sub_data$Element))
    all.combinations$Item <- NA
    all.combinations$Average <- NA
    all.combinations <- rbind(sub_data, all.combinations); rm(combinations, sub_data)
    rownames(all.combinations) <- 1:nrow(all.combinations)
  } else {
    all.combinations <- sub_data
  }
  
  # Fix regions names
  all.combinations$R_origin <- gsub(pattern='^America_North$', replacement='N\nAmerica', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^America_Central_and_Mexico$', replacement='C\nAmerica', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^America_Caribbean$', replacement='Caribbean', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^America_South_andean$', replacement='Andes', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^America_South_tropical$', replacement='Trop. S\nAmerica', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^America_South_temperate$', replacement='Temp. S\nAmerica', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Africa_West$', replacement='W\nAfrica', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Africa_Central$', replacement='C\nAfrica', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Africa_East$', replacement='E\nAfrica', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Africa_Southern$', replacement='S\nAfrica', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Africa_Indian_Ocean_Islands$', replacement='IOI', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Europe_Western_north$', replacement='NW\nEurope', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Europe_Western_south$', replacement='SW\nEurope', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Europe_Eastern_north$', replacement='NE\nEurope', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Europe_Eastern_south$', replacement='SE\nEurope', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Mediterranean_SouthandEast$', replacement='SE\nMediterranean', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Asia_West$', replacement='W\nAsia', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Asia_Central$', replacement='C\nAsia', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Asia_South$', replacement='S\nAsia', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Asia_East$', replacement='E\nAsia', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Asia_Southeast$', replacement='SE\nAsia', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Pacific_Region_tropical$', replacement='Pacific', x=all.combinations$R_origin)
  all.combinations$R_origin <- gsub(pattern='^Pacific_Region_ausnz$', replacement='ANZ', x=all.combinations$R_origin)
  
  all.combinations$R_recipients <- gsub(pattern='^America_North$', replacement='N\nAmerica', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^America_Central_and_Mexico$', replacement='C\nAmerica', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^America_Caribbean$', replacement='Caribbean', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^America_South_andean$', replacement='Andes', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^America_South_tropical$', replacement='Trop. S\nAmerica', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^America_South_temperate$', replacement='Temp. S\nAmerica', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Africa_West$', replacement='W\nAfrica', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Africa_Central$', replacement='C\nAfrica', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Africa_East$', replacement='E\nAfrica', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Africa_Southern$', replacement='S\nAfrica', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Africa_Indian_Ocean_Islands$', replacement='IOI', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Europe_Western_north$', replacement='NW\nEurope', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Europe_Western_south$', replacement='SW\nEurope', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Europe_Eastern_north$', replacement='NE\nEurope', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Europe_Eastern_south$', replacement='SE\nEurope', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Mediterranean_SouthandEast$', replacement='SE\nMediterranean', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Asia_West$', replacement='W\nAsia', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Asia_Central$', replacement='C\nAsia', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Asia_South$', replacement='S\nAsia', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Asia_East$', replacement='E\nAsia', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Asia_Southeast$', replacement='SE\nAsia', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Pacific_Region_tropical$', replacement='Pacific', x=all.combinations$R_recipients)
  all.combinations$R_recipients <- gsub(pattern='^Pacific_Region_ausnz$', replacement='ANZ', x=all.combinations$R_recipients)
  
  # All posible flows at matrix
  relationDF <- read.csv(flows[1], row.names=1); colnames(relationDF) <- rownames(relationDF)
  #relationDF <- as.data.frame(expand.grid(colnames(relationDF), rownames(relationDF)))
  #relationDF <- relationDF[,c(2,1)]; colnames(relationDF) <- c('R_origin', 'R_recipients')
  
  # rep(x, each=2)
  
  rowStatsFlow <- lapply(1:length(rownames(relationDF)), function(m)
  {
    colStatsFlow <- lapply(1:length(rownames(relationDF)), function(n)
    {
      # Subsetting by flow
      flow <- all.combinations[all.combinations$R_origin==rownames(relationDF)[m] & all.combinations$R_recipients==rownames(relationDF)[n],]
      flow <- unique(flow)
      flow <- flow[order(flow$Average, decreasing=TRUE),]; rownames(flow) <- 1:nrow(flow)
      
      values <- flow$Average; positions <- as.character(flow$Item)
      if(length(values)==1){
        if(sum(is.na(values))>0){values <- ''}
        if(sum(is.na(positions))){positions <- ''} else {
          grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')
          positions <- grep2(pattern=paste('^', positions, '$', sep=''), x=cropList)
          positions <- positions - 1
        }
      } else {
        grep2 <- Vectorize(FUN=grep, vectorize.args='pattern')
        positions <- grep2(pattern=paste('^', positions, '$', sep=''), x=cropList) # match(positions, cropList)
        positions <- positions - 1
      }
      
      if(is.numeric(values)){
        if(elements[[i]]=="Food supply (kcal/capita/day)"){
          metrics = 0
        } else {
          metrics = 1
        }
        flowInfo <- list(values = round(values,1),
                         positions = positions,
                         metrics = rep(metrics, length(positions)))
      } else {
        if(elements[[i]]=="Food supply (kcal/capita/day)"){
          metrics = 0
        } else {
          metrics = 1
        }
        flowInfo <- list(values = values,
                         positions = positions,
                         metrics = rep(metrics, length(positions)))
      }
      return(flowInfo)
    })
    
    colStatsFlow <- rep(colStatsFlow, each=2)
    return(colStatsFlow)
    
  })
  
  rowStatsFlow <- rep(rowStatsFlow, each=2)
  return(rowStatsFlow)
  
})

rm(all.combinations, elements)


### Define elements to construct JSON file

# {description} for JSON file
library(qdap)

work_dir <-'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
text_to_website <- read.transcript(file=paste(work_dir, "/_interactive/_useful_info/Interdependence-Regionstext.docx", sep=""))
colnames(text_to_website) <- c('field', 'text')
grep2 <- Vectorize(grep, vectorize.args='pattern')
mtch <- unlist(grep2(pattern=c('^Food supplies*'), text_to_website$field))
text_to_website <- text_to_website[mtch,]

description <- c(title   = text_to_website$text[1],
                 summary = text_to_website$text[2],
                 content = text_to_website$text[3])

# {labels} for JSON file
crop.labels <- cropList

# {names} for JSON file
mat.labels <- rownames(flowsFiles[[1]])

# {matrix} for JSON file
matrices <- lapply(flowsFiles, as.matrix)
names(matrices) <- c('calories','protein','fat','food_weight')

# {regions} for JSON file
regions <- setdiff(1:length(mat.labels),grep(pattern='*_rep$', x=mat.labels)) - 1

# Redo {names} for JSON file
mat.labels <- gsub(pattern='*_rep$', replacement='', x=mat.labels)
mat.labels <- gsub(pattern='^N\nAmerica$', replacement='N Amer', x=mat.labels)
mat.labels <- gsub(pattern='^C\nAmerica$', replacement='C Amer', x=mat.labels)
mat.labels <- gsub(pattern='^Caribbean$', replacement='Carib', x=mat.labels)
mat.labels <- gsub(pattern='^Trop. S.\nAmerica$', replacement='Trop S Amer', x=mat.labels)
mat.labels <- gsub(pattern='^Temp. S.\nAmerica$', replacement='Temp S Amer', x=mat.labels)
mat.labels <- gsub(pattern='^W\nAfrica$', replacement='W Africa', x=mat.labels)
mat.labels <- gsub(pattern='^C\nAfrica$', replacement='C Africa', x=mat.labels)
mat.labels <- gsub(pattern='^E\nAfrica$', replacement='E Africa', x=mat.labels)
mat.labels <- gsub(pattern='^S\nAfrica$', replacement='S Africa', x=mat.labels)
mat.labels <- gsub(pattern='^NW\nEurope$', replacement='NW Eur', x=mat.labels)
mat.labels <- gsub(pattern='^SW\nEurope$', replacement='SW Eur', x=mat.labels)
mat.labels <- gsub(pattern='^NE\nEurope$', replacement='NE Eur', x=mat.labels)
mat.labels <- gsub(pattern='^SE\nEurope$', replacement='SE Eur', x=mat.labels)
mat.labels <- gsub(pattern='^SE\nMediterranean$', replacement='SE Med', x=mat.labels)
mat.labels <- gsub(pattern='^W\nAsia$', replacement='W Asia', x=mat.labels)
mat.labels <- gsub(pattern='^C\nAsia$', replacement='C Asia', x=mat.labels)
mat.labels <- gsub(pattern='^S\nAsia$', replacement='S Asia', x=mat.labels)
mat.labels <- gsub(pattern='^E\nAsia$', replacement='E Asia', x=mat.labels)
mat.labels <- gsub(pattern='^SE\nAsia$', replacement='SE Asia', x=mat.labels)
mat.labels <- gsub(pattern='^Pacific$', replacement='Pac', x=mat.labels)
# mat.labels <- gsub(pattern='\n', replacement='<br />', x=mat.labels)

# {help} for JSON file

help <- top5_info
names(help) <- c('calories','protein','fat','food_weight')

# {names_description} for JSON file
library(qdap)

work_dir <-'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos'
text_to_website <- read.transcript(paste(work_dir, "/_interactive/_useful_info/Interdependence-Regionstext.docx", sep=""))
colnames(text_to_website) <- c('field', 'text')
text_to_website <- text_to_website[4:26,]

names_description <- lapply(1:nrow(text_to_website), function(i)
{
  nm_description <- c(title=as.character(text_to_website$field[[i]]), content=as.character(text_to_website$text[[i]]))
  return(nm_description)
})
names_description <- rep(names_description, each=2)

# {metrics} for JSON file

metrs <- c('kcal/capita/day', rep('g/capita/day', 3)) # c('calories','protein','fat','food_weight')


### Making JSON file

# Put all elements together in a list, after that apply toJSON function
# Sublist can contain different type of information to show
json.file <- list(names             = mat.labels,
                  labels            = cropList,
                  metrics           = metrs,
                  regions           = regions,
                  names_description = names_description, 
                  matrix            = matrices,
                  help              = help,
                  description       = description
)

sink(paste(work_dir, '/_interactive/_json/fs_interdependence_complex_text_weighted.json', sep='')) # redirect console output to a file
toJSON(json.file, pretty=FALSE)
sink()
