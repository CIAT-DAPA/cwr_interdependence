# Flow matrix by regions using information about crops
# H. Achicanoy & C. Khoury
# CIAT, 2016

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_regions'
fs_data_elements <- read.csv(paste(work_dir, "/_outputs/food_supplies_countries_regions_all_merge.csv", sep=''))

fs_data_elements <- fs_data_elements[,c("Element", "Item", "Country", 'Average', 'Region')]
fs_data_elements <- unique(fs_data_elements)

fmeas <- as.character(unique(fs_data_elements$Element))
fitem <- as.character(unique(fs_data_elements$Item))
fcoun <- as.character(unique(fs_data_elements$Country))

library(dplyr)

# Step 1: Average of population by country

popData <- read.csv(paste(work_dir, "/_inputs/Pop_177countriesFSandprod.csv", sep=''))
popData[,grep("Y",names(popData))] <- popData[,grep("Y",names(popData))]*1000
popData <- popData[,c('Country','Yaverage')]
popData <- unique(merge(popData, fs_data_elements[,c('Country','Region')], by.x='Country')); rownames(popData) <- 1:nrow(popData)

regions <- sort(as.character(unique(popData$Region)))

# Calculate weights by region for each country
popData <- lapply(1:length(regions), function(j) # Region filter
{
  regData <- popData[which(popData$Region==regions[[j]]),]
  totpop_reg <- sum(regData$Yaverage, na.rm=TRUE)
  regData$Percent <- regData$Yaverage/totpop_reg
  return(regData)
})
popData <- Reduce(function(...) rbind(..., deparse.level=1), popData)
rownames(popData) <- 1:nrow(popData)

# Calculate weighted item
fs_data_elements <- lapply(1:length(fcoun), function(i)
{
  counData <- fs_data_elements[which(fs_data_elements$Country==fcoun[i]),]
  counData$Average <- counData$Average*popData$Percent[popData$Country==fcoun[i]]
  return(counData)
})
fs_data_elements <- Reduce(function(...) rbind(..., deparse.level=1), fs_data_elements)
rownames(fs_data_elements) <- 1:nrow(fs_data_elements)

library(dplyr)
fs_sum_region_data_elements <- as.data.frame(dplyr::summarise(group_by(fs_data_elements, Region, Element, Item), sum(Average)))
names(fs_sum_region_data_elements)[ncol(fs_sum_region_data_elements)] <- 'Average'
fs_sum_region_data_elements <- unique(fs_sum_region_data_elements)
write.csv(fs_sum_region_data_elements, paste(work_dir, '/_outputs/circos/FS/fs_sum_items_region.csv', sep=''), row.names=FALSE)

region_crops <- read.csv(paste(work_dir, "/_outputs/food_supplies_countries_regions_all_merge.csv", sep=''))
region_crops <- region_crops[,c('Item','Region_crops')]
region_crops <- unique(region_crops); rownames(region_crops) <- 1:nrow(region_crops)

fs_sum_region_data_elements <- merge(fs_sum_region_data_elements, region_crops, by='Item')
fs_sum_region_data_elements <- unique(fs_sum_region_data_elements); rownames(fs_sum_region_data_elements) <- 1:nrow(fs_sum_region_data_elements)

library(dplyr)
fs_sum_regioncrops_data_elements <- as.data.frame(dplyr::summarise(group_by(fs_sum_region_data_elements, Region_crops, Region, Element), sum(Average)))
colnames(fs_sum_regioncrops_data_elements)[4] <- 'Average'

write.csv(fs_sum_regioncrops_data_elements, paste(work_dir, "/_outputs/circos/FS/regions_sourceofdiets_weighted.csv", sep=''), row.names=FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Circos plot
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

options(warn=-1)
library(circlize)
library(plyr)

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_regions'
cOrgData_region <- read.csv(paste(work_dir, "/_outputs/circos/FS/regions_sourceofdiets_weighted.csv", sep=''))
cOrgData_region <- cOrgData_region[,c("Region", "Region_crops", "Element", "Average")]
nice_regions <- read.csv(paste(work_dir, "/_inputs/Countries_FS_2009-2011_data_regions_2016_1_20.csv", sep=''))
#######note may need to replace the above with "Countries_FS_2009-2011_data_regions_2016_1_20" that includes change to Occupied Palestinean Territories; perhaps no need for change if just getting nice names here

code_regions <- nice_regions[,c("Region","Region_code")]
nice_regions <- nice_regions[,c("Region","Region_nice")]

# Step 1: Change region name to region nice name
cOrgData_region <- merge(cOrgData_region,code_regions,by="Region")
cOrgData_region <- merge(cOrgData_region,nice_regions,by="Region")
cOrgData_region <- unique(cOrgData_region)
cOrgData_region$Region <- NULL
names(cOrgData_region)[4:5] <- c("Rcode_recipients","R_recipients")

# Step 2: Change Region_crops to region nice name
names(nice_regions)[1] <- "Region_crops"
names(code_regions)[1] <- "Region_crops"

cOrgData_region <- merge(cOrgData_region,nice_regions,by="Region_crops"); cOrgData_region <- unique(cOrgData_region)
cOrgData_region <- merge(cOrgData_region,code_regions,by="Region_crops"); cOrgData_region <- unique(cOrgData_region)
cOrgData_region$Region_crops <- NULL
names(cOrgData_region)[5:6] <- c("R_origin","Rcode_origin")

cOrgData_region <- cOrgData_region[,c("R_origin","Rcode_origin","R_recipients","Rcode_recipients","Element","Average")]
rownames(cOrgData_region) <- 1:nrow(cOrgData_region)

cOrgData_region$R_recipients <- gsub(pattern='Central Africa',replacement='C\nAfrica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Central America',replacement='C\nAmerica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Central Asia',replacement='C\nAsia',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='East Africa',replacement='E\nAfrica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='East Asia',replacement='E\nAsia',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='North America',replacement='N\nAmerica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='NE Europe',replacement='NE\nEurope',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='NW Europe',replacement='NW\nEurope',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Southern Africa',replacement='S\nAfrica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='South Asia',replacement='S\nAsia',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Southeast Asia',replacement='SE\nAsia',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='SE Europe',replacement='SE\nEurope',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='SW Europe',replacement='SW\nEurope',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='West Africa',replacement='W\nAfrica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='West Asia',replacement='W\nAsia',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Trop. S. America',replacement='Trop. S\nAmerica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Temp. S. America',replacement='Temp. S\nAmerica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='SE Mediterranean',replacement='SE\nMediterranean',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Australia New Zealand',replacement='ANZ',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Trop. Pacific Region',replacement='Pacific',cOrgData_region$R_recipients,fixed=TRUE)

cOrgData_region$R_origin <- gsub(pattern='Central Africa',replacement='C\nAfrica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Central America',replacement='C\nAmerica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Central Asia',replacement='C\nAsia',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='East Africa',replacement='E\nAfrica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='East Asia',replacement='E\nAsia',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='North America',replacement='N\nAmerica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='NE Europe',replacement='NE\nEurope',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='NW Europe',replacement='NW\nEurope',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Southern Africa',replacement='S\nAfrica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='South Asia',replacement='S\nAsia',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Southeast Asia',replacement='SE\nAsia',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='SE Europe',replacement='SE\nEurope',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='SW Europe',replacement='SW\nEurope',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='West Africa',replacement='W\nAfrica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='West Asia',replacement='W\nAsia',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Trop. S. America',replacement='Trop. S\nAmerica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Temp. S. America',replacement='Temp. S\nAmerica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='SE Mediterranean',replacement='SE\nMediterranean',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Australia New Zealand',replacement='ANZ',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Trop. Pacific Region',replacement='Pacific',cOrgData_region$R_origin,fixed=TRUE)

umeas <- sort(as.character(unique(cOrgData_region$Element)))
measList <- c("fat","calories","food_weight","protein")
lapply(1:length(umeas),function(i)
{
  measData <- cOrgData_region[which(cOrgData_region$Element==umeas[[i]]),]
  
  origin=sort(as.character(unique(measData$R_origin)))
  destiny=sort(as.character(unique(measData$R_recipients)))
  
  origin_regions <- lapply(1:length(origin),function(j)
  {
    origin_data <- measData[which(measData$R_origin==origin[j]),c("R_recipients","Average")]
    origin_data <- origin_data[order(origin_data$R_recipients),]
    
    origin_region <- data.frame(t(origin_data$Average))
    names(origin_region) <- origin_data$R_recipients
    rownames(origin_region) <- paste(origin[j])
    return(origin_region)
  })
  origin_regions <- Reduce(function(...) rbind(..., deparse.level=1), origin_regions)
  mtch <- setdiff(1:length(colnames(origin_regions)),match(rownames(origin_regions),colnames(origin_regions)))
  origin_regions[nrow(origin_regions)+1:2,] <- 0
  rownames(origin_regions)[22:23] <- colnames(origin_regions)[mtch]
  
  #write.csv(origin_regions,paste("F:/CIAT/Interdependence/crop_importance/circos/inputs_circos/interchange_",measList[[i]],"_2009-2011.csv"),row.names=TRUE)
  
  order.regions <- c(4,23,8,2,18,3,9,20,11,1,14,12,22,10,19,21,15,16,13,6,5,7,17)
  #   colors <- read.table(system.file("science", "country_custom.txt", package = "migest"), skip=2, stringsAsFactors=FALSE)
  #   colors <- colors[,1:3]
  #   names(colors)<-c("order","rgb","region")
  #   set.seed(1235)
  #   colors <- sample(colors$rgb,size=23,replace=FALSE)
  
  df1 <- data.frame(order=order.regions, region=destiny)
  library("plyr")
  df1 <- df1[order(df1$order),]
  rownames(df1) <- 1:nrow(df1)
  
  levels.regions <- as.character(df1$region)
  origin_regions <- origin_regions[levels.regions, levels.regions]
  
  write.csv(origin_regions, paste(work_dir, "/_outputs/circos/FS/interchange_", measList[[i]], "_2009-2011_weighted.csv", sep=''), row.names=TRUE)
  
  origin_regions <- round(origin_regions,1)
  
  origin_regions <- as.matrix(origin_regions)
  dimnames(origin_regions)<-list(orig=levels.regions,dest=levels.regions)
  
  if(umeas[i]=="Food supply (kcal/capita/day)"|umeas[i]=="Food supply quantity (g/capita/day)")
  {
    origin_regions <- origin_regions/1000
  }
  
  ##
  ##define ranges of circos sectors and their colors (both of the sectors and the links)
  ##
  df1$xmin <- 0
  df1$xmax <- rowSums(origin_regions)+colSums(origin_regions)
  df1$region <- factor(df1$region, levels=df1$region)
  n <- nrow(df1)
  # Include RGB color
  colors <- read.csv(paste(work_dir, "/_inputs/id_colors_regions.csv", sep=''))
  colors <- colors[,c("Region","r","g","b")]
  names(colors)[1] <- "region"
  
  colors$region <- gsub(pattern='C Africa',replacement='C\nAfrica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='C America',replacement='C\nAmerica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='C Asia',replacement='C\nAsia',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='E Africa',replacement='E\nAfrica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='E Asia',replacement='E\nAsia',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='N America',replacement='N\nAmerica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='NE Europe',replacement='NE\nEurope',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='NW Europe',replacement='NW\nEurope',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='S Africa',replacement='S\nAfrica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='S Asia',replacement='S\nAsia',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='SE Asia',replacement='SE\nAsia',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='SE Europe',replacement='SE\nEurope',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='SW Europe',replacement='SW\nEurope',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='W Africa',replacement='W\nAfrica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='W Asia',replacement='W\nAsia',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='Trop. S. America',replacement='Trop. S\nAmerica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='Temp. S. America',replacement='Temp. S\nAmerica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='SE Mediterranean',replacement='SE\nMediterranean',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='Aus-N Zealand',replacement='ANZ',colors$region,fixed=TRUE)
  
  df1 <- merge(df1,colors,by="region")
  df1 <- df1[order(df1$order),]
  rownames(df1) <- 1:nrow(df1)
  df1$rcol<-rgb(df1$r, df1$g, df1$b, max = 255)
  df1$lcol<-rgb(df1$r, df1$g, df1$b, alpha=200, max = 255)
  
  ##
  ##plot sectors
  ##
  library("circlize")
  par(mar=rep(3.0,4),xpd=TRUE)
  circos.clear()
  
  #basic circos graphic parameters
  circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.1), start.degree = 90, gap.degree =4)
  
  #sector details
  circos.initialize(factors=df1$region, xlim=cbind(df1$xmin, df1$xmax))
  
  #plot sectors
  circos.trackPlotRegion(ylim = c(0, 1), factors=df1$region, track.height=0.1, bg.border = NA, bg.col = NA, bg.lty =0, bg.lwd=0.0001,
                         #panel.fun for each sector
                         panel.fun = function(x, y) {
                           #select details of current sector
                           name = get.cell.meta.data("sector.index")
                           z = get.cell.meta.data("sector.numeric.index")
                           xlim = get.cell.meta.data("xlim")
                           ylim = get.cell.meta.data("ylim")
                           
                           #                            #text direction (dd) and adjusmtents (aa)
                           #                            theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                           #                            dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
                           #                            aa = c(1, 0.5)
                           #                            if(theta < 90 || theta > 270)  aa =c(0, 0.5)
                           
                           #plot country labels
                           circos.text(x=mean(xlim), y=1.5, labels=name, facing='inside', cex=0.8, col=df1$rcol[z], font=2, niceFacing=TRUE)
                           
                           #plot main sector
                           circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                       col = df1$rcol[z], border=df1$rcol[z])
                           
                           #blank in part of main sector
                           circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(origin_regions)[z], ytop=ylim[1]+0.3, 
                                       col = "white", border = "white")
                           
                           #white line all the way around
                           circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.32, col = "white", border = "white")
                           
                           #                            #plot axis
                           #                            if(umeas[[i]]=="Fat supply quantity (g/capita/day)")
                           #                            {
                           #                              circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=100), minor.ticks=1,
                           #                                          labels.away.percentage = 0.15)
                           #                            } else {
                           #                              if(umeas[[i]]=="Food supply (kcal/capita/day)")
                           #                              {
                           #                                circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=4), minor.ticks=1,
                           #                                            labels.away.percentage = 0.15)
                           #                              } else {
                           #                                if(umeas[[i]]=="Food supply quantity (g/capita/day)")
                           #                                {
                           #                                  circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=2), minor.ticks=1,
                           #                                              labels.away.percentage = 0.15)
                           #                                } else {
                           #                                  circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=100), minor.ticks=1,
                           #                                              labels.away.percentage = 0.15)
                           #                                }
                           #                              }
                           #                            }
                           
                         })
  
  circos.par(track.margin=c(0,0)) 
  
  ##
  ##plot links
  ##
  #add sum values to df1, marking the x-position of the first links out (sum1) and in (sum2). Updated for further links in loop below.
  df1$sum1 <- colSums(origin_regions)
  df1$sum2 <- numeric(n)
  
  #create a data.frame of the flow matrix sorted by flow size, to allow largest flow plotted first
  df2<-cbind(as.data.frame(origin_regions),orig=rownames(origin_regions),stringsAsFactors=FALSE)
  df2<-reshape(df2,idvar="orig",varying=list(1:n),direction="long",timevar="dest",time=rownames(origin_regions),v.names="origin_regions")
  df2<-arrange(df2,desc(origin_regions))
  
  #keep only the largest flows to avoid clutter
  # df2<-subset(df2, origin_regions>quantile(origin_regions,0.95))
  df2$orig <- gsub(pattern='_',replacement=' ',df2$orig)
  df2$dest <- gsub(pattern='_',replacement=' ',df2$dest)
  
  for(k in 1:nrow(df2)){
    #m,n reference of flow matrix
    m<-match(df2$orig[k],df1$region)
    n<-match(df2$dest[k],df1$region)
    
    #plot link
    circos.link(sector.index1=df1$region[m], point1=c(df1$sum1[m], df1$sum1[m] + abs(origin_regions[m, n])),
                sector.index2=df1$region[n], point2=c(df1$sum2[n], df1$sum2[n] + abs(origin_regions[m, n])),
                col = df1$lcol[m])
    
    #update sum1 and sum2 for use when plotting the next link
    df1$sum1[m] = df1$sum1[m] + abs(origin_regions[m, n])
    df1$sum2[n] = df1$sum2[n] + abs(origin_regions[m, n])
  }
  
  dev.copy2pdf(file=paste(work_dir, "/_figures/interchange_", measList[[i]], "_2009-2011_all_weighted.pdf", sep=""), height=10, width=10)
  
})
