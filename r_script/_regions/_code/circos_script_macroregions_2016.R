# Where does our food come from - Food supplies - Circos
# H. Achicanoy & C. Khoury
# CIAT, 2016

#fs_data_elements <- read.csv("F:/CIAT/Interdependence/scripts/interdependence_fs_bayesian/dependence_2009-2011/food_supplies_countries_regions_all_merge.csv")
fs_data_elements <- read.csv("C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_outputs/food_supplies_countries_regions_all_merge.csv")
#note the above file is fine as per 2016_1_20; it only lists SEM for region for Occupied Palestianian Territories but fine because only used to extract item, country, element, data, and region_crops (not regions of countries) 

fs_data_elements <- fs_data_elements[,c("Item", "Country", "Element", paste("Y",2009:2011,sep=""))]
fs_data_elements <- unique(fs_data_elements)

fmeas <- as.character(unique(fs_data_elements$Element))
fitem <- as.character(unique(fs_data_elements$Item))
fcoun <- as.character(unique(fs_data_elements$Country))

library(dplyr)

data_exp <- fs_data_elements; rm(fs_data_elements)

#fs_data_elements_all <- read.csv("F:/CIAT/Interdependence/scripts/interdependence_fs_bayesian/dependence_2009-2011/food_supplies_countries_regions_all_merge.csv")
fs_data_elements_all <- read.csv("C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_outputs/food_supplies_countries_regions_all_merge.csv")
fs_data_elements_all <- fs_data_elements_all[,c("Item", "Region_crops")]
fs_data_elements_all <- unique(fs_data_elements_all)

data_exp <- merge(data_exp, fs_data_elements_all, by=c("Item"))

#write.csv(data_exp, "F:/CIAT/Interdependence/crop_importance/circos/FS_items_country_regions.csv", row.names=F)
work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_outputs'
if(dir.exists(paste(work_dir, '/circos', sep=''))==FALSE){dir.create(paste(work_dir, '/circos', sep=''))} else {cat("This folder exists\n")}
if(dir.exists(paste(work_dir, '/circos/FS', sep=''))==FALSE){dir.create(paste(work_dir, '/circos/FS', sep=''))} else {cat("This folder exists\n")}
write.csv(data_exp, paste(work_dir, "/circos/FS/FS_items_country_regions.csv", sep=''), row.names=F)
#note this file is perfect as is as per 2016_1_20 no update needed

#data_exp <- read.csv("F:/CIAT/Interdependence/crop_importance/circos/FS_items_country_regions.csv")
data_exp <- read.csv(paste(work_dir, "/circos/FS/FS_items_country_regions.csv", sep=''))

data_country <- lapply(2009:2011,function(i)
{
  eval(parse(text=paste('data_country <- as.data.frame(dplyr::summarise(group_by(data_exp, Region_crops, Country, Element), sum(Y',i,')))',sep='')))
})

data_country2 <- merge(data_country[[1]],data_country[[2]],by=c("Region_crops","Country","Element"))
data_country <- merge(data_country2,data_country[[3]],by=c("Region_crops","Country","Element")); rm(data_country2)
names(data_country)[4:6] <- paste("Y",2009:2011,sep="")

#write.csv(data_country, "F:/CIAT/Interdependence/crop_importance/circos/FS_sum_country_regions.csv", row.names=F)
write.csv(data_country, paste(work_dir, "/circos/FS/FS_sum_country_regions.csv", sep=''), row.names=F)
#note this file is perfect as is as per 2016_1_20 no update needed

# Load data

#data_country <- read.csv("F:/CIAT/Interdependence/crop_importance/circos/FS_sum_country_regions.csv")
data_country <- read.csv(paste(work_dir, "/circos/FS/FS_sum_country_regions.csv", sep=''))
#country_regions <- read.csv("F:/CIAT/Interdependence/crop_importance/circos/Countries_FS_2009-2011_data_regions_2015_3_24.csv")
country_regions <- read.csv('C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_inputs/Countries_FS_2009-2011_data_regions_2016_1_20.csv')
#######note need to replace the above with "Countries_FS_2009-2011_data_regions_2016_1_20" that includes change to Occupied Palestinean Territories

country_regions <- country_regions[,c('Country','Region_macronew')]
names(country_regions)[2] <- 'Region'
data_country <- merge(data_country,country_regions,by="Country")
data_country <- data_country[which(data_country$Region_crops!="Not_Specified"),]

#######could produce here an output file with merged countries, elements, summed values for years for each region_crops, and regions of countries. This should be ideal output file for circos at country level 
#ck attempt immediately below- which works!
write.csv(data_country, paste(work_dir, "/circos/FS/FS_sum_country_regions_cropregions.csv", sep=''), row.names=F)

########



##to produce circos at regional level:

## merge the countries using the weighted average of population

popData <- read.csv("C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_inputs/Pop_177countriesFSandprod.csv")
popData[,grep("Y",names(popData))] <- popData[,grep("Y",names(popData))]*1000
popData$Unit <- NULL
popData <- merge(popData,country_regions,by="Country")

popData_region <- lapply(2009:2011,function(i)
{
  library(dplyr)
  eval(parse(text=paste('popData_region <- as.data.frame(dplyr::summarise(group_by(popData, Region), sum(Y',i,')))',sep='')))
})

popData_region2 <- merge(popData_region[[1]],popData_region[[2]],by=c("Region"))
popData_region <- merge(popData_region2,popData_region[[3]],by=c("Region")); rm(popData_region2)
names(popData_region)[2:4] <- paste('Y',2009:2011,sep='')

ucoun <- sort(as.character(unique(data_country$Country)))
cOrgData <- data_country

iniYear <- 2009
endYear <- 2011

for (coun in ucoun) {
  #coun <- ucoun[1]
  cat("Country:",paste(coun),"\n")
  counData <- data_country[which(data_country$Country==paste(coun)),]
  umeas <- sort(as.character(unique(data_country$Element)))
  
  for (meas in umeas) {
    #meas <- umeas[1]
    cat("Measurement:",paste(meas),"\n")
    measData <- counData[which(counData$Element==paste(meas)),]
    ureg <- sort(as.character(measData$Region_crops))
    
    for (reg in ureg) {
      #comm <- ucomm[145]
      #cat("Commodity:",paste(comm),"\n")
      regData <- measData[which(measData$Region_crops==paste(reg)),]
      iniCol <- grep(paste("Y",iniYear,sep=""),names(regData))
      finCol <- grep(paste("Y",endYear,sep=""),names(regData))
      if (nrow(regData)==0) {regData[1,iniCol:finCol] <- NA; ismissing<-T} else {ismissing <- F}
      k <- data.frame(YEAR=seq(iniYear,endYear,by=1),VALUE=as.numeric(t(regData[,iniCol:finCol])))
      names(k) <- c("YEAR",paste(coun))
      k <- unique(k)
      
      wnames <- popData[which(popData$Country==paste(coun)),]
      wnames <- sort(as.character(unique(wnames$Country))) # Revisar!!!
      
      mergeSel <- popData[which(popData$Country==paste(coun)),]
      
      minYear <- iniYear
      maxYear <- endYear
      
      #loop through years that need population data to be merged
      for (yr in minYear:maxYear) {
        #yr <- (minYear:maxYear)[1]
        #totpop <- sum(as.numeric(popSelData[which(popSelData$YEAR == yr),3:ncol(popSelData)]),na.rm=T)
        totval <- 0
        for (wname in wnames) {
          
          dta <- k[which(k$YEAR==yr),paste(wname)]
          eval(parse(text=paste('pop <- mergeSel$Y',yr,sep='')))
          pop <- unique(pop)
          totpop <- popData_region[which(popData_region$Region==as.character(mergeSel$Region)[1]),paste('Y',yr,sep='')]
          
          if (is.na(dta)) {dta <- 0}
          if (is.na(pop)) {pop <- 0}
          
          val <- (dta*pop)/totpop
          
          if (wname==wnames[1]) {
            totval <- val
          } else {
            totval <- totval+val
          }
          
        }
        k[which(k$YEAR==yr),paste(coun)] <- totval
      }
      
      outrow <- regData
      outrow[,iniCol:finCol] <- k[,paste(coun)]
      outrow$Country <- coun
      outrow$Element <- meas
      outrow$Region_crops <- reg
      
      if (ismissing) {
        cOrgData <- rbind(cOrgData,outrow)
      } else {
        cOrgData[which(cOrgData$Country == paste(coun)
                       & cOrgData$Region_crops == paste(reg)
                       & cOrgData$Element == paste(meas)),iniCol:finCol] <- k[,paste(coun)]
      }
    }
  }
}

write.csv(cOrgData,paste(work_dir,"/circos/FS/Data_2009-2011_matrix_fixed-countries.csv",sep=""), quote=T, row.names=F)

cOrgData <- read.csv(paste(work_dir, "/circos/FS/Data_2009-2011_matrix_fixed-countries.csv", sep=''))

cOrgData_region <- lapply(2009:2011,function(i)
{
  library(dplyr)
  eval(parse(text=paste('cOrgData_region <- as.data.frame(dplyr::summarise(group_by(cOrgData, Region, Region_crops, Element), sum(Y',i,')))',sep='')))
})
cOrgData_region2 <- merge(cOrgData_region[[1]],cOrgData_region[[2]],by=c("Region","Region_crops","Element"))
cOrgData_region <- merge(cOrgData_region2,cOrgData_region[[3]],by=c("Region","Region_crops","Element")); rm(cOrgData_region2)
names(cOrgData_region)[4:6] <- paste('Y',2009:2011,sep='')

write.csv(cOrgData_region, paste(work_dir, "/circos/FS/regions_sourceofdiets.csv", sep=''),row.names=FALSE)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
# Circos plot
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

options(warn=-1)
library(circlize)
library(plyr)

work_dir <- 'C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_outputs'
cOrgData_region <- read.csv(paste(work_dir, "/circos/FS/regions_sourceofdiets.csv", sep=''))
names(cOrgData_region)[1:2] <- c('R_recipients','R_origin')
rownames(cOrgData_region) <- 1:nrow(cOrgData_region)
cOrgData_region$Average <- as.vector(rowMeans(cOrgData_region[,paste('Y',2009:2011,sep='')]))

cOrgData_region$R_recipients <- gsub(pattern='Central Asia',replacement='Central\nAsia',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='East Asia',replacement='East\nAsia',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Latin America',replacement='Latin\nAmerica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='North America',replacement='North\nAmerica',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='South Asia',replacement='South\nAsia',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='Southeast Asia',replacement='Southeast\nAsia',cOrgData_region$R_recipients,fixed=TRUE)
cOrgData_region$R_recipients <- gsub(pattern='W Asia and SE Mediterranean',replacement='W Asia and\nSE Mediterranean',cOrgData_region$R_recipients,fixed=TRUE)

cOrgData_region$R_origin <- gsub(pattern='Central Asia',replacement='Central\nAsia',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='East Asia',replacement='East\nAsia',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Latin America',replacement='Latin\nAmerica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='North America',replacement='North\nAmerica',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='South Asia',replacement='South\nAsia',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='Southeast Asia',replacement='Southeast\nAsia',cOrgData_region$R_origin,fixed=TRUE)
cOrgData_region$R_origin <- gsub(pattern='W Asia and SE Mediterranean',replacement='W Asia and\nSE Mediterranean',cOrgData_region$R_origin,fixed=TRUE)

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
  #mtch <- setdiff(1:length(colnames(origin_regions)),match(rownames(origin_regions),colnames(origin_regions)))
  #origin_regions[nrow(origin_regions)+1:2,] <- 0
  #rownames(origin_regions)[22:23] <- colnames(origin_regions)[mtch]
  
  #write.csv(origin_regions,paste("F:/CIAT/Interdependence/crop_importance/circos/inputs_circos/interchange_",measList[[i]],"_2009-2011.csv"),row.names=TRUE)
  
  order.regions <- c(3,6,8,4,2,1,10,7,9,5)
  #   colors <- read.table(system.file("science", "country_custom.txt", package = "migest"), skip=2, stringsAsFactors=FALSE)
  #   colors <- colors[,1:3]
  #   names(colors)<-c("order","rgb","region")
  #   set.seed(1235)
  #   colors <- sample(colors$rgb,size=23,replace=FALSE)
  
  df1 <- data.frame(order=order.regions,region=destiny)
  library("plyr")
  df1 <- df1[order(df1$order),]
  rownames(df1) <- 1:nrow(df1)
  
  levels.regions <- as.character(df1$region)
  origin_regions <- origin_regions[levels.regions,levels.regions]
  
  write.csv(origin_regions,paste(work_dir, "/circos/FS/interchange_", measList[[i]], "_2009-2011.csv",sep=""),row.names=TRUE)
  
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
  colors <- read.csv("C:/Users/haachicanoy/Documents/GitHub/interdependence_circos/_inputs/id_colors_macro_regions.csv")
  colors <- colors[,c("Region","r","g","b")]
  names(colors)[1] <- "region"
  
  colors$region <- gsub(pattern='Central Asia',replacement='Central\nAsia',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='East Asia',replacement='East\nAsia',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='Latin America',replacement='Latin\nAmerica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='North America',replacement='North\nAmerica',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='South Asia',replacement='South\nAsia',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='Southeast Asia',replacement='Southeast\nAsia',colors$region,fixed=TRUE)
  colors$region <- gsub(pattern='W Asia and SE Mediterranean',replacement='W Asia and\nSE Mediterranean',colors$region,fixed=TRUE)
  
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
                           
                           #text direction (dd) and adjusmtents (aa)
                           #                            theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                           #                            dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
                           #                            aa = c(1, 0.5)
                           #                            if(theta < 90 || theta > 270)  aa =c(0, 0.5)
                           
                           #plot country labels
                           circos.text(x=mean(xlim), y=1.5, labels=name, facing='inside', cex=0.8, col=df1$rcol[z], font=2, niceFacing=TRUE) # facing = dd, adj = aa,
                           
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
                           #                              circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=50), minor.ticks=1,
                           #                                          labels.away.percentage = 0.15)
                           #                            } else {
                           #                              if(umeas[[i]]=="Food supply (kcal/capita/day)")
                           #                              {
                           #                                circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=2), minor.ticks=1,
                           #                                            labels.away.percentage = 0.15)
                           #                              } else {
                           #                                if(umeas[[i]]=="Food supply quantity (g/capita/day)")
                           #                                {
                           #                                  circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=1), minor.ticks=1,
                           #                                              labels.away.percentage = 0.15)
                           #                                } else {
                           #                                  circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,floor(df1$xmax)[z],by=50), minor.ticks=1,
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
  # df2<-subset(df2, origin_regions>quantile(origin_regions, 0.55))
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
  
  dev.copy2pdf(file=paste(work_dir, "/circos/FS/interchange_",measList[[i]],"_2009-2011_all.pdf",sep=""), height=10, width=10)
  
})
