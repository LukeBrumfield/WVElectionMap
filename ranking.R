require(ggplot2)
require(RColorBrewer)
require(maps)
require(SDMTools)

partyCode <- "L"
years <- c("2016")
officeCodes <-
  c("AA", "BA", "DA", "EA", "FA", "GA", "HA", "IA")
prefixes <- list()
prefixes[[years[1]]] <- "/23-"

countylabels <-
  read.csv(
    "data/countylabels.csv",
    stringsAsFactors = FALSE,
    header = TRUE,
    colClasses = c(rep("character", 11))
  )

countyData <- list()
for (year in years) {
  countyData[[year]] <-
    read.csv(
      file = paste("data/", year, prefixes[[year]], "StateCountyTotals.csv", sep = ""),
      stringsAsFactors = FALSE
    )
}

  ranks <- countylabels[6]
  colnames(ranks)[1] <- "County"

  countyResults <- countyData[[year]]
  countyResults <-
    countyResults[countyResults$OfficialResults == "Yes",]
  countyResults <- countyResults[countyResults$PartyCode == partyCode, ]
  countyResults <- countyResults[countyResults$Type == "County",]
  countyResults$Percentage <- as.numeric(substr(countyResults$Percentage, 1, nchar(countyResults$Percentage)-1))
  
  
  for (officeCode in officeCodes)
  {
    data <- countyResults[countyResults$OfficeCode == officeCode, ]
    
    if(nrow(data) == 0)
    {
      next
    }
    
    data <- data[order(data$Percentage),]
    data$Order <- c(55:1)
    data <- data[order(data$CountyName),]
    
    ranks <- cbind(ranks, data$Order)
    colnames(ranks)[ncol(ranks)] <- unique(data$Name)
  }
  
  ranks$AVG <- apply(X = ranks[,2:8], MARGIN = 1, FUN = mean)
  ranks$AVG <- round(x = ranks$AVG, digits = 2)
  
  n <- 5
  colorscale <- rev(brewer.pal(n = n, name = "Blues"))
  
  data <- ranks[c(1,9)]
  data$Bucket <- 0
  
  data <- data[order(data$AVG), ]
  
  
  for (i in 1:n)
  {
    min <- 1 + (i-1)*55/n
    max <- 55/n + (i-1)*55/n
    data[min:max,]$Bucket <- colorscale[i]
  }

  data <- data[order(data$County), ]
  
  mypath <- paste("graphs/Graphtotal",
                  year,
                  ".png",
                  sep = "")
  png(filename = mypath)
  
  data$County <- paste("west virginia,",tolower(data$County))
  z <- data.frame(map('county','west virginia')$names)
  
  m <-
    map('county',
        'west virginia',
        col = data[z[,1],]$Bucket,
        fill = TRUE)
  
  title <-
    paste(year,"Weighted Results")
  title(main = title)
  
  dev.off()
  
  write.csv(x = ranks, "data/ranks.csv", row.names = FALSE)
  
  