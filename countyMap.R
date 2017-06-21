require(ggplot2)
require(RColorBrewer)
require(maps)
require(SDMTools)

years <- c("2012", "2014", "2016")
officeCodes <-
  c("CA")
  #c("AA", "BA", "DA", "EA", "FA", "GA", "HA", "IA")
prefixes <- list()
prefixes[[years[1]]] <- "/13-"
prefixes[[years[2]]] <- "/21-"
prefixes[[years[3]]] <- "/23-"
countylabels <-
  read.csv(
    "countylabels.csv",
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

partyCode <- "L"
n <- 3

for (year in years)
  for (officeCode in  officeCodes)
  {
    {
      countyResults <- countyData[[year]]
      countyResults <-
        countyResults[countyResults$OfficialResults == "Yes", ]
      
      countyResults$Bucket <- 0
      
      data <- countyResults[countyResults$PartyCode == partyCode,]
      data <- data[data$OfficeCode == officeCode,]
      
      statewide <-
        data[data$Type == "Statewide", ]$Percentage
      
      data <- data[data$Type == "County", ]
      
      colorscale <- rev(brewer.pal(n = n, name = "Blues"))
      colorscale <- c("#FFFFFF", colorscale)
      
      if(nrow(data) == 0)
      {
        next
      }
      
      data <- data[order(data$Percentage), ]
      data$Order <- 1:nrow(data)

      
      data <- data[order(data$Order), ]
      s <-
        split(data$Order, f = sort(rank(data$Order) %% n, decreasing = TRUE))
      
      
      for (i in 1:n)
      {
        data[s[[i]], ]$Bucket <- i
      }
      
      
      colorFrame <- as.data.frame(countylabels$NAME)
      colorFrame$Bucket <- 0
      range <-
        which(colorFrame$`countylabels$NAME` %in% data$CountyName)
      
      for (i in range)
      {
        colorFrame[i,]$Bucket <-
          data[data$CountyName == colorFrame[i,]$`countylabels$NAME`,]$Bucket
      }
      
      data <- data[order(data$CountyName), ]
      
      color <- colorscale[colorFrame$Bucket + 1]
      
      
      mypath <- paste("graphs/Graph",
                      unique(data$OfficeCode),
                      year,
                      ".png",
                      sep = "")
      png(mypath)
      m <-
        map('county',
            'west virginia',
            col = color,
            fill = TRUE)
      
      title <-
        paste(year,"Results",
              unique(data$OfficeDescription),
              unique(data$Name),
              sep = " ")
      title(main = title)
      mtext(
        text = paste("Total ", statewide),
        side = 3,
        adj = 1
      )
      
      dev.off()
    }
  }
