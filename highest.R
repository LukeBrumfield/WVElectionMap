partyCode <- "L"
years <- c("2016")
officeCodes <-
  c("AA", "CA", "DA", "EA", "FA", "GA", "HA", "IA")
prefixes <- list()
prefixes[[years[1]]] <- "/23-"

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

ranks <- countylabels[6]
colnames(ranks)[1] <- "County"
ranks$candidate <- ""
ranks$percentage <- 0

countyResults <- countyData[[year]]
countyResults <-
  countyResults[countyResults$OfficialResults == "Yes",]
countyResults <- countyResults[countyResults$PartyCode == partyCode, ]
countyResults <- countyResults[countyResults$Type == "County",]
countyResults$Percentage <- as.numeric(substr(countyResults$Percentage, 1, nchar(countyResults$Percentage)-1))

totals <- data.frame(stringsAsFactors = FALSE, name = character(), county = character(), percent = character())

for (officeCode in officeCodes)
{
  data <- countyResults[countyResults$OfficeCode == officeCode, ]
  if(nrow(data) == 0)
  {
    next
  }
  
  for(i in 1:55)
  {

    if(data[i,]$Percentage > ranks[ranks$County == data[i,]$CountyName,]$percentage && !is.na(data[i,]$ElectionId))
    {
      ranks[ranks$County == data[i,]$CountyName,]$percentage <- data[i,]$Percentage
      ranks[ranks$County == data[i,]$CountyName,]$candidate <- data[i,]$Name
    }
  }
  
  
  #z <- ranks$County %in% data$CountyName
  
  # for(i in 1:55)
  # {
  #   if(!is.na(data[i,]$ElectionId))
  #   {
  #     if(data[i,]$Percentage > ranks[i,]$percentage)
  #     {
  #       ranks[i,]$percentage <- data[i,]$Percentage
  #       ranks[i,]$candidate <- data[i,]$Name
  #     }
  #   }
  # }
  
  #data <- data[order(data$Percentage),]
  #totals <- rbind(totals, c(unique(data$Name), data[55,]$CountyName, data[55,]$Percentage))
}

ranks
