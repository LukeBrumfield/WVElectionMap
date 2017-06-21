partyCode <- "L"
years <- c("2016")
officeCodes <-
  c("AA", "BA", "CA", "DA", "EA", "FA", "GA", "HA", "IA")
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

highest <- data.frame(stringsAsFactors = FALSE)
lowest <- data.frame(stringsAsFactors = FALSE)

for (officeCode in officeCodes)
{
  data <- countyResults[countyResults$OfficeCode == officeCode, c(7,6,19)]
  
  if(nrow(data) == 0)
  {
    next
  }
  
  data <- data[order(decreasing = TRUE, data$Percentage),]
  highest <- rbind(highest, data[1,])
  
  data <- data[order(data$Percentage),]
  lowest <- rbind(lowest, data[1,])
}

highest
lowest