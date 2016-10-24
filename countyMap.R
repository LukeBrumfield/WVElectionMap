require(ggplot2)
require(RColorBrewer)
require(maps)
require(SDMTools)

years <- c("2012", "2014")
partyCode <- "L"
officeCodes <- c("BA", "CA", "DA", "AA")#Senate,House,Gov,Pres
prefixes <- list()
prefixes[[years[1]]] <- "/13-"
prefixes[[years[2]]] <- "/21-"
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
      file = paste(year, prefixes[[year]], "StateCountyTotals.csv", sep = ""),
      stringsAsFactors = FALSE
    )
}

bucket <- function(x)
{
  x <- substr(x, 0, nchar(x) - 1)
  x <- as.numeric(x)
  if (x > 1.50)
  {
    return(1)
  }
  if (x < 1.00)
  {
    return(3)
  }
  return(2)
}

year <- years[2]
officeCode <- officeCodes[1]
n <- 3

colorscale <- rev(brewer.pal(n = n, name = "Blues"))
colorscale <- c("#FFFFFF", colorscale)

countyResults <- countyData[[year]]
countyResults <-
  countyResults[countyResults$OfficialResults == "Yes",]

countyResults$Bucket <- 0

data <- countyResults[countyResults$PartyCode == partyCode, ]
data <- data[data$OfficeCode == officeCode, ]

statewide <-
  data[data$Type == "Statewide",]$Percentage

data <- data[data$Type == "County",]
data <- data[order(data$Percentage),]
data$Order <- 1:nrow(data)

colorFrame <- as.data.frame(countylabels$NAME)
colorFrame$Bucket <- 0



for (i in 1:nrow(data))
{
  data[i,]$Bucket <- bucket(data[i,]$Percentage)
}

data <- data[order(data$CountyName),]

range <- which(colorFrame$`countylabels$NAME` %in% data$CountyName)

for (i in range)
{
  colorFrame[i, ]$Bucket <-
    data[data$CountyName == colorFrame[i, ]$`countylabels$NAME`, ]$Bucket
}

m <-
  map('county', 'west virginia', col = colorscale[colorFrame$Bucket + 1], fill = TRUE)

title <-
  paste("Results",
        unique(data$OfficeDescription),
        unique(data$Name),
        sep = " ")
title(main = title)
mtext(text = paste("Total ", statewide),
      side = 3,
      adj = 1)
legend("topleft", c("> 1.5", "1.5 - 1.0", "< 1.0"), fill = colorscale[c(2, 3, 4)])



data <- data[order(data$Order),]
s <-
  split(data$Order, f = sort(rank(data$Order) %% n, decreasing = TRUE))


for (i in 1:n)
{
  data[s[[i]],]$Bucket <- i
}

colorFrame$Bucket <- 0
range <- which(colorFrame$`countylabels$NAME` %in% data$CountyName)

for (i in range)
{
  colorFrame[i, ]$Bucket <-
    data[data$CountyName == colorFrame[i, ]$`countylabels$NAME`, ]$Bucket
}

data <- data[order(data$CountyName),]
m <-
  map('county', 'west virginia', col = colorscale[colorFrame$Bucket + 1], fill = TRUE)


title <-
  paste("Results",
        unique(data$OfficeDescription),
        unique(data$Name),
        sep = " ")
title(main = title)
mtext(text = paste("Total ", statewide),
      side = 3,
      adj = 1)
