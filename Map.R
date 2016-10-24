
countylabels <- read.csv("countylabels.csv", stringsAsFactors = FALSE, header = TRUE, colClasses = c(rep("character",11)))
f <- paste("00" , as.character(countylabels$COUNTYFP), sep = "")
f <- substr(f, nchar(f)-3+1, nchar(f))
countylabels$COUNTYFP <- f

years <- c("2012", "2014")
prefixes <- list()
prefixes[[years[1]]] <- "/13-"
prefixes[[years[2]]] <- "/21-"

districts <- readOGR(dsn = "tl_2012_54_vtd10", layer = "tl_2012_54_vtd10", stringsAsFactors = FALSE)

precinctData <- list()
for (year in years) {
  precinctData[[year]] <- read.csv(file = paste("precinctNumbers", year, ".csv", sep = ""), stringsAsFactors = FALSE)
}

precinctLabels <- list()
for (year in years) {
  precinctLabels[[year]] <- read.csv(file = paste("precinctLabels", year, ".csv", sep = ""), stringsAsFactors = FALSE)
}

countyData <- list()
for (year in years) {
  countyData[[year]] <- read.csv(file = paste(year, prefixes[[year]], "StateCountyTotals.csv", sep = ""), stringsAsFactors = FALSE)
}
