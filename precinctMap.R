require(RColorBrewer)
require(maps)
require(scales)
#county maps with districts

years <- c("2012", "2014")
year <- "2014"
partyCode <- "L"
officeCodes <- c("BA","CA")
officeCode <- "BA"
county <- "Kanawha"

precinctShape <- districts[districts@data$COUNTYFP10 == countylabels$COUNTYFP[countylabels$NAME == county],]
countyPrecinctsData <- precinctData[[year]][precinctData[[year]]$CountyName == county,]

z <- intersect(which(countyPrecinctsData$OfficeCode == officeCode), which(countyPrecinctsData$PartyCode == partyCode))
p <- countyPrecinctsData[z, 18]/countyPrecinctsData[z, 19]
levels <- cut(x = p, breaks = fivenum(x = p, na.rm = TRUE), include.lowest = TRUE, labels = FALSE)  

colorscale  <- colorRampPalette(c("#FF3000", "#0D9EFF"))(length(unique(levels)))

map(database = precinctShape, col = colorscale, fill = TRUE)