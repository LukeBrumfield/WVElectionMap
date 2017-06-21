#Precinct Data Aggregator
years <- c("2012", "2014", "2016")
prefixes <- list()
prefixes[[years[1]]] <- "/13-"
prefixes[[years[2]]] <- "/21-"
prefixes[[years[3]]] <- "/23-"

for (year in years)
{
  precincts <- data.frame()
  for (i in 1:55) {
    p <-
      read.csv(
        paste("data/", year, prefixes[[year]], as.character(i), ".csv", sep = ""),
        stringsAsFactors = FALSE
      )
    p$VoteTotal <- 0
    for (i in 1:nrow(p))
    {
      p[i, 19] <-
        sum(p[intersect(which(p[i, 12] == p[, 12]), which(p[i, 7] == p[, 7])), 18])
    }
    p$DistrictName <- toupper(p$DistrictName)
    precincts <- rbind(precincts, p)
  }
  precincts$Percent <- round(precincts$Votes / precincts$VoteTotal, digits = 2)
  write.csv(
    x = precincts,
    file = paste("precinctNumbers", year, ".csv", sep = ""),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    x = precincts[, c(6, 4, 7, 12, 13, 14, 15, 16)],
    file = paste("precinctLabels", year, ".csv", sep = ""),
    row.names = FALSE,
    na = ""
  )
}