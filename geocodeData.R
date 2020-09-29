
# Geocoding the data?

library(ggmap)
library(plyr)


bc <- read.csv('./bike_collisions.csv', stringsAsFactors = FALSE)
reg <- bc[which(dd$COMMNAME == 'REGINA'),]
head(reg)

addy <- tail(reg[, c('USTREET1', 'USTREET2', "COMMNAME")])


gen_lookup <- function(df) {
  s1 <- df[[1]]
  s2 <- df[[2]]
  comm <- df[[3]]
  if (grepl('-', s2)) {
    num1 <- as.numeric(substr(s2, 1, regexpr('-', s2)[[1]]-1))
    num2 <- as.numeric(substr(s2, regexpr('-', s2)[[1]]+1, nchar(s2)))
    mid <- floor((num1 + num2)/2)
    lookup <- paste0(mid, ' ', s1, ', ', comm,', Saskatchewan')
  } else {
    lookup <- paste0(s1, ' and ', s2, ', ', comm, ', Saskatchewan')
  }
  return( lookup )
}


reg$lookup_str <- sapply(
  as.list(as.data.frame(t(reg[, c('USTREET1', 'USTREET2', 'COMMNAME')]), stringsAsFactors = FALSE)),
  gen_lookup
)

ll <- lapply(reg$lookup_str, 
  function(x) { 
    print(x) 
    geocode(x)
  }
)
dd <- rbind.fill(ll)
reg[, c('lat', 'lon')] <- dd[, c('lat', 'lon')]


write.csv(reg, file = './data/regina.csv', row.names = FALSE)

hit_pct <- 1 - (nrow(reg[which(is.na(reg$lat)), ])/nrow(reg))
print(hit_pct)

