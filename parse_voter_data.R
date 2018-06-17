library(tabulizer)

# Voter turnout data, by precinct, is technically available, but locked
# away in a pdf file. Someone will pay for this crime against data. In the
# meantime, here's some hacky code that converts to a usable format. This
# is very particular to this pdf, and should be expected to break whenever
# the pdf is updated. But it's better than typing the data manually.
file <- "./ensov.pdf"
rawpages <- extract_tables(file, pages=seq(1,6))
dims <- get_page_dims(file, pages=1)
# specifying this area - which is nominally the entire page - would seem to do
# nothing. I do it because without it, data is scrambled (candidate names
# interpolated into precinct names).
rawpages <- extract_tables(file, pages=seq(1,6), area=list(c(1, 1, dims[[1]][2], dims[[1]][1])))
trimmedpages <- lapply(rawpages, function(x) { df <- as.data.frame(x); return(df[,c(1, 2, 3, 4)]) })
voter_data <- do.call(rbind, trimmedpages)

voter_data$V1 = as.character(voter_data$V1)
voter_data$V2 <- as.integer(as.character(voter_data$V2))
voter_data$V3 <- as.integer(as.character(voter_data$V3))
voter_data$V4 <- as.integer(as.character(voter_data$V4))

# Untidy: on some pages, column 2 is blank. For those, shift columns 3 and 4
# left.
voter_data$'Registered Voters' <-
  ifelse(is.na(voter_data$V2), voter_data$V3, voter_data$V2)
voter_data$'Ballots Cast' <-
  ifelse(is.na(voter_data$V2), voter_data$V4, voter_data$V3)
voter_data$Precinct <- voter_data$V1

voter_data <- subset(voter_data, select=-c(V1, V2, V3, V4))
# Reorder the columns...
voter_data <- voter_data[c('Precinct', 'Registered Voters', 'Ballots Cast')]

# After the by-precinct data, there's a summary. So untidy!
# Remove rows starting at the one with "Precinct Totals"
index <- which(voter_data$Precinct == "Precinct Totals")
voter_data <- voter_data[seq(1, index - 1),]

# "Precinct Data" is messy. It appears in two forms:
# 10020SIMPKINS
# 10020 - Vote By Mail / Absentee R

# step 1: extract the "vote by mail/absentee" nature of the record to a new
# field
voter_data$Absentee <- grepl("Vote By Mail", voter_data$Precinct)

# step 2: trim the precinct field to just the 5-digit number
# (consider saving the precinct name, e.g. "SIMPKINS")
voter_data$Precinct <- substring(voter_data$Precinct, 1, 5)
voter_data$Precinct <- as.integer(voter_data$Precinct)

# Sanity checks
all(voter_data$'Registered Voters' >= voter_data$'Ballots Cast')

library(dplyr)
turnout_by_precinct <- group_by(voter_data, Precinct)
turnout_by_precinct <- 
  summarize(turnout_by_precinct, `Registered Voters` = first(`Registered Voters`), `Ballots Cast` = sum(`Ballots Cast`))
turnout_by_precinct$Turnout <- 
  turnout_by_precinct$'Ballots Cast' / turnout_by_precinct$'Registered Voters'

