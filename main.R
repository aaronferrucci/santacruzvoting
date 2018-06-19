library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)

data <- readOGR(dsn="./original", layer="Precincts")
data@data$id <- rownames(data@data)
dataPoints <- fortify(data, region="id")
dataDF <- merge(dataPoints, data@data, by="id")
dataDF$Precinct <- as.integer(as.character(dataDF$Precinct))

source("parse_voter_data.R")
# 'merge' reorders the data.frame, which ruins the choropleth...
# dataDF2 <- merge(sort=F, dataDF, turnout_by_precinct[, c("Precinct", "Turnout")], by="Precinct")
dataDF$Turnout <-
  turnout_by_precinct[match(dataDF$Precinct, turnout_by_precinct$Precinct, nomatch=which(turnout_by_precinct$Turnout == min(turnout_by_precinct$Turnout))[1]), "Turnout"]
dataDF$'Registered Voters' <- turnout_by_precinct$'Registered Voters'[match(dataDF$Precinct, turnout_by_precinct$Precinct)]
dataDF$'Registered Voters' <- ifelse(is.na(dataDF$'Registered Voters'), 0, dataDF$'Registered Voters')

dataDF$'Ballots Cast' <- turnout_by_precinct$'Ballots Cast'[match(dataDF$Precinct, turnout_by_precinct$Precinct)]
dataDF$'Ballots Cast' <- ifelse(is.na(dataDF$'Ballots Cast'), 0, dataDF$'Ballots Cast')

ggData <- ggplot(data=dataDF, aes(x=long, y=lat, group=group, fill=`Turnout`)) +
  geom_polygon() +
  geom_path(color="white", size=0.1) +
  scale_fill_gradient(low="darkgreen", high="green") +
  coord_equal() +
  theme(axis.text=element_blank())

print(ggData)
ggsave("plot.png")

