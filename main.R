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
dataDF$Turnout <- turnout_by_precinct[match(dataDF$Precinct, turnout_by_precinct$Precinct, nomatch=which(turnout_by_precinct$Turnout == min(turnout_by_precinct$Turnout))[1]), "Turnout"]

ggData <- ggplot(data=dataDF, aes(x=long, y=lat, group=group, fill=Turnout)) +
  geom_polygon() +
  geom_path(color="white") +
  scale_fill_gradient(low="darkgreen", high="green") +
  coord_equal() +
  theme(legend.position="none", title=element_blank(), axis.text=element_blank())

print(ggData)
ggsave("plot.png")

