library(ggplot2)
library(sp)
library(rgdal)
library(rgeos)

# https://earthworks.stanford.edu/catalog/stanford-tc328sr9298
data <- readOGR(dsn="./original", layer="Precincts")
data@data$id <- rownames(data@data)
dataPoints <- fortify(data, region="id")
dataDF <- merge(dataPoints, data@data, by="id")
dataDF$Precinct <- as.numeric(as.character(dataDF$Precinct))
ggData <- ggplot(data=dataDF, aes(x=long, y=lat, group=group, fill=Precinct)) +
  geom_polygon() +
  geom_path(color="white") +
  scale_fill_gradient(low="green", high="darkgreen") +
  coord_equal() +
  theme(legend.position="none", title=element_blank(), axis.text=element_blank())

print(ggData)
