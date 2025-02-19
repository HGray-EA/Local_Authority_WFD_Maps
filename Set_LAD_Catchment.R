library(sf)
library(tidyverse)

Som <- read_sf("/dbfs/FileStore/WSX_HGray/LAD_MAY_2024_UK_BFE.shp") %>% st_transform(4326) %>% 
  filter(LAD24NM == "Somerset")

# Load CDE data
CDE <- read.csv("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/CEP/WFD_Wessex_2024.csv")

# Load and then transform CAT
CAT <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/Interim_WFD_2022.shp")
CAT_Union <- st_union(CAT) %>% 
  st_transform(4326)
CAT_27700 <- CAT
CAT <- CAT %>%  st_transform(4326)
CAT_geo <- subset(CAT, select = c(WB_ID, geometry))


CDE %<>% 
  inner_join(CAT_geo, ., by = c("WB_ID" = "Water.Body.ID"))

CDE_e_2019 <- CDE %>% 
  filter(Classification.Item == "Physio-Chemical Quality Elements" & 
           Year == "2019")
# 2022 map
CDE_e_2022 <- CDE %>% 
  filter(Classification.Item == "Physio-Chemical Quality Elements" & 
           Year == sort(unique(CDE$Year), decreasing=TRUE)[1])     

CDE_Som <- CDE_e_2022[Som,]


DRN <- read_sf("/dbfs/mnt/lab/unrestricted/harry.gray@environment-agency.gov.uk/DRN/DRN_Merged_MCAT.shp")
DRN <- DRN[CDE_Som,]


# Styling #

# Define WFD palette
pal <- colorFactor(
  palette = c("#ADE8F4", "seagreen", "seagreen", "yellow", "#b71105","orange", "red"),
  levels = c("High", "Good", "Supports Good", "Moderate", "Bad", "Poor", "Fail"),
  na.color = "transparent"
)


# Leaflet layers order javascript: 

Layers_JS <- "function(el, x) {
                var map = this;
          
                map.on('overlayadd overlayremove', function(e) {
                  // Create an array to hold layers by zIndex
                  var layers = [];
                  
                  // Collect all layers with zIndex
                  map.eachLayer(function(layer) {
                    if (layer.options && layer.options.zIndex !== undefined) {
                      layers.push(layer);
                    }
                  });
          
                  // Sort layers by zIndex in ascending order
                  layers.sort(function(a, b) {
                    return a.options.zIndex - b.options.zIndex;
                  });
          
                  // Re-add layers to the map in sorted order
                  layers.forEach(function(layer) {
                    if (map.hasLayer(layer)) {
                      layer.bringToFront();
                    }
                });
              });
            }"
