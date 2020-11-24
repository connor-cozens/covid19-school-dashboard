# NOTES ------------------------------------------------------------------------

# https://open.toronto.ca/dataset/neighbourhoods/

# DEPENDENCIES -----------------------------------------------------------------

library(rgdal)

# LOAD DATA --------------------------------------------------------------------

source('data_downloader.R')

# MAIN -------------------------------------------------------------------------

neighborhoods <- readOGR(dsn = 'data/shapefiles/toronto_neighborhoods', layer = 'Neighbourhoods') 

# find neighborhood centers so we can label neighborhoods
centers <- data.frame(gCentroid(neighborhoods, byid = TRUE))
centers$neighborhood_name <- str_replace_all(neighborhoods$FIELD_8, '\\(.*', '') %>% 
	str_replace_all(., 'St\\.', 'St\\. ') %>%
	str_squish

# identify any neighborhood name mismatches
idx <- which(risk_rank_neighborhood$neighborhood_name %in% centers$neighborhood_name == FALSE)
(risk_rank_neighborhood[ idx, 'neighborhood_name' ])

# put risk rank risk_rank_neighborhood neighborhood_name in same order as in centers
idx <- sapply(centers$neighborhood_name, function(x) which(risk_rank_neighborhood$neighborhood_name == x)) %>% as.integer
rr <- risk_rank_neighborhood[ idx, ]
idx <- which(is.na(rr$of_100))
rr[ idx, 'of_100' ] <- 0

basemap <- leaflet(neighborhoods)
basemap <- setView(basemap, lng = -79.41432, lat = 43.69453, zoom = 13)
popup_html <- sprintf('<strong>%s</strong><br>Probability of contracting virus in crowd of 100: %s<br>Population: %s<br>7-Day Count: %s<br>One Infection Per: %s<br>Transmissible Cases before Isolation and Seroprevalence: %s<br>Population Density Per Square Kilometre: %s<br>Average Household Size: %s<br>Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%%): %s<br>', 
					  rr$neighborhood_name,
					  rr$of_100,
					  as.integer(rr$Population),
					  as.integer(rr$`7-Day Count`),
					  as.numeric(rr$`One Infection Per`),
					  as.numeric(rr$`Transmissible Cases before Isolation and Seroprevalence`),
					  as.numeric(rr$`Population Density Per Square Kilometre`),
					  as.numeric(rr$`Average Household Size`),
					  as.numeric(rr$`Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)`)) %>% 
	lapply(htmltools::HTML)
basemap <- addPolygons(basemap, 
					   weight = 1, 
					   fillColor = 'red', 
					   smoothFactor = 0.2, 
					   fillOpacity = rr$of_100,
					   popup = popup_html) %>%
	addLabelOnlyMarkers(data = centers,
						lng = ~x, lat = ~y, label = ~neighborhood_name,
						labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE))
basemap <- addProviderTiles(basemap, providers$Esri)
print(basemap)
