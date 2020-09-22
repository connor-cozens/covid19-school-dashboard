# NOTES ------------------------------------------------------------------------

# https://data.ontario.ca/dataset?keywords_en=COVID-19

# https://data.ontario.ca/dataset/summary-of-cases-in-schools
# https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/7fbdbb48-d074-45d9-93cb-f7de58950418/download/schoolcovidsummary.csv
# https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/8b6d22e2-7065-4b0f-966f-02640be366f2/download/schoolsactivecovid.csv

# https://data.ontario.ca/dataset/summary-of-cases-in-licensed-child-care-settings
# https://data.ontario.ca/dataset/5bf54477-6147-413f-bab0-312f06fcb388/resource/eee282d3-01e6-43ac-9159-4ba694757aea/download/lccactivecovid.csv

# https://data.ontario.ca/dataset/confirmed-positive-cases-of-covid-19-in-ontario
# https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv

# https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario
# https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/ed270bb8-340b-41f9-a7c6-e8ef587e6d11/download/covidtesting.csv

# https://data.ontario.ca/dataset/confirmed-positive-cases-of-covid-19-in-ontario
# https://data.ontario.ca/dataset/confirmed-positive-cases-of-covid-19-in-ontario
# https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/4f39b02b-47fe-4e66-95b6-e6da879c6910/download/conposcovidloc.geojson
# https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv

# DEPENDENCIES -----------------------------------------------------------------

library(ggmap)
library(httr)
library(leaflet)
library(lubridate)
library(RColorBrewer)
library(stringr)
library(xts)

# SETTINGS ---------------------------------------------------------------------

google_api_key <- 'AIzaSyCaT001gVQOgUi3BnJ5MJJHR8CgwcdIixQ' # you need a google api key with maps javascript api and geocoding api enabled

data_dir <- 'data'

geocodes_cache_file <- file.path(data_dir, 'geocode_cache.rdata')

max_file_age_hrs <- 24L

# MAIN -------------------------------------------------------------------------

# 1. download data -------------------------------------------------------------

url <- 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/7fbdbb48-d074-45d9-93cb-f7de58950418/download/schoolcovidsummary.csv'
fname_summary <- sprintf('%s/%s', data_dir, basename(url))
needs_refresh <- difftime(now(), as.POSIXct(file.info(fname_summary)$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh) GET(url, write_disk(fname_summary, overwrite = TRUE))

url <- 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/8b6d22e2-7065-4b0f-966f-02640be366f2/download/schoolsactivecovid.csv'
fname_active <- sprintf('%s/%s', data_dir, basename(url))
needs_refresh <- difftime(now(), as.POSIXct(file.info(fname_active)$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh) GET(url, write_disk(fname_active, overwrite = TRUE))

url <- 'https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv'
fname_all_cases <- sprintf('%s/%s', data_dir, basename(url))
needs_refresh <- difftime(now(), as.POSIXct(file.info(fname_all_cases)$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh) GET(url, write_disk(fname_all_cases, overwrite = TRUE))

# 2. load school summary data into memory --------------------------------------

covid19_schools_summary <- read.csv(fname_summary, fileEncoding = 'Windows-1252', stringsAsFactors = FALSE)

# 3. load school active cases data into memory ---------------------------------

covid19_schools_active <- read.csv(fname_active, fileEncoding = 'Windows-1252', stringsAsFactors = FALSE)

# 4. load all cases data into memory -------------------------------------------

covid19_all_cases <- read.csv(fname_all_cases, fileEncoding = 'Windows-1252', stringsAsFactors = FALSE)

# 5. build geocode db ----------------------------------------------------------

cached_geocodes <- data.frame(geo_query_str = NA, lon = NA, lat = NA)
if (file.exists(geocodes_cache_file)) load(file = geocodes_cache_file)
# create query strings
geo_query_str <- sprintf('%s,%s,Ontario,Canada', 
						 str_trim(covid19_schools_active$School), 
						 covid19_schools_active$Municipality)
geo_query_str <- unique(geo_query_str)
message(sprintf('we have %s geocode queries to make', length(geo_query_str)))
idx <- which(geo_query_str %in% cached_geocodes$geo_query_str)
message(sprintf('we have %s cached geocode entries', length(idx)))
if (length(idx) > 0) geo_query_str <- geo_query_str[ -idx ]
if (length(geo_query_str) > 0) {
	# we have queries to do
	message('processing geocoding requests')
	geo_query_str <- as.data.frame(geo_query_str)
	register_google(google_api_key)
	school_geocodes <- mutate_geocode(geo_query_str, geo_query_str)
	school_geocodes <- rbind(school_geocodes, cached_geocodes)
	# save known geocodes to local db to save api calls
	idx <- which(!is.na(school_geocodes$lon))
	cached_geocodes <- school_geocodes[ idx, ] 
	save('cached_geocodes', file = geocodes_cache_file)
	school_geocodes <- cached_geocodes
} else {
	# all of our data is already in cache
	message('all geocodes already in cache!')
	school_geocodes <- cached_geocodes
}

# 6. clean data ----------------------------------------------------------------

covid19_schools_active$Collected_Date <- as.Date(covid19_schools_active$Collected_Date)
covid19_schools_active$Report_Date <- as.Date(covid19_schools_active$Report_Date)

covid19_schools_summary$Collected_Date <- as.Date(covid19_schools_summary$Collected_Date)
covid19_schools_summary$Report_Date <- as.Date(covid19_schools_summary$Report_Date)

covid19_all_cases$Accurate_Episode_Date <- as.Date(covid19_all_cases$Accurate_Episode_Date)
covid19_all_cases$Accurate_Episode_Date <- as.Date(covid19_all_cases$Case_Reported_Date)
covid19_all_cases$Accurate_Episode_Date <- as.Date(covid19_all_cases$Test_Reported_Date)

# 7. exploratory data analysis -------------------------------------------------

# # summary
# (covid19_schools_summary)
# 
# dt <- as.Date(covid19_schools_summary[ , 'Collected_Date' ])
# 
# d1 <- covid19_schools_summary[ , 'Current_Schools_w_Cases' ]
# Current_Schools_w_Cases <- xts(d1, order.by = dt)
# plot(Current_Schools_w_Cases, main = 'current schools with cases')
# 
# d1 <- covid19_schools_summary[ , 'Current_Schools_w_Cases' ] / covid19_schools_summary[ , 'Current_Total_Number_Schools' ] * 1e2
# Current_Schools_w_Cases_percent <- xts(d1, order.by = dt)
# plot(Current_Schools_w_Cases_percent, main = 'current schools with cases percent')
# 
# d1 <- covid19_schools_summary[ , 'Cumulative_School_Related_Cases' ]
# Cumulative_School_Related_Cases <- xts(d1, order.by = dt)
# plot(Cumulative_School_Related_Cases, main = 'cumulative school related cases')
# 
# # active cases
# (covid19_schools_active)
# 
# dt <- as.Date(covid19_schools_active[ , 'Collected_Date' ])
# idx <- which(!is.na(dt))
# covid19_schools_active <- covid19_schools_active[ idx, ]
# dt <- dt[ idx ]
# 
# active_cases_by_municipality <- tapply(covid19_schools_active$Municipality,
# 									   list(covid19_schools_active$Collected_Date,
# 									   	 covid19_schools_active$Municipality),
# 									   length)
# active_cases_by_municipality <- as.xts(active_cases_by_municipality)
# plot(active_cases_by_municipality, type = 'b',
# 	 main = 'cases in schools by municipality',
# 	 legend.loc = 'topleft',
# 	 grid.ticks.lwd = 0.25)

# 8. plot cases by school on map --------------------------------------------------

geo_query_str <- sprintf('%s,%s,Ontario,Canada', 
						 str_trim(covid19_schools_active$School), 
						 covid19_schools_active$Municipality)
cases_per_school <- tapply(covid19_schools_active$Total_Confirmed_Cases, geo_query_str, sum)
cases_per_school <- data.frame(cases_per_school, geo_query_str = names(cases_per_school))
rownames(cases_per_school) <- NULL
cases_per_school <- merge(cases_per_school, school_geocodes, by = 'geo_query_str')
cases_per_school$school_name <- str_split(cases_per_school$geo_query_str, ',') %>% sapply('[', 1)
cases_per_school$city <- str_split(cases_per_school$geo_query_str, ',') %>% sapply('[', 2)
# m <- leaflet() %>% setView(lng = -85.3232, lat = 49, zoom = 5)
# m %>% addTiles() %>%
# 	addCircleMarkers(data = cases_per_school, 
# 					 lng = ~lon, 
# 					 lat = ~lat, 
# 					 radius = ~(cases_per_school), # ~(cases_per_school)^(1/5), 
# 					 weight = 1, 
# 					 # color = covid_col, 
# 					 fillOpacity = 0.1, 
# 					 label = sprintf('<strong>%s</strong><br/>Confirmed cases (cumulative): %s<br/>', 
# 					 				cases_per_school$school_name, 
# 					 				cases_per_school$cases_per_school) %>% lapply(htmltools::HTML), 
# 					 labelOptions = labelOptions(
# 					 	style = list("font-weight" = "normal", padding = "3px 8px"),
# 					 	textsize = "15px", direction = "auto"))
