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

# https://data.ontario.ca/dataset/school-information-and-student-demographics
# https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a/download/new_sif_data_table_2018_2019prelim_en_august.xlsx

# DEPENDENCIES -----------------------------------------------------------------

library(ggmap)
library(httr)
library(leaflet)
library(lubridate)
library(readxl)
library(RColorBrewer)
library(stringr)
library(xts)
# library(utils)
library(stringdist)

# SETTINGS ---------------------------------------------------------------------

google_api_key <- 'XXXXXXXXXXXXXXXXXXXXXXXXXX' # you need a google api key with maps javascript api and geocoding api enabled

data_dir <- 'data'

geocodes_cache_file <- file.path(data_dir, 'geocode_cache.rdata')

max_file_age_hrs <- 12L

# UTILITY FUNCTIONS ------------------------------------------------------------

#' clean_school_names
#' 
#' function to normalize school names so we can match schools in active cases dataset
#' with the school demographic dataset
#' 
clean_school_names <- function(school_names) {
	stopwords_en <- c('school', 'high', 'elementary', 'middle', 'secondary',
					  'public', 'collegiate', 'institute', 'junior', 'senior',
					  'jr', 'sr', 'learning', 'centre', 'adult', 'intstitute',
					  'elemenary', '&', '\'', 'for', 'of', 'and', 'academy') %>% 
		paste0(., collapse = '|')
	stopwords_fr <- c('école', 'l\'', 'secondaire', 'élémentaire', 'publique') %>% 
		paste0(., collapse = '|')
	tolower(school_names) %>% 
		str_replace_all(.,  stopwords_en, ' ') %>% 
		str_replace_all(.,  stopwords_fr, ' ') %>% 
		str_replace_all(.,  '\\.', ' ') %>% 
		str_replace_all(., '’', ' ') %>%
		str_replace_all(., '\\-', ' ') %>%
		str_replace_all(., ' s ', ' ') %>%
		str_replace_all(., '\'', ' ') %>%
		iconv(., 'ASCII//TRANSLIT', sub = 'byte') %>% 
		str_replace_all(.,  '\\s+', ' ') %>%
		str_replace_all(., '\\<c2\\>\\<a0\\>', ' ') %>%
		str_trim
}

# MAIN -------------------------------------------------------------------------

# 1. download data -------------------------------------------------------------

# school summary data
url <- 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/7fbdbb48-d074-45d9-93cb-f7de58950418/download/schoolcovidsummary.csv'
fname_summary <- sprintf('%s/%s', data_dir, basename(url))
needs_refresh <- difftime(now(), as.POSIXct(file.info(fname_summary)$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh | is.na(needs_refresh)) { 
	message('updating summary data file')
	GET(url, write_disk(fname_summary, overwrite = TRUE))
}

# schools active cases data
url <- 'https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/8b6d22e2-7065-4b0f-966f-02640be366f2/download/schoolsactivecovid.csv'
fname_active <- sprintf('%s/%s', data_dir, basename(url))
needs_refresh <- difftime(now(), as.POSIXct(file.info(fname_active)$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh | is.na(needs_refresh)) { 
	message('updating active cases data file')
	GET(url, write_disk(fname_active, overwrite = TRUE))
}

# ontario all covid cases data
url <- 'https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv'
fname_all_cases <- sprintf('%s/%s', data_dir, basename(url))
needs_refresh <- difftime(now(), as.POSIXct(file.info(fname_all_cases)$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh | is.na(needs_refresh)) { 
	message('updating all cases data file')
	GET(url, write_disk(fname_all_cases, overwrite = TRUE))
}

# schools demographic data
url <- 'https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a/download/new_sif_data_table_2018_2019prelim_en_august.xlsx'
fname_demographics <- sprintf('%s/%s', data_dir, basename(url))
needs_refresh <- difftime(now(), as.POSIXct(file.info(fname_demographics)$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh | is.na(needs_refresh)) { 
	message('updating student demographics data file')
	GET(url, write_disk(fname_demographics, overwrite = TRUE))
}

# 2. load school summary data into memory --------------------------------------

covid19_schools_summary <- read.csv(fname_summary, fileEncoding = 'Windows-1252', stringsAsFactors = FALSE)

# 3. load school active cases data into memory ---------------------------------

covid19_schools_active <- read.csv(fname_active, fileEncoding = 'Windows-1252', stringsAsFactors = FALSE)

# 4. load all cases data into memory -------------------------------------------

covid19_all_cases <- read.csv(fname_all_cases, fileEncoding = 'Windows-1252', stringsAsFactors = FALSE)

# 5. load school demographic data into memory ----------------------------------

school_demographics <- read_xlsx(fname_demographics)
school_demographics <- as.data.frame(school_demographics, stringsAsFactors = FALSE)

# 5. build/refresh school geocodes db ------------------------------------------

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
	geo_query_str <- data.frame(geo_query_str, stringsAsFactors = FALSE)
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

school_demographics$`Board Name` <- str_replace_all(school_demographics$`Board Name`, '’', '\'')
covid19_schools_active$School_Board <- str_replace_all(covid19_schools_active$School_Board, '’', '\'')

# 7. exploratory data analysis -------------------------------------------------

# summary
(covid19_schools_summary)

dt <- as.Date(covid19_schools_summary[ , 'Collected_Date' ])

d1 <- covid19_schools_summary[ , 'Current_Schools_w_Cases' ]
Current_Schools_w_Cases <- xts(d1, order.by = dt)
plot(Current_Schools_w_Cases, main = 'current schools with cases')

d1 <- covid19_schools_summary[ , 'Current_Schools_w_Cases' ] / covid19_schools_summary[ , 'Current_Total_Number_Schools' ] * 1e2
Current_Schools_w_Cases_percent <- xts(d1, order.by = dt)
plot(Current_Schools_w_Cases_percent, main = 'current schools with cases percent')

d1 <- covid19_schools_summary[ , 'Cumulative_School_Related_Cases' ]
Cumulative_School_Related_Cases <- xts(d1, order.by = dt)
plot(Cumulative_School_Related_Cases, main = 'cumulative school related cases')

# active cases
(covid19_schools_active)

dt <- as.Date(covid19_schools_active[ , 'Collected_Date' ])
idx <- which(!is.na(dt))
covid19_schools_active <- covid19_schools_active[ idx, ]
dt <- dt[ idx ]

active_cases_by_municipality <- tapply(covid19_schools_active$Municipality,
									   list(covid19_schools_active$Collected_Date,
									   	 covid19_schools_active$Municipality),
									   length)
active_cases_by_municipality <- as.xts(active_cases_by_municipality)
plot(active_cases_by_municipality, type = 'b',
	 main = 'cases in schools by municipality',
	 legend.loc = 'topleft',
	 grid.ticks.lwd = 0.25)

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

# 9. combine active school cases with demographic data -------------------------

# note: this whole section is still a work in progress!

# # all schools with active cases
# covid19_schools_active$School %>% unique %>% str_trim %>% sort
# 
# # find all mismatched school names
# idx <- which(covid19_schools_active$School %in% school_demographics$`School Name` == FALSE)
# covid19_schools_active$School[ idx ] %>% unique %>% str_trim %>% sort
# 
# 
# # clean school names in demographic dataset
# sn1 <- clean_school_names(school_demographics$`School Name`)
# 
# # clean school names in active cases dataset
# sn2 <- clean_school_names(covid19_schools_active$School)
# 
# # let's see how well our cleaning function does...
# idx <- which(sn2 %in% sn1 == FALSE)
# mismatched_school_names <- sn2[ idx ] %>% unique %>% str_trim %>% sort
# (mismatched_school_names)
# cat(mismatched_school_names, sep = '\n')
# (length(mismatched_school_names))
# 
# # carleton village sports wellness
# # catholique renaissance
# # clemens mills
# # eden rose
# # father f x o reilly catholic
# # guardian angel catholic es
# # humewood
# # ingleborough
# # l catholique saint charles garnier
# # mountain ash
# # north field fice
# # oodeenawi
# # ross drive
# # st alberts catholic
# # st jean brebeuf
# # st luke
# # walnut grove
# # william parkway
# 
# # use stringdist fuzzy matching to resolve mismatched names
# sdm <- stringdistmatrix(mismatched_school_names, sn1)
# 
# # visually inspect the quality of our fuzzy matches
# sprintf('%s --> %s', mismatched_school_names, sn1[ apply(sdm, 1, which.min) ]) %>% cat(., sep = '\n')
# 
# # use this code for string matching diagnostics
# # View(data.frame(sn1 %>% sort))
# # View(data.frame(sn2 %>% sort))
# # View(data.frame(covid19_schools_active$School))
# 
# # add cleaned names to demographic and active cases datasets
# covid19_schools_active$school_clean <- sn2
# school_demographics$school_clean <- sn1
# 
# # merge demographic and activ cases datasets
# covid19_schools_active_with_demographics <- merge(covid19_schools_active, school_demographics, by = 'school_clean', all.y = FALSE)
# summary(covid19_schools_active_with_demographics)
# 
# covid19_schools_active_with_demographics <- apply(covid19_schools_active, 1, function(x) {
# 	print(x)
# 	# browser()
# 	# match on cleaned school name
# 	idx <- which(school_demographics$school_clean == x[ 'school_clean' ])
# 	if (length(idx) > 1) {
# 		disambiguation_df <- data.frame(x[ 'School' ], 
# 										school_demographics[ idx, c('School Name', 'Board Name') ], 
# 										x[ 'School_Board' ], 
# 										stringsAsFactors = FALSE)
# 		print(disambiguation_df)
# 		# if all school boards are the same it is a question of whether this is an elemtary or a high school
# 		if (length(unique(disambiguation_df[ , 3 ])) == 1) {
# 			# are we looking at the high school or the elementary school?
# 			message('disambiguated by school type')
# 			# browser()	
# 			idx <- stringdistmatrix(disambiguation_df[ 1, 1 ], disambiguation_df[ , 2 ])
# 			idx <- which.min(idx)
# 			message(sprintf('chose entry %s', idx))
# 			result <- data.frame(t(x), school_demographics[ idx, ])
# 		} else {
# 			# fuzzy match the board names
# 			message('disambiguated by school board name')
# 			# browser()	
# 			idx <- stringdistmatrix(disambiguation_df[ 1, 4 ], disambiguation_df[ , 3 ])
# 			idx <- which.min(idx)
# 			message(sprintf('chose entry %s', idx))
# 			result <- data.frame(t(x), school_demographics[ idx, ])
# 		}
# 	} else if (length(idx) == 1) {
# 		# we have a perfect match
# 		result <- data.frame(t(x), school_demographics[ idx, ])	
# 	} else if (length(idx) == 0) {
# 		# this is one of the cleaned school names that is mismatched
# 		# browser()
# 		message('disambiguated by fuzzy matching of cleaned school')
# 		sdm <- stringdistmatrix(mismatched_school_names, sn1)
# 		
# 		# visually inspect the quality of our fuzzy matches
# 		sprintf('%s --> %s', mismatched_school_names, sn1[ apply(sdm, 1, which.min) ]) %>% cat(., sep = '\n')
# 		result <- data.frame(t(x), school_demographics[ idx, ])	
# 	}
# 	return(result)
# })
