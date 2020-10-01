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

library(data.table)
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
		# normalize misspelled and otherwise mangled names
		str_replace_all(., 'carleton village sports wellness', 'carleton village') %>%
		str_replace_all(., 'catholique renaissance', 'catholique la renaissance') %>%
		str_replace_all(., 'catholique riverside sud ii \\(jonathan pitre\\)', 'catholique riverside sud ii') %>%
		str_replace_all(., 'clemens mills', 'clemens mill') %>%
		str_replace_all(., 'eden rose', 'edenrose') %>%
		str_replace_all(., 'father f x o reilly catholic', 'father john kelly catholic') %>%
		str_replace_all(., 'guardian angel catholic es', 'guardian angels catholic') %>%
		str_replace_all(., 'humewood', 'pinewood') %>%
		str_replace_all(., 'ingleborough', 'marlborough') %>%
		str_replace_all(., 'kidsability authority', 'bloorview authority') %>%
		str_replace_all(., 'l <c3><a9>lementaire catholique sacr<c3><a9> c<c5><93>ur', 's<c3><a9>par<c3><a9>e sacr<c3><a9> coeur') %>%
		str_replace_all(., 'l catholique saint charles garnier', '<c3><a9>ic saint charles garnier') %>%
		str_replace_all(., 'martine grove', 'martingrove') %>%
		str_replace_all(., 'mountain ash', 'mountain view') %>%
		str_replace_all(., 'north field fice', 'north bendale') %>%
		str_replace_all(., 'oodeenawi', 'oodenawi') %>%
		str_replace_all(., 'ross drive', 'dorset drive') %>%
		str_replace_all(., 'saint columba catholic', 'st columba catholic') %>%
		str_replace_all(., 'saint francis de sales catholic', 'st francis de sales catholic') %>%
		str_replace_all(., 'st alberts catholic', 'st albert catholic') %>%
		str_replace_all(., 'st jean brebeuf', 'st joan arc') %>%
		str_replace_all(., 'st luke', 'st jude') %>%
		str_replace_all(., 'walnut grove', 'wilton grove') %>%
		str_replace_all(., 'william parkway', 'williams parkway') %>%
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

# 6. build/refresh school geocodes db ------------------------------------------

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

# 7. combine active school cases with demographic data -------------------------

# note: this whole section is still a work in progress!

# all schools with active cases
covid19_schools_active$School %>% unique %>% str_trim %>% sort

# find all mismatched school names
idx <- which(covid19_schools_active$School %in% school_demographics$`School Name` == FALSE)
covid19_schools_active$School[ idx ] %>% unique %>% str_trim %>% sort

# clean school names in demographic dataset
sn1 <- clean_school_names(school_demographics$`School Name`)

# clean school names in active cases dataset
sn2 <- clean_school_names(covid19_schools_active$School)

# let's see how well our cleaning function does...
# idx <- which(sn2 %in% sn1 == FALSE)
# mismatched_school_names <- sn2[ idx ] %>% unique %>% str_trim %>% sort
# (mismatched_school_names)
# cat(mismatched_school_names, sep = '\n')
# (length(mismatched_school_names))

# use stringdist fuzzy matching to resolve mismatched names
# sdm <- stringdistmatrix(mismatched_school_names, sn1)

# visually inspect the quality of our fuzzy matches
# sprintf('%s --> %s', mismatched_school_names, sn1[ apply(sdm, 1, which.min) ]) %>% cat(., sep = '\n')

# use this code for string matching diagnostics
# View(data.frame(sn1 %>% sort))
# View(data.frame(sn2 %>% sort))
# View(data.frame(covid19_schools_active$School))

# add cleaned names to demographic and active cases datasets
covid19_schools_active$school_clean <- sn2
school_demographics$school_clean <- sn1

# merge demographic and activ cases datasets
# merge does not work we have to use school name and board name
# covid19_schools_active_with_demographics <- merge(covid19_schools_active, school_demographics, by = 'school_clean', all.y = FALSE)
# summary(covid19_schools_active_with_demographics)

covid19_schools_active_with_demographics <- apply(covid19_schools_active, 1, function(x) {
	# print(x)
	# browser()
	# match on cleaned school name
	idx <- which(school_demographics$school_clean == x[ 'school_clean' ])
	if (length(idx) > 1) {
		disambiguation_df <- data.frame(x[ 'School' ],
										school_demographics[ idx, c('School Name', 'Board Name') ],
										x[ 'School_Board' ],
										stringsAsFactors = FALSE)
		# print(disambiguation_df)
		# if all school boards are the same it is a question of whether this is an elemtary or a high school
		if (length(unique(disambiguation_df[ , 3 ])) == 1) {
			# are we looking at the high school or the elementary school?
			# message('disambiguated by school type')
			# browser()
			idx <- stringdistmatrix(disambiguation_df[ 1, 1 ], disambiguation_df[ , 2 ])
			idx <- which.min(idx)
			message(sprintf('chose entry %s', idx))
			result <- data.frame(t(x), school_demographics[ idx, ])
		} else {
			# fuzzy match the board names
			# message('disambiguated by school board name')
			# browser()
			idx <- stringdistmatrix(disambiguation_df[ 1, 4 ], disambiguation_df[ , 3 ])
			idx <- which.min(idx)
			# message(sprintf('chose entry %s', idx))
			result <- data.frame(t(x), school_demographics[ idx, ])
		}
	} else if (length(idx) == 1) {
		# we have a perfect match
		result <- data.frame(t(x), school_demographics[ idx, ])
	} else if (length(idx) == 0) {
		# this is one of the cleaned school names that is mismatched
		# browser()
		message('disambiguated by fuzzy matching of cleaned school')
		sdm <- stringdistmatrix(mismatched_school_names, sn1)
		
		# visually inspect the quality of our fuzzy matches
		sprintf('%s --> %s', mismatched_school_names, sn1[ apply(sdm, 1, which.min) ]) %>% cat(., sep = '\n')
		result <- data.frame(t(x), school_demographics[ idx, ])
	}
	return(result)
})
covid19_schools_active_with_demographics <- rbindlist(covid19_schools_active_with_demographics)
covid19_schools_active_with_demographics <- data.frame(covid19_schools_active_with_demographics)

# 8. clean combined case and demographics data ---------------------------------
str(covid19_schools_active_with_demographics)
# 'data.frame':	1674 obs. of  62 variables:
# 	$ Collected_Date                                                                          : chr  "2020-09-10" "2020-09-10" "2020-09-10" "2020-09-10" ...
covid19_schools_active_with_demographics$Collected_Date <- as.Date(covid19_schools_active_with_demographics$Collected_Date)
# $ Report_Date                                                                             : chr  "2020-09-11" "2020-09-11" "2020-09-11" "2020-09-11" ...
covid19_schools_active_with_demographics$Report_Date <- as.Date(covid19_schools_active_with_demographics$Report_Date)
# $ School_Board                                                                            : chr  "Conseil des écoles catholiques du Centre-Est (CECCE)" "Conseil des écoles catholiques du Centre-Est (CECCE)" "Conseil des écoles catholiques du Centre-Est (CECCE)" "Conseil des écoles catholiques du Centre-Est (CECCE)" ...
# $ School                                                                                  : chr  "École élémentaire catholique Roger-Saint-Denis" "École élémentaire catholique Saint-François-d’Assise" "École élémentaire catholique Sainte-Anne" "École élémentaire catholique Laurier-Carrière" ...
# $ Municipality                                                                            : chr  "Ottawa" "Ottawa" "Ottawa" "Ottawa" ...
# $ Confirmed_Student_Cases                                                                 : chr  "0" "1" "1" "1" ...
covid19_schools_active_with_demographics$Confirmed_Student_Cases <- as.integer(covid19_schools_active_with_demographics$Confirmed_Student_Cases)
# $ Confirmed_Staff_Cases                                                                   : chr  "1" "0" "0" "0" ...
covid19_schools_active_with_demographics$Confirmed_Staff_Cases <- as.integer(covid19_schools_active_with_demographics$Confirmed_Staff_Cases)
# $ Confirmed_Unspecified_Cases                                                             : chr  NA NA NA NA ...
covid19_schools_active_with_demographics$Confirmed_Unspecified_Cases <- as.integer(covid19_schools_active_with_demographics$Confirmed_Unspecified_Cases)
# $ Total_Confirmed_Cases                                                                   : chr  "1" "1" "1" "1" ...
covid19_schools_active_with_demographics$Total_Confirmed_Cases <- as.integer(covid19_schools_active_with_demographics$Total_Confirmed_Cases)
# $ school_clean                                                                            : chr  "catholique roger saint denis" "catholique saint fran<c3><a7>ois d assise" "catholique sainte anne" "catholique laurier carri<c3><a8>re" ...
# $ Board.Number                                                                            : chr  "B67334" "B67334" "B67334" "B67334" ...
# $ Board.Name                                                                              : chr  "CSDC du Centre-Est de l'Ontario" "CSDC du Centre-Est de l'Ontario" "CSDC du Centre-Est de l'Ontario" "CSDC du Centre-Est de l'Ontario" ...
# $ Board.Type                                                                              : chr  "Cath Dist Sch Brd (E/F)" "Cath Dist Sch Brd (E/F)" "Cath Dist Sch Brd (E/F)" "Cath Dist Sch Brd (E/F)" ...
# $ School.Number                                                                           : chr  "753203" "793027" "860573" "734314" ...
# $ School.Name                                                                             : chr  "École élémentaire catholique Roger-Saint-Denis" "École élémentaire catholique Saint-François-d'Assise" "École élémentaire catholique Sainte-Anne" "École élémentaire catholique Laurier-Carrière" ...
# $ School.Type                                                                             : chr  "Catholic" "Catholic" "Catholic" "Catholic" ...
# $ School.Special.Condition.Code                                                           : chr  "Not applicable" "Not applicable" "Not applicable" "Not applicable" ...
# $ School.Level                                                                            : chr  "Elementary" "Elementary" "Elementary" "Elementary" ...
# $ School.Language                                                                         : chr  "French" "French" "French" "French" ...
# $ Grade.Range                                                                             : chr  "JK-6" "JK-6" "JK-8" "JK-6" ...
# $ Building.Suite                                                                          : chr  NA NA NA NA ...
# $ P.O..Box                                                                                : chr  NA NA NA NA ...
# $ Street                                                                                  : chr  "186 Barrow crois." "35 Melrose av." "235 Beausoleil promenade" "14 Four Seasons prom." ...
# $ Municipality.1                                                                          : chr  "Ottawa" "Ottawa" "Ottawa" "Ottawa" ...
# $ City                                                                                    : chr  "Kanata" "Ottawa" "Ottawa" "Nepean" ...
# $ Province                                                                                : chr  "Ontario" "Ontario" "Ontario" "Ontario" ...
# $ Postal.Code                                                                             : chr  "K2L2C7" "K1Y1T8" "K1N0C1" "K2E7P8" ...
# $ Phone.Number                                                                            : chr  "613-521-3815" "613-729-1463" "613-789-3933" "613-224-3211" ...
# $ Fax.Number                                                                              : chr  "613-592-8275" "613-729-2291" "613-789-1265" "613-225-3783" ...
# $ School.Website                                                                          : chr  "http://www.roger-saint-denis.ecolecatholique.ca/" "http://www.saint-francois-dassise.ecolecatholique.ca/" "http://www.sainte-anne.ecolecatholique.ca/" "http://www.laurier-carriere.ecolecatholique.ca/" ...
# $ Board.Website                                                                           : chr  "www.ecolecatholique.ca/fr/" "www.ecolecatholique.ca/fr/" "www.ecolecatholique.ca/fr/" "www.ecolecatholique.ca/fr/" ...
# $ Enrolment                                                                               : chr  "190" "345" "220" "400" ...
# $ Latitude                                                                                : num  45.3 45.4 45.4 45.4 45.3 ...
# $ Longitude                                                                               : num  -75.9 -75.7 -75.7 -75.7 -75.8 ...
# $ Percentage.of.Students.Whose.First.Language.Is.Not.English                              : chr  "65" "60" "80" "60" ...
covid19_schools_active_with_demographics$Percentage.of.Students.Whose.First.Language.Is.Not.English <- as.integer(covid19_schools_active_with_demographics$Percentage.of.Students.Whose.First.Language.Is.Not.English)
# $ Percentage.of.Students.Whose.First.Language.Is.Not.French                               : chr  "55" "45" "40" "50" ...
covid19_schools_active_with_demographics$Percentage.of.Students.Whose.First.Language.Is.Not.French <- as.integer(covid19_schools_active_with_demographics$Percentage.of.Students.Whose.First.Language.Is.Not.French)
# $ Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country        : chr  "10" "10" "20" "5" ...
covid19_schools_active_with_demographics$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country <- as.integer(covid19_schools_active_with_demographics$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.English.Speaking.Country)
# $ Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country         : chr  "10" "10" "20" "10" ...
covid19_schools_active_with_demographics$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country <- as.integer(covid19_schools_active_with_demographics$Percentage.of.Students.Who.Are.New.to.Canada.from.a.Non.French.Speaking.Country)
# $ Percentage.of.Students.Identified.as.Gifted                                             : chr  "25" "10" "20" "15" ...
covid19_schools_active_with_demographics$Percentage.of.Students.Identified.as.Gifted <- as.integer(covid19_schools_active_with_demographics$Percentage.of.Students.Identified.as.Gifted)
# $ Percentage.of.Students.Receiving.Special.Education.Services                             : chr  "0" "0" "0" "0" ...
covid19_schools_active_with_demographics$Percentage.of.Students.Receiving.Special.Education.Services <- as.integer(covid19_schools_active_with_demographics$Percentage.of.Students.Receiving.Special.Education.Services)
# $ Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading             : chr  "67%" "83%" "65%" "92%" ...
covid19_schools_active_with_demographics$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Reading <- NULL
# $ Change.in.Grade.3.Reading.Achievement.Over.Three.Years                                  : chr  "0" "-12" "-23" "5" ...
covid19_schools_active_with_demographics$Change.in.Grade.3.Reading.Achievement.Over.Three.Years <- NULL
# $ Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing             : chr  "54%" "73%" "71%" "81%" ...
covid19_schools_active_with_demographics$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Writing <- NULL
# $ Change.in.Grade.3.Writing.Achievement.Over.Three.Years                                  : chr  "-17" "-17" "-17" "-3" ...
covid19_schools_active_with_demographics$Change.in.Grade.3.Writing.Achievement.Over.Three.Years <- NULL
# $ Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics         : chr  "58%" "71%" "47%" "73%" ...
covid19_schools_active_with_demographics$Percentage.of.Grade.3.Students.Achieving.the.Provincial.Standard.in.Mathematics <- NULL
# $ Change.in.Grade.3.Mathematics.Achievement.Over.Three.Years                              : chr  "-9" "-9" "-41" "-6" ...
covid19_schools_active_with_demographics$Change.in.Grade.3.Mathematics.Achievement.Over.Three.Years <- NULL
# $ Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading             : chr  "96%" "96%" "97%" "79%" ...
covid19_schools_active_with_demographics$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Reading <- NULL
# $ Change.in.Grade.6.Reading.Achievement.Over.Three.Years                                  : chr  "13" "1" "14" "-13" ...
covid19_schools_active_with_demographics$Change.in.Grade.6.Reading.Achievement.Over.Three.Years <- NULL
# $ Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing             : chr  "96%" "89%" "85%" "67%" ...
covid19_schools_active_with_demographics$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Writing <- NULL
# $ Change.in.Grade.6.Writing.Achievement.Over.Three.Years                                  : chr  "13" "3" "38" "-16" ...
covid19_schools_active_with_demographics$Change.in.Grade.6.Writing.Achievement.Over.Three.Years <- NULL
# $ Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics         : chr  "96%" "91%" "82%" "67%" ...
covid19_schools_active_with_demographics$Percentage.of.Grade.6.Students.Achieving.the.Provincial.Standard.in.Mathematics <- NULL
# $ Change.in.Grade.6.Mathematics.Achievement.Over.Three.Years                              : chr  "13" "9" "35" "-18" ...
covid19_schools_active_with_demographics$Change.in.Grade.6.Mathematics.Achievement.Over.Three.Years <- NULL
# $ Percentage.of.Grade.9.Students.Achieving.the.Provincial.Standard.in.Academic.Mathematics: chr  "NA" "NA" "NA" "NA" ...
covid19_schools_active_with_demographics$Percentage.of.Grade.9.Students.Achieving.the.Provincial.Standard.in.Academic.Mathematics <- NULL
# $ Change.in.Grade.9.Academic.Mathematics.Achievement.Over.Three.Years                     : chr  "NA" "NA" "NA" "NA" ...
covid19_schools_active_with_demographics$Change.in.Grade.9.Academic.Mathematics.Achievement.Over.Three.Years <- NULL
# $ Percentage.of.Grade.9.Students.Achieving.the.Provincial.Standard.in.Applied.Mathematics : chr  "NA" "NA" "NA" "NA" ...
covid19_schools_active_with_demographics$Percentage.of.Grade.9.Students.Achieving.the.Provincial.Standard.in.Applied.Mathematics <- NULL
# $ Change.in.Grade.9.Applied.Mathematics.Achievement.Over.Three.Years                      : chr  "NA" "NA" "NA" "NA" ...
covid19_schools_active_with_demographics$Change.in.Grade.9.Applied.Mathematics.Achievement.Over.Three.Years <- NULL
# $ Percentage.of.Students.That.Passed.the.Grade.10.OSSLT.on.Their.First.Attempt            : chr  "NA" "NA" "NA" "NA" ...
covid19_schools_active_with_demographics$Percentage.of.Students.That.Passed.the.Grade.10.OSSLT.on.Their.First.Attempt <- NULL
# $ Change.in.Grade.10.OSSLT.Literacy.Achievement.Over.Three.Years                          : chr  "NA" "NA" "NA" "NA" ...
covid19_schools_active_with_demographics$Change.in.Grade.10.OSSLT.Literacy.Achievement.Over.Three.Years <- NULL
# $ Percentage.of.School.Aged.Children.Who.Live.in.Low.Income.Households                    : chr  "10" "30" "50" "20" ...
covid19_schools_active_with_demographics$Percentage.of.School.Aged.Children.Who.Live.in.Low.Income.Households <- as.integer(covid19_schools_active_with_demographics$Percentage.of.School.Aged.Children.Who.Live.in.Low.Income.Households)
# $ Percentage.of.Students.Whose.Parents.Have.Some.University.Education                     : chr  "55" "55" "35" "60" ...
covid19_schools_active_with_demographics$Percentage.of.Students.Whose.Parents.Have.Some.University.Education <- as.integer(covid19_schools_active_with_demographics$Percentage.of.Students.Whose.Parents.Have.Some.University.Education)
# $ Extract.Date                                                                            : POSIXct, format: "2020-09-04" "2020-09-04" "2020-09-04" "2020-09-04" ...
# $ school_clean.1                                                                          : chr  "catholique roger saint denis" "catholique saint fran<c3><a7>ois d assise" "catholique sainte anne" "catholique laurier carri<c3><a8>re" ...

# 9. clean active cases data ---------------------------------------------------

covid19_schools_active$Collected_Date <- as.Date(covid19_schools_active$Collected_Date)
covid19_schools_active$Report_Date <- as.Date(covid19_schools_active$Report_Date)

# 10. clean summary data --------------------------------------------------------

covid19_schools_summary$Collected_Date <- as.Date(covid19_schools_summary$Collected_Date)
covid19_schools_summary$Report_Date <- as.Date(covid19_schools_summary$Report_Date)

# 11. clean all cases data -----------------------------------------------------

covid19_all_cases$Accurate_Episode_Date <- as.Date(covid19_all_cases$Accurate_Episode_Date)
covid19_all_cases$Accurate_Episode_Date <- as.Date(covid19_all_cases$Case_Reported_Date)
covid19_all_cases$Accurate_Episode_Date <- as.Date(covid19_all_cases$Test_Reported_Date)

school_demographics$`Board Name` <- str_replace_all(school_demographics$`Board Name`, '’', '\'')
covid19_schools_active$School_Board <- str_replace_all(covid19_schools_active$School_Board, '’', '\'')

# 12. exploratory data analysis ------------------------------------------------

# summary data exploration
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

# active cases data exploration
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

# active cases with demographic data exploration

# 13. plot cases by school on map ----------------------------------------------

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