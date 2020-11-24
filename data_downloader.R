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
library(rvest)
library(stringr)
library(xts)
# library(utils)
library(stringdist)

# SETTINGS ---------------------------------------------------------------------

google_api_key <- 'XXXXXXXXXXXXXXXXXXXXXXXXXX' # you need a google api key with maps javascript api and geocoding api enabled

data_dir <- 'data'

geocodes_cache_file <- file.path(data_dir, 'geocode_cache.rdata')

max_file_age_hrs <- 24L

debug <- FALSE

# UTILITY FUNCTIONS ------------------------------------------------------------

get_utf_table <- function() {
	url <- 'https://www.utf8-chartable.de/'
	pg_html <- read_html(url)
	utf8_tbl <- html_table(pg_html)[[ 3 ]]
	utf8_tbl$my_transliteration <- str_extract_all(utf8_tbl$name, '(CAPITAL|SMALL) LETTER [a-zA-Z]{1}\\s?') %>% 
		str_squish %>% 
		str_replace_all(., 'character\\(0\\)', '') %>%
		str_replace_all(., 'CAPITAL LETTER ', '') %>%
		str_squish
	idx <- which(utf8_tbl$name == 'LATIN CAPITAL LETTER AE')
	utf8_tbl[ idx, 'my_transliteration' ] <- 'AE'
	idx <- which(utf8_tbl$name == 'LATIN SMALL LETTER AE')
	utf8_tbl[ idx, 'my_transliteration' ] <- 'ae'
	idx <- which(str_detect(utf8_tbl$my_transliteration, 'SMALL LETTER '))
	utf8_tbl[ idx, 'my_transliteration' ] <- str_replace_all(utf8_tbl[ idx, 'my_transliteration' ], 'SMALL LETTER ', '') %>% 
		tolower
	idx <- which(utf8_tbl$name == 'LATIN CAPITAL LETTER AE')
	utf8_tbl[ idx, 'my_transliteration' ] <- 'AE'
	idx <- which(utf8_tbl$my_transliteration != '')
	utf8_tbl <- utf8_tbl[ idx, ]
	idx <- which(nchar(utf8_tbl[ , 3 ]) > 2)
	utf8_tbl <- utf8_tbl[ idx, ]
	byte_1 <- str_split(utf8_tbl[ , 3 ], ' ') %>% sapply(., '[', 1)
	byte_2 <- str_split(utf8_tbl[ , 3 ], ' ') %>% sapply(., '[', 2)
	tl <- utf8_tbl[ , 5 ]
	utf8_tbl$regex <- sprintf('str_replace_all(., "<%s><%s>", "%s") %%>%%',
							  byte_1,
							  byte_2,
							  tl)
	utf8_tbl
}

#' clean_all_names
#' 
#' function to normalize school and school board names so we can match schools in active cases dataset
#' with the school demographic dataset
#' 
#' TODO: just create a static mapping file for school and school board names, this
#' has rapidly devolved into madness... as expected. *sigh*
#' 	
#' debugging
#' df1 <- data.frame(school_name = sort(unique(covid19_schools_active$school)), 
#' 		   clean_name = clean_all_names(sort(unique(covid19_schools_active$school))))
#' View(df1)
#' df2 <- data.frame(school_name = school_demographics$`school name`, 
#' 		   clean_name = clean_all_names(school_demographics$`school name`))
#' View(df2)
#' 
clean_all_names <- function(dirty_names) {
	
	# clean up 1: fix iconv transliterated characters and remove extraneous characters
	clean_names <- str_replace_all(dirty_names, '[0-9]+', ' ') %>%
		iconv(., 'ASCII//TRANSLIT', sub = 'byte') %>%
		tolower %>% 
		str_replace_all(., '<c2><a0>', ' ') %>%
		str_replace_all(., '\\s+', ' ') %>%
		str_replace_all(., ',', ' ') %>%
		str_replace_all(., '\\.', '') %>%
		str_replace_all(., '\\(', ' ') %>%
		str_replace_all(., '\\)', ' ') %>%
		str_replace_all(., '/', ' ') %>%
		str_replace_all(., '@', ' ') %>%
		str_replace_all(., '\\+', ' ') %>%
		str_replace_all(., '\\-', ' ') %>%
		str_replace_all(., '&', ' ') %>%
		str_replace_all(., '<e2><80><99>', '\'') %>%
		str_replace_all(., '<c3><83><c2><ae>', ' ') %>%
		str_replace_all(., '<c3><a2><e2><82><ac><e2><84><a2>', '\'') %>%
		str_replace_all(., '<c3><82>', ' ') %>%
		str_replace_all(., '<c3><a2>', 'a') %>%
		str_replace_all(., '<c3><83><c2><a1>', 'a') %>%
		str_replace_all(., '<c3><83><c2><a2>', 'a') %>%
		str_replace_all(., '<c3><a1>', 'a') %>%
		str_replace_all(., '<c3><83><c2><a7>', 'c') %>%
		str_replace_all(., '<c3><a7>', 'c') %>%
		str_replace_all(., '<c3><83><c2><a9>l<c3><83><c2><a9>', 'el') %>%
		str_replace_all(., '<c3><83><e2><80><b0>', 'e') %>%
		str_replace_all(., '<c3><83><e2><80><9c>', 'e') %>%	
		str_replace_all(., '<c3><83><c2><ab>', 'e') %>%	
		str_replace_all(., '<c3><83><c2><a9>', 'e') %>%
		str_replace_all(., '<c3><83><c2><a8>', 'e') %>%
		str_replace_all(., '<c3><89>', 'e') %>%
		str_replace_all(., '<c3><a8>', 'e') %>%
		str_replace_all(., '<c3><a9>', 'e') %>%
		str_replace_all(., '<c3><83><c2><af>', 'i') %>%
		str_replace_all(., '<c3><ae>', 'i') %>%
		str_replace_all(., '<c5><93>', 'oe') %>%
		str_replace_all(., '<c3><85><e2><80><9c>', 'oe') %>%
		str_squish
	
	stopwords <- c('<c3><89><c3><89>c', 
				   '<c3><89>a', 
				   '<c3><89>cole', 
				   '<c3><89>ducation', 
				   '<c3><89>ep', 
				   '<c3><89>ic', 
				   '<c3><89> <c3><89>l<c3><a9>m c ',
				   '<c3><89>l<c3><a9>mentaire',
				   '<c3><89>lementaire',
				   '<c3><89>p',
				   '<c3><89>sac',
				   '<c3><89>sc',
				   '<c3><a9>coles', 
				   '<c3><a9>l<c3><a9>mentaire', 
				   'aaec\'s',
				   'acad<c3><a9>mie',
				   'academie',
				   'academy', 
				   'academy-', 
				   'adolescent', 
				   'adult',
				   'adultes', 
				   # 'alternate',
				   # 'alternative',
				   # 'alternatives', 
				   ' and ',
				   ' at ',
				   'authority',
				   'c<c3><89>p',
				   'catholic',
				   'catholique',
				   'catholiques', 
				   'c elem s',
				   'centre',
				   ' ces',
				   ' ci$',
				   'coll<c3><a8>ge', 
				   'college',
				   'collegiate', 
				   'community',
				   'conted', 
				   'continuing',
				   ' cvi\\s?',
				   'c vi\\s?',
				   'ecole',
				   '^eec ',
				   '^eep ',
				   '^eic ',
				   '^e elem c ',
				   'education', 
				   'elementaire', 
				   'elemenary', 
				   'elementary',
				   'elementry',
				   'elmentaire',
				   ' e s$',
				   ' es$',
				   '^esp ',
				   ' for ',
				   ' hs\\s?',
				   ' high\\s?', 
				   'highschool', 
				   'instititue', 
				   'institute', 
				   'institute-secondary',
				   'interm<c3><a9>diaire', 
				   'intermediate', 
				   'intermediaire',
				   'intstitute',
				   'junior',
				   'jr',
				   'jonathan pitre',
				   'l\'<c3><89>cole', 
				   'l\'acad<c3><a9>mie', 
				   'learning',
				   ' md\\s?', 
				   'middle ', 
				   ' ps\\s?', 
				   'pssb', 
				   'pte',
				   'pubic', 
				   'public', 
				   'publique',
				   'publiques', 
				   'school', 
				   'schools', 
				   'scolaire', 
				   'secondaire', 
				   'secondary',
				   'senior',
				   'separate',
				   'sports',
				   ' sr\\s?',
				   ' ss\\s?',
				   '\'s',
				   ' the',
				   'vocational',
				   'wellness')
	
	# clean up 2: remove stopwords
	stopwords_regex <- paste0(stopwords, collapse = '|')
	clean_names <- lapply(clean_names, str_replace_all, stopwords_regex, ' ')
	clean_names <- lapply(clean_names, str_squish)
	clean_names <- unlist(clean_names)
	
	# clean up 3: consistent formatting for catholic school names
	clean_names <- str_replace_all(clean_names, 'monsignor', 'msgr')
	clean_names <- str_replace_all(clean_names, 'monseigneur', 'msgr')
	clean_names <- str_replace_all(clean_names, 'saint([ |\\-])', 'st\\1')
	clean_names <- str_replace_all(clean_names, 'sainte([ |\\-])', 'ste\\1')
	clean_names <- str_squish(clean_names)
	
	# clean up 4: hard-coded disambiguations
	clean_names <- str_replace_all(clean_names, 'banting memorial district', 'banting memorial')
	clean_names <- str_replace_all(clean_names, 'blessed margherita of citta castello', 'blessed margherita of citta di castello')
	clean_names <- str_replace_all(clean_names, '^city calc$', 'calc')
	clean_names <- str_replace_all(clean_names, '^clifton drive$', 'clifton')
	clean_names <- str_replace_all(clean_names, '^de hearst$', 'hearst')
	clean_names <- str_replace_all(clean_names, 'provincial demonstration s', 'e c drury trillium demonstration')
	clean_names <- str_replace_all(clean_names, 'dunrankin drive', 'dunrankin')
	clean_names <- str_replace_all(clean_names, 'father henri nouwen', 'father henri j m nouwen')
	clean_names <- str_replace_all(clean_names, 'fisher park summit as', 'fisher park summit alternative')
	clean_names <- str_replace_all(clean_names, 'frank panabaker north ancaster', 'frank panabaker north')
	clean_names <- str_replace_all(clean_names, 'jean vanier formerly known as our lady queen of world', 'our lady queen of world ca')
	clean_names <- str_replace_all(clean_names, 'jean vanier renamed st joan of arc', 'our lady queen of world ca')
	clean_names <- str_replace_all(clean_names, 'john clarke richardson', 'j clarke richardson')
	clean_names <- str_replace_all(clean_names, 'mcnaughton avenue', 'mcnaughton ave')
	clean_names <- str_replace_all(clean_names, 'r h lagerquiste', 'robert h lagerquist')
	clean_names <- str_replace_all(clean_names, 'l\' st charles garnier', 'esc st charles garnier')
	clean_names <- str_replace_all(clean_names, '^msgr fraser$', 'msgr fraser midland')
	clean_names <- str_replace_all(clean_names, '^msgr fraser isabella$', 'msgr fraser isabella campus')
	clean_names <- str_replace_all(clean_names, '^msgr fraser norfinch$', 'msgr fraser norfinch campus')
	clean_names <- str_replace_all(clean_names, 'nora frances henderson', 'nora henderson')
	clean_names <- str_replace_all(clean_names, 'notre dame de la jeunesse ajax', 'notre dame de la jeunesse')
	clean_names <- str_replace_all(clean_names, 'notre dame de la jeunesse niagraf', 'notre dame de la jeunesse')
	clean_names <- str_replace_all(clean_names, '^of experiential$', 'experiential')
	clean_names <- str_replace_all(clean_names, 'ottawa technical storefront special', 'ottawa technical')
	clean_names <- str_replace_all(clean_names, 'pavillon l\'escale', 'l\'escale')
	clean_names <- str_replace_all(clean_names, 'pope st francis', 'pope francis')
	clean_names <- str_replace_all(clean_names, 'sarborough alternative studies', 'scarborough alternative studi')
	clean_names <- str_replace_all(clean_names, 'st john xxlll', 'st john xxiii')
	clean_names <- str_replace_all(clean_names, 'ursuline chatham', 'ursuline')
	clean_names <- str_replace_all(clean_names, '^unnamed formerly known as vaughan$', 'vaughan')
	clean_names <- str_replace_all(clean_names, '^unnamed formerly know as vaughan$', 'vaughan')
	clean_names <- str_replace_all(clean_names, 'western tech commercial', 'western technical commercial')
	clean_names <- str_replace_all(clean_names, '^western technical$', 'western technical commercial')
	
	clean_names
}

#' 

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


if (needs_refresh | is.na(needs_refresh)) {
	
	# REFRESH ALL DATA ---------------------------------------------------------
	
	# 1. load school summary data into memory ----------------------------------
	
	covid19_schools_summary <- read.csv(fname_summary, fileEncoding = 'Windows-1252', stringsAsFactors = FALSE)
	
	# 2. load school active cases data into memory -----------------------------
	
	covid19_schools_active <- read.csv(fname_active, fileEncoding = 'Windows-1252', stringsAsFactors = FALSE)
	
	# 3. load all cases data into memory ---------------------------------------
	
	covid19_all_cases <- read.csv(fname_all_cases, fileEncoding = 'Windows-1252', stringsAsFactors = FALSE)
	
	# 4. load school demographic data into memory ------------------------------
	
	school_demographics <- read_xlsx(fname_demographics)
	school_demographics <- as.data.frame(school_demographics, stringsAsFactors = FALSE)
	
	# 5. load school risk rank data --------------------------------------------
	
	fname_school_risk_rank <- file.path(data_dir, 'COVID19NeighbRiskRank_TCDSBElemSecond_2020-08-20.xlsx')
	risk_rank_elementary <- read_xlsx(fname_school_risk_rank, sheet = 2, skip = 3, col_names = TRUE)
	risk_rank_secondary <- read_xlsx(fname_school_risk_rank, sheet = 4, skip = 3, col_names = TRUE)
	
	# 6. load neighborhood risk rank data --------------------------------------
	
	fname_neighborhood_risk_rank <- file.path(data_dir, '11042020 with Demographics WALLACE.xlsx')
	risk_rank_neighborhood <- read_xlsx(fname_neighborhood_risk_rank, sheet = 1, skip = 2, col_names = FALSE)
	risk_rank_neighborhood <- risk_rank_neighborhood[ , setdiff(1:29, c(21, 26))]
	colnames(risk_rank_neighborhood) <- c('neighborhood_name',
										  'of_10',	
										  'of_15',	
										  'of_17',	
										  'of_19',	
										  'of_21',	
										  'of_23',	
										  'of_25',	
										  'of_30',	
										  'of_33',	
										  'of_50',	
										  'of_100',	
										  'of_250',	
										  'of_500',	
										  'of_750',	
										  'of_1000',	
										  'of_1250',
										  'of_1500',	
										  'of_1750',	
										  'of_2000',
										  'Population',
										  '7-Day Count',
										  'One Infection Per',
										  'Transmissible Cases before Isolation and Seroprevalence',
										  'Population Density Per Square Kilometre',
										  'Average Household Size',
										  'Prevalence of low income based on the Low-income cut-offs, after tax (LICO-AT) (%)')
	fn <- file.path(data_dir, 'risk_rank_neighborhood.rdata')
	save('risk_rank_neighborhood', file = fn)
	
	# 7. clean active cases data -----------------------------------------------
	
	colnames(covid19_schools_active) <- tolower(colnames(covid19_schools_active))
	covid19_schools_active$collected_date <- as.Date(covid19_schools_active$collected_date)
	covid19_schools_active$reported_date <- as.Date(covid19_schools_active$reported_date)
	covid19_schools_active$school <- str_squish(covid19_schools_active$school)
	covid19_schools_active$school_board <- str_squish(covid19_schools_active$school_board)
	covid19_schools_active$municipality <- str_squish(covid19_schools_active$municipality)
	fn <- file.path(data_dir, 'covid19_schools_active.rdata')
	save('covid19_schools_active', file = fn)
	
	# 8. clean summary data ----------------------------------------------------
	
	colnames(covid19_schools_summary) <- tolower(colnames(covid19_schools_summary))
	covid19_schools_summary$collected_date <- as.Date(covid19_schools_summary$collected_date)
	covid19_schools_summary$reported_date <- as.Date(covid19_schools_summary$reported_date)
	colnames(covid19_schools_summary) <- str_replace(colnames(covid19_schools_summary), 'current_schools_w_cases', 'current_schools_with_cases')
	colnames(covid19_schools_summary) <- str_replace(colnames(covid19_schools_summary), 'new_school_related_unspecified_cases', 'new_school_related_unidentified_cases')
	colnames(covid19_schools_summary) <- str_replace(colnames(covid19_schools_summary), 'recent_school_related_unspecified_cases', 'recent_school_related_unidentified_cases')
	colnames(covid19_schools_summary) <- str_replace(colnames(covid19_schools_summary), 'past_school_related_unspecified_cases', 'past_school_related_unidentified_cases')
	colnames(covid19_schools_summary) <- str_replace(colnames(covid19_schools_summary), 'cumulative_school_related_unspecified_cases', 'cumulative_school_related_unidentified_cases')
	fn <- file.path(data_dir, 'covid19_schools_summary.rdata')
	save('covid19_schools_summary', file = fn)
	
	# 7. clean all cases data --------------------------------------------------
	
	colnames(covid19_all_cases) <- tolower(colnames(covid19_all_cases))
	covid19_all_cases$accurate_episode_date <- as.Date(covid19_all_cases$accurate_episode_date)
	covid19_all_cases$case_reported_date <- as.Date(covid19_all_cases$case_reported_date)
	covid19_all_cases$test_reported_date <- as.Date(covid19_all_cases$test_reported_date)
	fn <- file.path(data_dir, 'covid19_all_cases.rdata')
	save('covid19_all_cases', file = fn)
	
	# 8. clean school demographics data ----------------------------------------
	
	colnames(school_demographics) <- tolower(colnames(school_demographics))
	school_demographics$`board name` <- str_replace_all(school_demographics$`board name`, '’', '\'')
	covid19_schools_active$school_board <- str_replace_all(covid19_schools_active$school_board, '’', '\'')
	school_demographics$`board name` <- str_squish(school_demographics$`board name`)
	school_demographics$`school name` <- str_squish(school_demographics$`school name`)
	school_demographics$municipality <- str_squish(school_demographics$municipality)
	fn <- file.path(data_dir, 'school_demographics.rdata')
	save('school_demographics', file = fn)
	
	# 9. clean risk assessment data --------------------------------------------
	
	risk_rank_elementary <- risk_rank_elementary[ , c(1:9, 13) ]
	colnames(risk_rank_elementary) <- tolower(colnames(risk_rank_elementary))
	fn <- file.path(data_dir, 'risk_rank_elementary.rdata')
	save('risk_rank_elementary', file = fn)
	
	risk_rank_secondary <- risk_rank_secondary[ , c(1:9, 13) ]
	colnames(risk_rank_secondary) <- tolower(colnames(risk_rank_secondary))
	fn <- file.path(data_dir, 'risk_rank_secondary.rdata')
	save('risk_rank_secondary', file = fn)
	
	# 9. build/refresh school geocodes db --------------------------------------
	
	cached_geocodes <- data.frame(geo_query_str = NA, lon = NA, lat = NA)
	if (file.exists(geocodes_cache_file)) load(file = geocodes_cache_file)
	# create query strings
	geo_query_str <- sprintf('%s,%s,Ontario,Canada', 
							 str_trim(covid19_schools_active$school), 
							 covid19_schools_active$municipality)
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
	
	# 10. combine active school cases with demographic data --------------------
	
	# note: this whole section is still a work in progress!
	
	# all schools with active cases
	# covid19_schools_active$school %>% unique %>% str_trim %>% sort
	
	# find all mismatched school names
	idx <- which(covid19_schools_active$school %in% school_demographics$`school name` == FALSE)
	covid19_schools_active$school[ idx ] %>% unique %>% str_trim %>% sort
	
	# clean school names in demographic dataset
	sn1 <- clean_all_names(school_demographics$`school name`)
	
	# clean school names in active cases dataset
	sn2 <- clean_all_names(covid19_schools_active$school)
	
	# let's see how well our cleaning function does...
	idx <- which(sn2 %in% sn1 == FALSE)
	mismatched_school_names <- sn2[ idx ] %>% unique %>% str_trim %>% sort
	# (mismatched_school_names)
	# cat(mismatched_school_names, sep = '\n')
	# (length(mismatched_school_names))
	
	# use stringdist fuzzy matching to resolve mismatched names
	sdm <- stringdistmatrix(mismatched_school_names, sn1)
	
	# visually inspect the quality of our fuzzy matches
	message(sprintf('fuzzy matching based on minimum string distance yields the following matches between names in cases and demographics datasets:'))
	sprintf('%s --> %s', mismatched_school_names, sn1[ apply(sdm, 1, which.min) ]) %>% cat(., sep = '\n')
	
	# use this code for string matching diagnostics
	# View(data.frame(sn1 %>% sort))
	# View(data.frame(sn2 %>% sort))
	# View(data.frame(covid19_schools_active$school))
	# View(data.frame(school_demographics$`school name`))
	
	# add cleaned names to school demographics and school active cases datasets
	covid19_schools_active$school_clean <- sn2
	school_demographics$school_clean <- sn1
	
	# combine school demographics and school active cases datasets
	ambiguous_school_names <- character(0)
	
	covid19_schools_active_with_demographics <- apply(covid19_schools_active, 1, function(x) {
		# print(x)
		# browser()
		# match on cleaned school name
		idx <- which(school_demographics$school_clean == x[ 'school_clean' ])
		if (length(idx) > 1) {
			disambiguation_df <- data.frame(x[ 'school' ] %>% as.character,
											school_demographics[ idx, c('school name', 'board name') ],
											x[ 'school_board' ] %>% as.character,
											stringsAsFactors = FALSE)
			# print(disambiguation_df)
			# if all school boards are the same it is a question of whether this is an elementary or a high school
			if (length(unique(disambiguation_df[ , 3 ])) == 1) {
				# are we looking at the high school or the elementary school?
				# message('disambiguated by school type')
				# browser()
				idx <- stringdistmatrix(disambiguation_df[ 1, 1 ], disambiguation_df[ , 2 ])
				idx <- which.min(idx)
				# message(sprintf('chose entry %s', idx))
				idx2 <- which(school_demographics$`school name` == disambiguation_df[ idx, 'school.name' ] & 
							  	school_demographics$`board name` == disambiguation_df[ idx, 'board.name' ])
				result <- data.frame(t(x), school_demographics[ idx2, ])
			} else {
				# fuzzy match the board names
				# message('disambiguated by school board name')
				bn <- disambiguation_df[ 1, 4 ]
				bn <- str_replace(bn, 'Conseil des écoles catholiques', 'CSDC')
				bn <- str_replace(bn, '\\(.*\\)', '')
				bn <- str_trim(bn)
				idx <- stringdistmatrix(bn, disambiguation_df[ , 3 ])
				idx <- which.min(idx)
				idx2 <- which(school_demographics$`school name` == disambiguation_df[ idx, 'school.name' ] & 
							  	school_demographics$`board name` == disambiguation_df[ idx, 'board.name' ])
				# message(sprintf('chose entry %s', idx))
				result <- data.frame(t(x), school_demographics[ idx2, ])
				# print(x)
				# print(result)
				# browser()
			}
		} else if (length(idx) == 1) {
			# we have a perfect match
			result <- data.frame(t(x), school_demographics[ idx, ])
		} else if (length(idx) == 0) {
			# this is one of the cleaned school names that is mismatched
			# browser()
			# before we give up let's see if we have any fuzzy matches...
			sdm <- stringdistmatrix(x[ 'school_clean'], school_demographics$school_clean)
			sdm <- as.integer(sdm)
			idx1 <- which(sdm < 3)
			if (length(idx1) > 0) {
				if (length(idx1) > 1) {
					# break ties 
					# disambiguate by school board
					sdm2 <- stringdistmatrix(x[ 'school_board' ], school_demographics[ idx1, 'board name'])
					sdm2 <- as.integer(sdm2)
					idx2 <- which.min(sdm2)
					idx1 <- idx1[ idx2 ]
					result <- data.frame(t(x), school_demographics[ idx1, ])	
					# browser()
				} else {
					# message(sprintf('found fuzzy match for school name "%s"', x[ 'school_clean' ]))
					result <- data.frame(t(x), school_demographics[ idx1, ])	
				}
			} else {
				# if (debug) message(sprintf('no corresponding school name found for "%s" using dummy data', x[ 'school_clean' ]))
				ambiguous_school_names <- get('ambiguous_school_names', envir = .GlobalEnv)
				ambiguous_school_names <- c(ambiguous_school_names, x[ 'school_clean' ])
				ambiguous_school_names <- unique(ambiguous_school_names)
				ambiguous_school_names <- sort(ambiguous_school_names)
				assign('ambiguous_school_names', ambiguous_school_names, envir = .GlobalEnv)
				dummy_df <- data.frame(t(rep('', 52)))
				colnames(dummy_df) <- colnames(school_demographics)
				result <- data.frame(t(x))	
			}
		}
		if (nrow(result) > 1) { 
			# use municipality to disambiguate
			idx <- which(tolower(str_trim(result$municipality)) == tolower(str_trim(result$municipality.1)))
			if (length(idx) == 1) {
				# if (debug) message(sprintf('disambiguated by municipality for %s!', x[ 'school_clean' ]))
				result <- result[ idx, ]
			} else {
				# if (debug) message(sprintf('sloppy disambiguation for %s!', x[ 'school_clean' ]))
				result <- result[ 1, ]
			}
		}
		return(result)
	})
	
	# use this to update the clean_all_names function so that it can properly match
	# entries in active cases dataset with corresponding entries in school demographics 
	# dataset
	if (debug) { 
		message(sprintf('no corresponding school name found for: %s', paste0(ambiguous_school_names, collapse = ', ')))
		df1 <- covid19_schools_active[ , c('school', 'municipality', 'school_board') ]
		df1 <- unique(df1)
		df1$clean_name <- clean_all_names(df1$school)
		idx <- order(df1$school)
		df1 <- df1[ idx, ]
		View(df1)
		df2 <- school_demographics[ , c('school name', 'municipality', 'board name') ]
		df2 <- unique(df2)
		df2$clean_name <- clean_all_names(df2$`school name`)
		idx <- order(df2$school)
		df2 <- df2[ idx, ]
		View(df2)
	}
	
	covid19_schools_active_with_demographics <- rbindlist(covid19_schools_active_with_demographics, use.names = TRUE, fill = TRUE)
	covid19_schools_active_with_demographics <- data.frame(covid19_schools_active_with_demographics)
	
	# 11. clean covid19_schools_active_with_demographics -----------------------
	# str(covid19_schools_active_with_demographics)
	# 'data.frame':	1674 obs. of  62 variables:
	# 	$ collected_date                                                                          : chr  "2020-09-10" "2020-09-10" "2020-09-10" "2020-09-10" ...
	covid19_schools_active_with_demographics$collected_date <- as.Date(covid19_schools_active_with_demographics$collected_date)
	# $ reported_date                                                                             : chr  "2020-09-11" "2020-09-11" "2020-09-11" "2020-09-11" ...
	covid19_schools_active_with_demographics$reported_date <- as.Date(covid19_schools_active_with_demographics$reported_date)
	# $ school_board                                                                            : chr  "Conseil des écoles catholiques du Centre-Est (CECCE)" "Conseil des écoles catholiques du Centre-Est (CECCE)" "Conseil des écoles catholiques du Centre-Est (CECCE)" "Conseil des écoles catholiques du Centre-Est (CECCE)" ...
	# $ school                                                                                  : chr  "École élémentaire catholique Roger-Saint-Denis" "École élémentaire catholique Saint-François-d’Assise" "École élémentaire catholique Sainte-Anne" "École élémentaire catholique Laurier-Carrière" ...
	# $ municipality                                                                            : chr  "Ottawa" "Ottawa" "Ottawa" "Ottawa" ...
	# $ confirmed_student_cases                                                                 : chr  "0" "1" "1" "1" ...
	covid19_schools_active_with_demographics$confirmed_student_cases <- as.integer(covid19_schools_active_with_demographics$confirmed_student_cases)
	# $ confirmed_staff_cases                                                                   : chr  "1" "0" "0" "0" ...
	covid19_schools_active_with_demographics$confirmed_staff_cases <- as.integer(covid19_schools_active_with_demographics$confirmed_staff_cases)
	# $ confirmed_unspecified_cases                                                             : chr  NA NA NA NA ...
	covid19_schools_active_with_demographics$confirmed_unidentified_cases <- as.integer(covid19_schools_active_with_demographics$confirmed_unspecified_cases)
	covid19_schools_active_with_demographics$confirmed_unspecified_cases <- NULL
	# $ total_confirmed_cases                                                                   : chr  "1" "1" "1" "1" ...
	covid19_schools_active_with_demographics$total_confirmed_cases <- as.integer(covid19_schools_active_with_demographics$total_confirmed_cases)
	# $ school_clean                                                                            : chr  "catholique roger saint denis" "catholique saint fran<c3><a7>ois d assise" "catholique sainte anne" "catholique laurier carri<c3><a8>re" ...
	covid19_schools_active_with_demographics$school_clean <- NULL
	# $ board.number                                                                            : chr  "B67334" "B67334" "B67334" "B67334" ...
	# $ board.name                                                                              : chr  "CSDC du Centre-Est de l'Ontario" "CSDC du Centre-Est de l'Ontario" "CSDC du Centre-Est de l'Ontario" "CSDC du Centre-Est de l'Ontario" ...
	# $ board.type                                                                              : chr  "Cath Dist Sch Brd (E/F)" "Cath Dist Sch Brd (E/F)" "Cath Dist Sch Brd (E/F)" "Cath Dist Sch Brd (E/F)" ...
	# $ school.number                                                                           : chr  "753203" "793027" "860573" "734314" ...
	# $ school.name                                                                             : chr  "École élémentaire catholique Roger-Saint-Denis" "École élémentaire catholique Saint-François-d'Assise" "École élémentaire catholique Sainte-Anne" "École élémentaire catholique Laurier-Carrière" ...
	# $ school.type                                                                             : chr  "Catholic" "Catholic" "Catholic" "Catholic" ...
	# $ school.special.condition.code                                                           : chr  "Not applicable" "Not applicable" "Not applicable" "Not applicable" ...
	# $ school.level                                                                            : chr  "Elementary" "Elementary" "Elementary" "Elementary" ...
	# $ school.language                                                                         : chr  "French" "French" "French" "French" ...
	# $ grade.range                                                                             : chr  "JK-6" "JK-6" "JK-8" "JK-6" ...
	# $ building.suite                                                                          : chr  NA NA NA NA ...
	covid19_schools_active_with_demographics$building.suite <- NULL
	# $ p.o..box                                                                                : chr  NA NA NA NA ...
	covid19_schools_active_with_demographics$p.o..box <- NULL
	# $ street                                                                                  : chr  "186 Barrow crois." "35 Melrose av." "235 Beausoleil promenade" "14 Four Seasons prom." ...
	# $ municipality.1                                                                          : chr  "Ottawa" "Ottawa" "Ottawa" "Ottawa" ...
	covid19_schools_active_with_demographics$municipality.1 <- NULL
	# $ city                                                                                    : chr  "Kanata" "Ottawa" "Ottawa" "Nepean" ...
	# $ province                                                                                : chr  "Ontario" "Ontario" "Ontario" "Ontario" ...
	# $ postal.code                                                                             : chr  "K2L2C7" "K1Y1T8" "K1N0C1" "K2E7P8" ...
	# $ phone.number                                                                            : chr  "613-521-3815" "613-729-1463" "613-789-3933" "613-224-3211" ...
	covid19_schools_active_with_demographics$phone.number <- NULL
	# $ fax.number                                                                              : chr  "613-592-8275" "613-729-2291" "613-789-1265" "613-225-3783" ...
	covid19_schools_active_with_demographics$fax.number <- NULL
	# $ school.website                                                                          : chr  "http://www.roger-saint-denis.ecolecatholique.ca/" "http://www.saint-francois-dassise.ecolecatholique.ca/" "http://www.sainte-anne.ecolecatholique.ca/" "http://www.laurier-carriere.ecolecatholique.ca/" ...
	covid19_schools_active_with_demographics$school.website <- NULL
	# $ board.website                                                                           : chr  "www.ecolecatholique.ca/fr/" "www.ecolecatholique.ca/fr/" "www.ecolecatholique.ca/fr/" "www.ecolecatholique.ca/fr/" ...
	covid19_schools_active_with_demographics$board.website <- NULL
	# $ Enrolment                                                                               : chr  "190" "345" "220" "400" ...
	# $ latitude                                                                                : num  45.3 45.4 45.4 45.4 45.3 ...
	# $ longitude                                                                               : num  -75.9 -75.7 -75.7 -75.7 -75.8 ...
	# $ percentage.of.students.whose.first.language.is.not.english                              : chr  "65" "60" "80" "60" ...
	covid19_schools_active_with_demographics$percentage.of.students.whose.first.language.is.not.english <- as.integer(covid19_schools_active_with_demographics$percentage.of.students.whose.first.language.is.not.english)
	# $ percentage.of.students.whose.first.language.is.not.french                               : chr  "55" "45" "40" "50" ...
	covid19_schools_active_with_demographics$percentage.of.students.whose.first.language.is.not.french <- as.integer(covid19_schools_active_with_demographics$percentage.of.students.whose.first.language.is.not.french)
	# $ percentage.of.students.who.are.new.to.canada.from.a.non.english.speaking.country        : chr  "10" "10" "20" "5" ...
	covid19_schools_active_with_demographics$percentage.of.students.who.are.new.to.canada.from.a.non.english.speaking.country <- as.integer(covid19_schools_active_with_demographics$percentage.of.students.who.are.new.to.canada.from.a.non.english.speaking.country)
	# $ percentage.of.students.who.are.new.to.canada.from.a.non.french.speaking.country         : chr  "10" "10" "20" "10" ...
	covid19_schools_active_with_demographics$percentage.of.students.who.are.new.to.canada.from.a.non.french.speaking.country <- as.integer(covid19_schools_active_with_demographics$percentage.of.students.who.are.new.to.canada.from.a.non.french.speaking.country)
	# $ percentage.of.students.identified.as.gifted                                             : chr  "25" "10" "20" "15" ...
	covid19_schools_active_with_demographics$percentage.of.students.identified.as.gifted <- as.integer(covid19_schools_active_with_demographics$percentage.of.students.identified.as.gifted)
	# $ percentage.of.students.receiving.special.education.services                             : chr  "0" "0" "0" "0" ...
	covid19_schools_active_with_demographics$percentage.of.students.receiving.special.education.services <- NULL # as.integer(covid19_schools_active_with_demographics$percentage.of.students.receiving.special.education.services)
	# $ percentage.of.grade.3.students.achieving.the.provincial.standard.in.reading             : chr  "67%" "83%" "65%" "92%" ...
	covid19_schools_active_with_demographics$percentage.of.grade.3.students.achieving.the.provincial.standard.in.reading <- NULL
	# $ change.in.grade.3.reading.achievement.over.three.years                                  : chr  "0" "-12" "-23" "5" ...
	covid19_schools_active_with_demographics$change.in.grade.3.reading.achievement.over.three.years <- NULL
	# $ percentage.of.grade.3.students.achieving.the.provincial.standard.in.writing             : chr  "54%" "73%" "71%" "81%" ...
	covid19_schools_active_with_demographics$percentage.of.grade.3.students.achieving.the.provincial.standard.in.writing <- NULL
	# $ change.in.grade.3.writing.achievement.over.three.years                                  : chr  "-17" "-17" "-17" "-3" ...
	covid19_schools_active_with_demographics$change.in.grade.3.writing.achievement.over.three.years <- NULL
	# $ percentage.of.grade.3.students.achieving.the.provincial.standard.in.mathematics         : chr  "58%" "71%" "47%" "73%" ...
	covid19_schools_active_with_demographics$percentage.of.grade.3.students.achieving.the.provincial.standard.in.mathematics <- NULL
	# $ change.in.grade.3.mathematics.achievement.over.three.years                              : chr  "-9" "-9" "-41" "-6" ...
	covid19_schools_active_with_demographics$change.in.grade.3.mathematics.achievement.over.three.years <- NULL
	# $ percentage.of.grade.6.students.achieving.the.provincial.standard.in.reading             : chr  "96%" "96%" "97%" "79%" ...
	covid19_schools_active_with_demographics$percentage.of.grade.6.students.achieving.the.provincial.standard.in.reading <- NULL
	# $ change.in.grade.6.reading.achievement.over.three.years                                  : chr  "13" "1" "14" "-13" ...
	covid19_schools_active_with_demographics$change.in.grade.6.reading.achievement.over.three.years <- NULL
	# $ percentage.of.grade.6.students.achieving.the.provincial.standard.in.writing             : chr  "96%" "89%" "85%" "67%" ...
	covid19_schools_active_with_demographics$percentage.of.grade.6.students.achieving.the.provincial.standard.in.writing <- NULL
	# $ change.in.grade.6.writing.achievement.over.three.years                                  : chr  "13" "3" "38" "-16" ...
	covid19_schools_active_with_demographics$change.in.grade.6.writing.achievement.over.three.years <- NULL
	# $ percentage.of.grade.6.students.achieving.the.provincial.standard.in.mathematics         : chr  "96%" "91%" "82%" "67%" ...
	covid19_schools_active_with_demographics$percentage.of.grade.6.students.achieving.the.provincial.standard.in.mathematics <- NULL
	# $ change.in.grade.6.mathematics.achievement.over.three.years                              : chr  "13" "9" "35" "-18" ...
	covid19_schools_active_with_demographics$change.in.grade.6.mathematics.achievement.over.three.years <- NULL
	# $ percentage.of.grade.9.students.achieving.the.provincial.standard.in.academic.mathematics: chr  "na" "na" "na" "na" ...
	covid19_schools_active_with_demographics$percentage.of.grade.9.students.achieving.the.provincial.standard.in.academic.mathematics <- NULL
	# $ change.in.grade.9.academic.mathematics.achievement.over.three.years                     : chr  "na" "na" "na" "na" ...
	covid19_schools_active_with_demographics$change.in.grade.9.academic.mathematics.achievement.over.three.years <- NULL
	# $ percentage.of.grade.9.students.achieving.the.provincial.standard.in.applied.mathematics : chr  "na" "na" "na" "na" ...
	covid19_schools_active_with_demographics$percentage.of.grade.9.students.achieving.the.provincial.standard.in.applied.mathematics <- NULL
	# $ change.in.grade.9.applied.mathematics.achievement.over.three.years                      : chr  "na" "na" "na" "na" ...
	covid19_schools_active_with_demographics$change.in.grade.9.applied.mathematics.achievement.over.three.years <- NULL
	# $ percentage.of.students.that.passed.the.grade.10.osslt.on.their.first.attempt            : chr  "na" "na" "na" "na" ...
	covid19_schools_active_with_demographics$percentage.of.students.that.passed.the.grade.10.osslt.on.their.first.attempt <- NULL
	# $ change.in.grade.10.osslt.literacy.achievement.over.three.years                          : chr  "na" "na" "na" "na" ...
	covid19_schools_active_with_demographics$change.in.grade.10.osslt.literacy.achievement.over.three.years <- NULL
	# $ percentage.of.school.aged.children.who.live.in.low.income.households                    : chr  "10" "30" "50" "20" ...
	covid19_schools_active_with_demographics$percentage.of.school.aged.children.who.live.in.low.income.households <- as.integer(covid19_schools_active_with_demographics$percentage.of.school.aged.children.who.live.in.low.income.households)
	# $ percentage.of.students.whose.parents.have.some.university.education                     : chr  "55" "55" "35" "60" ...
	covid19_schools_active_with_demographics$percentage.of.students.whose.parents.have.some.university.education <- as.integer(covid19_schools_active_with_demographics$percentage.of.students.whose.parents.have.some.university.education)
	# $ extract.date                                                                            : POSIXct, format: "2020-09-04" "2020-09-04" "2020-09-04" "2020-09-04" ...
	covid19_schools_active_with_demographics$extract.date <- NULL
	# $ school_clean.1                                                                          : chr  "catholique roger saint denis" "catholique saint fran<c3><a7>ois d assise" "catholique sainte anne" "catholique laurier carri<c3><a8>re" ...
	covid19_schools_active_with_demographics$school_clean.1 <- NULL
	fn <- file.path(data_dir, 'covid19_schools_active_with_demographics.rdata')
	save('covid19_schools_active_with_demographics', file = fn)
	
	# 12. covid19_schools_active_with_demographics_most_recent -----------------
	idx <- sprintf('%s%s', 
				   clean_all_names(covid19_schools_active_with_demographics$school), 
				   clean_all_names(covid19_schools_active_with_demographics$school_board))
	covid19_schools_active_with_demographics_most_recent <- lapply(unique(idx), function(x) {
		idx2 <- which(idx == x)
		df <- covid19_schools_active_with_demographics[ idx2, ]
		idx3 <- which.max(df$collected_date)
		df <- df[ idx3, ]
	})
	covid19_schools_active_with_demographics_most_recent <- rbindlist(covid19_schools_active_with_demographics_most_recent)
	covid19_schools_active_with_demographics_most_recent <- data.frame(covid19_schools_active_with_demographics_most_recent, 
																	   stringsAsFactors = FALSE)
	fn <- file.path(data_dir, 'covid19_schools_active_with_demographics_most_recent.rdata')
	save('covid19_schools_active_with_demographics_most_recent', file = fn)
	
	# 13. create cases_per_school to plot cases by school on map ---------------
	geo_query_str <- sprintf('%s,%s,Ontario,Canada', 
							 str_trim(covid19_schools_active_with_demographics_most_recent$school), 
							 covid19_schools_active_with_demographics_most_recent$municipality)
	school_board <- covid19_schools_active_with_demographics_most_recent$board.name
	names(school_board) <- geo_query_str
	school_level <- covid19_schools_active_with_demographics_most_recent$school.level
	names(school_level) <- geo_query_str
	school_language <- covid19_schools_active_with_demographics_most_recent$school.language
	names(school_language) <- geo_query_str
	school_enrolment <- covid19_schools_active_with_demographics_most_recent$enrolment
	names(school_enrolment) <- geo_query_str
	low_income <- covid19_schools_active_with_demographics_most_recent$percentage.of.school.aged.children.who.live.in.low.income.households
	names(low_income) <- geo_query_str
	# special_education <- covid19_schools_active_with_demographics_most_recent$percentage.of.students.receiving.special.education.services
	# names(special_education) <- geo_query_str
	non_english <- covid19_schools_active_with_demographics_most_recent$percentage.of.students.whose.first.language.is.not.english
	names(non_english) <- geo_query_str
	non_french <- covid19_schools_active_with_demographics_most_recent$percentage.of.students.whose.first.language.is.not.french
	names(non_french) <- geo_query_str
	from_non_english <- covid19_schools_active_with_demographics_most_recent$percentage.of.students.who.are.new.to.canada.from.a.non.english.speaking.country
	names(from_non_english) <- geo_query_str
	from_non_french <- covid19_schools_active_with_demographics_most_recent$percentage.of.students.who.are.new.to.canada.from.a.non.french.speaking.country
	names(from_non_french) <- geo_query_str
	some_university <- covid19_schools_active_with_demographics_most_recent$percentage.of.students.whose.parents.have.some.university.education
	names(some_university) <- geo_query_str
	cases_per_school_staff <- tapply(covid19_schools_active_with_demographics_most_recent$confirmed_staff_cases, geo_query_str, sum)
	cases_per_school_student <- tapply(covid19_schools_active_with_demographics_most_recent$confirmed_student_cases, geo_query_str, sum)
	cases_per_school_unidentified <- tapply(covid19_schools_active_with_demographics_most_recent$confirmed_unidentified_cases, geo_query_str, sum)
	cases_per_school <- tapply(covid19_schools_active_with_demographics_most_recent$total_confirmed_cases, geo_query_str, sum)
	cases_per_school <- data.frame(cases_per_school, 
								   cases_per_school_staff,
								   cases_per_school_student,
								   cases_per_school_unidentified,
								   geo_query_str = names(cases_per_school))
	rownames(cases_per_school) <- NULL
	cases_per_school <- merge(cases_per_school, school_geocodes, by = 'geo_query_str')
	cases_per_school$school_name <- str_split(cases_per_school$geo_query_str, ',') %>% sapply('[', 1)
	cases_per_school$city <- str_split(cases_per_school$geo_query_str, ',') %>% sapply('[', 2)
	cases_per_school$school_board <- sapply(cases_per_school$geo_query_str, function(x) school_board[[ x ]][ 1 ]) %>% as.character
	cases_per_school$school_level <- sapply(cases_per_school$geo_query_str, function(x) school_level[[ x ]][ 1 ]) %>% as.character
	cases_per_school$school_language <- sapply(cases_per_school$geo_query_str, function(x) school_language[[ x ]][ 1 ]) %>% as.character
	cases_per_school$school_enrolment <- sapply(cases_per_school$geo_query_str, function(x) school_enrolment[[ x ]][ 1 ]) %>% as.integer
	cases_per_school$low_income <- sapply(cases_per_school$geo_query_str, function(x) low_income[[ x ]][ 1 ]) %>% as.integer
	# cases_per_school$special_education <- sapply(cases_per_school$geo_query_str, function(x) special_education[[ x ]][ 1 ]) %>% as.integer
	cases_per_school$non_english <- sapply(cases_per_school$geo_query_str, function(x) non_english[[ x ]][ 1 ]) %>% as.integer
	cases_per_school$non_french <- sapply(cases_per_school$geo_query_str, function(x) non_french[[ x ]][ 1 ]) %>% as.integer
	cases_per_school$from_non_english <- sapply(cases_per_school$geo_query_str, function(x) from_non_english[[ x ]][ 1 ]) %>% as.integer
	cases_per_school$from_non_french <- sapply(cases_per_school$geo_query_str, function(x) from_non_french[[ x ]][ 1 ]) %>% as.integer
	cases_per_school$some_university <- sapply(cases_per_school$geo_query_str, function(x) some_university[[ x ]][ 1 ]) %>% as.integer
	fn <- file.path(data_dir, 'cases_per_school.rdata')
	save('cases_per_school', file = fn)
	
} else {
	# LOAD ALL CACHED DATA -----------------------------------------------------
	fn <- file.path(data_dir, 'covid19_schools_active.rdata')
	load(file = fn, envir = .GlobalEnv)
	fn <- file.path(data_dir, 'covid19_schools_summary.rdata')
	load(file = fn, envir = .GlobalEnv)
	fn <- file.path(data_dir, 'covid19_all_cases.rdata')
	load(file = fn, envir = .GlobalEnv)
	fn <- file.path(data_dir, 'school_demographics.rdata')
	load(file = fn, envir = .GlobalEnv)
	fn <- file.path(data_dir, 'covid19_schools_active_with_demographics.rdata')
	load(file = fn, envir = .GlobalEnv)
	fn <- file.path(data_dir, 'covid19_schools_active_with_demographics_most_recent.rdata')
	load(file = fn, envir = .GlobalEnv)
	fn <- file.path(data_dir, 'cases_per_school.rdata')
	load(file = fn, envir = .GlobalEnv)
	fn <- file.path(data_dir, 'risk_rank_elementary.rdata')
	load(file = fn, envir = .GlobalEnv)
	fn <- file.path(data_dir, 'risk_rank_secondary.rdata')
	load(file = fn, envir = .GlobalEnv)
	fn <- file.path(data_dir, 'risk_rank_neighborhood.rdata')
	load(file = fn, envir = .GlobalEnv)
}

