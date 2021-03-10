# NOTES ------------------------------------------------------------------------

# https://www.donneesquebec.ca/recherche/dataset/covid-19-portrait-quotidien-des-hospitalisations/resource/2d8bd4f8-4715-4f33-8cb4-eefcec60a4c9
# https://msss.gouv.qc.ca/professionnels/statistiques/documents/covid19/COVID19_Qc_HistoHospit.csv
# https://www.donneesquebec.ca/recherche/dataset/99517f1f-9e4d-4853-98da-47f7cafb4d77/resource/99ba845f-458a-4ad6-b75c-9a812df5807b/download/listevariables_notesmetho_hospit_20201117.pdf

# https://www.quebec.ca/en/health/health-issues/a-z/2019-coronavirus/situation-coronavirus-in-quebec/

# DEPENDENCIES -----------------------------------------------------------------

library(httr)
library(lubridate)
library(rvest)
library(stringr)
library(xts)
library(tabulizer)

# SETTINGS ---------------------------------------------------------------------

data_dir <- 'data/quebec'

max_file_age_hrs <- 24L

debug <- FALSE

# UTILITY FUNCTIONS ------------------------------------------------------------

# MAIN -------------------------------------------------------------------------

# 1. download hospitalization data ---------------------------------------------

# school summary data
url <- 'https://msss.gouv.qc.ca/professionnels/statistiques/documents/covid19/COVID19_Qc_HistoHospit.csv'
fname_hospitalizations <- sprintf('%s/%s', data_dir, basename(url))
needs_refresh <- difftime(now(), as.POSIXct(file.info(fname_hospitalizations)$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh | is.na(needs_refresh)) { 
	message('updating summary data file')
	GET(url, write_disk(fname_hospitalizations, overwrite = TRUE))
}

# 2. scrape summary statistics -------------------------------------------------

# let's see if we can discover the csv data links for the quebec data...
url <- 'https://www.quebec.ca/en/health/health-issues/a-z/2019-coronavirus/situation-coronavirus-in-quebec/'
pg_html <- read_html(url)
csv_links <- html_text(pg_html) %>% str_extract_all(., 'https://.*\\.csv') %>% unlist
fn <- sprintf('%s/%s', data_dir, basename(csv_links))
needs_refresh <- difftime(now(), as.POSIXct(file.info(fn[ 1 ])$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh | is.na(needs_refresh)) { 
	lapply(csv_links, function(x) {
		message(sprintf('downloading %s', x))
		fn <- sprintf('%s/%s', data_dir, basename(x))
		GET(x, write_disk(fn, overwrite = TRUE))
		Sys.sleep(1)
	})
}

# quebec list of schools
ts <- as.integer(now())
url <- sprintf('https://cdn-contenu.quebec.ca/cdn-contenu/adm/min/education/publications-adm/covid-19/reseauScolaire_listeEcoles_ANG.pdf?%s', ts)
fname_schools <- sprintf('%s/%s', data_dir, basename(url))
fname_schools <- str_replace_all(fname_schools, '\\?.*', '')
needs_refresh <- difftime(now(), as.POSIXct(file.info(fname_schools)$mtime), units = 'hours') >= max_file_age_hrs
if (needs_refresh | is.na(needs_refresh)) { 
	GET(url, write_disk(fname_schools, overwrite = TRUE))
}


tbls <- extract_tables(fname_schools, pages = 1:get_n_pages(fname_schools)) # skip page 1 for now
cols <- sapply(tbls, ncol)
(table(cols))
idx <- which(cols == 5)
lapply(idx, function(x) {
	# browser()
	message(sprintf('page %s has 5 columns instead of 3, fixing', x))
	tbl <- tbls[[ x ]]
	
	# get rid of header rows
	tbl <- tbl[ 3:nrow(tbl), ]
	
	# name columns
	colnames(tbl) <- c('region', 'board', 'school', 'extra1', 'extra2')
	
	# green highlighted columns in pdf get chopped up for some reason
	# put the school name back together as best you can
	idx1 <- which((tbl[ , 2 ] != '') & (tbl[ , 4 ] != ''))
	# tbl[ sort(c(idx1, idx1 + 1)), ] # visualize
	if (length(idx1) > 1) {
		p1 <- apply(tbl[ idx1, 4:5 ], 1, paste0, collapse = ' ') %>% str_squish
		p2 <- apply(tbl[ idx1 + 1, 3:5 ], 1, paste0, collapse = ' ') %>% str_squish	
	} else {
		p1 <- paste0(tbl[ idx1, 4:5 ], collapse = ' ') %>% str_squish
		p2 <- paste0(tbl[ idx1 + 1, 3:5 ], collapse = ' ') %>% str_squish
	}
	tbl[ sort(c(idx1)), 3 ] <- paste(p1, p2)
	
	# clean up empty rows
	idx1 <- which(tbl[ , 2 ] != '')
	tbl <- tbl[ idx1, ]
	
	# remove garbage columns
	tbl <- tbl[ , 1:3 ]
	
	# clean up whitespace
	tbl <- apply(tbl, 2, str_replace_all, '\\s+', ' ')
	tbl <- apply(tbl, 2, str_squish)
	
	# print for debug
	# print(tbl)
	
	# assign
	tbls[[ x ]] <<- tbl
})

idx <- which(cols == 2)
lapply(idx, function(x) {
	# browser()
	message(sprintf('page %s has 2 columns instead of 3, fixing', x))
	tbl <- tbls[[ x ]]
	tbl[ 5:nrow(tbl), 1 ]
})
