# NOTES ------------------------------------------------------------------------

# https://www.donneesquebec.ca/recherche/dataset/covid-19-portrait-quotidien-des-hospitalisations/resource/2d8bd4f8-4715-4f33-8cb4-eefcec60a4c9
# https://msss.gouv.qc.ca/professionnels/statistiques/documents/covid19/COVID19_Qc_HistoHospit.csv
# https://www.donneesquebec.ca/recherche/dataset/99517f1f-9e4d-4853-98da-47f7cafb4d77/resource/99ba845f-458a-4ad6-b75c-9a812df5807b/download/listevariables_notesmetho_hospit_20201117.pdf

# https://www.quebec.ca/en/health/health-issues/a-z/2019-coronavirus/situation-coronavirus-in-quebec/

# DEPENDENCIES -----------------------------------------------------------------

library(httr)
library(rvest)
library(stringr)
library(xts)

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
