# DEPENDENCIES -----------------------------------------------------------------

library(readxl)
library(httr)
library(magrittr)

# SCRIPT SETTINGS --------------------------------------------------------------

data_dir <- 'data'

refresh_data <- TRUE

# LOAD DATA --------------------------------------------------------------------

## 1a. download private school contact info data set ---------------------------
# https://data.ontario.ca/dataset/private-school-contact-information
url <- 'https://data.ontario.ca/dataset/7a049187-cf29-4ffe-9028-235b95c61fa3/resource/6545c5ec-a5ce-411c-8ad5-d66363da8891/download/private_schools_contact_information_march_2021_en.xlsx'
fname <- file.path(data_dir, basename(url))
if (refresh_data) {
	message('updating private school contact information data file')
	GET(url, verbose(), write_disk(path = fname, overwrite = TRUE))	
}

## 1b. load private school contact info data into memory -----------------------
private_school_contact_info <- read_xlsx(fname)

## 2a. download statistics canada school type data set -------------------------
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3710010901
url <- 'https://www150.statcan.gc.ca/t1/tbl1/en/dtl!downloadDbLoadingData-nonTraduit.action?pid=3710010901&latestN=0&startDate=20180101&endDate=20180101&csvLocale=en&selectedMembers=%5B%5B7%5D%2C%5B1%2C2%2C3%2C4%5D%2C%5B1%2C2%2C3%2C4%5D%5D'
fname <- file.path(data_dir, 'enrolment_by_school_type_2018_2019.csv')
if (refresh_data) {
	message('updating statscan school type data file')
	GET(url, verbose(), write_disk(path = fname, overwrite = TRUE))
}

## 2b. load statistics canada school type data into memory ---------------------
statscan_school_types <- read.csv(fname)

## 3a. download schools demographic data set -----------------------------------
# https://data.ontario.ca/dataset/school-information-and-student-demographics
url <- 'https://data.ontario.ca/dataset/d85f68c5-fcb0-4b4d-aec5-3047db47dcd5/resource/602a5186-67f5-4faf-94f3-7c61ffc4719a/download/new_sif_data_table_2018_2019prelim_en_august.xlsx'
fname <- sprintf('%s/%s', data_dir, basename(url))
if (refresh_data) {
	message('updating student demographics data file')
	GET(url, write_disk(fname, overwrite = TRUE))
}

## 3b. load school demographic data into memory --------------------------------

school_demographics <- read_xlsx(fname)
school_demographics <- as.data.frame(school_demographics, stringsAsFactors = FALSE)

## 4a. download school enrolment data set --------------------------------------

# https://data.ontario.ca/en/dataset/private-school-enrolment-by-gender
url <- 'https://data.ontario.ca/dataset/b80d8f5f-c73e-47a7-a8b3-c7c9b09ddbba/resource/6a354bc5-17d5-4449-b994-1ff0e22a3ed9/download/private_school_enrolment_en_1819.txt'
fname <- sprintf('%s/%s', data_dir, basename(url))
if (refresh_data) {
	message('updating private school enrolment data file')
	GET(url, write_disk(fname, overwrite = TRUE))
}

## 3b. load private school enrolment data into memory --------------------------

private_school_enrolment <- read.delim(fname, sep = '|')
private_school_enrolment <- as.data.frame(private_school_enrolment, stringsAsFactors = FALSE)
idx <- which(private_school_enrolment$Total.Male.Enrolment %in% c('SP', ''))
private_school_enrolment$Total.Male.Enrolment[ idx ] <- NA
unknown_males <- length(idx)
private_school_enrolment$Total.Male.Enrolment <- as.integer(private_school_enrolment$Total.Male.Enrolment)
idx <- which(private_school_enrolment$Total.Female.Enrolment %in% c('SP', ''))
unknown_females <- length(idx)
private_school_enrolment$Total.Female.Enrolment[ idx ] <- NA
private_school_enrolment$Total.Female.Enrolment <- as.integer(private_school_enrolment$Total.Female.Enrolment)
summary(private_school_enrolment)

# ANALYZE DATA -----------------------------------------------------------------

## estimate total private school enrolment -------------------------------------
enr1 <- sum(private_school_enrolment$Total.Male.Enrolment, 
			private_school_enrolment$Total.Female.Enrolment, 
			na.rm = TRUE) +
	sum(unknown_males, unknown_females)
message('total private school enrolment according to province of ontario: ', enr1)

## get statscan total private school enrolment ---------------------------------
idx <- which(statscan_school_types$School.type == 'Private/independent schools' &
			 	statscan_school_types$Program.type == 'Regular programs for youth')
enr2 <- statscan_school_types[ idx, 'VALUE' ]
message('total private school enrolment according to statistics canada: ',
		enr2)

## estimate total public school enrolment --------------------------------------
idx <- which(school_demographics$Enrolment %in% c('SP', ''))
school_demographics$Enrolment[ idx ] <- NA
school_demographics$Enrolment <- as.integer(school_demographics$Enrolment)
enr3 <- sum(school_demographics$Enrolment, na.rm = TRUE)
message('total public school enrolment according to province of ontario: ', enr3)

## get statscan total public school enrolment ----------------------------------
idx <- which(statscan_school_types$School.type == 'Public schools' &
			 	statscan_school_types$Program.type == 'Regular programs for youth')
enr4 <- statscan_school_types[ idx, 'VALUE' ]
message('total public school enrolment according to statistics canada: ',
		enr4)

## estimate total school enrolment ---------------------------------------------
message('total school enrolment according to province of ontario (public + private): ', enr1 + enr3)

## get statscan total public school enrolment ----------------------------------
message('total school enrolment according to statistics canada (public + private, excl home schools): ',
		enr2 + enr4)

## estimate proportion of private school students ------------------------------
message('proportion of students in private school according to province of ontario: ', round(enr1 / (enr1 + enr3), 2))
message('proportion of students in private school according to statistics canada: ', round(enr2 / (enr2 + enr4), 2))

## what types of schools are in the provincial private schools data set? -------
public_schools_count <- nrow(school_demographics)
private_schools_count <- nrow(private_school_contact_info)
message('proportion of public schools: ', 
		round(public_schools_count / (public_schools_count + private_schools_count), 2))
