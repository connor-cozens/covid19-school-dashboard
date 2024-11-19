# FUNCTIONS --------------------------------------------------------------------

# get_summary_table
#
# generate Daily summary table
get_summary_table <- function(givenDate) {
  df <- covid19_schools_summary
  idx <- order(df$collected_date)
  df <- df[ idx, ]
  cn <- c(
    'collected_date', 
    'cumulative_school_related_cases', 
    'new_total_school_related_cases', 
    'current_schools_with_cases', 
    'current_schools_closed'
  )
  #Take only the columns outlined in cn from covid19_schools_summary
  df <- df[ , cn ]
  if (givenDate == 0){
    idx <- which(df$collected_date <= as.Date(now()))
  }
  else{
    idx <- which(df$collected_date <= givenDate)
  }
  idx <- max(idx)
  #Take only the 2 most recently dated rows from df, the dataframe
  df <- df[ (idx - 1):idx, ]
  colnames(df) <- str_replace_all(colnames(df), '_', ' ')
  #Rearrange df as df1 such that the columns become rows (with values that are the difference between the two original rows)
  df1 <- reshape2::melt(apply(df[ , -1 ], 2, diff))
  df1$variable <- rownames(df1)
  colnames(df1) <- c('change', 'variable')
  #Rearrange df as df2 such that the columns become rows (values are the more recent of the two entries)
  df2 <- reshape2::melt(df[ 2, -1 ])
  #Merge df1 and df2 together (row names, most recent value, change between the today and last)
  df <- merge(df2, df1, on = 'variable', all = TRUE)
  colnames(df) <- c('Variable', 'Count', 'Change')
  idx <- which(df$Change > 0)
  df[ idx, 'Change' ] <- sprintf('+%s', df[ idx, 'Change' ])
  df$Variable <- str_to_sentence(df$Variable)
  df$Variable <- str_replace_all(df$Variable, ' w ', ' with ')
  df$Variable <- str_replace_all(df$Variable, 'school related', 'school\\-related')
  #Add in %'s for Current Schools with Cases and Current Schools Closed
  schools_count <- max(covid19_schools_summary$current_total_number_schools)
  df$Percentage <- NA
  df[ 2, 'Percentage' ] <- round(df[ 2, 'Count' ] / schools_count, 4) * 1e2
  df[ 3, 'Percentage' ] <- round(df[ 3, 'Count' ] / schools_count, 4) * 1e2
  df <- df[ c(1, 4, 3, 2), ]
  
  #Add in schools with >5 cases
  df2 <- data.frame("Current schools with >5 cases", sum(cases_per_school$cases_per_school >5), "NA", round(sum(cases_per_school$cases_per_school > 5) / schools_count, 4) * 1e2)
  names(df2) <- c("Variable", "Count", "Change", "Percentage")
  df <- rbind(df,df2)
  #Add in schools with >1 cases
  df3 <- data.frame("Current schools with >1 cases", sum(cases_per_school$cases_per_school > 1), "NA", round(sum(cases_per_school$cases_per_school > 1) / schools_count, 4) * 1e2)
  names(df3) <- c("Variable", "Count", "Change", "Percentage")
  df <- rbind(df,df3)
  df <- df[ c(1, 2, 3, 5, 6, 4), ]
  #Return the table
  df
}

# last_week_obtain
#
# Obtains the dates (Year-Month-Day) for the previous weeks Monday and Friday
last_week_obtain <- function(givenDate) {
  if (givenDate == 0){
    theDate <- as.Date(max(covid19_schools_active$reported_date)) - 7
    while(weekdays(theDate) != "Friday"){
      theDate <- theDate + 1
    }
    earlyDate <- theDate - 4
    df <- list(earlyDate, theDate)
  }
  else{
    while(weekdays(givenDate) != "Friday"){
      givenDate <- givenDate + 1
    }
    earlyDate <- givenDate - 4
    df <- list(earlyDate, givenDate)
  }
  return(df)
}

# last_two_weeks_obtain
# 
# Obtains the dates (Year-Month-Day) for 2 weeks ago Monday and last weeks Friday
last_two_weeks_obtain <- function(givenDate) {
  if (givenDate == 0){
    theDate <- as.Date(max(covid19_schools_active$reported_date)) - 7
    while(weekdays(theDate) != "Friday"){
      theDate <- theDate + 1
    }
    earlyDate <- theDate - 11
    df <- list(earlyDate, theDate)
  }
  else{
    while(weekdays(givenDate) != "Friday"){
      givenDate <- givenDate + 1
    }
    earlyDate <- givenDate - 11
    df <- list(earlyDate, givenDate)
  }
  return(df)
}

# get_weekly_summary_table
# 
# generate Weekly summary table
# similar in function to the daily summary table, but with differences calculated over 7 or 14 days instead of 2
get_weekly_summary_table <- function(timeFrame, givenDate) {
  df <- covid19_schools_summary
  idx <- order(df$collected_date)
  df <- df[ idx, ]
  cn <- c(
    'collected_date', 
    'cumulative_school_related_cases', 
    'current_schools_with_cases', 
    'current_schools_closed'
  )
  df <- df[ , cn ]
  #if timeFrame == TRUE, 7 days view, otherwise 14 days view
  if (timeFrame == TRUE) {
    dates <- last_week_obtain(givenDate)
  }
  if (timeFrame == FALSE) {
    dates <- last_two_weeks_obtain(givenDate)
  }
  
  idx1 <- match(dates[[1]], df[,1])
  idx2 <- match(dates[[2]], df[,1])
  df <- rbind(df[idx1,], df[idx2,])
  
  colnames(df) <- str_replace_all(colnames(df), '_', ' ')
  
  df1 <- reshape2::melt(apply(df[ , -1 ], 2, diff))
  df1$variable <- rownames(df1)
  colnames(df1) <- c('change', 'variable')
  df2 <- reshape2::melt(df[ 2, -1 ])
  df <- merge(df2, df1, on = 'variable', all = TRUE)
  colnames(df) <- c('Variable', 'Count', 'Change')
  idx <- which(df$Change > 0)
  df[ idx, 'Change' ] <- sprintf('+%s', df[ idx, 'Change' ])
  df$Variable <- str_to_sentence(df$Variable)
  df$Variable <- str_replace_all(df$Variable, ' w ', ' with ')
  df$Variable <- str_replace_all(df$Variable, 'school related', 'school\\-related')
  schools_count <- max(covid19_schools_summary$current_total_number_schools)
  df$Percentage <- NA
  df[ 2, 'Percentage' ] <- round(df[ 2, 'Count' ] / schools_count, 4) * 1e2
  df[ 3, 'Percentage' ] <- round(df[ 3, 'Count' ] / schools_count, 4) * 1e2
  df <- df[ c(1, 3, 2), ]
  df
}

# get_schools_no_cases_closures
#
# return dataframe from school_geocodes with schools that have cases removed from it.
get_schools_no_cases_closures <- function() {
  df <- cases_per_school
  cn <- c(
    'school_name',
    'city'
  )
  df <- df[ , cn]
  df2 <- school_demographics
  
  df2 <- df2[!(df2$'school name' %in% df$school_name), , drop = FALSE]
  return (df2)
}

# get_schools_no_cases
#
# return dataframe from school_geocodes with schools that have cases removed from it.
get_schools_no_cases <- function() {
  df <- cases_per_school
  cn <- c(
    'school_name',
    'city'
  )
  df <- df[ , cn]
  df2 <- school_demographics
  
  df2 <- df2[!(df2$'school name' %in% df$school_name), , drop = FALSE]
  return (df2)
}

# get_schools_no_cases_20_21
# 
# return dataframe from school_geocodes with schools that have cases removed from it. Data from 2020/2021
get_schools_no_cases_20_21 <- function() {
  df <- cases_per_school_20_21
  cn <- c(
    'school_name',
    'city'
  )
  df <- df[ , cn]
  df2 <- school_demographics_20_21
  
  df2 <- df2[!(df2$'school name' %in% df$school_name), , drop = FALSE]
  return (df2)
}

#create timeline image
#create_timeline <- function() {
df <- read.csv('data/timeline.csv', header=TRUE, fileEncoding="UTF-8-BOM")
df$date <- with(df, ymd(sprintf('%04d%02d%02d', year, month, 1)))
df <- df[with(df, order(date)), ]
status_levels <- c("2019-2020", "2020-2021", "2021-2022")
status_colors <- c("#0070C0", "#00B050", "#FFC000")
positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
  "date"=unique(df$date),
  "position"=rep(positions, length.out=length(unique(df$date))),
  "direction"=rep(directions, length.out=length(unique(df$date)))
)

df <- merge(x=df, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date, status)), ]

df$status <- factor(df$status, levels=status_levels, ordered=TRUE)

text_offset <- 0.05

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$text_position <- (df$month_count * text_offset * df$direction) + df$position

month_buffer <- 4

month_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

year_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)