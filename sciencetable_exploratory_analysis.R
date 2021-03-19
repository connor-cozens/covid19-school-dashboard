# DEPENDENCIES -----------------------------------------------------------------

library(plotly)

# load all data
source('data_downloader.R')

# SCRIPT SETTINGS --------------------------------------------------------------

data_length_days <- 180L

data_start_date <- min(covid19_schools_active$reported_date)

message('data starts on ', data_start_date)
data_end_date <- data_start_date + days(data_length_days)
message('data ends on ', data_end_date)

# LOAD DATA --------------------------------------------------------------------

## load policy tracing data set ------------------------------------------------
policy_tracing <- read.csv('data/policy_tracing.csv', sep = '|')
policy_tracing$Date <- as.Date(policy_tracing$Date, format =  '%d %b %Y')

# EXPLORATORY DATA ANALYSIS ----------------------------------------------------

## relationship between poverty and infection rates ----------------------------

### create poverty rates categorical variable and analyze ----------------------
poverty_rates <- as.integer(school_demographics$`percentage of school-aged children who live in low-income households`)
hist(poverty_rates, main = 'empirical distribution of poverty rates', xlab = 'poverty rate (%)')
summary(poverty_rates) # median = 15%
high_poverty_rate_threshold <- 15 # percentage of students living in poverty to define a "high poverty" school
poverty_rate_categorical <- cut(cases_per_school$low_income, 
								breaks = c(0, high_poverty_rate_threshold, 100), 
								labels = c('low', 'high'))
prop.table(table(poverty_rate_categorical))

### create infection rates categorical variable and analyze --------------------
infection_rates <- cases_per_school$cases_per_school / cases_per_school$school_enrolment
hist(infection_rates, main = 'empirical distribution of infection rates', xlab = 'infection rate')
summary(infection_rates)
high_infection_rate_threshold <- as.numeric(quantile(infection_rates, na.rm = TRUE, probs = 0.75))
infection_rate_categorical <- cut(infection_rates, 
								  breaks = c(0, high_infection_rate_threshold, 1), 
								  labels = c('low', 'high'))
prop.table(table(infection_rate_categorical))

### plot case counts vs poverty rates categorical ------------------------------
tbl <- tapply(cases_per_school$cases_per_school, 
			  poverty_rate_categorical,
			  sum)
print(tbl)
barplot(tbl, main = 'case counts by poverty rate')

### plot infection rates vs poverty rates categorical --------------------------
tbl <- tapply(infection_rates, 
			  poverty_rate_categorical,
			  median)
print(tbl)
barplot(tbl, main = 'infection rates by poverty rate')
# is the relationship between infection rates and poverty rates statistically significant?

### create contingency table ---------------------------------------------------
tbl_contingency <- table(infection_rate_categorical,
						 poverty_rate_categorical)
print(tbl_contingency)

### perform chi-squared test ---------------------------------------------------
chisq_results <- chisq.test(tbl_contingency)
print(chisq_results)
# print(chisq_results$expected)
# print(chisq_results$observed)

## relationship between school enrolment and infection rates -------------------

### analyze school enrolment ---------------------------------------------------
# table(school_demographics$`school level`)
idx <- which(school_demographics$`school level` == 'Elementary')
enrolment <- as.integer(school_demographics$enrolment[ idx ])
hist(enrolment, 
	 main = 'elementary school enrolment empirical distribution')
summary(enrolment)
enrolment_primary_cuts <- as.integer(quantile(enrolment, na.rm = TRUE))[ 2:5 ]
idx <- which(school_demographics$`school level` == 'Secondary')
enrolment <- as.integer(school_demographics$enrolment[ idx ])
hist(enrolment, 
	 main = 'secondary school enrolment empirical distribution')
summary(enrolment)
enrolment_secondary_cuts <- as.integer(quantile(enrolment, na.rm = TRUE))[ 2:5 ]

### cases by enrolment elementary ----------------------------------------------
idx <- which(cases_per_school$school_level == 'Elementary')
enrolment_elementary <- as.integer(cases_per_school$school_enrolment[ idx ])
enrolment_elementary_categorical <- cut(enrolment_elementary, 
										breaks = enrolment_primary_cuts, 
										labels = c('small', 'medium', 'large'))
case_count_elementary <- as.integer(cases_per_school$cases_per_school[ idx ])
tapply(case_count_elementary, 
	   enrolment_elementary_categorical, 
	   sum) %>%
	barplot(main = 'infection counts by elementary school size')
tapply(case_count_elementary / enrolment_elementary, 
	   enrolment_elementary_categorical, 
	   median) %>% 
	barplot(main = 'infection rates by elementary school size')

### cases by enrolment secondary -----------------------------------------------
idx <- which(cases_per_school$school_level == 'Secondary')
enrolment_secondary <- as.integer(cases_per_school$school_enrolment[ idx ])
enrolment_secondary_categorical <- cut(enrolment_secondary, 
									   breaks = enrolment_primary_cuts, 
									   labels = c('small', 'medium', 'large'))
case_count_secondary <- as.integer(cases_per_school$cases_per_school[ idx ])
tapply(case_count_secondary, 
	   enrolment_secondary_categorical, 
	   sum) %>%
	barplot(main = 'infection counts by secondary school size')
tapply(case_count_secondary / enrolment_secondary, 
	   enrolment_secondary_categorical, 
	   median) %>%
	barplot(main = 'infection rates by secondary school size')

### school sizes by school board -----------------------------------------------
idx <- which(cases_per_school$school_level == 'Secondary')
enrolment_secondary <- as.integer(cases_per_school$school_enrolment[ idx ])
enrolment_secondary_categorical <- cut(enrolment_secondary, 
									   breaks = enrolment_primary_cuts, 
									   labels = c('small', 'medium', 'large'))
sizes_tbl <- table(cases_per_school$school_board[ idx ],
				   enrolment_secondary_categorical)
# sizes_tbl <- as.data.frame(sizes_tbl)
large_school_rate <- sizes_tbl[ , 3 ] / rowSums(sizes_tbl)

### cases rates by school board ------------------------------------------------
infection_rates <- cases_per_school$cases_per_school / cases_per_school$school_enrolment
tapply(infection_rates, 
	   cases_per_school$school_board, 
	   median) %>% 
	sort

# PLOTS ------------------------------------------------------------------------

## school_related_cases_details_plot -------------------------------------------

df <- covid19_schools_summary
idx <- which((df$collected_date >= data_start_date) & (df$collected_date <= data_end_date))
df <- df[ idx, ]
fig <- plot_ly(df, x = ~collected_date, y = ~cumulative_school_related_cases, name = 'Cumulative school-related cases', type = 'scatter', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~cumulative_school_related_student_cases, name = 'Cumulative school-related student cases', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~cumulative_school_related_staff_cases, name = 'Cumulative school-related staff cases', mode = 'lines+markers') 
fig <- fig %>% add_trace(y = ~cumulative_school_related_unidentified_cases, name = 'Cumulative school-related unidentified cases', mode = 'lines+markers')
fig <- fig %>% layout(title = 'Cumulative school-related cases',
					  xaxis = list(title = 'Collected date'),
					  yaxis = list (title = 'Cumulative cases'))
# fig <- fig %>% add_annotations(x = policy_tracing$Date,
# 							   y = 0,
# 							   text = format(policy_tracing$Date, '%m-%d'),
# 							   xref = 'x',
# 							   yref = 'y',
# 							   showarrow = TRUE,
# 							   arrowhead = 4,
# 							   arrowsize = .5,
# 							   ax = 20,
# 							   ay = -40)
event_lines <- lapply(policy_tracing$Date, function(x) {
	list(
		type = 'line', 
		y0 = 0, 
		y1 = 1,
		yref = 'paper', # i.e. y as a proportion of visible region
		x0 = x, 
		x1 = x, 
		line = list(width = 0.25, dash = 'dash', color = 'green')
	)
})
fig <- fig %>% layout(shapes = event_lines)
fig

## school_related_new_cases_details_plot ---------------------------------------
df <- covid19_schools_summary
idx <- which((df$collected_date >= data_start_date) & (df$collected_date <= data_end_date))
df <- df[ idx, ]
fig <- plot_ly(df, x = ~collected_date, y = ~new_total_school_related_cases, name = 'New total school-related cases', type = 'scatter', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~new_school_related_student_cases, name = 'New school-related student cases', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~new_school_related_staff_cases, name = 'New school-related staff cases', mode = 'lines+markers')
fig <- fig %>% add_trace(y = ~new_school_related_unidentified_cases, name = 'New school-related unidentified cases', mode = 'lines+markers')
fig <- fig %>% layout(title = 'New school-related cases',
					  xaxis = list(title = 'Collected date'),
					  yaxis = list (title = 'New cases'))
event_lines <- lapply(policy_tracing$Date, function(x) {
	list(
		type = 'line', 
		y0 = 0, 
		y1 = 1,
		yref = 'paper', # i.e. y as a proportion of visible region
		x0 = x, 
		x1 = x, 
		line = list(width = 0.25, dash = 'dash', color = 'green')
	)
})
fig <- fig %>% layout(shapes = event_lines)
fig

## schools_with_cases_plot -----------------------------------------------------
df <- covid19_schools_summary[ , c('collected_date', 'current_schools_with_cases') ]
idx <- which((df$collected_date >= data_start_date) & (df$collected_date <= data_end_date))
df <- df[ idx, ]
fig <- plot_ly(df, x = ~collected_date, y = ~current_schools_with_cases, name = 'Current schools with cases', type = 'scatter', mode = 'lines+markers')
fig <- fig %>% layout(title = 'Schools with cases',
					  xaxis = list(title = 'Collected date'),
					  yaxis = list (title = 'Schools'))
event_lines <- lapply(policy_tracing$Date, function(x) {
	list(
		type = 'line', 
		y0 = 0, 
		y1 = 1,
		yref = 'paper', # i.e. y as a proportion of visible region
		x0 = x, 
		x1 = x, 
		line = list(width = 0.25, dash = 'dash', color = 'green')
	)
})
fig <- fig %>% layout(shapes = event_lines)
fig

## schools_with_cases_percentage_plot ------------------------------------------
df <- covid19_schools_summary[ , c('collected_date', 'current_schools_with_cases') ]
idx <- which((df$collected_date >= data_start_date) & (df$collected_date <= data_end_date))
df <- df[ idx, ]
df$percent <- df$current_schools_with_cases / nrow(school_demographics)
fig <- plot_ly(df, x = ~collected_date, y = ~percent, name = 'Current schools with cases', type = 'scatter', mode = 'lines+markers')
fig <- fig %>% layout(title = 'Proportion of schools with cases',
					  xaxis = list(title = 'Collected date'),
					  yaxis = list (title = 'Schools'))
event_lines <- lapply(policy_tracing$Date, function(x) {
	list(
		type = 'line', 
		y0 = 0, 
		y1 = 1,
		yref = 'paper', # i.e. y as a proportion of visible region
		x0 = x, 
		x1 = x, 
		line = list(width = 0.25, dash = 'dash', color = 'green')
	)
})
fig <- fig %>% layout(shapes = event_lines)
fig

## active_cases_by_municipality_plot -------------------------------------------
active_cases_by_municipality <- tapply(covid19_schools_active$municipality,
									   list(covid19_schools_active$collected_date,
									   	 covid19_schools_active$municipality),
									   length)
active_cases_by_municipality <- na.locf(active_cases_by_municipality)
colidx <- order(active_cases_by_municipality[ nrow(active_cases_by_municipality), ], decreasing = TRUE)
active_cases_by_municipality <- active_cases_by_municipality[ , colidx ] 
active_cases_by_municipality <- data.frame(active_cases_by_municipality)
active_cases_by_municipality <- data.frame(collected_date = as.Date(rownames(active_cases_by_municipality)), active_cases_by_municipality)
rownames(active_cases_by_municipality) <- NULL
df <- active_cases_by_municipality[ , 1:11 ]
idx <- which((df$collected_date >= data_start_date) & (df$collected_date <= data_end_date))
df <- df[ idx, ]
nms <- colnames(df)[ -1 ]
nms2 <- colnames(df)[ -1 ] %>% str_replace_all(., '\\.', ' ') %>% str_replace_all(., '\\s+', ' ') %>% str_trim
code_str <- sprintf('
                            fig <- plot_ly(df, x = ~collected_date, y = ~%s, name = \'%s\', type = \'scatter\', mode = \'lines+markers\')
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig
                            ', 
					nms[ 1 ], nms2[ 1 ],
					nms[ 2 ], nms2[ 2 ],
					nms[ 3 ], nms2[ 3 ],
					nms[ 4 ], nms2[ 4 ],
					nms[ 5 ], nms2[ 5 ],
					nms[ 6 ], nms2[ 6 ],
					nms[ 7 ], nms2[ 7 ],
					nms[ 8 ], nms2[ 8 ],
					nms[ 9 ], nms2[ 9 ],
					nms[ 10 ], nms2[ 10 ])
fig <- parse(text = code_str) %>% eval
fig <- fig %>% layout(title = 'Active school cases by municipality (top 10)',
					  xaxis = list(title = 'Collected date'),
					  yaxis = list (title = 'Active cases'))
event_lines <- lapply(policy_tracing$Date, function(x) {
	list(
		type = 'line', 
		y0 = 0, 
		y1 = 1,
		yref = 'paper', # i.e. y as a proportion of visible region
		x0 = x, 
		x1 = x, 
		line = list(width = 0.25, dash = 'dash', color = 'green')
	)
})
fig <- fig %>% layout(shapes = event_lines)
fig

## active_cases_by_board_plot --------------------------------------------------
active_cases_by_board <- tapply(covid19_schools_active$school_board,
								list(covid19_schools_active$collected_date,
									 covid19_schools_active$school_board),
								length)
active_cases_by_board <- na.locf(active_cases_by_board)
colidx <- order(active_cases_by_board[ nrow(active_cases_by_board), ], decreasing = TRUE)
active_cases_by_board <- active_cases_by_board[ , colidx ] 
active_cases_by_board <- data.frame(active_cases_by_board)
active_cases_by_board <- data.frame(collected_date = as.Date(rownames(active_cases_by_board)), active_cases_by_board)
rownames(active_cases_by_board) <- NULL
df <- active_cases_by_board[ , 1:11 ]
idx <- which((df$collected_date >= data_start_date) & (df$collected_date <= data_end_date))
df <- df[ idx, ]
nms <- colnames(df)[ -1 ]
nms2 <- colnames(df)[ -1 ] %>% str_replace_all(., '\\.', ' ') %>% str_replace_all(., '\\s+', ' ') %>% str_trim
code_str <- sprintf('
                            fig <- plot_ly(df, x = ~collected_date, y = ~%s, name = \'%s\', type = \'scatter\', mode = \'lines+markers\')
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig <- fig %%>%% add_trace(y = ~%s, name = \'%s\', mode = \'lines+markers\') 
                            fig
                            ', 
					nms[ 1 ], nms2[ 1 ],
					nms[ 2 ], nms2[ 2 ],
					nms[ 3 ], nms2[ 3 ],
					nms[ 4 ], nms2[ 4 ],
					nms[ 5 ], nms2[ 5 ],
					nms[ 6 ], nms2[ 6 ],
					nms[ 7 ], nms2[ 7 ],
					nms[ 8 ], nms2[ 8 ],
					nms[ 9 ], nms2[ 9 ],
					nms[ 10 ], nms2[ 10 ])
fig <- parse(text = code_str) %>% eval
fig <- fig %>% layout(title = 'Active school cases by school board (top 10)',
					  xaxis = list(title = 'Collected date'),
					  yaxis = list (title = 'Active cases'),
					  legend = list(x = 0.1, y = 0.9, bgcolor = 'rgba(0,0,0,0)'))
event_lines <- apply(policy_tracing, 1, function(x) {
	list(
		type = 'line', 
		y0 = 0, 
		y1 = 1,
		yref = 'paper', # i.e. y as a proportion of visible region
		x0 = x[ 'Date' ], 
		x1 = x[ 'Date' ],
		text = x[ 'Policy' ],
		hoverinfo = 'text',
		line = list(width = 0.25, dash = 'dash', color = 'black')
	)
})
fig <- fig %>% layout(shapes = event_lines)
fig

# POLICY CHANGES ---------------------------------------------------------------
# tag plots with policy change events
# visual marker for implementation of asymptomatic testing
