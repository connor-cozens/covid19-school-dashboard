# Plots ----
## Cumulative ----
output$cumulative_plot <- renderPlotly({
  df <- covid19_schools_summary
  fig <- plot_ly(df, x = ~collected_date, y = ~cumulative_school_related_cases, name = 'Cumulative school-related cases', type = 'scatter', mode = 'lines+markers')
  fig <- fig %>% add_trace(y = ~cumulative_school_related_student_cases, name = 'Cumulative school-related student cases', mode = 'lines+markers') 
  fig <- fig %>% add_trace(y = ~cumulative_school_related_staff_cases, name = 'Cumulative school-related staff cases', mode = 'lines+markers') 
  fig <- fig %>% add_trace(y = ~cumulative_school_related_unidentified_cases, name = 'Cumulative school-related unidentified cases', mode = 'lines+markers')
  fig <- fig %>% layout(title = 'Cumulative school-related cases', 
                        legend = list(x = 0.1, y = 0.9),
                        xaxis = list(title = 'Collected date'),
                        yaxis = list (title = 'Cumulative cases'))
  fig
})

## Which Week View ----
observeEvent(input$weeklyRadio,{
  ### 7-Day View ----
  if (input$weeklyRadio == "7-day view"){
    output$whichWeekView <- renderUI({
      div(
        h6(div('Data reported from'), textOutput('clean_week_old_date_text'), align = 'right'),
        
        # weekly_summary_1_dt -----------------
        div(tableOutput('weekly_summary_1_dt'), style = 'font-size: small; width: 100%'),
        
        h6('Drag this box to move it', align = 'right')
      )
    })
  }
  else { 
    ### 14-day view ----
    output$whichWeekView <- renderUI({
      div(
        h6(div('Data reported from'), textOutput('clean_two_weeks_old_date_text'), align = 'right'),
        
        # weekly_summary_3_dt -----------------
        div(tableOutput('weekly_summary_3_dt'), style = 'font-size: small; width: 100%'),
        
        h6('Drag this box to move it', align = 'right')
      )
    })
  }
})

# cumulative_plot_20_21 ----------------------------------------------------------
output$cumulative_plot_20_21 <- renderPlotly({
  df <- covid19_schools_summary_20_21
  fig <- plot_ly(df, x = ~collected_date, y = ~cumulative_school_related_cases, name = 'Cumulative school-related cases', type = 'scatter', mode = 'lines+markers')
  fig <- fig %>% add_trace(y = ~cumulative_school_related_student_cases, name = 'Cumulative school-related student cases', mode = 'lines+markers') 
  fig <- fig %>% add_trace(y = ~cumulative_school_related_staff_cases, name = 'Cumulative school-related staff cases', mode = 'lines+markers') 
  fig <- fig %>% add_trace(y = ~cumulative_school_related_unidentified_cases, name = 'Cumulative school-related unidentified cases', mode = 'lines+markers')
  fig <- fig %>% layout(title = 'Cumulative school-related cases', 
                        legend = list(x = 0.1, y = 0.9),
                        xaxis = list(title = 'Collected date'),
                        yaxis = list (title = 'Cumulative cases'))
  fig
})

# whichWeekView2 (2021-2020) -----------------------------------------------
observeEvent(input$weeklyRadio2,{
  if (input$weeklyRadio2 == "7-day view"){
    output$whichWeekView2 <- renderUI({
      div(
        h6(div('Data reported from'), textOutput('clean_week_old_date_text20_21'), align = 'right'),
        
        # weekly_summary_4_dt -----------------
        div(tableOutput('weekly_summary_4_dt'), style = 'font-size: small; width: 100%'),
        
        h6('Drag this box to move it', align = 'right')
      )
    })
  }
  else { #14 day view
    output$whichWeekView2 <- renderUI({
      div(
        h6(div('Data reported from'), textOutput('clean_two_weeks_old_date_text20_21'), align = 'right'),
        
        # weekly_summary_2_dt -----------------
        div(tableOutput('weekly_summary_2_dt'), style = 'font-size: small; width: 100%'),
        
        h6('Drag this box to move it', align = 'right')
      )
    })
  }
})

# daily_summary_1_dt -------------------------------------------------------
output$daily_summary_1_dt <- renderTable({
  get_summary_table(0)
}, align = 'r', striped = TRUE, width = '100%')

# daily_summary_2_dt -------------------------------------------------------
output$daily_summary_2_dt <- renderTable({
  get_summary_table(0)
}, align = 'r', striped = TRUE, width = '100%')

# weekly_summary_1_dt -------------------------------------------------------
output$weekly_summary_1_dt <- renderTable({
  get_weekly_summary_table(TRUE, 0) #FIX
}, align = 'r', striped = TRUE, width = '100%')

# weekly_summary_2_dt -------------------------------------------------------
output$weekly_summary_2_dt <- renderTable({
  get_weekly_summary_table(FALSE, 0) #FIX
}, align = 'r', striped = TRUE, width = '100%')

# 1 and 4, 2 and 3  have to exist separately because separate tabs can't use them at the same time
#IE one tab can't be using 1 and another tab be using 1 as well
#So different ones for 2022-2021 and 2021-2020

# weekly_summary_3_dt -------------------------------------------------------
output$weekly_summary_3_dt <- renderTable({
  get_weekly_summary_table(FALSE, 0) #FIX
}, align = 'r', striped = TRUE, width = '100%')

# weekly_summary_4_dt -------------------------------------------------------
output$weekly_summary_4_dt <- renderTable({
  get_weekly_summary_table(TRUE, 0) #FIX
}, align = 'r', striped = TRUE, width = '100%')

# school_details_dt --------------------------------------------------------
output$school_details_dt <- renderDT({
  #df1 <- covid19_schools_active_with_demographics_most_recent
  df <- covid19_schools_active_with_demographics_most_recent[ , c(2, 12, 4, 15, 6:8) ]
  colnames(df) <- str_replace_all(colnames(df), '_', ' ')
  colnames(df) <- str_replace_all(colnames(df), '\\.', ' ')
  colnames(df) <- str_to_title(colnames(df))
  datatable(
    df,
    options = list(
      pageLength = 10,
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# school_details_dt_20_21 --------------------------------------------------------
output$school_details_dt_20_21 <- renderDT({
  df <- covid19_schools_active_with_demographics_most_recent_20_21[ , 2:8 ]
  colnames(df) <- str_replace_all(colnames(df), '_', ' ')
  colnames(df) <- str_to_title(colnames(df))
  datatable(
    df,
    options = list(
      pageLength = 10,
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# clean_date_reactive_text -------------------------------------------------
output$clean_date_reactive_text <- renderText({
  #Changed from covid19_schools_active to covid19_schools_summary, which has the correct latest date matching with the case count given
  format(max(covid19_schools_summary$reported_date), '%d %B %Y')
})

# clean_date_reactive_text_20_21 -------------------------------------------------
output$clean_date_reactive_text_20_21 <- renderText({
  format(max(covid19_schools_summary_20_21$reported_date), '%d %B %Y')
})

# clean_week_old_date_text -------------------------------------------------
output $clean_week_old_date_text <- renderText ({
  dates <- last_week_obtain(0)
  #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
  dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
  dateString
})

# clean_two_weeks_old_date_text -------------------------------------------------
output $clean_two_weeks_old_date_text <- renderText ({
  dates <- last_two_weeks_obtain(0)
  #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
  dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
  dateString
})

#Duplicates for logic on separate tabs, two tabs can't be using the same one at the same time!

# clean_week_old_date_text20_21 -------------------------------------------------
output $clean_week_old_date_text20_21 <- renderText ({
  dates <- last_week_obtain(0)
  #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
  dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
  dateString
})

# clean_two_weeks_old_date_text20_21 -------------------------------------------------
output $clean_two_weeks_old_date_text20_21 <- renderText ({
  dates <- last_two_weeks_obtain(0)
  #Cheating on the dates a little bit, but the data is only updated / reported Monday-Friday anyway
  dateString <- paste(format(dates[[1]] - 1, '%d %B %Y'), "to", format(dates[[2]] + 1, '%d %B %Y'))
  dateString
})

# cumulative_case_count_text -----------------------------------------------
output$cumulative_case_count_text <- renderText({
  idx <- max(which(covid19_schools_summary$collected_date <= as.Date(now())))
  count <- last(covid19_schools_summary[ idx, 'cumulative_school_related_cases' ])
  paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
})

# cumulative_case_count_text_20_21 -----------------------------------------------
output$cumulative_case_count_text_20_21 <- renderText({
  idx <- max(which(covid19_schools_summary_20_21$collected_date <= as.Date(now())))
  count <- last(covid19_schools_summary_20_21[ idx, 'cumulative_school_related_cases' ])
  paste0(prettyNum(count, big.mark = ','), ' cumulative cases')
})

# school_related_cases_details_plot ----------------------------------------
output$school_related_cases_details_plot <- renderPlotly({
  df <- covid19_schools_summary
  idx <- which(df$collected_date >= as.Date(input$minimum_date))
  df <- df[ idx, ]
  fig <- plot_ly(df, x = ~collected_date, y = ~cumulative_school_related_cases, name = 'Cumulative school-related cases', type = 'scatter', mode = 'lines+markers')
  fig <- fig %>% add_trace(y = ~cumulative_school_related_student_cases, name = 'Cumulative school-related student cases', mode = 'lines+markers') 
  fig <- fig %>% add_trace(y = ~cumulative_school_related_staff_cases, name = 'Cumulative school-related staff cases', mode = 'lines+markers') 
  fig <- fig %>% add_trace(y = ~cumulative_school_related_unidentified_cases, name = 'Cumulative school-related unidentified cases', mode = 'lines+markers')
  fig <- fig %>% layout(title = 'Cumulative school-related cases',
                        xaxis = list(title = 'Collected date'),
                        yaxis = list (title = 'Cumulative cases'))
  fig
})

# school_related_new_cases_details_plot ------------------------------------
output$school_related_new_cases_details_plot <- renderPlotly({
  df <- covid19_schools_summary
  idx <- which(df$collected_date >= as.Date(input$minimum_date))
  df <- df[ idx, ]
  fig <- plot_ly(df, x = ~collected_date, y = ~new_total_school_related_cases, name = 'New total school-related cases', type = 'scatter', mode = 'lines+markers')
  fig <- fig %>% add_trace(y = ~new_school_related_student_cases, name = 'New school-related student cases', mode = 'lines+markers')
  fig <- fig %>% add_trace(y = ~new_school_related_staff_cases, name = 'New school-related staff cases', mode = 'lines+markers')
  fig <- fig %>% add_trace(y = ~new_school_related_unidentified_cases, name = 'New school-related unidentified cases', mode = 'lines+markers')
  fig <- fig %>% layout(title = 'New school-related cases',
                        xaxis = list(title = 'Collected date'),
                        yaxis = list (title = 'New cases'))
  fig
})

# schools_with_cases_plot --------------------------------------------------
output$schools_with_cases_plot <- renderPlotly({
  df <- covid19_schools_summary[ , c('collected_date', 'current_schools_w_cases') ]
  idx <- which(df$collected_date >= as.Date(input$minimum_date))
  df <- df[ idx, ]
  fig <- plot_ly(df, x = ~collected_date, y = ~current_schools_w_cases, name = 'Current schools with cases', type = 'scatter', mode = 'lines+markers')
  fig <- fig %>% layout(title = 'Schools with cases',
                        xaxis = list(title = 'Collected date'),
                        yaxis = list (title = 'Schools'))
  fig
})

# active_cases_by_municipality_plot ----------------------------------------
output$active_cases_by_municipality_plot <- renderPlotly({
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
  idx <- which(df$collected_date >= as.Date(input$minimum_date))
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
  fig
})

# active_cases_by_board_plot -----------------------------------------------
output$active_cases_by_board_plot <- renderPlotly({
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
  idx <- which(df$collected_date >= as.Date(input$minimum_date))
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
                        yaxis = list (title = 'Active cases'))
  fig
})

# school_summary_data_dt ---------------------------------------------------
output$school_summary_data_dt <- renderDT({
  df <- covid19_schools_summary
  idx <- order(df$reported_date, decreasing = TRUE)
  df <- df[ idx, ]
  colnames(df) <- str_replace_all(colnames(df), '_', ' ')
  colnames(df) <- str_to_title(colnames(df))
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# school_summary_data_dt_20_21 ---------------------------------------------------
output$school_summary_data_dt_20_21 <- renderDT({
  df <- covid19_schools_summary_20_21
  idx <- order(df$reported_date, decreasing = TRUE)
  df <- df[ idx, ]
  colnames(df) <- str_replace_all(colnames(df), '_', ' ')
  colnames(df) <- str_to_title(colnames(df))
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# school_summary_data_dictionary_dt ----------------------------------------
output$school_summary_data_dictionary_dt <- renderDT({
  field <- c('Collected Date',
             'Reported Date',
             'Current Schools W Cases',
             'Current Schools Closed',
             'Current Total Number Schools', 
             'New Total School Related Cases',
             'New School Related Student Cases',
             'New School Related Staff Cases', 
             'New School Related Unidentified Cases', 
             'Recent Total School Related Cases', 
             'Recent School Related Student Cases',
             'Recent School Related Staff Cases',
             'Recent School Related Unidentified Cases', 
             'Past Total School Related Cases', 
             'Past School Related Student Cases',
             'Past School Related Staff Cases', 
             'Past School Related Unidentified Cases', 
             'Cumulative School Related Cases',
             'Cumulative School Related Student Cases',
             'Cumulative School Related Staff Cases',
             'Cumulative School Related Unidentified Cases')
  description <- c('Date results collected',
                   'Date results reported',
                   'Count of schools with active cases currently',
                   'Count of schools closed',
                   'Total number of schools in province',
                   'Total new school-related cases of all types since last reporting date',
                   'New school-related student cases since last reporting date',
                   'New school-related staff cases since last reporting date',
                   'New school-related unidentified cases since last reporting date. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                   'Total recent school-related cases reported in the past 14 days',
                   'Recent school-related student cases reported in the past 14 days',
                   'Recent school-related staff cases reported in the past 14 days',
                   'Recent school-related unidentified cases reported in the past 14 days. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                   'Total past school-related cases',
                   'Past school-related student cases',
                   'Past school-related staff cases',
                   'Past school-related unidentified cases. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                   'Cumulative total school-related cases',
                   'Cumulative school-related student cases',
                   'Cumulative school-related staff cases',
                   'Cumulative school-related unidentified cases. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.')
  df <- data.frame(field, description)
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# school_summary_data_dictionary_dt_20_21 ----------------------------------------
output$school_summary_data_dictionary_dt_20_21 <- renderDT({
  field <- c('Collected Date',
             'Reported Date',
             'Current Schools W Cases',
             'Current Schools Closed',
             'Current Total Number Schools', 
             'New Total School Related Cases',
             'New School Related Student Cases',
             'New School Related Staff Cases', 
             'New School Related Unidentified Cases', 
             'Recent Total School Related Cases', 
             'Recent School Related Student Cases',
             'Recent School Related Staff Cases',
             'Recent School Related Unidentified Cases', 
             'Past Total School Related Cases', 
             'Past School Related Student Cases',
             'Past School Related Staff Cases', 
             'Past School Related Unidentified Cases', 
             'Cumulative School Related Cases',
             'Cumulative School Related Student Cases',
             'Cumulative School Related Staff Cases',
             'Cumulative School Related Unidentified Cases')
  description <- c('Date results collected',
                   'Date results reported',
                   'Count of schools with active cases currently',
                   'Count of schools closed',
                   'Total number of schools in province',
                   'Total new school-related cases of all types since last reporting date',
                   'New school-related student cases since last reporting date',
                   'New school-related staff cases since last reporting date',
                   'New school-related unidentified cases since last reporting date. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                   'Total recent school-related cases reported in the past 14 days',
                   'Recent school-related student cases reported in the past 14 days',
                   'Recent school-related staff cases reported in the past 14 days',
                   'Recent school-related unidentified cases reported in the past 14 days. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                   'Total past school-related cases',
                   'Past school-related student cases',
                   'Past school-related staff cases',
                   'Past school-related unidentified cases. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.',
                   'Cumulative total school-related cases',
                   'Cumulative school-related student cases',
                   'Cumulative school-related staff cases',
                   'Cumulative school-related unidentified cases. Unidentified cases: Where the type of case was not identified in the dataset as either student/child or staff/provider/partner due to privacy considerations. These only include unidentified students/children or staff/providers/partners and not visitors or parents. These cases are tracked in the dataset as "individuals" but not included in the "student/child" or "staff/provider" columns.')
  df <- data.frame(field, description)
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# download_csv_button_1 ----------------------------------------------------
output$download_csv_button_1 <- downloadHandler(
  filename = function() {
    paste('schoolcovidsummary_', format(now(), '%Y%m%d'), '.csv', sep='')
  },
  content = function(file) {
    write.csv(covid19_schools_summary, file)
  }
)

# download_csv_button_1_20_21 ----------------------------------------------------
output$download_csv_button_1_20_21 <- downloadHandler(
  filename = function() {
    paste('schoolcovidsummary_20_21_', format(now(), '%Y%m%d'), '.csv', sep='')
  },
  content = function(file) {
    write.csv(covid19_schools_summary_20_21, file)
  }
)

# school_cases_demo_data_dt ------------------------------------------------
output$school_cases_demo_data_dt <- renderDT({
  #df <- covid19_schools_active_with_demographics_most_recent[ , c(2, 12, 4, 15, 6:8) ]
  df <- covid19_schools_active_with_demographics[ , c(1, 2, 12, 4, 15, 6:11, 13, 14, 16:36)]
  idx <- order(df$reported_date, decreasing = TRUE)
  df <- df[ idx, ]
  colnames(df) <- str_replace_all(colnames(df), '_|\\.', ' ')
  colnames(df) <- str_to_title(colnames(df))
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# school_cases_demo_data_dt_20_21 ------------------------------------------------
output$school_cases_demo_data_dt_20_21 <- renderDT({
  df <- covid19_schools_active_with_demographics_20_21
  idx <- order(df$reported_date, decreasing = TRUE)
  df <- df[ idx, ]
  df$board.name <- NULL
  df$school.name <- NULL
  colnames(df) <- str_replace_all(colnames(df), '_|\\.', ' ')
  colnames(df) <- str_to_title(colnames(df))
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# school_cases_demo_data_dictionary_dt -------------------------------------
output$school_cases_demo_data_dictionary_dt <- renderDT({
  field <- c('Collected Date', 'Reported Date', 'School Board', 'School', 
             'Municipality', 'Confirmed Student Cases', 'Confirmed Staff Cases',
             'Confirmed Unidentified Cases', 'Total Confirmed Cases',
             'Board Number', 'Board Name', 'Board Type', 'School Number', 
             'School Name', 'School Type', 'School Special Condition Code', 
             'School Level', 'School Language', 'Grade Range', 'Street', 'City',
             'Province', 'Postal Code', 'Enrolment', 'Latitude', 'Longitude', 
             'Percentage Of Students Whose First Language Is Not English', 
             'Percentage Of Students Whose First Language Is Not French', 
             'Percentage Of Students Who Are New To Canada From A Non English Speaking Country',
             'Percentage Of Students Who Are New To Canada From A Non French Speaking Country', 
             'Percentage Of Students Identified As Gifted', 
             'Percentage Of School Aged Children Who Live In Low Income Households', 
             'Percentage Of Students Whose Parents Have Some University Education')
  description <- c('Collected Date', 'Reported Date', 'School Board', 'School', 
                   'Municipality', NA, NA, NA, NA,
                   'Board Number', 'Board Name', 'Board Type', 'School Number', 
                   'School Name', 'School Type', 'School Special Condition Code', 
                   'School Level', 'School Language', 'Grade Range', 'Street', 'City',
                   'Province', 'Postal Code', 'Enrolment', 'Latitude', 'Longitude', 
                   'Percentage Of Students Whose First Language Is Not English', 
                   'Percentage Of Students Whose First Language Is Not French', 
                   'Percentage Of Students Who Are New To Canada From A Non English Speaking Country',
                   'Percentage Of Students Who Are New To Canada From A Non French Speaking Country', 
                   'Percentage Of Students Identified As Gifted', 
                   'Percentage Of School Aged Children Who Live In Low Income Households', 
                   'Percentage Of Students Whose Parents Have Some University Education')
  df <- data.frame(field, description)
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# school_cases_demo_data_dictionary_dt_20_21 -------------------------------------
output$school_cases_demo_data_dictionary_dt_20_21 <- renderDT({
  field <- c('Collected Date', 'Reported Date', 'School Board', 'School', 
             'Municipality', 'Confirmed Student Cases', 'Confirmed Staff Cases',
             'Confirmed Unidentified Cases', 'Total Confirmed Cases',
             'Board Number', 'Board Name', 'Board Type', 'School Number', 
             'School Name', 'School Type', 'School Special Condition Code', 
             'School Level', 'School Language', 'Grade Range', 'Street', 'City',
             'Province', 'Postal Code', 'Enrolment', 'Latitude', 'Longitude', 
             'Percentage Of Students Whose First Language Is Not English', 
             'Percentage Of Students Whose First Language Is Not French', 
             'Percentage Of Students Who Are New To Canada From A Non English Speaking Country',
             'Percentage Of Students Who Are New To Canada From A Non French Speaking Country', 
             'Percentage Of Students Identified As Gifted', 
             'Percentage Of School Aged Children Who Live In Low Income Households', 
             'Percentage Of Students Whose Parents Have Some University Education')
  description <- c('Collected Date', 'Reported Date', 'School Board', 'School', 
                   'Municipality', NA, NA, NA, NA,
                   'Board Number', 'Board Name', 'Board Type', 'School Number', 
                   'School Name', 'School Type', 'School Special Condition Code', 
                   'School Level', 'School Language', 'Grade Range', 'Street', 'City',
                   'Province', 'Postal Code', 'Enrolment', 'Latitude', 'Longitude', 
                   'Percentage Of Students Whose First Language Is Not English', 
                   'Percentage Of Students Whose First Language Is Not French', 
                   'Percentage Of Students Who Are New To Canada From A Non English Speaking Country',
                   'Percentage Of Students Who Are New To Canada From A Non French Speaking Country', 
                   'Percentage Of Students Identified As Gifted', 
                   'Percentage Of School Aged Children Who Live In Low Income Households', 
                   'Percentage Of Students Whose Parents Have Some University Education')
  df <- data.frame(field, description)
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# download_csv_button_2 ----------------------------------------------------
output$download_csv_button_2 <- downloadHandler(
  filename = function() {
    paste('schoolsactivecovidwithdemographics_', format(now(), '%Y%m%d'), '.csv', sep='')
  },
  content = function(file) {
    write.csv(covid19_schools_active_with_demographics, file)
  }
)

# download_csv_button_2_20_21 ----------------------------------------------------
output$download_csv_button_2_20_21 <- downloadHandler(
  filename = function() {
    paste('schoolsactivecovidwithdemographics_20_21_', format(now(), '%Y%m%d'), '.csv', sep='')
  },
  content = function(file) {
    write.csv(covid19_schools_active_with_demographics_20_21, file)
  }
)

# download_csv_button_3_20_21 ----------------------------------------------------
output$download_csv_button_3_20_21 <- downloadHandler(
  filename = function() {
    paste('schoolcovidsummary20_21.csv', format(now(), '%Y%m%d'), '.csv', sep='')
  },
  content = function(file) {
    write.csv(covid19_schools_active_with_demographics_20_21, file)
  }
)

# risk_assessment_elementary_dt --------------------------------------------
output$risk_assessment_elementary_dt <- renderDT({
  df <- risk_rank_elementary
  colnames(df) <- str_to_title(colnames(df))
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# risk_assessment_secondary_dt ---------------------------------------------
output$risk_assessment_secondary_dt <- renderDT({
  df <- risk_rank_secondary
  colnames(df) <- str_to_title(colnames(df))
  datatable(
    df,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'Bfrtip'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

# risk_assessment_neighborhood_dt ------------------------------------------
output$risk_assessment_neighborhood_dt <- renderDT({
  df <- risk_rank_neighborhood
  colnames(df) <- str_replace_all(colnames(df), '_', ' ')
  colnames(df) <- str_to_title(colnames(df))
  brks <- seq(0, 1, by = 0.05)
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    { paste0('rgb(255,', ., ',', ., ')') }
  datatable(
    df, 
    callback = JS(sprintf('var tips = [ %s],
                            firstRow = $("#risk_assessment_neighborhood_dt thead tr th");
                            for (var i = 1; i < tips.length; i++) {
                              $(firstRow[i]).attr("title", tips[i]);
                            }', paste0(rep('"Chance of Encountering a Transmissible COVID-19 Infection in a Group Of..."', 19), collapse = ', '))),
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE
    ),
    rownames = FALSE,
    class = 'display'
  ) %>% formatCurrency(2:27, '') %>% formatStyle(names(df)[ 2:20 ], backgroundColor = styleInterval(brks, clrs))
})

# variables_details_dt -----------------------------------------------------
output$variables_details_dt <- renderDT({
  c1 <- c('COVID-19 Case Rate',
          '% Low-income',
          '% Living in multigenerational homes',
          '% Visible minority'
  )
  c2 <- c(2,
          1,
          1,
          1
  )
  c3 <- c('Number of COVID-19 cases in the neighbourhood (confirmed/probable cases, regardless of whether they were associated with an outbreak), per 100,000 neighbourhood population',
          'Low-income measure after tax (LIM-AT, see Statistics Canada4 for further details, Census, 2016)',
          'Multigenerational households include at least three generations of the same family (Census, 2016).',
          'Visible minority population as defined by the Employment Equity Act (Census, 2016)'
  )
  c4 <- c('Areas with a high concentration of cases, proportionate to area population, would result in a higher risk of transmission.',
          'Based on recent analysis, areas with a higher proportion of lower-income households have shown disproportional impacts of COVID-19.',
          'Multigenerational homes may put older adults at higher risk.',
          'Based on recent analysis, areas with more visible minorities how disproportional impacts of COVID-19.'
          
  )
  df <- data.frame(Indicator = c1, Weight = c2, Definition = c3, Rationale = c4)
  datatable(
    df,
    options = list(
      paging = FALSE,
      searching = FALSE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = FALSE,
      dom = 'Bfrti'
    ),
    rownames = FALSE,
    class = 'display'
  )
})

timeline_plot<-ggplot(df,aes(x=date,y=0, col=status, label=milestone))
timeline_plot<-timeline_plot+labs(col="Milestones")
timeline_plot<-timeline_plot+scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)
timeline_plot<-timeline_plot+theme_classic()

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='black', size=0.2)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom"
)

# Show text for each month
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90)
# Show year text
timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=2.5, color='black')
# Show text for each milestone
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=milestone),size=2.5)
#ggsave('timeline_plot.jpg', timeline_plot, device="jpg", path="www")