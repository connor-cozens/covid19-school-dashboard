overview_panel <- tabPanel('Overview & Search', tabsetPanel(
  tabPanel(
    '2021-2022',
    # cumulative_plot
    h3('Cumulative Case Chart'),
    plotlyOutput('cumulative_plot', width = '100%'),
    hr(),
    
    # daily_summary_2_dt
    h3('Daily Summary', align = 'left'),
    div(tableOutput('daily_summary_2_dt'), style = 'font-size: small; width: 100%'),
    hr(),
    
    # weeklyRadio2
    h3('Weekly Summary', align = 'left'),
    div(
      radioButtons(
        inputId = "weeklyRadio2",
        label = strong("Select a timeframe:"),
        choices = list("7-day view", "14-day view"),
        inline = TRUE
      ),
      align = "right"
    ),
    
    #whichWeekView2
    uiOutput("whichWeekView2"),
    hr(),
    # school_details_dt
    h3('Search Function and Table', align = 'left'),
    div(
      'Search schools, boards, municipalities for confirmed cases of COVID-19.',
      width = '100%',
      align = 'left'
    ),
    br(),
    div(DTOutput('school_details_dt'), style = 'font-size: small; width: 100%')
  ),
  tabPanel(
    '2020-2021',
    # cumulative_plot_20_21
    h3('Cumulative Case Chart'),
    plotlyOutput('cumulative_plot_20_21', width = '100%'),
    hr(),
    # school_details_dt_20_21
    h3('Search Function and Table', align = 'left'),
    div(
      'Search schools, boards, municipalities for confirmed cases of COVID-19.',
      width = '100%',
      align = 'left'
    ),
    br(),
    div(DTOutput('school_details_dt_20_21'), style = 'font-size: small; width: 100%')
  )
  
))