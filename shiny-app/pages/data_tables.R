data_tables_panel <- tabPanel('Data Tables & Data Dictionary',
                              tabsetPanel(
                                tabPanel('2021-2022', br(), tabsetPanel(
                                  tabPanel(
                                    'Summary of cases in schools',
                                    h3('Summary of cases in schools'),
                                    br(),
                                    downloadButton('download_csv_button_1', 'Download as CSV'),
                                    br(),
                                    br(),
                                    p('Scroll down to see data dictionary of terms for this table.'),
                                    DTOutput('school_summary_data_dt'),
                                    br(),
                                    'Adapted from data published by Government of Ontario: ',
                                    a(
                                      href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools',
                                      target = '_blank',
                                      'Summary of cases in schools'
                                    ),
                                    br(),
                                    hr(),
                                    h3('Data dictionary'),
                                    DTOutput('school_summary_data_dictionary_dt')
                                  ),
                                  tabPanel(
                                    'Schools with active cases and school demographic data',
                                    h3('Schools with active cases and school demographic data'),
                                    br(),
                                    downloadButton('download_csv_button_2', 'Download as CSV'),
                                    br(),
                                    br(),
                                    p('Scroll down to see data dictionary of terms for this table.'),
                                    DTOutput('school_cases_demo_data_dt'),
                                    br(),
                                    'Adapted from data published by Government of Ontario: ',
                                    a(
                                      href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools',
                                      target = '_blank',
                                      'Schools with active COVID-19 cases'
                                    ),
                                    ', ',
                                    a(
                                      href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics',
                                      target = '_blank',
                                      'School information and student demographics'
                                    ),
                                    br(),
                                    hr(),
                                    h3('Data dictionary'),
                                    DTOutput('school_cases_demo_data_dictionary_dt')
                                  )
                                )),
                                tabPanel('2020-2021', br(), tabsetPanel(
                                  tabPanel(
                                    'Summary of cases in schools',
                                    h3('Summary of cases in schools'),
                                    br(),
                                    downloadButton('download_csv_button_1_20_21', 'Download as CSV'),
                                    br(),
                                    br(),
                                    p('Scroll down to see data dictionary of terms for this table.'),
                                    DTOutput('school_summary_data_dt_20_21'),
                                    br(),
                                    'Adapted from data published by Government of Ontario during the 2020-2021 academic year: ',
                                    a(
                                      href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools',
                                      target = '_blank',
                                      'Summary of cases in schools'
                                    ),
                                    br(),
                                    hr(),
                                    h3('Data dictionary'),
                                    DTOutput('school_summary_data_dictionary_dt_20_21')
                                  ),
                                  tabPanel(
                                    'Schools with active cases and school demographic data',
                                    h3('Schools with active cases and school demographic data'),
                                    br(),
                                    downloadButton('download_csv_button_2_20_21', 'Download as CSV'),
                                    br(),
                                    br(),
                                    p('Scroll down to see data dictionary of terms for this table.'),
                                    DTOutput('school_cases_demo_data_dt_20_21'),
                                    br(),
                                    'Adapted from data published by Government of Ontario during the 2020-2021 academic year: ',
                                    a(
                                      href = 'https://data.ontario.ca/dataset/summary-of-cases-in-schools',
                                      target = '_blank',
                                      'Schools with active COVID-19 cases'
                                    ),
                                    ', ',
                                    a(
                                      href = 'https://data.ontario.ca/dataset/school-information-and-student-demographics',
                                      target = '_blank',
                                      'School information and student demographics'
                                    ),
                                    br(),
                                    hr(),
                                    h3('Data dictionary'),
                                    DTOutput('school_cases_demo_data_dictionary_dt_20_21')
                                  )
                                ))
                              ))