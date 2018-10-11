tabsetPanel(
  tabPanel("Local Authority Breakdown",
           sidebarLayout(
             sidebarPanel(
               h4(strong("Apprenticeships by Sector Subject Area T1")),
               "Local Authority report broken down by Age and Level",
               br(),
               downloadButton("downloadfsmchar", "Download"),
               h5(strong("Pick a local authority")),                            
               selectInput(
                 "select2",
                 label = NULL,
                 choices = sort(unique(la_plot_data$la_name)),
                 selected = 'Darlington'
               ),                           
               
               
               h5(strong("Pick an Age Group")),
               selectInput("reason_type1",
                           label = NULL,
                           choices = list(
                             "Under 19" = 'Under 19',
                             "19-24" = '19-24',
                             "25+" = '25+',
                             "All Ages" = 'Totals'),
                           selected = 'Totals', width='30%'),
               
               h5(strong("Pick a level")),
               selectInput("reason_type2",
                           label = NULL,
                           choices = list(
                             "Intermediate" = 'Intermediate Apprenticeship',
                             "Advanced" = 'Advanced Apprenticeship',
                             "Higher" = 'Higher Apprenticeship',
                             "All Levels" = 'Totals'),
                           selected = 'Totals', width='30%'),                            
               #downloadButton("downloadfsmchar", "Download the data for this page"),
               #  width=12),
               # mainPanel()), 
               
               
               
               width=12),
             mainPanel()),
           splitLayout(
             verticalLayout(h3("Starts"),
                            strong("Starts by LA, Age and Level"),
                            em("2016/17 academic year"),
                            br(),
                            plotOutput("perm_reason"), #-- make changes here - add Age and level as function arguments
                            br(),
                            br(),
                            strong("Starts broken down by Sector Subject Area"),
                            em("2016/17 academic year"),
                            br(),
                            tableOutput("perm_reason_t")),
             verticalLayout(h3("Achievements"), 
                            strong("Achievements by LA, Age and Level"),
                            em("2016/17 academic year"),
                            br(),
                            plotOutput("fixed_reason"), #-- make changes here - add Age and level as function arguments
                            br(),
                            br(),
                            strong("Achievements broken down by Sector Subject Area"),
                            em("2016/17 academic year"),
                            br(),
                            tableOutput("fixed_reason_t")),
           ), # Split layoput
  tabPanel("PCON Breakdown",
           sidebarLayout(
             sidebarPanel(
               h4(strong("Apprenticeships by Sector Subject Area T1")),
               "Local Authority report broken down by Age and Level",
               br(),
               downloadButton("downloadfsmchar", "Download"),
               h5(strong("Pick a local authority")),                            
               selectInput(
                 "select2",
                 label = NULL,
                 choices = sort(unique(la_plot_data$la_name)),
                 selected = 'Darlington'
               ),                           
               
               
               h5(strong("Pick an Age Group")),
               selectInput("reason_type1",
                           label = NULL,
                           choices = list(
                             "Under 19" = 'Under 19',
                             "19-24" = '19-24',
                             "25+" = '25+',
                             "All Ages" = 'Totals'),
                           selected = 'Totals', width='30%'),
               
               h5(strong("Pick a level")),
               selectInput("reason_type2",
                           label = NULL,
                           choices = list(
                             "Intermediate" = 'Intermediate Apprenticeship',
                             "Advanced" = 'Advanced Apprenticeship',
                             "Higher" = 'Higher Apprenticeship',
                             "All Levels" = 'Totals'),
                           selected = 'Totals', width='30%'),                            
               #downloadButton("downloadfsmchar", "Download the data for this page"),
               #  width=12),
               # mainPanel()), 
               
               
               
               width=12),
             mainPanel()),
           splitLayout(
             verticalLayout(h3("Starts"),
                            strong("Starts by LA, Age and Level"),
                            em("2016/17 academic year"),
                            br(),
                            plotOutput("perm_reason"), #-- make changes here - add Age and level as function arguments
                            br(),
                            br(),
                            strong("Starts broken down by Sector Subject Area"),
                            em("2016/17 academic year"),
                            br(),
                            tableOutput("perm_reason_t")),
             verticalLayout(h3("Achievements"), 
                            strong("Achievements by LA, Age and Level"),
                            em("2016/17 academic year"),
                            br(),
                            plotOutput("fixed_reason"), #-- make changes here - add Age and level as function arguments
                            br(),
                            br(),
                            strong("Achievements broken down by Sector Subject Area"),
                            em("2016/17 academic year"),
                            br(),
                            tableOutput("fixed_reason_t"))
  # tabPanel("Age and gender", "contents"),
  # tabPanel("Ethnic group", "contents")
)), # PCON 
#),



###############
current tab




tabPanel("Local Authority Breakdown",
         sidebarLayout(
           sidebarPanel(
             h4(strong("Apprenticeships by Sector Subject Area T1")),
             "Local Authority report broken down by Age and Level",
             br(),
             downloadButton("downloadfsmchar", "Download"),
             h5(strong("Pick a local authority")),                            
             selectInput(
               "select2",
               label = NULL,
               choices = sort(unique(la_plot_data$la_name)),
               selected = 'Darlington'
             ),                           
             
             
             h5(strong("Pick an Age Group")),
             selectInput("reason_type1",
                         label = NULL,
                         choices = list(
                           "Under 19" = 'Under 19',
                           "19-24" = '19-24',
                           "25+" = '25+',
                           "All Ages" = 'Totals'),
                         selected = 'Totals', width='30%'),
             
             h5(strong("Pick a level")),
             selectInput("reason_type2",
                         label = NULL,
                         choices = list(
                           "Intermediate" = 'Intermediate Apprenticeship',
                           "Advanced" = 'Advanced Apprenticeship',
                           "Higher" = 'Higher Apprenticeship',
                           "All Levels" = 'Totals'),
                         selected = 'Totals', width='30%'),                            
             #downloadButton("downloadfsmchar", "Download the data for this page"),
             #  width=12),
             # mainPanel()), 
             
             
             
             width=12),
           mainPanel()),
         splitLayout(
           verticalLayout(h3("Starts"),
                          strong("Starts by LA, Age and Level"),
                          em("2016/17 academic year"),
                          br(),
                          plotOutput("perm_reason"), #-- make changes here - add Age and level as function arguments
                          br(),
                          br(),
                          strong("Starts broken down by Sector Subject Area"),
                          em("2016/17 academic year"),
                          br(),
                          tableOutput("perm_reason_t")),
           verticalLayout(h3("Achievements"), 
                          strong("Achievements by LA, Age and Level"),
                          em("2016/17 academic year"),
                          br(),
                          plotOutput("fixed_reason"), #-- make changes here - add Age and level as function arguments
                          br(),
                          br(),
                          strong("Achievements broken down by Sector Subject Area"),
                          em("2016/17 academic year"),
                          br(),
                          tableOutput("fixed_reason_t"))