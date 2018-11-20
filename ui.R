#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#library(shiny)
source("Nov app first page plots.R")

# Define UI 
shinyUI( 
  navbarPage("Apprenticeships Statistics", id = "nav", theme = "shiny.css",
                    
  #Tab1
# tabPanel("Static version",
  
  # Sidebar
#  sidebarLayout(
#    sidebarPanel(verticalLayout(
#      h3(strong("Apprenticeship starts in England - analysis tool")),
#      hr(),
#      "This tool is designed to enable users to drill down further into more detailed information on apprenticeship starts and is currently under development",
#      hr(),
#      h4(strong("Background")),
#      "[Text to be agreed] The purpose of this dashboard is to present an overview of apprenticeship starts in England and to allow lower level breakdowns of starts than those included within our National Statistics Release.",
#      "It provides a summary of full-year apprenticeship starts from 2012/13 to 2017/18 and offers an interactive way to investigate starts in the latest academicyear in more detail.",
#      hr(),
#      h4(strong("Latest National Statistics")),
#      "For more further education (FE) statistics, please refer to the: ", 
#      a(" FE and skills statistical first release (SFR), ", 
#        href = "https://www.gov.uk/government/collections/further-education-and-skills-statistical-first-release-sfr",
#        target="_blank"),"and the",a(" FE data library", 
#                                     href = "https://www.gov.uk/government/collections/fe-data-library",
#                                     target="_blank"),
#      hr(),
#      h4(strong("Further education statistics dissemination team")),
#      "Department for Education", 
#      
#      "2 St Paul's Place",
#      
#      "Sheffield",
      
#      "S1 2FJ",  
      
#      "Email",
#      a("FE Official Statistics", href="mailto:FE.OFFICIALSTATISTICS@education.gov.uk?subject=FE statistics enquiry",
#        target="_blank"),
#      hr(),
#      h4(strong("Guidance and methodology")),
#      "Add something about methodolgy and guidance - separate tab at end of app or refer back to main publication?",
#      a("Link to wherever - this'll take you to school workforce for now",
#        href = "https://www.gov.uk/government/collections/statistics-school-workforce",
 #       target = "_blank"))
 #      
 #   ),
    
    
 #   mainPanel(h4(strong("Apprenticeship starts by age-group, 2012/13 to 2017/18")),
 #             "Choice of where commentary (if we have any here) goes?",
#              "It could go above the chart (or below) as here or move to the side of the  plot like below",
 #             hr(),
#              plotOutput("age_plot"),
 #             hr(),
#          column(verticalLayout(
 #           "Choice of where commentary (if we have any) goes?",
#            "It could go at the side of (or to the right) as here or move to the top/bottom of the  plot like above"),width = 3),
#          column(verticalLayout(
#              h4(strong("Apprenticeship starts by level, 2012/13 to 2017/18")),
#              plotOutput("level_plot")),width = 9
 #                )
              
#  )
# )),
 
 #Tab2
 tabPanel("Key Trends",
 
 # Sidebar with a slider input for number of bins 
 sidebarLayout(
   sidebarPanel(verticalLayout(
     h3(strong("Apprenticeship starts in England - analysis tool")),
     hr(),
     "This tool is designed to enable users to drill down further into more detailed information on apprenticeship starts and is currently under development",
     hr(),
     h4(strong("Background")),
     "[Text to be agreed] The purpose of this dashboard is to present an overview of apprenticeship starts in England and to allow lower level breakdowns of starts than those included within our National Statistics Release.",
     "It provides a summary of full-year apprenticeship starts from 2012/13 to 2017/18 and offers an interactive way to investigate starts in the latest academicyear in more detail.",
     hr(),
     h4(strong("Latest National Statistics")),
     "For more further education (FE) statistics, please refer to the: ", 
     a(" FE and skills statistical first release (SFR), ", 
       href = "https://www.gov.uk/government/collections/further-education-and-skills-statistical-first-release-sfr",
       target="_blank"),"and the",a(" FE data library", 
                                    href = "https://www.gov.uk/government/collections/fe-data-library",
                                    target="_blank"),
     hr(),
     h4(strong("Further education statistics dissemination team")),
     "Department for Education", 
     
     "2 St Paul's Place",
     
     "Sheffield",
     
     "S1 2FJ",  
     
     "Email",
     a("FE Official Statistics", href="mailto:FE.OFFICIALSTATISTICS@education.gov.uk?subject=FE statistics enquiry",
       target="_blank"),
     hr(),
     h4(strong("Guidance and methodology")),
     "Add something about methodolgy and guidance - separate tab at end of app or refer back to main publication?",
     a("Link to wherever - this'll take you to school workforce for now",
       href = "https://www.gov.uk/government/collections/statistics-school-workforce",
       target = "_blank"))
       ),
   
   # Show a plot of the selected distribution
   mainPanel(
     selectInput("breakdown","Select dimension to see time-series of starts",
                 c("age-group","level")),
 # Fix/7 - text on three lines rather than one
     #    h3(strong("Apprenticeship starts by ",textOutput("selection")," 2012/13 to 2017/18")),
 ## h3(strong("Apprenticeship starts for 2012/13 to 2017/18")),# text on one line
      h3(strong(htmlOutput("selection") )),
     plotOutput("inter_plot"),
     hr(),
   #  "Can also shove in a table of the values", 
     tableOutput("datatable")
   )
 )),
 # tags$head(
 #   tags$style(type = 'text/css', 
 #              HTML('.navbar { background-color: navy;}
 #                   .navbar-default .navbar-brand{color: white;}
 #                   .tab-panel{ background-color: navy; color: white;}
 #                   .navbar-default .navbar-nav > .active > a, 
 #                   .navbar-default .navbar-nav > .active > a:focus, 
 #                   .navbar-default .navbar-nav > .active > a:hover {
 #                   color: black;
 #                   background-color: lightblue;
 #                   }'
 #                          ))),
#),
#),
################
#Jon code
###########
# 4. Tab 4 ----

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
         )
         
),

# 3. Tab 3 ----

# titlePanel("Mapping application for starts and achievements"),

tabPanel("Map",
         sidebarLayout(
           sidebarPanel(
             h4(strong("Mapping")),
             em("Starts and achievements"),
             br(),
             br(),
             h5(strong("Pick a measure")),
             selectInput(
               "select_map",
               label = NULL,
               choices = list("Starts" = 'starts',
                              "Achievements" = 'achievements'),
               selected = 'starts'
             ),
             width = 3,
             hr(),
             h5(strong("Instructions")),
             "Select the measure of your choice and wait for map to load.",
             br()),
           mainPanel(
             leafletOutput("map", width = '25cm', height = '25cm') %>%
               #spinner to appear while chart is loading
               withSpinner(
                 color = "blue",
                 type = 5,
                 size = getOption("spinner.size", default = 0.4)
               )
           )
         )
),
#                       hr())
#   )),
#  # 2. Tab 2 ----
#  
#              tabPanel("LA Breakdown",
#                       sidebarLayout(
#                         sidebarPanel(
#                           h4(strong("Exclusions by reason")),
#                           "Schools report exclusions broken down by reason",
#                           h5(strong("Pick a school type")),
#                           selectInput("reasonschtype",
#                                       label = NULL,
#                                       choices = list(
#                                         "Primary" = 'State-funded primary',
#                                         "Secondary" = 'State-funded secondary',
#                                         "Special" = 'Special school',
#                                         "All schools" = 'Total'),
#                                       selected = 'Total', width='30%'),
#                           "INSERT A DOWNLOAD BUTTON",
#                          width=12),
#                         mainPanel()),
#                       splitLayout(
#                         verticalLayout(h3("Permanent"),
#                                        strong("Permanent exclusions broken down by reason"),
#                                        em("2015/16 academic year"),
#                                        br(),
#                                         plotOutput("perm_reason"),
#                                         br(),
#                                        br(),
#                                        strong("Permanent exclusions broken down by reason"),
#                                        em("2011/12 to 2015/16 academic year"),
#                                         br(),
#                                         tableOutput("perm_reason_t")),
#                         verticalLayout(h3("Fixed period"), 
#                                         strong("Fixed period exclusions broken down by reason"),
#                                         em("2015/16 academic year"),
#                                         br(),
#                                         plotOutput("fixed_reason"),
#                                         br(),
#                                         br(),
#                                         strong("Fixed period exclusions broken down by reason"),
#                                         em("2011/12 to 2015/16 academic year"),
#                                         br(),
#                                         tableOutput("fixed_reason_t")))),
#               
#               

# 5. Tab 5 ----

tabPanel("Data and methods",
         h4(strong("Data sources")),
         "This release provides headline experimental statistics on the use of the apprenticeship service. These include apprenticeship service account registrations (ASAs) and number of commitments (reported to November 2017), where an apprentice who is expected to go on to start has been recorded in the system.
         
         New to this release are monthly apprenticeship starts information for the first 3 months of the 2017 to 2018 academic year and apprenticeship levy declarations (reported to November 2017).
         ",
         br(),
         br(),
         # Cannot download XLSX files
         #               h5(strong("Main text: SFR08/2018")),
         #       "This file may not be suitable for users of assistive technology",
         #       br(),
         #       downloadButton("downloadtext_ud", "Download"),
         #       br(),
         #       br(),
         #        h5(strong("Main tables: SFR08/2018")),
         #       "This file may not be suitable for users of assistive technology",
         #       br(),
         #       downloadButton("downloadtable_ud", "Download"),
         #       br(),
         #       br(),
         h5(strong("Apprenticeship and levy statistics: February 2018")),
         "For the full report please refer to the: ", 
         a(" Main Text", 
           href = "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/682464/SFR13_2018_Appandlevystats_Feb_Commentary.pdf",
           target="_blank"),"and the",a(" Main Table", 
                                        href = "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/682459/SFR13_2018_Appandlevystats_Feb_Main_table.xlsx",
                                        target="_blank"),
         #   "How to view a PDF doc in Shiny",
         br(),
         # tags$iframe(style="height:600px; width:100%", src="http://localhost/data/SFR08_2018_App_and_Levy_Stats_commentary.pdf")),
         br(),
         br(),
         h4(strong("Definitions")),
         hr(),
         fluidRow(column(
           h5(href = "https://www.gov.uk/", "Apprenticeship Start"), width =3),
           column(
             "A start refers to the number of apprenticeship programmes that begin in a given time period. 
             This measure is helpful in determining the take-up of programmes. 
             An apprentice is counted for each individual apprenticeship they start; 
             for example, if one individual started one intermediate level apprenticeship 
             and one advanced level apprenticeship, they would be counted as two starts.", width = 9)),
         hr(),
         fluidRow(column(
           h5("Commitments"), width =3),
           column(
             "A commitment is where a potential apprentice, who is expected to go 
             on to start an apprenticeship, has been recorded in the system. 
             The apprenticeship service provides a self-managed service on which 
             organisations and providers can add the details of an apprentice. 
             These commitments may be either", strong("fully agreed "),"or",strong("pending approval."),  
             "These were able to be entered from March 2017.",
             br(),
             br(),
             "A",strong("fully agreed commitment")," has agreement on the apprenticeship service from 
             both the organisation and the training provider.",
             br(),
             br(),
             "A",strong("pending approval commitment")," means that details of an apprentice have been added but 
             neither the provider and/or the organisation have finalised the commitment in the service. These commitments 
             show an intent for an apprentice to start, however may not materialise into a fully agreed commitment in the future.",
             width = 9)),
         hr(),
         fluidRow(column(
           h5("Apprenticeship levy"), width =3),
           column(
             "The UK wide apprenticeship levy came into force on 6 April 2017 requiring 
             all UK public and private sector employers with an annual pay bill of £3 million or more to invest 
             in apprenticeship training. Since May 2017, funding arrangements for apprenticeships changed to give
             employers greater control over funding for apprenticeship training.
             More information on paying the apprenticeship levy can be found at this link:",a(" Apprenticeship Levy, ", 
                                                                                              href = "https://www.gov.uk/guidance/pay-apprenticeship-levy",
                                                                                              target="_blank"), width = 9)),
         hr()
         )

           )

           ) # to resolve errors
