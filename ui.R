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
  navbarPage("Apprenticeships Statistics", id = "nav", theme = "shiny.css", header=singleton(tags$head(includeScript('www/google-analytics.js'))),
                    
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
     h3(strong("Apprenticeship starts in England",br(),
               "- experimental analysis tool")),
     hr(),
     h3(strong("LATEST 2017/18 FINAL DATA TO BE PUBLISHED SHORTLY AFTER 9:30AM THURSDAY 6th DECEMBER")),
     hr(),
     "This tool is designed to enable users to drill down further into more detailed information on apprenticeship starts and is currently under development",
     hr(),
     h4(strong("Background")),
     "The purpose of this tool is to present an overview of apprenticeship starts in England and to allow lower-level breakdowns of starts to compliment those included within our latest statistics releases, published on the 6th December 2018.",
     br(),
     "It provides a time-series of full-year apprenticeship starts from 2012/13 to 2017/18 and offers an interactive way to investigate starts in the latest academic year in different Local Authorities in more detail.",
     hr(),
     h4(strong("Latest National Statistics")),
     "For more further education (FE) statistics, please refer to the: ", 
     a(" FE and skills statistical release, ", 
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
       target="_blank"))
     #hr(),
     #h4(strong("Guidance and methodology")),
     #"A quality and methodology information document accompanies our 'Further Education and Skills' statistical publication: ",
     #a("Quality and methodology document",
     #   href = "https://www.gov.uk/government/statistics/announcements/further-education-and-skills-november-2018",
     #  target = "_blank"))
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
             h4(strong("Apprenticeships by Sector Subject Area Tier 1")),
             "Local Authority report broken down by Age and Level",
             br(),
             downloadButton("downloadfsmchar", "Download"),
             h5(strong("Pick a local authority district")),                            
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
             em("Starts and achievements for local authority district"),
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
         h4(strong("Overview")),
         "This tool presents information on apprenticeships (16+) in England. Apprenticeships are paid jobs that incorporate on- and off-the-job training leading to nationally recognised qualifications. As an employee, apprentices earn as they learn and gain practical skills in the workplace.", 
         br(),
         br(),
         "A time-series of apprenticeship starts for full academic years 2012/13 to 2017/18, by age-group and level are included in this tool plus interactive breakdowns of local authority district apprenticeship starts and achievements by age, level and subject area for final 2017/18 data. Starts and achievement volumes by local authority are also available as a map.
         ",
         br(),
         br(),
         h4(strong("Apprenticeship and levy statistics: November 2018")),
         "For the full release, please refer to the ", 
         a(" publication page", 
           href = "https://www.gov.uk/government/statistics/announcements/apprenticeship-and-levy-statistics-november-2018",
           target="_blank"),
         br(),
         br(),
         h4(strong("Guidance and methodology")),
         "A quality and methodology information document accompanies our 'Further Education and Skills' statistical publication: ",
         a("Quality and methodology document",href = "https://www.gov.uk/government/statistics/announcements/further-education-and-skills-november-2018",target = "_blank"),
         br(),
         br(),
         h4(strong("Apprenticeship starts")),
         "Starts refer to the number of apprenticeship programmes that begin in a given time period.This measure is helpful in determining the take-up of programmes.",
         br(),
         "An apprentice is counted for each individual apprenticeship they start; for example, if one individual started one intermediate level apprenticeship and one advanced level apprenticeship, they would be counted as two starts.",
         br(),
         br(),
         h4(strong("When reviewing tables, please note that:")),
         hr(),
         fluidRow(column(
           h5("We preserve confidentiality"), width =2),
           column(
             "The ",a("Code of Practice for Official Statistics",href="https://www.statisticsauthority.gov.uk/monitoring-and-assessment/code-of-practice/",
                      target="-blank"), "requires us to take reasonable steps to ensure that our published or disseminated statistics protect confidentiality.",
             width = 8)),
         hr(),
         fluidRow(column(
           h5("We suppress some figures"), width =2),
           column(
             "In the local authority tables, volumes below 5 are suppressed (this includes values of zero).", width = 8)),
         hr(),
         fluidRow(column(
           h5("We adopt symbols to help identify suppression"), width =2),
           column(
             "Symbols are used in the tables as follows:",
             br(),"'-' indicates a volume of below 5 in the local authority tables.", width = 8)),
         hr(),
         fluidRow(column(
           h5("We round figures"), width =2),
           column(
             "In the headline time-series, volumes are rounded to the nearest 100. In the local authority tables, volumes are rounded to the nearest 10.", width = 8)),
         hr()
)#,

#         hr()
         )
#
           )

 #          ) # to resolve errors
