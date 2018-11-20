#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
######
#Rick's code
#library(shiny)


# Define server logic required to draw a histogram
#shinyServer(function(input, output) {
   
######
#Jon's code
#

source("Nov app first page plots.R")


server <- function(input, output) {
  

  output$age_plot <- renderPlot({
    ggplot(data=startsbyage) +
      geom_col(mapping = aes(x=Age, y=Starts, fill=Age))+
      facet_grid(.~academic_year, switch = "x") +
      labs(x="Age-group by academic year", y="Starts", fill="Age-group")+
      scale_fill_manual(values=c("lightblue3","blue","navy"))+
      scale_y_continuous(breaks = seq(0,250000,50000),expand = c(0, 0), 
                         limits = c(0,250000), 
                         labels = function(d){paste0(format(d,big.mark=",",trim=TRUE))}) +
      theme_classic() +
      theme(panel.border = element_blank(),
            panel.grid.major.y = element_line(colour="grey80"), 
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.placement = "outside",
           strip.background.x = element_rect(linetype = 0),
            strip.text.x = element_text(size = 12),
            axis.line = element_line(colour = "grey80"),
            axis.ticks = element_line(colour = "grey80"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12),
            legend.text = element_text(size=12),
            legend.title = element_text(size=12))
      })
  
  output$level_plot <- renderPlot({
    ggplot(data=startsbylev) +
      geom_col(mapping = aes(x=Level, y=Starts, fill=Level))+
      facet_grid(.~academic_year,switch = "x")+
      labs(x="Level by academic year", y="Starts", fill="Level")+
      scale_fill_manual(values=c("lightblue3","blue","navy")) +
      scale_y_continuous(breaks = seq(0,300000,50000),expand = c(0, 0), 
                         limits = c(0,300000), 
                         labels = function(d){paste0(format(d,big.mark=",",trim=TRUE))}) +
      theme_classic() +
      theme(panel.border = element_blank(),
            panel.grid.major.y = element_line(colour="grey80"), 
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.placement = "outside",
            strip.background.x = element_rect(linetype = 0),
            strip.text.x = element_text(size = 12),
            axis.line = element_line(colour = "grey80"),
            axis.ticks = element_line(colour = "grey80"),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=12),
            legend.text = element_text(size=12),
            legend.title = element_text(size=12))
    }) 

  output$inter_plot <- renderPlot({
    if(input$breakdown == 'age-group') {
      ggplot(data=startsbyage) +
        geom_col(mapping = aes(x=Age, y=Starts, fill=Age))+
        facet_grid(.~academic_year, switch = "x") +
        labs(x="Age-group by academic year", y="Starts", fill="Age-group")+
        scale_fill_manual(values=c("lightblue3","blue","navy"))+
        scale_y_continuous(breaks = seq(0,250000,50000),expand = c(0, 0), 
                           limits = c(0,250000), 
                           labels = function(d){paste0(format(d,big.mark=",",trim=TRUE))}) +
        theme_classic() +
        theme(panel.border = element_blank(),
              panel.grid.major.y = element_line(colour="grey80"), 
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              strip.placement = "outside",
              strip.background.x = element_rect(linetype = 0),
              strip.text.x = element_text(size = 12),
              axis.line = element_line(colour = "grey80"),
              axis.ticks = element_line(colour = "grey80"),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size=12),
              legend.text = element_text(size=12),
              legend.title = element_text(size=12)) 
    } else if (input$breakdown == 'level') {
      ggplot(data=startsbylev) +
        geom_col(mapping = aes(x=Level, y=Starts, fill=Level))+
        facet_grid(.~academic_year,switch = "x")+
        labs(x="Level by academic year", y="Starts", fill="Level")+
        scale_fill_manual(values=c("lightblue3","blue","navy")) +
        scale_y_continuous(breaks = seq(0,300000,50000),expand = c(0, 0), 
                           limits = c(0,300000), 
                           labels = function(d){paste0(format(d,big.mark=",",trim=TRUE))}) +
        theme_classic() +
        theme(panel.border = element_blank(),
              panel.grid.major.y = element_line(colour="grey80"), 
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              strip.placement = "outside",
              strip.background.x = element_rect(linetype = 0),
              strip.text.x = element_text(size = 12),
              axis.line = element_line(colour = "grey80"),
              axis.ticks = element_line(colour = "grey80"),
              axis.title.x = element_text(size=12),
              axis.title.y = element_text(size=12),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size=12),
              legend.text = element_text(size=12),
              legend.title = element_text(size=12))
    }
    
    })

 #Rick Old front page data 
  #  output$datatable <- renderTable({
   # if (input$breakdown == 'age-group'){
  #    print.data.frame(dcast(data = startsbyage, formula = Age~academic_year, fun.aggregate = sum, value.var = "Starts"))}
  #  else if (input$breakdown == 'level'){
  #    print.data.frame(dcast(data = startsbylev, formula = Level~academic_year, fun.aggregate = sum, value.var = "Starts"))}
    
  #  })
  
  #And then the server code becomes - Rick front page:
  
  output$datatable <- renderTable({
    
    if (input$breakdown == 'age-group'){
      
      print.data.frame(startsbyagetab)}
    
    else if (input$breakdown == 'level'){
      
      print.data.frame(startsbylevtab)}
  })
  
  output$selection <- renderText({input$breakdown})
  ## End rick code
  ##########
  # Jon code
  output$p_bar <- renderPlotly({national_bars('P')})
  
  output$f_bar <- renderPlotly({national_bars('F')})
  
  output$gridPlot1 <- renderPlot({
    #     plot.new()
    # l <- linesGrob()
    # Draw it
    #  grid.draw(l) # works straing line
    
    # grid.draw (ggplotGrob(Plot7)) doesn#t work
  })
  
  # 2. Reason ----
  
  # output$perm_reason <- renderPlot({perm_reason_bar(input$reasonschtype)})
  # output$fixed_reason <- renderPlot({fixed_reason_bar(input$reasonschtype)})
  
  # output$perm_reason_t <- renderTable({perm_reason_table(input$reasonschtype)},  bordered = TRUE, spacing = 'm')
  # output$fixed_reason_t <- renderTable({fixed_reason_table(input$reasonschtype)},  bordered = TRUE, spacing = 'm')
  
  
  # 2. Pivot table ----
  
  #  output$mypivot = renderRpivotTable({
  #  rpivotTable(
  #     pivot,
  #     rows = c("LAD","Level","variable"),
  #     cols = c("Age"), ##cols = c("Age","SSA T1"),
  #     aggregatorName = "Sum",
  #     inclusions = c(list( LAD = list("Sheffield")),list( variable = list("Starts_R"))),
  ##     exclusions = c(list( LAD = list("Totals")),list( Region = list("Totals")),list( PCON = list("Totals")),list( Level = list("Totals"))),
  #     vals = "value",
  #     rendererName = "Table"
  #   )  
  
  # })
  
  
  # 3. LA Breakdown ---- See LA trends for multi reasons
  #####################
  # Starts and achievements bar chart
  
  output$perm_reason <- renderPlot({perm_reason_bar(input$select2,input$reason_type1,input$reason_type2)})
  output$fixed_reason <- renderPlot({fixed_reason_bar(input$select2,input$reason_type1,input$reason_type2)})
  
  output$perm_reason_t <- renderTable({perm_reason_table(input$select2,input$reason_type1,input$reason_type2)},  bordered = TRUE, spacing = 'm')
  output$fixed_reason_t <- renderTable({fixed_reason_table(input$select2,input$reason_type1,input$reason_type2)},  bordered = TRUE, spacing = 'm')
  
  
  
  # 4. Map ----
  
  output$map <- renderLeaflet({map(input$select_map)})
  
  # 5. Methods ----
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$select2, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(la_sch_table(input$select2), file, row.names = FALSE)
    }
  )
  
  output$downloadmain_ud <- downloadHandler(
    filename = function() {
      paste("national_region_la_school_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(main_ud, file, row.names = FALSE)
    }
  )  
 #################
 # LA PT downloadmai
  #################
  output$downloadfsmchar <- downloadHandler(
    filename = function() {
      paste("poc3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(poc3, file, row.names = FALSE)
    }
  ) 
  
  output$downloadreason_ud <- downloadHandler(
    filename = function() {
      paste("reason_for_exclusion", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reason_ud, file, row.names = FALSE)
    }
  )  
  
  output$downloadnatchar_ud <- downloadHandler(
    filename = function() {
      paste("national_characteristics", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(char_ud, file, row.names = FALSE)
    }
  ) 
  
}
#)

