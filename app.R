

library(shiny)
library(dplyr)
library(forcats)
library(DT)
library(ggplot2)
library(treemapify)
rankings=read.csv("qs-world-university-rankings-2017-to-2022-V2.csv")
summary(rankings)
rankings$rank_display[is.na(rankings$rank_display)] <- 100
rankings$international_students[is.na(rankings$international_students)] <- 0
rankings$rank_display <- as.numeric(rankings$rank_display)
rankings$international_students <- as.numeric(as.numeric(gsub(",", "", rankings$international_students)))
summary(rankings)
ui <- navbarPage(
  title = "QS world rankings", 
  about_page <- tabPanel(
    title = "About",
    titlePanel("About"),
    "Created with R Shiny",
    br(),
    "This application gives a gist of Top University Statistics based on Qs world rankings",
    br(),
    "2022 March"
  ),
  
  main_page <- tabPanel(
    title = "General Analysis",
    titlePanel("Analysis"),
    sidebarLayout(
      sidebarPanel(
        title = "Inputs",
        selectInput(inputId = "year", 
                    label = "Year: ",
                    choices = c(2022,2021,2020,2019,2018,2017,2016), 
                    selected ="2021"),

        
        hr(),
        sliderInput(inputId = "rank_display", 
                    label = "Rank:",
                    min = 1, max = 40, 
                    value = "10"),
        
        
        
      ),
      mainPanel(
        tabsetPanel(
          br(),
          tabPanel("Plot", plotOutput("ggplot")),
          tabPanel("Table", DT::dataTableOutput("table")),
          tabPanel("University Sizes", plotOutput("sizeuniversity")),
          tabPanel("University year scores", plotOutput("scorereport")),
          tabPanel("International Students", plotOutput("international")),
         
          
        )
      )
    )
  ),
  
  region_page<- tabPanel(
    title = "Region Analysis",
    titlePanel("Analysis"),
    sidebarLayout(
      sidebarPanel(
        title = "Inputs",
        selectInput(inputId = "year1", 
                    label = "Year: ",
                    choices = c(2022,2021,2020,2019,2018,2017,2016), 
                    selected ="2021"),
        hr(),
        selectInput(inputId = "region", 
                    label = "Region:",
                    choices = c("Asia","Africa","Europe","Latin America","Oceania","North America","Oceania"), 
        ),
        
        hr(),
        sliderInput(inputId = "rank_display1", 
                    label = "Rank:",
                    min = 1, max = 100, 
                    value = "70"),
    
      ),
      mainPanel(
        tabsetPanel(
          br(),
          tabPanel("Plot", plotOutput("plot")),
          tabPanel("Table", DT::dataTableOutput("regiontable")),
          tabPanel("Student Faculty Ratio", plotOutput("Treemap")),
          tabPanel("University year scores", plotOutput("regionscorereport")),
        )
      )
    )
  )
  
)

server <- function(input,output) {
  output$plot = renderPlot({ 
    rankings %>% select(year, university, rank_display,region,type) %>% group_by(region, university) %>% 
      filter(rank_display <= input$rank_display1 & year == input$year1 & region == input$region ) %>%
      ggplot(aes(x = rank_display, y = reorder(university, -rank_display), fill = type)) + geom_bar(stat = 'identity')  + geom_label(aes(label = rank_display), size = 3)+
      labs(x="Rank ",y="University", 
           title = "Top ranking universities based on Continents")+theme(
             plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
             legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
             legend.text = element_text(color = "black"),
             legend.position="bottom",
             axis.text.y = element_text( color="cadetblue", size=14,face="bold"),
             axis.text.x = element_text( color="cadetblue", angle=90, size=11,face="bold"),
             legend.box.background = element_rect(color="cadetblue", size=1),
             legend.box.margin = margin(1, 1),
             legend.key = element_rect(fill = "white", colour = "cadetblue")) },height=600)
    
  output$ggplot = renderPlot({ 
    
    rankings %>% select(year, university, rank_display,type) %>% 
      group_by(university, rank_display) %>% 
      filter(rank_display <= as.numeric(input$rank_display) & year == input$year ) %>%
      ggplot(aes(x = rank_display, y = reorder(university, -rank_display),fill=type)) + geom_bar(stat = 'identity')  + geom_label(aes(label = rank_display), size = 3)+
      labs(x="Rank ",y="University", 
                                                                      title = "World ranking universities ")+theme(
                                                                        plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
                                                                        legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
                                                                        legend.text = element_text(color = "black"),
                                                                        legend.position="bottom",
                                                                        axis.text.y = element_text( color="cadetblue", size=14,face="bold"),
                                                                        axis.text.x = element_text( color="cadetblue", angle=90, size=11,face="bold"),
                                                                        legend.box.background = element_rect(color="cadetblue", size=1),
                                                                        legend.box.margin = margin(1, 1),
                                                                        legend.key = element_rect(fill = "white", colour = "cadetblue")) },height=600)
  
  output$sizeuniversity = renderPlot({ 
    
    rankings %>% select(year, university, score,size,rank_display) %>% 
      group_by(university, rank_display) %>% 
      filter(rank_display <= as.numeric(input$rank_display) & year == input$year ) %>%
      ggplot(aes(x = rank_display, y = reorder(university, -rank_display),fill=size)) + geom_bar(stat = 'identity')  + geom_label(aes(label = score), size = 3)+
      labs(x="Rank ",y="university", 
           title = "World ranking universities based on size")+theme(
             plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
             legend.title = element_text(color = "cadetblue", size = 10,face = "bold"),
             legend.text = element_text(color = "black"),
             legend.position="bottom",
             axis.text.y = element_text( color="cadetblue", size=14,face="bold"),
             axis.text.x = element_text( color="cadetblue", angle=90, size=11,face="bold"),
             legend.box.background = element_rect(color="cadetblue", size=1),
             legend.box.margin = margin(1, 1),
             legend.key = element_rect(fill = "white", colour = "cadetblue")) },height=600)
  
  output$scorereport = renderPlot({ 
    
    rankings %>% select(year, university, rank_display,type,score) %>% 
      group_by(university, rank_display) %>% 
      filter(rank_display <= as.numeric(input$rank_display) ) %>%
      ggplot(aes(x = year, y = score,group =university)) + geom_line(aes(color=university)) + geom_point(aes(color=university))+
      labs(x="Year ",y="Score", 
           title = "Score Report Over Years")+theme(
             plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
             legend.title = element_text(color = "cadetblue", size = 12,face = "bold"),
             legend.text = element_text(color = "black",size=10),
             legend.position="bottom",
             axis.text.y = element_text( color="cadetblue", size=14,face="bold"),
             axis.text.x = element_text( color="cadetblue", angle=90, size=11,face="bold"),
             legend.box.background = element_rect(color="cadetblue", size=1),
             legend.box.margin = margin(1, 1),
             legend.key = element_rect(fill = "white", colour = "cadetblue")) },height=600)
  
  
  output$regionscorereport = renderPlot({ 
    
    rankings %>% select(year, university, rank_display,type,score,region) %>% 
      group_by(university, rank_display) %>% 
      filter(rank_display <= as.numeric(input$rank_display1)& region==input$region ) %>%
      ggplot(aes(x = year, y = score,group =university)) + geom_line(aes(color=university)) + geom_point(aes(color=university))+
      labs(x="Year ",y="Score", 
           title = "Score Report Over Years")+theme(
             plot.title = element_text(color="cadetblue", size=14, face="bold.italic"),
             legend.title = element_text(color = "cadetblue", size = 12,face = "bold"),
             legend.text = element_text(color = "black",size=10),
             legend.position="bottom",
             axis.text.y = element_text( color="cadetblue", size=14,face="bold"),
             axis.text.x = element_text( color="cadetblue", angle=90, size=11,face="bold"),
             legend.box.background = element_rect(color="cadetblue", size=1),
             legend.box.margin = margin(1, 1),
             legend.key = element_rect(fill = "white", colour = "cadetblue")) },height=600)
  
  
  output$table   <- DT::renderDataTable({
    generalrankings <-     rankings %>% select(year, university, rank_display,type,score) %>% 
      group_by(university, rank_display) %>% 
      filter(rank_display <= as.numeric(input$rank_display) & year == input$year)
    DT::datatable(data = generalrankings, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  output$regiontable   <- DT::renderDataTable({
    regionalrankings <-       rankings %>% select(year, university, rank_display,region,type,score) %>% group_by(region, university) %>% 
      filter(rank_display <= input$rank_display1 & year == input$year1 & region == input$region )
    DT::datatable(data = regionalrankings, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })

  output$Treemap <- 
    renderPlot({ rankings %>% select(year, university, rank_display,region,type,student_faculty_ratio) %>% group_by(region, university) %>% 
        filter(rank_display <= input$rank_display1 & year == input$year1 & region == input$region ) %>%
    ggplot(aes(area= student_faculty_ratio,fill=student_faculty_ratio,label = paste(university,student_faculty_ratio,sep = "\n"))) + geom_treemap()+
       
        geom_treemap_text(colour = "white",
                          place = "center",
                          size = 15) },height=600)
  
  output$international <- renderPlot({ 
    
    students <- rankings %>% select(year, university, rank_display,international_students,region) %>% 
      group_by(university, rank_display) %>% 
      filter(rank_display <= as.numeric(input$rank_display) & year == input$year )
    students$fraction <- students$international_students / sum(students$international_students)
    
    
    
    library(dplyr)
    tablestudents <- students %>% 
      group_by(region) %>% 
      summarise(international_students = sum(international_students))
    tablestudents
    tablestudents$fraction <- tablestudents$international_students / sum(tablestudents$international_students)
    
    # Compute the cumulative percentages (top of each rectangle)
    tablestudents$ymax <- cumsum(tablestudents$fraction)
    
    # Compute the bottom of each rectangle
    tablestudents$ymin <- c(0, head(tablestudents$ymax, n=-1))
    
    # Compute label position
    tablestudents$labelPosition <- (tablestudents$ymax + tablestudents$ymin) / 2
    
    # Compute a good label
    tablestudents$label <- paste0(tablestudents$international_students)
    tablestudents
    ggplot(tablestudents, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=region)) +
      geom_rect() +
      geom_text( x=3.5, aes(y=labelPosition, label=label), size=5) + # x here controls label position (inner / outer)
      # scale_fill_brewer(palette=5) +
      #scale_color_brewer(palette=5) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() 
    #  theme(legend.position = "none")
  })
}

shinyApp(ui = ui,server = server)