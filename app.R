#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load libraries
library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
#library(tidytuesdayR)

coaster <- read_csv("./rollercoaster_edited.csv")#readr::read_csv('https://raw.githubusercontent.com/maryryan/stats7-SS2-2020-lab-regression/master/datasets_1307_3124_rollercoasters.csv?token=ADKNEZQBUPLKRJEG2OF6LDK7GR7EG')
coaster <- coaster[,-1]
coaster.mat <- data.frame(matrix(unlist(coaster), ncol=20))

double_cols <- c(1,4, 6, 8, 10:20)
coaster.mat[,double_cols] <- sapply(coaster.mat[, double_cols], as.numeric)
colnames(coaster.mat) <- colnames(coaster)

# Define UI for application that displays data and scatterplot
ui <- fluidPage(theme=shinytheme("readable"),
   
   # Application title
   titlePanel("Muliple Linear Regression"),
   # Make multi-tab/dataset display for showing both datatable and plotting data 
   tabsetPanel(
      id = 'dataset',
      tabPanel("Instructions",
               h4(div("You are a member of a project team at Peter Point Amusement Park. 
                      Your team is responsible for designing a new roller coaster for the park. 
                      You know that rides with a higher excitement score bring in more park-goers.")),
               div(HTML("<br>")),
               h4(div("Currently, your team is interested in how the intensity of the ride is related to excitement. 
                      You have taken a sample of existing roller coasters, and recorded characteristics about them.
                       There is a data table of this research in the next tab.
                      Scroll left and right on the data table to see all the available variables. 
                      Scroll down past the data table to access the plotting and regression tools.")),
               div(HTML("<br>")),
               h4(div("Use the following tips to perform the analyses outlined in your discussion assignment found on Canvas:")),
               HTML("<div style='font-size:18px'>
                    <ul><li>Selected variables will have regression performed on them.</li>
                    <li>The first variable selected will be treated as your response variable.</li>
                    <li>All other variables will be treated as independent variables.</li>
                    <li>Only the first and second selected variables will be plotted in the scatterplot.</li>
                    <li>To select variables, click on their (un-highlighted) columns and highlight them. Don't click the column title.</li>
                    <li>To de-select variables, click on their highlighted columns and un-highlight them. Don't click the column title.</li>
                    </ul></div>")),
      tabPanel("Roller Coaster Analysis",#DT::dataTableOutput("mytable2")
               fluidRow(
                  column(
                     DT::dataTableOutput("mytable"), width=12)),
               fluidRow(column(6,plotOutput("plot")),
                        column(6, fluidRow(tableOutput("info11"),
                               verbatimTextOutput("info13"),
                               verbatimTextOutput("info12"))))
               ),
      tabPanel("Data Dictionary",
               HTML("<div style='font-size:18px'>
                    <ul><li><b>park_id</b>: Unique park identifier, zero-indexed.</li>
                    <li><b>theme</b>: Title of the park scenario.</li>
                    <li><b>rollercoaster_type</b>: Category of roller coaster.</li>
                    <li><b>excitement</b>: Ride excitement from 0 (very low) with no specified maximum, but it is very rarely above 10.0. Larger numbers are always better.</li>
                    <li><b>intensity</b>: Ride intensity from 0 (very dull) with no specified maximum, though most (well-designed) rides are under 10.0. Each customer has their own intensity preference.</li>
                    <li><b>nausea</b>: Ride nausea from 0 (very low) with no specified maximum, but lower is better and rides rarely have values above 10.0.</li>
                    <li><b>excitement_rating, intensity_rating, nausea_rating</b>: Descriptors of the excitement, intensity, and nausea ratings.</li>
                    <li><b>max_speed</b>: Maximum speed (mph) the ride reaches.</li>
                    <li><b>avg_speed</b>: Average speed (mph) of the ride.</li>
                    <li><b>ride_time</b>: Total duration of the ride in seconds.</li>
                    <li><b>ride_length</b>: Length of the ride in feet.</li>
                    <li><b>max_pos_gs, max_neg_gs, max_lateral_gs</b>: Values describing the maximum observed positive, negative, and lateral G-Forces.</li>
                    <li><b>total_air_time</b>: Number of seconds in which riders experience weightlessness.</li>
                    <li><b>drops</b>: Number of downhill segments.</li>
                    <li><b>highest_drop_height</b>: Highest height (with 0 being sea-level) from which a drop takes place. Note: rides can drop to -6 feet.</li>
                    <li><b>inversions</b>: Number of times riders are upside-down during the ride. This accounts for loops, corkscrews, etc.</li>
                    </ul></div>"))
      )
      
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # create function that will plot selected variables
   scatter_fun <- function(dat, x, y){
      ggplot(dat, aes(x = .data[[x]], y = .data[[y]]) ) +
         geom_point() +
         labs(x = x,
              y = y )
   }
   
   # tuition cost stuff
   output$mytable <-  DT::renderDataTable(
      coaster,
      options = list(scrollX = TRUE),
      # allow variable selection for plotting #
      selection = list(mode='multiple', selected=c(1,2), target = 'column')
   )
   
   output$plot <- renderPlot({
      # identify variables to be plotted
      if(!(is.null(input$mytable_columns_selected))){
      x1 <- input$mytable_columns_selected
      scatter_fun(coaster, colnames(coaster)[x1[2]], colnames(coaster)[x1[1]])
      }else{NULL}
   })
   
   output$info11 <- renderTable({
      if(!(is.null(input$mytable_columns_selected))){
         
         # get the indices of the selected columns
         x1 <- input$mytable_columns_selected
         # get the names of the selected columns
         names_x1 <- colnames(coaster.mat)[x1]
         len <- length(x1)
         # make a regression formula and regress
         fmla <- as.formula(paste(paste(names_x1[1])," ~ ", paste(names_x1[2:len], collapse= "+")))
         tbl <- summary(lm( fmla, data=coaster.mat[,x1] ))$coef[,1:2]
         
         # fix row names when a regressor is a factor
         counter <- 2
         for(i in 2:len){
            
            if(is.factor(coaster.mat[,x1[i]])){
               
               tbl_lvls <- levels(coaster.mat[,x1[i]])
               tbl_lvls <- tbl_lvls[-1]
               rownames(tbl)[counter:(counter+length(tbl_lvls)-1)] <- paste(names_x1[i], ": ", tbl_lvls)
               
               counter <- counter+length(tbl_lvls)
            }
            
         }
         
         tbl
      }else{NULL}
   }, rownames=TRUE)
   
   output$info12 <- renderPrint({
      if(!(is.null(input$mytable_columns_selected))){
         # get the indices of the selected columns
         x1 <- input$mytable_columns_selected
         # get the names of the selected columns
         names_x1 <- colnames(coaster.mat)[x1]
         len <- length(x1)
         correlation <- cor(coaster.mat[,x1[1]], coaster.mat[,x1[2]])
         #paste("The correlation of ",names_x1[1], " and ", names_x1[2], " is ", correlation,".\n")
         # make a regression formula and regress
         fmla <- as.formula(paste(paste(names_x1[1])," ~ ", paste(names_x1[2:len], collapse= "+")))
         
         if(len > 2){
            r <- summary(lm( fmla, data=coaster.mat[,x1] ))$adj.r.squared
            paste("The correlation of ",names_x1[1], " and ", names_x1[2], " is ", correlation,".")
            paste("The adjusted r-squared is:", r)
            
         }else{
            r <- summary(lm( fmla, data=coaster.mat[,x1] ))$r.squared
            paste("The correlation of ",names_x1[1], " and ", names_x1[2], " is ", correlation,".")
            paste("The r-squared is:", r)
         }
         
      }else{NULL}
   })
   
   output$info13 <- renderPrint({
      if(!(is.null(input$mytable_columns_selected))){
         # get the indices of the selected columns
         x1 <- input$mytable_columns_selected
         # get the names of the selected columns
         names_x1 <- colnames(coaster.mat)[x1]
         len <- length(x1)
         correlation <- cor(coaster.mat[,x1[1]], coaster.mat[,x1[2]])
         
         paste("The correlation of",names_x1[1], "and", names_x1[2], "is", correlation)
         
      }else{NULL}
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

