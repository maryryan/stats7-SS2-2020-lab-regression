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

coaster <- readr::read_csv('https://raw.githubusercontent.com/maryryan/stats7-SS2-2020/master/stats7-SS2-2020-labs/lab-regression/datasets_1307_3124_rollercoasters.csv?token=ADKNEZWF2DNWDKUF7WOBQUK7GRYCU')

coaster.mat <- data.frame(matrix(unlist(coaster), ncol=21))

double_cols <- c(1,4:5, 7, 9, 11:21)
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
                               verbatimTextOutput("info12"))))
               )
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
      scatter_fun(coaster, colnames(coaster)[x1[1]], colnames(coaster)[x1[2]])
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
         # make a regression formula and regress
         fmla <- as.formula(paste(paste(names_x1[1])," ~ ", paste(names_x1[2:len], collapse= "+")))
         r <- summary(lm( fmla, data=coaster.mat[,x1] ))$r.squared
         paste("The r-squared is:", r)
      }else{NULL}
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

