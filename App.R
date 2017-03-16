#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load Tda R -- need lots of other packages
library(Rmpfr)
library(FNN)
library(igraph)
library(parallel)
library(scales)
library(Rcpp)
library(gmp)
library(TDA)
# load scipts
source('genSampleDataNoNoise.R')
source('genSampleDataNoise.R')

library(shiny)


# Define UI for application 
ui <- shinyUI(fluidPage(#
  # Application title
  titlePanel("Persistent Homology Using Rips"),
  sidebarLayout(#
    sidebarPanel("This app computes the persistent homology of two point clouds using Rips filtration.  By playing with the scale it is possible to identify the toplogocial features in both dimension 0 and 1.  The dimension 0 features correspond to connected components and are denoted by dots which are black.   The dimension 1 features correspond to tunnels and are denoted by red triangles.  In practice we want the scale as small as possible so that the computations are as small as possible", br(), br(), br(),#
                 numericInput("n", "Number of points", 100,  min = 10, max = 200, step = 10),
                 numericInput("stadard_dev","Amount of Noise", 0.5, min = 0, max = 10, step = 0.05),
                 numericInput("skale","Scale", 4, min = 0, max = 10, step = 0.5),
                 numericInput("r1", "Radius of Circle", 4, min=0.5, max=100, step = 1), width=5),
    mainPanel(fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("circleNoNoise"),plotOutput("circleNoise"))
    ),
      fluidRow(strong("Persistence Diagrams"),
                          splitLayout(cellWidths = c("50%", "50%"), plotOutput("noNoise"), plotOutput("noise"))
                        ), br(), br(), br(), br(), "Once the scale is about 2.5 the dimension 1 features have significant persistence.  As compared to the non-noise example the circle with noise has many more connected components and tunnels which have some persistence.  This is due to the noise being detected as small tunnels.  But as we look across many scales these features do not have the lifespan the fundamental tunnel does.  I'm curious if anyone has identified an approximate cutoff for the level of noise the algorithm can handle.  I'm amazed how well it does with very high amounts of noise.", br(), br(), "I noticed that if the circle is sampled fewer times most of the tunnels with little to no persistence disappear altogether.  Something to keep in mind, it might be useful when working with very large datasets to slowly increase the sample size, trying to detect the macro features (once low persistence features show up, perhaps we don't need to sample any longer).  Almost like making a filtration for the data as well. i.e., start sampling at 20 and increase, notice that by 50 we start getting low persistence dimension 1 features.  I'm not sure if this idea generalizes, but it is worth a look.", width = 7)
    )
)#
)

# Define Server
server <- function(input, output){
  value1 <- reactive(#
    {sample1 <- genSampleDataNoNoise(input$n,input$r1)
    DiagNoNoise <- ripsDiag(X=sample1, maxdimension = 1, maxscale = input$skale)
    persistNoNoise <- DiagNoNoise[["diagram"]]
    persistNoNoise})
  
  value2 <- reactive(#
    {sample2 <- genSampleDataNoise(input$n, input$r1, stdev = input$stadard_dev)
    DiagNoise <- ripsDiag(X=sample2, maxdimension = 1, maxscale = input$skale)
    persistNoise <- DiagNoise[["diagram"]]
    persistNoise})
  
  sample1X <- reactive({
    sample1 <- genSampleDataNoNoise(input$n,input$r1)
    x1<-sample1[,1]
    x1})
  sample1Y <- reactive({
    sample1 <- genSampleDataNoNoise(input$n,input$r1)
    y1<-sample1[,2]
    y1})
  sample2X <- reactive({
    sample2 <- genSampleDataNoise(input$n, input$r1, stdev = input$stadard_dev)
    x2 <- sample2[,1]
    x2})
  sample2Y <- reactive({
    sample2 <- genSampleDataNoise(input$n, input$r1, stdev = input$stadard_dev)
    y2 <- sample2[,2]
    y2})
  
    output$circleNoNoise <- renderPlot({plot(sample1X(),sample1Y(), main="Circle with no noise", ylab = '', xlab = '')})
    
    output$circleNoise <- renderPlot({plot(sample2X(),sample2Y(), main = "Circle with noise", ylab = '', xlab = '')})
    
    output$noNoise <- renderPlot({plot(value1())})
    
    output$noise <- renderPlot({plot(value2())})
}

shinyApp(ui = ui, server = server)  
