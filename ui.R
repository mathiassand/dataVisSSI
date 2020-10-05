#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
    headerPanel("DK Covid cases overview"),
    sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     selected = ';'),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     'Double Quote')
    ),
    mainPanel(
        leafletOutput("myMap")
    )
))