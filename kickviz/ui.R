#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(shinydashboard)
library(tidyverse)
library(plotly)


# dashboard

dashboardPage(
    dashboardHeader(title="Kickstarter Projects"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Prelim Observations", tabName = "Prelim", icon=icon("dashboard")),
            menuItem("Understanding Relationships", tabName="Understanding", icon=icon("th")),
            menuItem("Inference and Prediction", tabName="Inference", icon=icon("th"))
            
        )
    ),
    dashboardBody(
        tabItems(
            #first tab
            tabItem(tabName = "Prelim",
                    fluidRow(
                        box(title="Inputs",
                            status="primary",
                            width=12,
                            solidHeader=TRUE,
                            selectInput("country", "Select Country:", 
                                        choices= c(" "," "))
                        ),
                        box(title="Plot",
                            status="primary",
                            width=12,
                            solidHeader=TRUE,
                            plotlyOutput("plot")
                        ),
                        
                        box(width=12,
                            title="info",
                            solidHeader=TRUE,
                            uiOutput("datesUI") 
                            # dateRangeInput("daterange", 
                            #                label = h3("Date range"),
                            #                min = "2009-05-03",
                            #                max = "2018-03-03",
                            #                format = "mm/dd/yy",
                            #                separator=" - ")),
                            
                        ),
                        box(width=6,
                            title="PEOPLE",
                            solidHeader=TRUE,
                            valueBoxOutput(width=12, "outA")
                        ),
                        box(width=6,
                            title="PEOPLE",
                            solidHeader=TRUE,
                            valueBoxOutput(width=12, "outC")
                        ),
                        box(width=12,
                            title="OVERALL",
                            solidHeader=TRUE,
                            valueBoxOutput(width=12, "outE")
                        ),
                        box(width=6,
                            title="PROJECT",
                            solidHeader=TRUE,
                            valueBoxOutput(width=12, "outD")
                        ),
                        box(width=6,
                            title="PROJECT",
                            solidHeader=TRUE,
                            valueBoxOutput(width=12, "outB")
                        ),
                        
                        
                        
                    )
            ),
            
            #second tab
            tabItem(tabName = "Understanding",
                    fluidRow(
                        box(width=12,
                            status="info",
                            title="Funded",
                            solidHeader=TRUE)
                    )
            ),
            #third tab
            tabItem(tabName="Inference",
                    fluidRow(
                        infoBox(width=12,
                                title= "Log-Odds of Success",
                                "=.813+(-.0001974*Goal+.04059*Backers+-0.017372*Days+Category+Country)"
                        ),
                        box(width=12,
                            status="info",
                            title="Prediction from Bayesian Generalized Linear Mixed Model",
                            solidHeader=TRUE,
                            valueBoxOutput(width=6, "outPred"),
                            valueBoxOutput(width=6, "outProb")
                        ),
                        box(width=12,
                            status="primary",
                            title="Inputs",
                            solidHeader=TRUE,
                            sliderInput("goalslider", "Project Goal:", 1000, 100000, 5000),
                            sliderInput("backerslider", "Number of Backers:", 1, 1000, 50),
                            sliderInput("dayslider", "Duration in Days:", 1, 60, 30),
                            selectInput("category", "Select Category:", 
                                        choices= c(" "," ")),
                            selectInput("location", "Select Country:", 
                                        choices= c(" "," "))
                        )
                        
                        
                    )
            )
        )
        
    )
)  


