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
                        valueBox("kickstarter", "Data as of 3/3/2018",
                            width=12
                        ),
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
                            plotlyOutput("projplot")
                        ),
                        
                        box(width=12,
                            title="info",
                            solidHeader=TRUE,
                            uiOutput("datesUI") 
                        ),
                        box(width=12,
                            title="OVERALL",
                            solidHeader=TRUE,
                            valueBoxOutput(width=12, "outpledged"),
                            valueBoxOutput(width=12, "outfunded"),
                            valueBoxOutput(width=12, "outpropsuccess")
                        ),
                        box(width=4,
                            title="PEOPLE",
                            solidHeader=TRUE,
                            valueBoxOutput(width=12, "outbackers")
                        ),
                        box(width=4,
                            title="PROJECT",
                            solidHeader=TRUE,
                            valueBoxOutput(width=12, "outgoal")
                        ),
                        box(width=4,
                            title="PROJECT",
                            solidHeader=TRUE,
                            valueBoxOutput(width=12, "outsuccess")
                        )


                    )
            ),
            
            #second tab
            tabItem(tabName = "Understanding",
                    fluidRow(
                        box(
                            width=NULL,
                            selectInput("projbycountry", "Select Country:", choices= c(" "," "))
                        ),
                        tabBox(
                            title="Big Picture",
                            id="tab1",
                            width=12,
                            height= "500px",
                            tabPanel("Projects by Country", plotlyOutput("projbycountryplot")),
                            tabPanel("Funded Breakdown", plotlyOutput("fundedplot"))
                        )),
                    fluidRow(
                        valueBox("In Depth", "Data on Success",
                                 width=12
                        ),
                        tabBox(
                            id="tab2",
                            width=6,
                            height= "450px",
                            tabPanel("Boxplot", plotlyOutput("goalsboxplot")),
                            tabPanel("Successful Goals",plotlyOutput("successgoalhist")),
                            tabPanel("Unsuccessful Goals", plotlyOutput("unsuccessgoalhist"))
                        ),
                        tabBox(
                            side="right",
                            height="450px",
                            tabPanel("Unsuccessful Backers", plotlyOutput("unsuccessbackershist")),
                            tabPanel("Successful Backers", plotlyOutput("successbackershist")),
                            tabPanel("Boxplot", plotlyOutput("backersboxplot"))
                        )
                    ),
                    fluidRow(
                        box(width=12,
                            status="info",
                            title= "Scope of Data",
                            solidHeader = TRUE,
                            valueBoxOutput(width=6, "maxpledged"),
                            valueBoxOutput(width=6, "maxbackers"),
                            valueBoxOutput(width=6, "maxpledgedInfo"),
                            valueBoxOutput(width=6, "maxbackersInfo")
                        )
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


