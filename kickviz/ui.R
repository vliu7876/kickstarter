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
    skin="black",
    dashboardHeader(title="Kickstarter Projects"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Prelim Observations", tabName = "Prelim", icon=icon("eye")),
            menuItem("Understanding Relationships", tabName="Understanding", icon= icon("link", lib = "font-awesome")),
            menuItem("Inference and Prediction", tabName="Inference", icon= icon("wifi", lib = "font-awesome"))
            
        )
    ),
    dashboardBody(
        tabItems(
            ## FIRST TAB
          
          ## PRELIM OBSREVATIONS
            tabItem(tabName = "Prelim",
                    fluidRow(
                        valueBox("Understanding Success On Kickstarter", "Data as of 3/3/2018 - Victoria Liu",
                            width=12,
                            color="teal"
                        ),
                        infoBox(
                            "Introduction", 
                            width=12,
                            HTML(paste0("Kickstarter projects are funded through backers who pledge monetary support and when pledges meet project goals, they are funded. Now, around the world across different fields, let's explore indicators of success on Kickstarter!",br())),
                            color = "green",
                            icon = icon("kickstarter-k", lib = "font-awesome")
                            ),
                      
                        box(title="Country",
                            status="warning",
                            width=6,
                            solidHeader=TRUE,
                            selectInput("country", "Select Country:", 
                                        choices= c(" "," ")),
                            height="125px"
                        ),
                        
                        box(width=6,
                            title="Scope",
                            status="warning",
                            solidHeader=TRUE,
                            uiOutput("datesUI") 
                        ),
                        box(title="Overview",
                            status="info",
                            width=12,
                            solidHeader=TRUE,
                            plotlyOutput("projplot")
                        ),

                        box(width=12,
                            title="Summary Statistics",
                            solidHeader=TRUE,
                            valueBoxOutput(width=4, "outpledged"),
                            valueBoxOutput(width=4, "outfunded"),
                            valueBoxOutput(width=4, "outpropsuccess"),
                            valueBoxOutput(width=4, "outbackers"),
                            valueBoxOutput(width=4, "outgoal"),
                            valueBoxOutput(width=4, "outsuccess")
                        ),
                        infoBox(
                          "Disclaimer", 
                          width=12,
                          HTML(paste0("Please note: ", br(), "- Data for this site is from years 2009-2018", br(), 
                                      "- All monetary amounts on the site are in USD real, converted regardless of original currency",br(),
                                      "- Predictions on the third tab should be taken as estimations and not depictions of actual circumstances. Calculations are produced by the model disclosed on the page and should not be used as guarantees or in advisory circumstances",br(),
                                      "- User retains liability over use and distribution of information through site",br(),
                                      br(),
                                      "Copyright Victoria Liu 2020")),
                          color = "black",
                          icon = icon("eye", lib = "font-awesome")
                        )
                       


                    )
            ),
            
            ## SECOND TAB
            
            ## UNDERSTANDING RELATIONSHIPS
            
            tabItem(tabName = "Understanding",
                    fluidRow(
                        valueBox("Big Picture", "Projects By Segment",
                        width=12
                    ),
                        box(
                            title="Country",
                            solidHeader=TRUE,
                            width=6,
                            status="warning",
                            selectInput("country2ndtab", "Select Country:", choices= c(" "," "))
                        ),
                    box(
                        title="Category",
                        solidHeader = TRUE,
                        status="warning",
                        width=6,
                        selectInput("category2ndtab", "Select Category:", choices= c(" "," "))
                        
                    )
                    ),
                    fluidRow(
                        valueBox("On Success", "In Depth",
                                 width=12,
                        ),
                        tabBox(
                            id="tab1",
                            width=12,
                            height= "500px",
                            tabPanel("Projects by Country", plotlyOutput("projbycountryplot")),
                            tabPanel("Funded Breakdown", plotlyOutput("fundedplot"))
                        ),

                        tabBox(
                            id="tab2",
                            width=6,
                            height= "550px",
                            tabPanel("Boxplot", plotlyOutput("goalsboxplot")),
                            tabPanel("Successful Goals",plotlyOutput("successgoalhist")),
                            tabPanel("Unsuccessful Goals", plotlyOutput("unsuccessgoalhist"))
                        ),
                        tabBox(
                            side="right",
                            height="550px",
                            tabPanel("Unsuccessful Backers", plotlyOutput("unsuccessbackershist")),
                            tabPanel("Successful Backers", plotlyOutput("successbackershist")),
                            tabPanel("Boxplot", plotlyOutput("backersboxplot"))
                        )),
                  
                    fluidRow(
                        box(width=12,
                            status="info",
                            title= "Scope of Data",
                            solidHeader = TRUE,
                            valueBoxOutput(width=6, "avgpledged"),
                            valueBoxOutput(width=6, "avgbackers"),
                            valueBoxOutput(width=6, "maxpledged"),
                            valueBoxOutput(width=6, "maxbackers"),
                            valueBoxOutput(width=6, "maxpledgedInfo"),
                            valueBoxOutput(width=6, "maxbackersInfo")
                        )
                    )),
            
            
            ## THIRD TAB
            
            ## INFERENCE AND PREDICTION
            
            tabItem(tabName="Inference",
                    fluidRow(
                        valueBox("Predicting Success", "What are your chances? Utilising values from 1MM samples from a Bayesian Generalized Linear Mixed Model",
                                 width=12
                        ),

                        # infoBox(width=12,
                        #         title= "Log-Odds of Success",
                        #         "=.813-.0001974*Goal+.04059*Backers-0.017372*Days+Category+Country",
                        #         icon= icon("battle-net", lib = "font-awesome"),
                        #         color="maroon"
                        # ),

                        box(width=6,
                            status="warning",
                            title="Calculation",
                            solidHeader=TRUE,
                            selectInput("category", "Select Category:", 
                                        choices= c(" "," ")),
                            selectInput("location", "Select Country:", 
                                        choices= c(" "," ")),
                            sliderInput("goalslider", "Project Goal:", 1000, 100000, 5000),
                            sliderInput("backerslider", "Number of Backers:", 1, 1000, 50),
                            sliderInput("dayslider", "Duration in Days:", 1, 60, 30)

                        ),
                        tabBox(
                            title="Prediction",
                            height="200px",
                            tabPanel("Probability of Success", valueBoxOutput(width=12, "outProb")),
                            tabPanel("Odds of Success", valueBoxOutput(width=12, "outPred")),
                            tabPanel("Log-Odds Model", infoBoxOutput(width=12, "logoddsmodel")),
                            tabPanel("Model Priors", infoBoxOutput(width=12, "modelpriors"))
                        )
                        
                        
                    )
            )
        )
    )
)
        
    
  


