#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)
library(readr)
library(shinydashboard)
library(dplyr)
library(rsconnect)
library(shinyWidgets)

# Define server logic required to draw a histogram
# server logic
shinyServer(function(input, output, session) {
    
    kic <- reactiveFileReader(intervalMillis=20000,
                              session=session,
                              filePath="data/ks18shiny.csv",
                              readFunc=read_csv) 
    ## FIRST TAB OBSERVE
    
    ## observe country     
    observeEvent(input$country, {
        kic0 <- kic() %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "JP", "HK", "SG"))
        
        updateSelectInput(session, "country", 
                          choices=c("All Countries", unique(kic0$country)))
    }, once=TRUE)
    
    ## THIRD TAB OBSERVE CATEGORY/LOCATION-COUNTRY
    
    ## observe location - country
    observeEvent(input$location, {
        kic0 <- kic() %>% filter(category %in% c("Art","Design","Fashion","Film","Food",
                                                 "Games","Music", "Publishing", "Tech", "Theater")) %>%
            filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "JP", "HK", "SG")) 
        
        updateSelectInput(session, "location", 
                          choices=c("All Countries", unique(kic0$country)))
    }, once=TRUE)
    
    ## observe category
    observeEvent(input$category, {
        kic0 <- kic() %>% filter(category %in% c("Art","Design","Fashion","Film","Food",
                                                 "Games","Music", "Publishing", "Tech", "Theater"))
        
        updateSelectInput(session, "category", 
                          choices=c("All Categories", unique(kic0$category)))
    }, once=TRUE)
    
    
    ## 2ND TAB OBSERVE EVENT
    
    ## observe country
    observeEvent(input$country2ndtab, {
        kic0 <- kic() %>% filter(category %in% c("Art","Design","Fashion","Film","Food",
                                                 "Games","Music", "Publishing", "Tech", "Theater")) %>%
            filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG")) 
        
        updateSelectInput(session, "country2ndtab", 
                          choices=c("All Countries", unique(kic0$country)))
    }, once=TRUE)
    
    ##observe category
    observeEvent(input$category2ndtab, {
        kic0 <- kic() %>% filter(category %in% c("Art","Design","Fashion","Film","Food",
                                                 "Games","Music", "Publishing", "Tech", "Theater"))
        
        updateSelectInput(session, "category2ndtab", 
                          choices=c("All Categories", unique(kic0$category)))
    }, once=TRUE)
    
    
    ## FIRST SIDE TAB 
    
    ## TAB 1 dates UI
    output$datesUI <- renderUI({
        mydates <- req(kic()) %>%
            select(deadline) %>%
            summarize(range=range(deadline))
        begins <- mydates$range[1]
        ends <- mydates$range[2]
        dateRangeInput("dates", "Date Range:",
                       start=begins,
                       end=ends,
                       min=begins,
                       max=ends)
    })
    
    
    ## TAB 1 plot output
    # rendering plot
    output$projplot <- renderPlotly({
        #%>% group_by(country) %>% count(country, sort=TRUE)  
        if(input$country=="All Countries"){
            kic1 <- kic() %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "JP", "HK", "SG")) %>% 
                mutate(countr = fct_lump(country, n=8)) %>% 
                count(countr)
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country) %>%
                mutate(countr = fct_lump(country, n=8)) %>% 
                count(countr)
        }
        
        countrychart <- plot_ly(data=kic1, 
                                type="bar", 
                                x=~reorder(countr,n),
                                y=~n,
                                color=~countr) %>% 
            layout(title= "Project Totals By Country",
                   xaxis= list(title="Country(s)"),
                   yaxis= list(title="Counts"))
        countrychart
        return(countrychart)
        
    })

    ## TAB 1 INFO BOXES
    output$outpledged <- renderValueBox({
        if(input$country == "All Countries"){
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]), deadline <= req(input$dates[2]))
        }
        else{
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]), deadline <= req(input$dates[2])) %>%
                filter(country== input$country)
        }
        total1 <- sum(kic1$usd_pledged_real) %>% prettyNum(big.mark = ",")
        valueBox(
            value=total1,
            width=12,
            color="maroon",
            subtitle="Total Pledged",
            icon= icon("comments-dollar", lib = "font-awesome")
            
        )
    })
    
    output$outsuccess <- renderValueBox({
        
        if(input$country == "All Countries"){
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]), deadline <= req(input$dates[2]))
        }
        else{
            kic1 <- kic()%>% filter(deadline >= req(input$dates[1]), deadline <= req(input$dates[2])) %>% 
                filter(country== input$country)
        }
        total1 <- sum(kic1$state=="successful") 
        valueBox(
            value=total1 %>% prettyNum(big.mark = ","),
            subtitle="Successful Projects",
            icon= icon("chart-line", lib = "font-awesome")
        )
    })
    output$outbackers <- renderValueBox({
        
        if(input$country == "All Countries"){
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]), deadline <= req(input$dates[2]))
        }
        else{
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]), deadline <= req(input$dates[2])) %>% 
                filter(country== input$country)
        }
        total1 <- sum(kic1$backers) %>% prettyNum(big.mark = ",")
        valueBox(
            value=total1,
            subtitle="Backers",
            color="teal",
            icon= icon("users", lib = "font-awesome")
        )
    })
    output$outgoal <- renderValueBox({
        
        if(input$country == "All Countries"){
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]), deadline <= req(input$dates[2]))
        }
        else{
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]), deadline <= req(input$dates[2])) %>%
                filter(country== input$country)
        }
        total1 <- sum(kic1$usd_goal_real) %>% prettyNum(big.mark = ",")
        valueBox(
            value=total1,
            subtitle="Project Goals",
            color="orange",
            icon= icon("bullseye", lib = "font-awesome")
        )
    })
    output$outpropsuccess <- renderValueBox({
        if(input$country == "All Countries"){
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]),deadline <= req(input$dates[2]))
        }
        else{
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]),deadline <= req(input$dates[2])) %>%
                filter(country== input$country)
        }
        len <- nrow(kic1)
        total1 <- sum(kic1$state == "successful")
        total2 <- (total1/len)*100
        
        valueBox(
            value= total2 %>% prettyNum(big.mark = ","),
            color="red",
            subtitle="Proportion of Success",
            icon= icon("percent", lib = "font-awesome")
        )
    })
    
    output$outfunded <- renderValueBox({
        if(input$country == "All Countries"){
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]),deadline <= req(input$dates[2]))
        }
        else{
            kic1 <- kic() %>% filter(deadline >= req(input$dates[1]),deadline <= req(input$dates[2])) %>%
                filter(country== input$country)
        }
        len <- nrow(kic1)
        
        kic2 <- kic1 %>% filter(state=="successful")
        total1 <- sum(kic2$usd_pledged_real)
        
        valueBox(
            value= total1 %>% prettyNum(big.mark = ","),
            color="light-blue",
            subtitle="Total Funded",
            icon= icon("money-bill-wave", lib = "font-awesome")
        )
    })
    
    ## SECOND SIDE TAB
    
    ## TAB 2 PROJECT COUNTRY/CATEGORY PLOT
    output$projbycountryplot <- renderPlotly({
        #%>% group_by(country) %>% count(country, sort=TRUE)  
        if(input$country2ndtab=="All Countries"){
            kic1 <- kic()
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) 
        }
        kic2 <- kic1 %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG"))  %>% 
            group_by(country) %>% count(category, country) %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater"))  
        projchart <- plot_ly(data=kic2, 
                               type="bar", 
                               x=~n,
                               y=~country,
                               orientation='h',
                               color=~reorder(category, n),
                               text = paste0("Country: ", kic2$country, "<br>",
                                             "Project Total: ", kic2$n%>% prettyNum(big.mark = ",") , "<br>",
                                             "Category: ", kic2$category, "<br>"),
                               hoverinfo="text") %>% 
            layout(title= "Number of Projects By Country",
                   xaxis= list(title="Counts"),
                   yaxis= list(title="Country(s)"),
                   barmode= 'stack')
        return(projchart)
    })
    
    ## TAB 2 FUNDED PLOT
    output$fundedplot <- renderPlotly({
        #%>% group_by(country) %>% count(country, sort=TRUE)  
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        kic2 <- kic1%>% filter(state == "successful") %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG"))  %>% 
            group_by(country) %>% count(category, country) %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater")) %>%
            mutate(prop = n/sum(n))
        fundedchart <- plot_ly(data=kic2, 
                               type="bar", 
                               x=~n,
                               y=~country,
                               orientation='h',
                               color=~reorder(category, n),
                               text = paste0("Country: ", kic2$country, "<br>",
                                             "Funded in Country: ",round(kic2$prop,3)*100,"%", "<br>",
                                             "Category: ", kic2$category, "<br>"),
                               hoverinfo="text") %>% 
            layout(title= "Funded Projects By Country",
                   xaxis= list(title="$"),
                   yaxis= list(title="Country(s)"),
                   barmode='stack')
        return(fundedchart)
        
    })
    

    ## TAB 2 HISTOGRAMS
    
    output$successgoalhist <- renderPlotly({
        #%>% group_by(country) %>% count(country, sort=TRUE)  
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        kic2 <- kic1 %>% filter(state == "successful") %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG"))  %>% 
            group_by(country) %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater"))  
        sgoalhist <- plot_ly(data=kic2,
                             x=~kic2$usd_goal_real,
                             type="histogram"
        ) %>% 
            layout(title= "Successful Goals",
                   yaxis= list(title="Counts"),
                   xaxis= list(title="Goal Amounts ($)"),
                   barmode= 'stack')
        return(sgoalhist) %>% config(displayModeBar = F)
        
    })
    output$unsuccessgoalhist <- renderPlotly({
        #%>% group_by(country) %>% count(country, sort=TRUE)  
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        kic2 <- kic1%>% filter(state == "failed") %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG"))  %>% 
            group_by(country)  %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater"))  
        usgoalhist <- plot_ly(data=kic2,
                              x=~kic2$usd_goal_real,
                              type="histogram"
        ) %>% 
            layout(title= "Unsuccessful Goals",
                   yaxis= list(title="Counts"), #, range=c(0,10000)),
                   xaxis= list(title="Goal Amounts ($)"), #, range=c(0,2000000)),
                   barmode= 'stack')
        return(usgoalhist) %>% config(displayModeBar = F)
        
    })
    
    output$unsuccessbackershist <- renderPlotly({
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        kic2 <- kic1%>% filter(state == "failed") %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG"))  %>% 
            group_by(country)  %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater"))  
        unsuccessbackershist <- plot_ly(data=kic2,
                                        x=~kic2$backers,
                                        type="histogram"
        ) %>% 
            layout(title= "Unsuccessful Backers",
                   yaxis= list(title="Counts"),
                   xaxis= list(title="Backers"), # range=c(0,100)),
                   barmode= 'stack') %>% config(displayModeBar = F)
        return(unsuccessbackershist)
    })
    output$successbackershist <- renderPlotly({
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        kic2 <- kic1%>% filter(state == "successful") %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG"))  %>% 
            group_by(country)  %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater"))  
        successbackershist <- plot_ly(data=kic2,
                                        x=~kic2$backers,
                                        type="histogram"
        ) %>% 
            layout(title= "Successful Backers",
                   yaxis= list(title="Counts"),
                   xaxis= list(title="Backers"), #range=c(0,1000)),
                   barmode= 'stack') %>% config(displayModeBar = F)
        return(successbackershist)
        
        
    })
    
    ## TAB 2 BOXPLOTS
    output$goalsboxplot <- renderPlotly({
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        kic2 <- kic1%>% filter(state == "successful") %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG"))  %>% 
            group_by(country)  %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater")) 
        
        kic3 <- kic1%>% filter(state == "failed") %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG"))  %>% 
            group_by(country)  %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater")) 
        
        goalsboxplot <- plot_ly(data=kic3,
                                  x=~kic3$usd_goal_real,
                                  type="box",
                                  name="Unsuccessful Projects"
        ) %>% 
            layout(title= "Distribution of Project Goals",
                   xaxis= list(title="Goals"), #range=c(0,1000000)),
                   barmode= 'stack') %>% add_trace(x=~kic2$usd_goal_real, name="Successful Projects")%>% 
            config(displayModeBar = F)
        
        return(goalsboxplot)
        
    })
    
    output$backersboxplot <- renderPlotly({  
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        kic2 <- kic1%>% filter(state == "successful") %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ", "HK", "SG"))  %>% 
            group_by(country)  %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater")) 
        
        kic3 <- kic1%>% filter(state == "failed") %>% filter(country %in% c("GB","US","CA","AU","IT","DE","FR", "NL", "NZ","HK", "SG"))  %>% 
            group_by(country)  %>% 
            filter(category %in% c("Art","Design","Fashion","Film","Food", "Games","Music", "Publishing", "Tech", "Theater")) 
        
        backersboxplot <- plot_ly(data=kic3,
                x=~kic3$backers,
                type="box",
                name="Unsuccessful Projects"
        ) %>% 
            layout(title= "Distribution of Project Backers",
                   xaxis= list(title="Backers"), #range=c(0,7000)),
                   barmode= 'stack') %>% add_trace(x=~kic2$backers, name="Successful Projects") %>% 
            config(displayModeBar = F)
        return(backersboxplot)
        
    })
    

    ## TAB 2 INFO BOXES
    output$maxpledged <- renderInfoBox({
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        maxpledged <- max(kic1$usd_pledged_real) 
        extraction <- kic1[kic1$usd_pledged_real==maxpledged,]
        infoBox(
            "Highest pledged",maxpledged %>% prettyNum(big.mark = ","),color="maroon", fill=TRUE,
            icon= icon("money-bill-wave", lib = "font-awesome")
        )

    })
    
    output$avgpledged <- renderInfoBox({
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }

        infoBox("Average pledged per project", 
                mean(kic1$usd_pledged_real)%>% prettyNum(big.mark = ","), 
                color="teal", fill=TRUE,
                icon= icon("comments-dollar", lib = "font-awesome")
        )
        
    })
    
    output$maxpledgedInfo <- renderInfoBox({
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        maxpledged <- max(kic1$usd_pledged_real) 
        extraction <- kic1[kic1$usd_pledged_real==maxpledged,]
        
        infoBox(
            "Project Info", HTML(paste0("Name: ",extraction$name,br(), "Location: ", extraction$country)),
            color = "orange",
            icon= icon("medal", lib = "font-awesome")
        )
    })
    
    output$maxbackers <- renderInfoBox({
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        maxbackers <- max(kic1$backers) 
        extraction <- kic1[kic1$backers==maxbackers,]
        
        infoBox(
            "Highest number of backers", maxbackers %>% prettyNum(big.mark = ","), color="maroon", fill=TRUE,
            icon= icon("users", lib = "font-awesome")
        )
        
    })
    output$avgbackers <- renderInfoBox({
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }

        infoBox("Average backers per project", 
                mean(kic1$backers)%>% prettyNum(big.mark = ","), 
                color="teal", fill=TRUE,
                icon= icon("user-friends", lib = "font-awesome")
        )
        
    })
    
    output$maxbackersInfo <- renderInfoBox({
        if(input$country2ndtab=="All Countries" && input$category2ndtab=="All Categories"){
            kic1 <- kic()
        }
        else if(input$country2ndtab=="All Countries"){
            kic1 <- kic()%>% filter(category %in% input$category2ndtab) 
        }
        else if(input$category2ndtab=="All Categories"){
            kic1 <- kic()%>% filter(country %in% input$country2ndtab) 
        }
        else{
            kic1 <- kic() %>% filter(country %in% input$country2ndtab) %>% filter(category %in% input$category2ndtab) 
        }
        maxbackers <- max(kic1$backers) 
        extraction <- kic1[kic1$backers==maxbackers,]
        infoBox(
            "Project Info", HTML(paste0("Name: ",extraction$name, br(),"Location: ", extraction$country)),
            color = "orange",
            icon= icon("award", lib = "font-awesome")
        )
    })
    
    ## THIRD SIDE TAB
    output$outPred <- renderInfoBox({
        catval <- 0
        locval <- 0
        #category coefficients
        if(input$category == "Art"){catval <- .21014}
        if(input$category == "Design"){catval <- -.23082}
        if(input$category == "Fashion"){catval <-  -.3432}
        if(input$category == "Film"){catval <- .4281}
        if(input$category == "Food"){catval <- -.06853}
        if(input$category == "Games"){catval <- -.9996}
        if(input$category == "Music"){catval <- .3507}
        if(input$category == "Publishing"){catval <- -.2976}
        if(input$category == "Tech"){catval <- -.1245}
        if(input$category == "Theater"){catval <- 1.0517}
        
        #country coefficients
        if(input$location == "GB"){locval <- .01174}
        if(input$location == "US"){locval <- .063029}
        if(input$location == "CA"){locval <- -.0960515}
        if(input$location == "AU"){locval <- -.04457}
        if(input$location == "IT"){locval <- -.02805}
        if(input$location == "DE"){locval <- .08817}
        if(input$location == "FR"){locval <- .05614}
        if(input$location == "NL"){locval <- .0016974}
        
        logodds <- .813+(-.0001974*input$goalslider+.04059*input$backerslider+-0.017372*input$dayslider+catval+locval)  
        odds <- exp(logodds)/(1+logodds)
        if(odds <= 0) {odds <- 0}
        
        # switch for category and country?? or do i just do 22 if loops?
        infoBox(
            "odds", round(odds,3), 
            color="fuchsia", 
            fill=TRUE,
            icon= icon("dice-three", lib = "font-awesome")
        )
        
    })
    output$outProb <- renderInfoBox({
        catval <- 0
        locval <- 0
        #category coefficients
        if(input$category == "Art"){catval <- .21014}
        if(input$category == "Design"){catval <- -.23082}
        if(input$category == "Fashion"){catval <-  -.3432}
        if(input$category == "Film"){catval <- .4281}
        if(input$category == "Food"){catval <- -.06853}
        if(input$category == "Games"){catval <- -.9996}
        if(input$category == "Music"){catval <- .3507}
        if(input$category == "Publishing"){catval <- -.2976}
        if(input$category == "Tech"){catval <- -.1245}
        if(input$category == "Theater"){catval <- 1.0517}
        
        #country coefficients
        if(input$location == "GB"){locval <- .01174}
        if(input$location == "US"){locval <- .063029}
        if(input$location == "CA"){locval <- -.0960515}
        if(input$location == "AU"){locval <- -.04457}
        if(input$location == "IT"){locval <- -.02805}
        if(input$location == "DE"){locval <- .08817}
        if(input$location == "FR"){locval <- .05614}
        if(input$location == "NL"){locval <- .0016974}
        if(input$location == "JP"){locval <- .001517} ##rrr
        if(input$location == "HK"){locval <- .03614}## rrr
        if(input$location == "SG"){locval <- .036974} ## rrr
        
        logodds <- .813+(-.0001974*input$goalslider+.04059*input$backerslider+-0.017372*input$dayslider+catval+locval)  
        odds <- exp(logodds)/(1+logodds)
        predprob <- odds/(1+odds) 
        if(predprob <= 0) {predprob <- 0}
        # switch for category and country??
        if(predprob >=1) {predprob <-1}
        infoBox(
            "probability", round(predprob,3), 
            color="maroon", 
            fill=TRUE,
            icon= icon("dice", lib = "font-awesome")
        )
        
    })
    output$logoddsmodel <- renderInfoBox({
        infoBox(
            "Category/Country Random Effects", "=.813-.0001974*Goal+.04059*Backers-0.017372*Days+Category+Country",
            color="orange",
            icon=icon("equals", lib = "font-awesome"),
            fill=TRUE
        )
    })
    
})   
