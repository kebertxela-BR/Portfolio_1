#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(dplyr)

democracy=readRDS("C:/Users/benru/Documents/2022-2023 school work/archive/Data502/Semester Project/Country-Year V-Dem Full and others/Country_Year_V-Dem_Full+others_R_v12/V-Dem-CY-Full+Others-v12.rds")

fert<-read_csv("https://raw.githubusercontent.com/kitadasmalley/FA2020_DataViz/main/data/gapminderFert.csv") 
fert=fert%>% select(Country,continent) %>% unique()

vdem=left_join(democracy,fert,by = c('country_name' = 'Country') ) %>% arrange(country_name)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Freedom Index"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("country",
                        h4("Select a country:"),
                        choices = unique(vdem$country_name))  
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
      
      thisCountry=input$country
        
      chosen=vdem %>% filter(country_name %in% (thisCountry), year>=1900)
      
      p<- vdem%>%filter(year>=1900, country_name!=thisCountry) %>% 
        ggplot(aes(y=v2x_libdem,x=v2x_freexp_altinf, color=continent, frame=year)) +
        labs(x="Freedom of Expression", y = "Liberal Freedom", 
             caption = "Vdem data", 
             color = 'country_name') + 
        geom_point(aes(text=country_name))+
        labs(color="Continent")+
        geom_point(data=chosen,aes(y=v2x_libdem,x=v2x_freexp_altinf, frame=year),color="black")
      
      ggplotly(p)

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
