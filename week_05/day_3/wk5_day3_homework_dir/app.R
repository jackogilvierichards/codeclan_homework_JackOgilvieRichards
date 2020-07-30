library(shiny)
library(dplyr)
library(ggplot2)
library(CodeClanData)
library(shinythemes)


all_teams <- unique(olympics_overall_medals$team)
ui <- fluidPage(
    
    theme = shinytheme("flatly"),
    
    titlePanel("Olympic Medals"),

    navbarPage(
        
#    tabsetPanel(

#----tab 1 ----
                        
        tabPanel("Team Medals by Season",
            
            sidebarLayout(
                
                sidebarPanel(
                    
                    "Which season?",
                    radioButtons("season",
                                 "Summer or Winter Olympics?",
                                 choices = c("Summer", "Winter")
                    )
                ),
                
                sidebarPanel(
                    
                    "Which team?",
                    selectInput("team",
                                "Which Team?",
                                choices = all_teams
                    )
                ),
            
                mainPanel("Medal Plot",
                 plotOutput("medal_plot")
                )
            )
        ),

#---- tab 2 ----
        
        tabPanel("Five Country Medal Comparison",
                 
                 sidebarLayout(
                     
                     sidebarPanel(
                         
                         radioButtons("s",
                                      "Summer or Winter Olympics?",
                                      choices = c("Summer", "Winter")
                         ),

                        
                         radioButtons("m",
                                      "What medal type are you interested in?",
                                      choices = c("Bronze", "Silver", "Gold")
                         )
                         
                     ),
                    
                 
                 mainPanel(
                     
                     "Country Plot",
                 plotOutput("country_plot")
                 
                 )
                 
        )
        
        ),
        
#---- tab 3 ----
        
        tabPanel("Link to Site",
                 tags$a("More Information", href = "https://www.olympic.org/olympic-results/")
        )

#----

        
    )
    
)



server <- function(input, output) {
    
    output$medal_plot <- renderPlot({
        olympics_overall_medals %>%
            filter(team == input$team) %>%
            filter(season == input$season) %>%
            ggplot() +
            aes(x = medal, y = count, fill = medal) +
            geom_col() +
            scale_fill_manual(
                values = c(
                    "Gold" = "gold3",
                    "Silver" = "grey84",
                    "Bronze" = "orange3")
            )
    })
    
    output$country_plot <- renderPlot({
        olympics_overall_medals %>%
            filter(team %in% c("United States",
                               "Soviet Union",
                               "Germany",
                               "Italy",
                               "Great Britain")) %>%
            filter(medal == input$m) %>%
            filter(season == input$s) %>%
            ggplot() +
            aes(x = team, y = count, fill = input$m) +
            geom_col() 
        
        
        
        
    })
    

}
shinyApp(ui = ui, server = server)



