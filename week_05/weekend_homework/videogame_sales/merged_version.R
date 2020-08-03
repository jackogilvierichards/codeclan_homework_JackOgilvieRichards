library(shiny)
library(shinythemes)
library(tidyverse) 
#less typing but likely will only use packages `ggplot2` and `dplyr`

library(CodeClanData)

#glimpse(game_sales)
#unique(game_sales$publisher)
#unique(game_sales$platform)

#View(game_sales)


# I had some ideas which hopefully I will eventually impliment into the app on 
# different tabs really going into some good detail on the topic - for the commit
# this may be read for I am going to prepare a button-updated graph depicting the 
# comparison of user and critic scores



vg_genre <- unique(game_sales$genre)
vg_platform <- unique(game_sales$platform)

ui <- fluidPage(
  
  fluidRow(
    
    column(6,
           
           selectInput("game_genre",
                        "What kind of game are you interested in?",
                        choices = vg_genre)
           
           ),
    
    
    column(6,
           
           selectInput("game_platform",
                        "What platform do you play games on?",
                        choices = vg_platform)
           
           )
    
  ), 
  
  
  actionButton("update", "Compare titles"),
  
  plotOutput("game_comparison")
  
)





server <- function(input, output){
  
  
  games_filtered <- eventReactive(input$update, {
    
    game_sales %>%
      select(user_score, critic_score, genre, platform, name) %>%
      filter(genre == input$game_genre) %>%
      filter(platform == input$game_platform)
    
    
  })
  
 
  
  output$game_comparison <- renderPlot({
    
    ggplot(games_filtered()) +
      aes(x = user_score, y = critic_score, alpha = abs((critic_score - 10*user_score)/100), text = name) +
      geom_point() +
      labs(
        title = "Critic/User Ratings",
        subtitle = "For genres on a platform",
        alpha = "Disparity of reviews",
        x = "User Score",
        y = "Critic Score") 
    #+
      #geom_text(aes(label = name, alpha = 1))
    
    
  })
  
# The choice of this plot was to illustrate both the range of scoring
# between titles and equally the range of scoring between the user base
# and critic reviews, of which the latter often have influencing factors. 
# The alpha effect I included was to highlight a qualitative idea of 
# certain genres having a larger critic or user disparity - something I 
# I would create tabs and perhaps additional graphs for if I had time.   
  
  
  
    
}



shinyApp(ui, server)



