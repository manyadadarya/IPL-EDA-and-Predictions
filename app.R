library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(tuneR)
library(caret)
library(randomForest)


setwd(getwd())
matches <- as.data.frame(read.csv('matches.csv'))
deliveries <- as.data.frame(read.csv('deliveries.csv'))

df <- deliveries %>% inner_join(matches, by = c('match_id' = 'id'))

ui <- dashboardPage(
  dashboardHeader(title = 'IPL Analysis'
  ),
  dashboardSidebar( tags$style(HTML('.main-sidebar{width: 220px;}')),
                    sidebarMenu(
                      menuItem('Overview', tabName = 'about', icon = icon('pen')),
                      menuItem('Top Performances', tabName = 'top_performance', icon = icon('chart-bar')),
                      menuItem('Player Profile', tabName = 'player', icon = icon('id-card')),
                      menuItem('Player Performance', tabName = 'performance', icon = icon('tachometer')),
                      menuItem('Twitter Reactions', tabName = 'WordCloud', icon = icon('th')),
                      menuItem('Predictions', tabName = 'predictions', icon = icon('trophy'))
                    )
  ),
  dashboardBody(
    tabItems(
      ##Predictions
      
      
      # Overview -------------------------------------------------------------------
      tabItem(tabName = 'about',
              h2('Indian Premier League', align = 'center'),
              tags$p('The Indian Premier League (IPL) is a professional Twenty20 cricket league in India contested during
              March or April and May of every year by eight teams representing eight different cities in India.
              The league was founded by the Board of Control for Cricket in India (BCCI) in 2008. 
              The IPL has an exclusive window in ICC Future Tours Programme. The IPL is the most-attended cricket 
              league in the world and in 2014 ranked sixth by average attendance among all sports leagues.', 
                     style = 'font-size: 120%;margin-left:2.5em;'),
              h5('Source: Wikipedia', align = 'right'),  
              
              fluidPage(
                tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/QkSbdMdQe08?controls=0", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
              ),
              h3('Teams of the League', align = 'center'),
              fluidRow(
                shinydashboard::box(width = 12, background = 'black',
                                    valueBox(tags$p('Chennai Super Kings (CSK)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'CSK.jpeg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 3, color = 'black'),
                                    valueBox(tags$p('Mumbai Indians (MI)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'MI.jpeg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 3, color = 'black'),
                                    valueBox(tags$p('Sunrises Hyderabad (SRH)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'SRH.jpeg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 3, color = 'black'),
                                    valueBox(tags$p('Delhi Capitals (DC)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'DC.jpeg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 3, color = 'black'),
                                    valueBox(tags$p('Kolkata Knight Riders (KKR)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'KKR.jpeg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 3, color = 'black'),
                                    valueBox(tags$p('Kings XI  Punjab (KXIP)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'KXIP.jpeg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 3, color = 'black'),
                                    valueBox(tags$p('Royal Challengers Bangalore (RCB)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'RCB.jpeg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 3, color = 'black'),
                                    valueBox(tags$p('Rajasthan Royals (RR)', style = 'font-size: 30%;text-align: center;'), 
                                             div(img(src = 'RR.jpeg', height = '120', width = '160'),style='text-align: center;'), 
                                             icon = NULL, width = 3, color = 'black')
                ),
                
              ),
              fluidRow(
                infoBoxOutput('matches'),tags$style('#matches {width:250px;}'),
                infoBoxOutput('teams'),tags$style('#teams {width:250px;}'),
                infoBoxOutput('runs'),tags$style('#runs {width:250px;}'),
                infoBoxOutput('wickets'),tags$style('#wickets {width:250px;}'),
                infoBoxOutput('venues'),tags$style('#venues {width:250px;}'),
                infoBoxOutput('fours'),tags$style('#fours {width:250px;}'),
                infoBoxOutput('sixes'),tags$style('#sixes {width:250px;}'),
                infoBoxOutput('seasons'),tags$style('#seasons {width:250px;}'),
                infoBoxOutput('mom'),tags$style('#mom {width:250px;}')
              )
              
      ),
      
      # Season Analysis ---------------------------------------------------------
      
      tabItem(tabName = 'top_performance',
              fluidPage(
                fluidRow(
                  shinydashboard::box(title = 'Batting Analysis',width = 8,solidHeader = T, background = 'black',
                                      tabBox(width = 12,
                                             title = NULL,
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = 'tabset1', height = '260px',
                                             tabPanel(tags$p('Most Runs', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_batsman', height = 200)),
                                             tabPanel(tags$p('Most Hundreds', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('most_100s', height = 200)),
                                             tabPanel(tags$p('Most Fifties', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('most_50s', height = 200)),
                                             tabPanel(tags$p('Most Sixes', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_6s', height = 200)),
                                             tabPanel(tags$p('Most Fours', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_4s', height = 200))
                                      )
                  ),
                  shinydashboard::box(title = 'Bowling Analysis',width = 8,solidHeader = T, background = 'black',
                                      tabBox(width = 12,
                                             title = NULL,
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = 'tabset2', height = '270px',
                                             tabPanel(tags$p('Most Wickets', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('top_10_bowlers', height = 210)),
                                             tabPanel(tags$p('Most Maidens', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('maiden', height = 210)),
                                             tabPanel(tags$p('Most Dot Balls', style = 'color:black;font-weight:bold;'), 
                                                      plotOutput('dot_balls', height = 210)),
                                             tabPanel(tags$p('Most 4 Wickets', style = 'color:black;font-weight:bold;'),
                                                      plotOutput('wickets_4', height = 210))
                                      )
                  )
                )
              )
      ),
      
      # Player profile ----------------------------------------------------------
      tabItem(tabName = 'player',
              fluidPage(
                selectInput('player', 'Select Player:',
                            unique(df$batsman)),
                fluidRow(
                  shinydashboard::box(
                    title = 'Batting', 
                    width = 4, solidHeader = TRUE, background = 'black',
                    uiOutput('batting_ui')
                    
                  ),
                  shinydashboard::box(
                    title = 'Bowling', width = 4, solidHeader = TRUE, background = 'black',
                    uiOutput('bowling_ui')
                    
                  )
                )
              )
      ),
      # Player performance ------------------------------------------------------
      
      tabItem(tabName = 'performance',
              fluidPage(
                selectInput("player1", "Select Player:",
                            unique(df$batsman)),
                fluidRow(
                  shinydashboard::box(title = 'Performance By Opponents',width = 6,solidHeader = T, background = 'black',
                                      tabBox(width = 12,
                                             title = NULL,
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = 'tabset3', height = '575px',
                                             tabPanel(tags$p('Batting', style = 'color:black;font-weight:bold;'), span(
                                               uiOutput('batting_opponents'), style ='color:red')),
                                             tabPanel(tags$p('Bowling', style = 'color:black;font-weight:bold;'), span(
                                               uiOutput('bowling_opponents'), style ='color:red'))
                                      )
                  ),
                  shinydashboard::box(title = 'Performance By Venue',width = 6,solidHeader = T, background = 'black',
                                      tabBox(width = 12,
                                             title = NULL,
                                             # The id lets us use input$tabset1 on the server to find the current tab
                                             id = 'tabset3', height = '575',
                                             tabPanel(tags$p('Batting', style = 'color:black;font-weight:bold;'), span(
                                               uiOutput('batting_venue'), style ='color:red')),
                                             tabPanel(tags$p('Bowling', style = 'color:black;font-weight:bold;'),  span(
                                               uiOutput('bowling_venue'), style ='color:red'))
                                      )
                  )
                )
              )
      ),
      #Word Cloud
      tabItem(tabName = 'WordCloud',
              h1('Twitter Reactions', align = 'center'),
              fluidRow(
                img(src = "wordcloud.jpeg", height = 720, width = 720), align = 'center'
              )
      ),
      ##Predictions
      tabItem(tabName = 'predictions',
              fluidPage(
                selectInput("venue", "Venue:",
                            unique(matches$venue)),
                selectInput("Team1", "Team 1:",
                            unique(matches$team1)),
                selectInput("Team2", "Team 2:",
                            unique(matches$team2)),
                selectInput("toss_winner", "Toss Winner:",
                            unique(matches$toss_winner)),
                selectInput("toss_decision", "Toss Decision:",
                            unique(matches$toss_decision)),
                
                fluidRow(
                  shinydashboard::box(title = 'Predicted Match Winner',solidHeader = T,
                                      span(
                                        uiOutput('Winner_pred'), style ='color:red'
                                      )
                                      ),
                                      
                  shinydashboard::box(title = 'Probability of winning',solidHeader = T,
                                      span(
                                        uiOutput('prob_result'), style ='color:red'
                                      )
                                    
                )
              
              )
      ))
      
      
    )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output, session) { 
  
  # Overview Outputs --------------------------------------------------------
  
  output$matches <- renderInfoBox({
    infoBox('# Matches', df %>% summarise(matches = n_distinct(match_id)),
            icon = icon('handshake'), color = 'olive', fill = T, width = 1)
  })
  output$runs <- renderInfoBox({
    infoBox('# Runs', df %>% summarise(runs = sum(total_runs)), 
            icon = icon('walking'), color = 'olive', fill = T, width = 1)
  })
  output$teams <- renderInfoBox({
    infoBox('# Teams', df %>% summarise(teams = n_distinct(batting_team)), 
            icon = icon('users'), color = 'olive', fill = T, width = 1)
  })
  output$wickets <- renderInfoBox({
    infoBox('# Wickets', df %>% filter(dismissal_kind %in% 
                                         c('bowled', 'caught', 'caught and bowled', 'lbw', 'hit wicket', 'stumped'))%>%  summarise(wickets = n()), 
            icon = icon('hand-pointer'), color = 'olive', fill = T, width = 1)
  })
  output$venues <- renderInfoBox({
    infoBox('# Venues', df %>% summarise(matches = n_distinct(venue)),
            icon = icon('handshake'), color = 'olive', fill = T, width = 1)
  })
  output$fours <- renderInfoBox({
    infoBox('# Fours', filter(df, batsman_runs == 4) %>% summarise(fours = n()), 
            icon = icon('dice-four'), color = 'olive', fill = T, width = 1)
  })
  output$sixes <- renderInfoBox({
    infoBox('# Sixes', filter(df, batsman_runs == 6) %>% summarise(fours = n()), 
            icon = icon('dice-six'), color = 'olive', fill = T, width = 1)
  })
  output$mom <- renderInfoBox({
    infoBox('Most MOM', tags$p((df %>% group_by(player_of_match) %>% summarise(num = n_distinct(match_id)) %>% 
                                  arrange(desc(num)) %>% head(1)), style = 'font-size: 90%;'),
            icon = icon('user-plus'), color = 'olive', fill = T, width = 1)
  })
  
  # Season Outputs --------------------------------------------------------
  
  output$top_10_batsman <- renderPlot({
    filter(df %>% group_by(batsman) %>% summarise(runs = sum(batsman_runs))) %>% arrange(desc(runs)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-runs), y = runs)) + geom_bar(stat="identity", width=0.6, fill="darkolivegreen4") +
      geom_text(aes(label = as.numeric(runs), vjust = 2)) +
      labs(x = 'Batsman', y = 'Total Runs Scored') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$most_100s <- renderPlot({
    filter(df %>% group_by(batsman, match_id) %>% summarise(runs = sum(batsman_runs)),
           runs >= 100) %>% group_by(batsman) %>% summarise(hundreds = n()) %>% 
      arrange(desc(hundreds)) %>% 
      ggplot(aes(x = reorder(batsman,-hundreds), y = hundreds)) + geom_bar(stat="identity", width=0.6, fill="darkolivegreen4") +
      geom_text(aes(label = as.numeric(hundreds), vjust = 2)) +
      labs(x = 'Batsman', y = 'No. of 50s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$most_50s <- renderPlot({
    filter(df %>% group_by(batsman, match_id) %>% summarise(runs = sum(batsman_runs)),
           runs >= 50) %>% group_by(batsman) %>% summarise(fifties = n()) %>% 
      arrange(desc(fifties)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-fifties), y = fifties)) + geom_bar(stat="identity", width=0.6, fill="darkolivegreen4") +
      geom_text(aes(label = as.numeric(fifties), vjust = 2)) +
      labs(x = 'Batsman', y = 'No. of 50s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$top_10_6s <- renderPlot({
    filter(as.data.frame(filter(df, batsman_runs == 6) %>% group_by(batsman) %>% summarise(sixes = n()))) %>% arrange(desc(sixes)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-sixes), y = sixes)) + geom_bar(stat="identity", width=0.6, fill="darkolivegreen4") +
      geom_text(aes(label = as.numeric(sixes), vjust = 2)) +
      labs(x = 'Batsman', y = 'No. of 6s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$top_10_4s <- renderPlot({
    filter(as.data.frame(filter(df, batsman_runs == 4) %>% group_by(batsman) %>% summarise(fours = n()))) %>% arrange(desc(fours)) %>% head(10) %>%
      ggplot(aes(x = reorder(batsman,-fours), y = fours)) + geom_bar(stat="identity", width=0.6, fill="darkolivegreen4") +
      geom_text(aes(label = as.numeric(fours), vjust = 2)) +
      labs(x = 'Batsman', y = 'No. of 4s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$top_10_bowlers <- renderPlot({
    filter(df %>% filter(dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 'lbw', 'hit wicket', 'stumped')) %>%
             group_by(bowler) %>% summarise(wickets = n())) %>% arrange(desc(wickets)) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler, -wickets), y = wickets)) + geom_bar(stat = 'identity', width = 0.6 , fill = 'darkolivegreen4') +
      geom_text(aes(label = as.numeric(wickets), vjust = 2)) +
      labs(x = 'Bowler', y = 'Total Wickets Taken') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$maiden <- renderPlot({
    data.frame(filter(filter(df)%>% group_by(match_id,inning, over,bowler) %>% 
                        summarise(runs = sum(batsman_runs)-sum(bye_runs)), runs ==0) %>% group_by(bowler) %>% summarise(maiden =n())) %>%
      inner_join( data.frame(filter(df)%>% group_by(match_id,inning, over,ball,bowler) %>% 
                               summarise(runs_given = sum(batsman_runs)-sum(bye_runs)-sum(legbye_runs)) %>% group_by(bowler) %>% 
                               summarise(runs_given= sum(runs_given), overs = n_distinct(match_id,over))) %>% mutate(econ = round(runs_given/overs,2)),
                  by = "bowler") %>% arrange(desc(maiden), econ) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler, -maiden), y = maiden)) + geom_bar(stat = 'identity', width = 0.6 , fill = 'darkolivegreen4') +
      geom_text(aes(label = as.numeric(maiden), vjust = 2)) +
      labs(x = 'Bowler', y = '# of times 4 Wickets') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$dot_balls <- renderPlot({
    filter(df, total_runs == 0)%>% group_by(bowler) %>% summarise(dot = n())  %>% 
      arrange(desc(dot)) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler, -dot), y = dot)) + 
      geom_bar(stat = 'identity', width = 0.6 , fill = 'darkolivegreen4') +
      geom_text(aes(label = as.numeric(dot), vjust = 2)) +
      labs(x = 'Bowler', y = '# of dot balls') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  output$wickets_4 <- renderPlot({
    filter(df %>% 
             filter(dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                          'lbw','hit wicket', 'stumped')) %>% group_by(bowler, match_id) %>% summarise(wickets = n()), 
           wickets == 4) %>% group_by(bowler) %>% summarise(Four_wickets = n()) %>% 
      ggplot(aes(x = reorder(bowler, -Four_wickets), y = Four_wickets)) + 
      geom_bar(stat = 'identity', width = 0.6 , fill = 'darkolivegreen4') +
      geom_text(aes(label = as.numeric(Four_wickets), vjust = 2)) +
      labs(x = 'Bowler', y = '# of times 4 Wickets') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  })
  
  # Player Profile Output ---------------------------------------------------
  
  output$teamsplayedui <- renderUI({
    span(style = 'color:black;font-weight:bold;', DT::DTOutput('teamsplayed'))
  })
  
  output$teamsplayed <- DT::renderDataTable({
    filter(df,batsman == input$player) %>% distinct(batting_team)
  }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), colnames = NULL,  rownames = NULL
  )
  
  output$batting_ui <- renderUI({
    if(nrow(filter(df,batsman == input$player)) == 0) 
      return('No data to show')
    span(style = 'color:black;font-weight:bold;', DT::DTOutput('batting_table'))
  })
  
  output$batting_table <- DT::renderDataTable(
    {
      t(distinct(data.frame(if(nrow(filter(df, batsman == input$player) %>% summarise(Innings = n_distinct(match_id,inning))) == 0) {0} 
                            else {filter(df, batsman == input$player) %>% summarise(Innings = n_distinct(match_id,inning))},
                            if(nrow(filter(df, batsman == input$player) %>% summarise(Runs = sum(batsman_runs))) == 0) {0} 
                            else {filter(df, batsman == input$player) %>% summarise(Runs = sum(batsman_runs))},
                            if(nrow(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                    summarise(runs = sum(batsman_runs)) %>% filter(runs == max(runs)) %>% select(runs)) == 0) {0} 
                            else {filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                summarise(runs = sum(batsman_runs)) %>% filter(runs == max(runs)) %>% select(runs)},
                            if(nrow(filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs >= 50 & runs <100) %>% summarise(fifties = n())) == 0) {0} 
                            else {filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs >= 50 & runs <100) %>%  summarise(fifties = n())},
                            if(nrow(filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs >= 100) %>%  summarise(Hundreds = n())) == 0) {0} 
                            else {filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs >= 100) %>%  summarise(Hundreds = n())},
                            if(nrow(filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs == 0) %>%  summarise(Ducks = n())) == 0) {0} 
                            else {filter(filter(df, batsman == input$player) %>% group_by(match_id)  %>%  
                                           summarise(runs = sum(batsman_runs)), runs == 0) %>%  summarise(Ducks = n())},
                            if(nrow(filter(df, batsman == input$player & batsman_runs == 4) %>% summarise(Fours = n())) == 0) {0} 
                            else {filter(df, batsman == input$player & batsman_runs == 4) %>% summarise(Fours = n())},
                            if(nrow(filter(df, batsman == input$player & batsman_runs == 6) %>% summarise(Sixes = n())) == 0) {0} 
                            else { filter(df, batsman == input$player & batsman_runs == 6) %>% summarise(Sixes = n())}
      ))) 
      
    }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), colnames = NULL,
    rownames = c('Innings', 'Runs', 'Highest Score', 'Fifties', 'Hundreds', 'Ducks', 'Fours', 'Sixes')
  ) 
  
  output$bowling_ui <- renderUI({
    if(nrow(filter(df,bowler == input$player)) == 0) 
      return('No data to show')
    span(style = 'color:black;font-weight:bold;', DT::DTOutput('bowling_table'))
  })
  
  output$bowling_table <- DT::renderDataTable(
    {
      t(distinct(data.frame(if(nrow(filter(df,bowler == input$player) %>% summarise(Overs = n_distinct(match_id,over))) == 0) {0} 
                            else {filter(df,bowler == input$player) %>% summarise(Overs = n_distinct(match_id,over))},
                            
                            if(nrow(filter(df,bowler == input$player) %>% summarise(Balls = n_distinct(match_id,over,ball))) == 0) {0} 
                            else {filter(df,bowler == input$player) %>% summarise(Balls = n_distinct(match_id,over,ball))},
                            
                            if(nrow(data.frame(filter(filter(df,bowler == input$player) %>% group_by(match_id,over,bowler) %>% 
                                                      summarise(runs= sum(total_runs)), runs == 0) %>% group_by(runs) %>%
                                               summarise(maiden = n())) %>% select('maiden')) == 0) {0} 
                            else {data.frame(filter(filter(df,bowler == input$player) %>% group_by(match_id,over,bowler) %>% 
                                                      summarise(runs= sum(total_runs)), runs == 0) %>% group_by(runs) %>%
                                               summarise(maiden = n())) %>% select('maiden')},
                            
                            if(nrow(filter(df,bowler == input$player & total_runs == 0 ) %>% summarise(dot = n())) == 0) {0} 
                            else {filter(df,bowler == input$player & total_runs == 0 ) %>% summarise(dot = n())},
                            
                            if(nrow(filter(df,bowler == input$player) %>% summarise(runs_given = sum(total_runs))) == 0) {0} 
                            else {filter(df,bowler == input$player) %>% summarise(runs_given = sum(total_runs)) %>% select(runs_given)},
                            
                            if(nrow(filter(df,bowler == input$player & dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                                                                             'lbw','hit wicket', 'stumped'))%>% summarise(wickets = n())) == 0) {0} 
                            else {filter(df,bowler == input$player & dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                                                                           'lbw', 'hit wicket', 'stumped'))%>% summarise(wickets = n())},
                            
                            if(nrow(filter(filter(df, bowler == input$player  & dismissal_kind %in% c('bowled', 'caught', 
                                                                                                      'caught and bowled', 'lbw','hit wicket', 'stumped')) %>% group_by(bowler, match_id) %>% 
                                           summarise(wickets = n()),  wickets == 4) %>% summarise(Four_wickets = n()) %>% select(Four_wickets)) == 0) {0} 
                            else {filter(filter(df, bowler == input$player  & dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                                                                                    'lbw','hit wicket', 'stumped')) %>% group_by(bowler, match_id) %>% summarise(wickets = n()), 
                                         wickets == 4) %>% summarise(Four_wickets = n()) %>% select(Four_wickets)},
                            
                            if(nrow(filter(df, bowler == input$player) %>% summarise(runs_given = sum(total_runs), 
                                                                                     overs = n_distinct(match_id,over)) %>% mutate(econ = round(runs_given/overs,2)) %>% select(econ)) == 0) {0} 
                            else {filter(df, bowler == input$player) %>% summarise(runs_given = sum(total_runs), 
                                                                                   overs = n_distinct(match_id,over)) %>% mutate(econ = round(runs_given/overs,2)) %>% select(econ)}
      )))
      
    }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), colnames = NULL,
    rownames = c('Overs', 'Balls', 'Maiden', 'Dot Balls', 'Run Conceded', 'Wickets', '4 Wickets in Innings', 'Economy' )
  ) 
  
  # Performance Outputs -----------------------------------------------------
  
  output$batting_opponents <- renderUI({
    
    if(nrow(filter(df,batsman == input$player1)) == 0) 
      return('No data to show')
    
    span(style = 'color:black;font-weight:bold;font-size: 80%;', DT::DTOutput('batting_opp_table'))
  })
  
  output$batting_opp_table <- DT::renderDataTable(
    {
      data.frame(
        filter(df,batsman == input$player1) %>% group_by(bowling_team) %>%
          summarise(matches = n_distinct(match_id), runs = sum(batsman_runs)) %>%
          left_join(filter(df,batsman == input$player1) %>% group_by(match_id, bowling_team) %>% summarise(runs = sum(batsman_runs)) %>%
                      group_by(bowling_team) %>% summarise(max_runs = max(runs)), by = 'bowling_team') %>%
          left_join(filter(filter(df,batsman == input$player1) %>% group_by(match_id, bowling_team) %>% summarise(runs = sum(batsman_runs)) %>%
                             group_by(bowling_team), runs >= 50 & runs < 100) %>% summarise(fifties = n()), by = 'bowling_team') %>%
          left_join(filter(filter(df,batsman == input$player1) %>% group_by(match_id, bowling_team) %>% summarise(runs = sum(batsman_runs)) %>%
                             group_by(bowling_team), runs >= 100) %>% summarise(hundreds = n()), by = 'bowling_team') %>% 
          replace_na(list(fifties = 0, hundreds = 0))
      ) 
      
    }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), rownames = NULL,
    colnames = c('Matches', 'Runs', 'HS', '50s', '100s')
  ) 
  
  output$bowling_opponents <- renderUI({
    
    if(nrow(filter(df,bowler == input$player1)) == 0) 
      return('No data to show')
    
    span(style = 'color:black;font-weight:bold;font-size: 80%;', DT::DTOutput('bowling_opp_table'))
  })
  
  output$bowling_opp_table <- DT::renderDataTable(
    {
      data.frame(
        filter(df,bowler == input$player1) %>% group_by(batting_team) %>%
          summarise(matches = n_distinct(match_id,inning), runs = sum(total_runs)) %>%
          left_join(filter(df,bowler == input$player1) %>% group_by(batting_team) %>% 
                      summarise(overs = n_distinct(match_id, over)), by = 'batting_team') %>%
          left_join(filter(df, bowler == input$player1 & dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                                                               'lbw','hit wicket','stumped')) %>% group_by(batting_team) %>% summarise(wickets = n()), by = 'batting_team')%>%
          left_join( setDT(filter(df, bowler == input$player1  & dismissal_kind %in% c('bowled', 'caught', 'caught and bowled',
                                                                                       'lbw','hit wicket','stumped')) %>% group_by(batting_team, match_id) %>% summarise(wickets = n()) %>%
                             left_join(filter(df, bowler == input$player1) %>% group_by(bowler, match_id) %>% summarise(runs_given = sum(total_runs)
                             ),  by = 'match_id') %>% arrange(desc(wickets), runs_given) %>% ungroup() %>%  select(c(1,3,5)))
                     [order(-wickets), head(.SD, 1), by = batting_team] %>% unite('Best', wickets:runs_given, sep = '/'), by = 'batting_team'
          ) %>% replace_na(list(wickets = 0, best = 'NA'))
      ) 
      
    }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), rownames = NULL,
    colnames = c('Matches', 'Runs Given', 'Overs', 'Wickets', 'Best')
  ) 
  
  output$batting_venue <- renderUI({
    
    if(nrow(filter(df,batsman == input$player1)) == 0) 
      return('No data to show')
    
    span(style = 'color:black;font-weight:bold;font-size: 80%;', DT::DTOutput('batting_venue_table'))
  })
  
  output$batting_venue_table <- DT::renderDataTable(
    {
      data.frame(
        filter(df,batsman == input$player1) %>% group_by(city) %>%
          summarise(matches = n_distinct(match_id), runs = sum(batsman_runs)) %>% arrange(desc(matches)) %>% head(15) %>%
          left_join(filter(df,batsman == input$player1) %>% group_by(match_id, city) %>% summarise(runs = sum(batsman_runs)) %>%
                      group_by(city) %>% summarise(max_runs = max(runs)), by = 'city') %>%
          left_join(filter(filter(df,batsman == input$player1) %>% group_by(match_id, city) %>% summarise(runs = sum(batsman_runs)) %>%
                             group_by(city), runs >= 50 & runs < 100) %>% summarise(fifties = n()), by = 'city') %>%
          left_join(filter(filter(df,batsman == input$player1) %>% group_by(match_id, city) %>% summarise(runs = sum(batsman_runs)) %>%
                             group_by(city), runs >= 100) %>% summarise(hundreds = n()), by = 'city') %>% 
          replace_na(list(fifties = 0, hundreds = 0))
      ) 
      
    }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), rownames = NULL,
    colnames = c('Matches', 'Runs', 'HS', '50s', '100s')
  ) 
  
  output$bowling_venue <- renderUI({
    
    if(nrow(filter(df,bowler == input$player1)) == 0) 
      return('No data to show')
    
    span(style = 'color:black;font-weight:bold;font-size: 80%;', DT::DTOutput('bowling_venue_table'))
  })
  
  output$bowling_venue_table <- DT::renderDataTable(
    {
      data.frame(
        filter(df,bowler == input$player1) %>% group_by(city) %>%
          summarise(matches = n_distinct(match_id,inning), runs = sum(total_runs)) %>% arrange(desc(matches)) %>% head(15) %>%
          left_join(filter(df,bowler == input$player1) %>% group_by(city) %>% 
                      summarise(overs = n_distinct(match_id, over)), by='city') %>%
          left_join(filter(df, bowler == input$player1 & dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                                                               'lbw','hit wicket','stumped')) %>% group_by(city) %>% summarise(wickets = n()), by = 'city')%>%
          left_join(setDT(filter(df, bowler == input$player1  & dismissal_kind %in% c('bowled', 'caught', 
                                                                                      'caught and bowled', 'lbw','hit wicket','stumped')) %>% group_by(city, match_id) %>% summarise(wickets = n()) %>%
                            left_join(filter(df, bowler == input$player1) %>% group_by(bowler, match_id) %>% summarise(runs_given = 
                                                                                                                         sum(total_runs)), by = 'match_id') %>% arrange(desc(wickets), runs_given) %>% ungroup() %>% 
                            select(c(1,3,5)) )[order(-wickets), head(.SD, 1), by = city] %>%
                      unite('Best', wickets:runs_given, sep = '/'), by = 'city') %>% replace_na(list(wickets = 0, best = 'NA'))
      ) 
      
    }, options = list(searching = FALSE, paging = FALSE, dom = 't', bSort=FALSE), rownames = NULL,
    colnames = c('Matches', 'Runs Given', 'Overs', 'Wickets', 'Best')
  ) 
  
  # Predictions Outputs -----------------------------------------------------
  
  matches <- matches %>%
    mutate(matches, 
           toss_team1 = ifelse(matches$toss_winner == matches$team1, 1,0),
           bat_team1 = ifelse ((matches$toss_winner == matches$team1 && matches$toss_decision == 'bat') | (matches$toss_winner == team2 && matches$toss_decision == 'field'),1,0),
           win_team1 = ifelse(winner == matches$team1, 1,0)
    )
  
  # Selecting transformed columns
  matches <- matches[,c('team1','team2','bat_team1','toss_team1','venue','win_team1')]
  str(matches)
  
  d<- matches
  
  library(caret)
  dummies <- dummyVars(win_team1 ~ ., data = d)            # create dummies for Xs
  ex <- data.frame(predict(dummies, newdata = d))  # actually creates the dummies
  names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
  d <- cbind(d$win_team1, ex)                              # combine target var with Xs
  names(d)[1] <- "win_team1"                               # name target var 'y'
  # #rm(dummies, ex)                                  # clean environment
  
  ################################################################################
  # Identifying linear dependencies and remove them
  ################################################################################
  # Find if any linear combinations exist and which column combos they are.
  # Below I add a vector of 1s at the beginning of the dataset. This helps ensure
  # the same features are identified and removed.
  library(caret)
  # first save response
  y <- d$win_team1
  
  # create a column of 1s. This will help identify all the right linear combos
  d <- cbind(rep(1, nrow(d)), d[2:ncol(d)])
  names(d)[1] <- "ones"
  
  # identify the columns that are linear combos
  comboInfo <- findLinearCombos(d)
  comboInfo
  
  # remove columns identified that led to linear combos
  d <- d[, -comboInfo$remove]
  
  # remove the "ones" column in the first column
  d <- d[, c(2:ncol(d))]
  
  # Add the target variable back to our data.frame
  d <- cbind(y, d)
  
  rm(y, comboInfo)  # clean up
  ################################################################################
  # Remove features with limited variation
  ################################################################################
  # remove features where the values they take on is limited
  # here we make sure to keep the target variable and only those input
  # features with enough variation
  nzv <- nearZeroVar(d, saveMetrics = TRUE)
  d <- d[, c(TRUE,!nzv$zeroVar[2:ncol(d)])]
  
  ################################################################################
  # Get the target variable how we want it for modeling with caret
  ################################################################################
  # if greater than 50k make 1 other less than 50k make 0
  d$y <- as.factor(d$y)
  class(d$y)
  
  # make names for target if not already made
  levels(d$y) <- make.names(levels(factor(d$y)))
  levels(d$y)
  
  # levels of a factor are re-ordered so that the level specified is first and 
  # "X1" is what we are predicting. The X before the 1 has nothing to do with the
  # X variables. It's just something weird with R. 'X1' is the same as 1 for the Y 
  # variable and 'X0' is the same as 0 for the Y variable.
  d$y <- relevel(d$y,"X1")
  
  ################################################################################
  # Data partitioning
  ################################################################################
  set.seed(1234) # set a seed so you can replicate your results
  library(caret)
  
  # identify records that will be used in the training set. Here we are doing a
  # 70/30 train-test split. You might modify this.
  inTrain <- createDataPartition(y = d$y,   # outcome variable
                                 p = .70,   # % of training data you want
                                 list = F)
  # create your partitions
  train <- d[inTrain,]  # training data set
  test <- d[-inTrain,]  # test data set
  df1 <- d[1,]
  
  # down-sampled training set
  dnTrain <- downSample(x=train[,2:ncol(d)], y=train$y)
  names(dnTrain)[24] <- "y"
  
  ################################################################################
  # Specify cross-validation design
  ################################################################################
  ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                       number=3,        # k number of times to do k-fold
                       classProbs = T,  # if you want probabilities
                       summaryFunction = twoClassSummary, # for classification
                       #summaryFunction = defaultSummary,  # for regression
                       allowParallel=T)
  
  # # train a feed-forward neural net on train set
  myModel1 <- train(y ~ .,               # model specification
                    data = train,        # train set used to build model
                    method = "nnet",     # type of model you want to build
                    trControl = ctrl,    # how you want to learn
                    tuneLength = 1:5,   # how many tuning parameter combos to try
                    maxit = 100,         # max # of iterations
                    metric = "ROC"  ,     # performance measure,
                    na.action=na.exclude
  )
  myModel1
  
  
  # train a feed-forward neural net on the down-sampled train set using a customer
  # tuning parameter grid
   myGrid <-  expand.grid(size = c(10,15,20)     # number of units in the hidden layer.
                          , decay = c(.09,0.12))  #parameter for weight decay. Default 0.
   myModel2 <- train(y ~ .,              # model specification
                     data = dnTrain,       # train set used to build model
                     method = "nnet",    # type of model you want to build
                     trControl = ctrl,   # how you want to learn
                     tuneGrid = myGrid,  # tuning parameter combos to try
                     maxit = 100,        # max # of iterations
                     metric = "ROC"      # performance measure
  )
  myModel2
  
  # train a random forest on down-sampled dataset
  myGrid <-  expand.grid(mtry = c(10,15,20))  # number of times to bag
  myModel3 <- train(y ~ .,              # model specification
                     data = dnTrain,     # train set used to build model
                     method = "rf",      # type of model you want to build
                     trControl = ctrl,   # how you want to learn
                     tuneGrid = myGrid,  # tuning parameter combos to try
                     metric = "ROC"  ,    # performance measure,
                     na.action=na.exclude
   )
  
  # train an XG-boost model on down-sampled dataset
  myGrid <-  expand.grid(nrounds = c(50,100,150)
                          , max_depth = c(1,2,3)
                          , eta = c(.3,.4)
                         , gamma = 0
                          , colsample_bytree = c(.6, .8)
                          , min_child_weight = 1
                          , subsample = c(.5,.75,1))
   myModel4 <- train(y ~ .,              # model specification
                     data = dnTrain,     # train set used to build model
                     method = "xgbTree",      # type of model you want to build
                     trControl = ctrl,   # how you want to learn
                     tuneGrid = myGrid,  # tuning parameter combos to try
                     metric = "ROC"      # performance measure
   )
  
  
  
  # # model 1
  nn1_trp <- predict(myModel1, newdata=train, type='prob')[,1]
  nn1_trc <- predict(myModel1, newdata=train)
  nn1_tep <- predict(myModel1, newdata=test, type='prob')[,1]
  nn1_tec <- predict(myModel1, newdata=test)
  # # model 2
  nn2_trp <- predict(myModel2, newdata=dnTrain, type='prob')[,1]
  nn2_trc <- predict(myModel2, newdata=dnTrain)
  nn2_tep <- predict(myModel2, newdata=test, type='prob')[,1]
  nn2_tec <- predict(myModel2, newdata=test)
  # # model 3
  rf_trp <- predict(myModel3, newdata=train, type='prob')[,1]
  rf_trc <- predict(myModel3, newdata=train)
  rf_tep <- predict(myModel3, newdata=test, type='prob')[,1]
  rf_tec <- predict(myModel3, newdata=test)
  # # model 4
  xgb_trp <- predict(myModel4, newdata=dnTrain, type='prob')[,1]
  xgb_trc <- predict(myModel4, newdata=dnTrain)
  xgb_tep <- predict(myModel4, newdata=test, type='prob')[,1]
  xgb_tec <- predict(myModel4, newdata=test)
  ###############################################################################################################
  ##Prediction on user input
  output$Winner_pred <- renderInfoBox({
    
    col_name1 = paste('team1',input$Team1)
    col_name1 = gsub(" ", "", col_name1, fixed = TRUE)
    col_name2 = paste('team2',input$Team2)
    col_name2 = gsub(" ", "", col_name2, fixed = TRUE)
    
    for(i in 1:length(colnames(df1))){
      if (colnames(df1)[i] == col_name1){
        df1[i] = 1
      }
      else if (colnames(df1)[i] == col_name2){
        df1[i] = 1
      }
      else
        df1[i] = 0
    }
    
    df1['bat_team1'] <- ifelse ((input$toss_winner == input$Team1 && input$toss_decision == 'bat') | (input$toss_winner == input$Team2 && input$toss_decision == 'field'),1,0)
    df1['toss_team1'] <-ifelse(input$toss_winner == input$Team1, 1,0)
    df1['venue'] <- input$Venue
                             # name target var 'y'
    preds <- predict(myModel1, newdata = df1, type = 'prob')
    result <- round(as.numeric(preds[[2]]), 2)
    
    if (result >= 0.5) {
      final_winner = input$Team1
    }
    else{
      final_winner = input$Team2
    }
    infoBox('#Winner',final_winner,
           icon = icon('trophy'), color = 'olive', fill = T)
  })
  
  output$prob_result <- renderInfoBox({
    col_name1 = paste('team1',input$Team1)
    col_name1 = gsub(" ", "", col_name1, fixed = TRUE)
    col_name2 = paste('team2',input$Team2)
    col_name2 = gsub(" ", "", col_name2, fixed = TRUE)
    
    for(i in 1:length(colnames(df1))){
      if (colnames(df1)[i] == col_name1){
        df1[i] = 1
      }
      else if (colnames(df1)[i] == col_name2){
        df1[i] = 1
      }
      else
        df1[i] = 0
    }
    
    df1['bat_team1'] <- ifelse ((input$toss_winner == input$Team1 && input$toss_decision == 'bat') | (input$toss_winner == input$Team2 && input$toss_decision == 'field'),1,0)
    df1['toss_team1'] <-ifelse(input$toss_winner == input$Team1, 1,0)
    df1['venue'] <- input$Venue
    # name target var 'y'
    preds <- predict(myModel1, newdata = df1, type = 'prob')
    result <- round(as.numeric(preds[[2]]), 2)
    
    if (result >= 0.5) {
      final_prob = result
    }
    else{
      final_prob = 1- result
    }
    
    infoBox('#Probability',final_prob,
            icon = icon('trophy'), color = 'olive', fill = T)
  })
        
#   test_data <- reactive({
#     data.frame(team1=input$Team1,
#                team2=input$Team2,
#                bat_team1 = ifelse ((input$toss_winner == input$Team1 && input$toss_decision == 'bat') | (input$toss_winner == input$Team2 && input$toss_decision == 'field'),1,0),
#                toss_team1 = ifelse(input$toss_winner == input$Team1, 1,0),
#                venue = input$Venue)
#                
#   })
#   
#  
#   dummies <- reactive(dummyVars( ., data = test_data()))           # create dummies for Xs
#   test_data <- reactive(data.frame(predict(dummies, newdata = test_data()))) # actually creates the dummies
#   
#    pred <- reactive({ 
#             predict(myModel1,newdata = test_data(), type='prob')[,1]
#      })
# # 
#   winner = reactive(ifelse(pred() > 0.5, input$Team1,input$Team2))
  
  
  
  # output$Winner_pred <- renderInfoBox({
  #   infoBox('#Winner',final_winner,
  #           icon = icon('trophy'), color = 'olive', fill = T)

  #})
  
  # Word Cloud
  output$image_display <- renderImage({
    list(
      src="wordcloud.jpeg"
    )
  })
  
  
}

shinyApp(ui, server)
