## ui.R ##
#library(shinydashboard)

header = dashboardHeader(title = "NBA Prospect Twitter")

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("info")),
    menuItem("Sentiment", tabName = "sent", icon = icon("dashboard")),
    menuItem("Usage", tabName = "freq", icon = icon("equalizer", lib = "glyphicon")),
    menuItem("Topics", tabName = "topc", icon = icon("tags")),
    menuItem("Projection", tabName = "proj", icon = icon("usd")),
    menuItem("Data", tabName = "data", icon = icon("th"))
  )
)

body = dashboardBody(
  tabItems(
    tabItem(tabName = "about",
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Goal", status = "primary", solidHeader = TRUE,
                  width = NULL,
                  collapsible = TRUE, collapsed = FALSE,
                  h3("Using Twitter to Project NBA Performance"),
                  p("Social media, especially personal social media accounts, 
                  offers us a glimpse into individual personality. The goal of
                  this project was to leverage personality insights gleaned
                  from NBA prospects' twitter accounts as part of the
                  scouting process. In other words, this project answers
                  two main questions."), 
                  br(),
                  p("1. Can we see a relationship between a player's social media
                  usage and their on-court performance?"),
                  br(),
                  p("2. Can we use a player's twitter account to help project their
                  on-court performance?")
                ),
                box(
                  title = "Background", status = "primary", solidHeader = TRUE,
                  width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  "This app was developed as part of an undergraduate sports data 
                  science class project at Brigham Young University in Provo, UT.
                  This project was a continuation of work on a ",
                  tagList("", a("project", href="https://nate-hawkins.shinyapps.io/nba_personality")),
                  " started by Nate Hawkins."
                )
              ),
              column(
                width = 8,
                box(
                  title = "App Explanation", status = "primary", solidHeader = TRUE,
                  width = NULL,
                  collapsible = TRUE, collapsed = FALSE,
                  "This app is comprised of 3 main interactive models and
                  a projections tab where you can use those models on a user-input
                  twitter handle.",
                  br(),
                  h3("Sentiment"),
                  "The Sentiment tab has input dropdowns for NBA draftee (any player
                    drafted in the NBA draft from 2010-2020, inclusive) and season,
                    which varies from player to player. By selecting a player and season,
                    you will see the sentiment scores (Anger, Disgust, Fear, Joy, Sadness)
                    from their twitter account for
                    that season calculated by the ",
                  tagList("", a("IBM NLU API", 
                                href="https://www.ibm.com/cloud/watson-natural-language-understanding")),
                  ".",
                  br(),
                  h3("Usage"),
                  "Similar to the Sentiment tab, 
                  the Usage tab has input dropdowns for NBA draftee (any player
                    drafted in the NBA draft from 2010-2020, inclusive) and season,
                    which varies from player to player. By selecting a player and season,
                    you will see two usage scores - average length of tweet, and 
                    number of tweets per year. These were calculated directly using ",
                  tagList("", a("Twitter's API", href = "https://developer.twitter.com/en/docs")),
                  ".",
                  br(),
                  h3("Topic"),
                  "The Topic tab has input dropdowns for NBA draftee (any player
                    drafted in the NBA draft from 2010-2020, inclusive) and season,
                    which varies from player to player. By selecting a player and season,
                    you will see the topic scores
                    from their twitter account for
                    that season, again calculated by the ",
                  tagList("", a("IBM NLU API", 
                                href="https://www.ibm.com/cloud/watson-natural-language-understanding")),
                  "."
                ),
                box(
                  title = "Project Analysis", status = "primary", solidHeader = TRUE,
                  width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  "The 3 BPM models used in this app are useful but not perfect. Below we 
                  see a plot of the predicted vs actual BPM values in the testing dataset 
                  for the sentiment model, with a few points labeled.",
                  br(),
                  imageOutput("resid_plot")
                ),
                box(
                  title = "About the Author", status = "primary", solidHeader = TRUE,
                  width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  "This app was developed by ",
                  tagList("", a("Parker Seegmiller", 
                                href="https://pkseeg.com/")),
                  "."
                  
                )
              )
            )
    ),
    
    tabItem(tabName = "sent",
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Player", status = "primary", solidHeader = TRUE,
                  width = NULL,
                  selectInput("sent_player", 
                              "Choose a player:",
                              sort(unique(data$Player)),
                              selected = "Georges Niang"),
                  selectInput("sent_season", 
                              "Choose a season:",
                              c("Average","2020-2021"),
                              selected = "Average")
                ),
                infoBoxOutput("sent_pred_box", width = 16),
                infoBoxOutput("sent_perc_box", width = 16),
                box(
                  title = "About SBPM and SBPM%", solidHeader = TRUE, width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  "The SBPM (Sentiment Box Plus Minus) projection is based on an 
                  XGBoost model, which uses the sentiment
                  scores calculated from a player's twitter account to predict BPM.
                  The SBPM% is the percentile at which the selected player's SBPM
                  resides.", 
                  br()
                )
              ),
              column(
                width = 8,
                box(
                 width = NULL,
                 title = "Sentiment Scores", status = "primary", solidHeader = TRUE,
                 plotlyOutput("sent_plot", height = 375)
                ),
                box(
                  title = "About Sentiment Scores", solidHeader = TRUE, width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  "The Sentiment scores (Anger, Disgust, Fear, Joy, Sadness) are calculated
                  from the twitter account data for
                  the selected player's season using the ",
                  tagList("", a("IBM NLU API", 
                                href="https://www.ibm.com/cloud/watson-natural-language-understanding")),
                  ".", 
                  br()
                )
              )
            )
    ),
    tabItem(tabName = "freq",
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Player", status = "primary", solidHeader = TRUE,
                  width = NULL,
                  selectInput("freq_player", 
                              "Choose a player:",
                              sort(unique(data$Player)),
                              selected = "Georges Niang"),
                  selectInput("freq_season", 
                              "Choose a season:",
                              c("Average","2020-2021"),
                              selected = "Average")
                ),
                infoBoxOutput("freq_pred_box", width = 16),
                infoBoxOutput("freq_perc_box", width = 16),
                box(
                  title = "About UBPM and UBPM%", solidHeader = TRUE, width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  "The UBPM (Usage Box Plus Minus) projection is based on an 
                  XGBoost model, which uses the usage
                  rates (length of tweets, # tweets/year, etc) calculated from a player's twitter
                  account to predict BPM.
                  The UBPM% is the percentile at which the selected player's UBPM
                  resides.", 
                  br()
                )
              ),
              column(
                width = 8,
                box(
                  width = NULL,
                  title = "Usage Measurements", status = "primary", solidHeader = TRUE,
                  plotlyOutput("freq_plot", height = 375)
                ),
                box(
                  title = "About Usage Measurements", solidHeader = TRUE, width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  "The Usage Measurements (length of tweets, # tweets/year, etc) are calculated
                  from the twitter account metadata for
                  the selected player's season using the ",
                  tagList("", a("Twitter API", href = "https://developer.twitter.com/en/docs")),
                  br()
                )
              )
            )
    ),
    
    tabItem(tabName = "topc",
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Player", status = "primary", solidHeader = TRUE,
                  width = NULL,
                  selectInput("topic_player", 
                              "Choose a player:",
                              sort(unique(data$Player)),
                              selected = "Georges Niang"),
                  selectInput("topic_season", 
                              "Choose a season:",
                              c("Average","2020-2021"),
                              selected = "Average")
                ),
                infoBoxOutput("topic_pred_box", width = 16),
                infoBoxOutput("topic_perc_box", width = 16),
                box(
                  title = "About TBPM and TBPM%", solidHeader = TRUE, width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  "The TBPM (Topic Box Plus Minus) projection is based on an 
                  XGBoost model, which uses the topic scores
                  (strength of topics like 'Education', 'Art and Entertainment', etc) calculated from a player's twitter
                  account to predict BPM.
                  The TBPM% is the percentile at which the selected player's TBPM
                  resides.", 
                  br()
                ),
                box(
                  title = "About Topic Measurements", solidHeader = TRUE, width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  "The Topic scores (strength of topics like 'Education', 'Art and Entertainment', etc) are calculated
                  from the twitter account data for
                  the selected player's season using the ",
                  tagList("", a("IBM NLU API", 
                                href="https://www.ibm.com/cloud/watson-natural-language-understanding")),
                  ".", 
                  br()
                )
              ),
              column(
                width = 8,
                box(
                  width = NULL,
                  title = "Tweet Topics", status = "primary", solidHeader = TRUE,
                  plotlyOutput("topic_plot", height = 375)
                ),
                box(
                  title = "NBA Average Tweet Topics", solidHeader = TRUE, width = NULL,
                  collapsible = TRUE, collapsed = TRUE,
                  plotlyOutput("topic_plot_average")
                )
              )
            )
    ),
    tabItem(tabName = "proj",
            fluidRow(
              column(
                width = 5,
                box(
                  title = "Twitter Handle Entry", width = NULL, 
                  status = "primary", solidHeader = TRUE,
                  textInput("proj_handle", "Player Twitter Handle:", value = "@twitter_handle", placeholder = "@twitter_handle"),
                  actionButton("proj_submit", label="Get Twitter Data", icon = NULL),
                  br(),
                  br(),
                  verbatimTextOutput("rendering_text", placeholder = TRUE)
                ),
                box(id = "proj_proj",
                  title = "BPM Projections", width = NULL, status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  dataTableOutput('proj_scores_table')
                ),
                box(id = "proj_freq",
                  width = NULL,
                  title = "Usage Measurements", status = "primary",
                  solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                  plotlyOutput("proj_freq_plot", height = 375)
                )
              ),
              column(
                width = 7,
                box(id = "proj_topic",
                  width = NULL,
                  title = "Tweet Topics", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = TRUE,
                  plotlyOutput("proj_topic_plot", height = 375)
                ),
                box(id = "proj_sent",
                  width = NULL, collapsible = TRUE, collapsed = TRUE,
                  title = "Sentiment Scores", status = "primary", solidHeader = TRUE,
                  plotlyOutput("proj_sent_plot", height = 375)
                )
              )
            )

    ),
    
    tabItem(tabName = "data",
            fluidRow(
              column(width = 12,
                box(
                  width = 12, collapsible = TRUE, collapsed = TRUE,
                  title = "About Data", status = "primary", solidHeader = TRUE,
                  "These 3 tables were created by concatenating data from ",
                  tagList("",a("Basketball Reference", href = "https://www.basketball-reference.com/")),
                  " draft data over the past 10 years (2010-2020), and twitter data via the ",
                  tagList("", a("Twitter API", href = "https://developer.twitter.com/en/docs"))
                ),
                box(
                  width = 12, collabsible = TRUE, collapsed = FALSE,
                  title = "Sentiment Data", status = "primary", solidHeader = TRUE,
                  dataTableOutput("sent_dataframe")
                ),
                box(
                  width = 12, collabsible = TRUE, collapsed = FALSE,
                  title = "Usage Data", status = "primary", solidHeader = TRUE,
                  dataTableOutput("freq_dataframe")
                ),
                box(
                  width = 12, collabsible = TRUE, collapsed = FALSE,
                  title = "Topic Data", status = "primary", solidHeader = TRUE,
                  dataTableOutput("topic_dataframe")
                )
              )
              
            )
    )
  )
)

dashboardPage(header, sidebar, body)












