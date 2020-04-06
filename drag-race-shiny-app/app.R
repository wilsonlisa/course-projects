options(scipen = 999)

# load libraries
library(tidyverse)
library(shiny)
library(plotly)
library(shinyWidgets)
library(scales)
library(RColorBrewer)
library(Cairo)

# data --> could put in source file
twitter <- read_csv("twitter.csv")
twitter.diff <- read_csv("twitter_diff.csv")

# plot drawing functions
twitter_plots_diff = function(data, number) {
  lims <- c(min(filter(twitter.diff, season_number == number)$twit_diff), max(filter(twitter.diff, season_number == number)$twit_diff))
  
  # ggplot(data, aes(reorder(contestant_name, -season_outcome), twit_diff)) +
  ggplot(data, aes(reorder(stringr::str_wrap(contestant_name, 15), -season_outcome), twit_diff)) +
    geom_point(aes(color = twit_diff, text = sprintf("Gain in followers: %i <br>Placement: %i <br>Age: %i", twit_diff, season_outcome, age)), size = 2) +
    scale_color_gradientn(colors = c("deepskyblue2", "purple", "magenta3", "deeppink1"), limits = lims) +
    labs(y = "Gain in Twitter followers over season", x = "Contestants in order of season placement", title = paste("Season", number, sep = " ")) +
    theme_light() +
    # theme(legend.position = "none", panel.background = element_rect(fill = "lavenderblush"), axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10), axis.text.y = element_text(size = 8)) +
    theme(legend.position = "none", panel.background = element_rect(fill = "lavenderblush")) +
    coord_flip()
}

twitter_plots_time = function(data, seasondat, number, start, end) {
  # use colorRampPalette on one of brewer palettes
  # fix seasons to color values
  # https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
  
  scalelen <- length(unique(seasondat$contestant_name))
  timepal <- hue_pal(direction = -1)(scalelen)
  names(timepal) <- levels(reorder(stringr::str_wrap(seasondat$contestant_name, 10), seasondat$season_outcome))
  # names(timepal) <- levels(reorder(seasondat$contestant_name, seasondat$season_outcome))
  
  # getPalette <- colorRampPalette(brewer.pal(12, "Paired"))
  
  ggplot(data, aes(lubridate::as_date(datetime), followers_twitter)) +
    # geom_line(aes(color = reorder(stringr::str_wrap(contestant_name, 10), season_outcome), text = sprintf("%s <br>Followers: %i", contestant_name, followers_twitter)), size = 0.8) +
    geom_line(aes(color = reorder(stringr::str_wrap(contestant_name, 10), season_outcome), text = sprintf("%s <br>Placement: %i <br>Age: %i", contestant_name, season_outcome, age)), size = 0.8) +
    # geom_line(aes(color = reorder(contestant_name, season_outcome), text = sprintf("%s <br>Placement: %i <br>Age: %i", contestant_name, season_outcome, age)), size = 0.8) +
    scale_color_manual(values = timepal) +
    # scale_color_viridis_d(option = "plasma") +
    # scale_color_manual(values = getPalette(scalelen)) +
    labs(y = "Twitter followers", color = "Contestant\nname", x = "Fridays over the course of the season", title = paste("Season", number, sep = " ")) +
    scale_x_date(date_labels = "%b \n%d", breaks = seq(from = start, to = end, by = "week")) +
    theme_light() +
    theme(panel.background = element_rect(fill = "lavenderblush"))
}

# RColorBrewer::display.brewer.all(colorblindFriendly=TRUE)
# colormod <- brewer.pal(9, "PuRd")[3:9]
colormod <- brewer.pal(7, "Set2")
colorval <- c("4" = colormod[7], "5" = colormod[6],
              "6" = colormod[5], "7" = colormod[4],
              "8" = colormod[3], "9" = colormod[2],
              "10" = colormod[1]
              )

twitter_plots_inter = function(data) {
    ggplot(data, aes(reorder(contestant_name, twit_diff), twit_diff)) +
    # ggplot(data, aes(reorder(stringr::str_wrap(contestant_name, 15), twit_diff), twit_diff)) +
      geom_point(aes(color = factor(season_number), text = sprintf("Gain in followers: %i <br>Season %i, Placement: %i <br>Age: %i", twit_diff, season_number, season_outcome, age)), size = 2) +
      #scale_color_gradientn(colors = c("deepskyblue2", "purple", "magenta3", "deeppink1")) +
      # scale_color_manual(values = c("turquoise3", "deepskyblue2", "slateblue1", "mediumpurple1", "darkorchid2", "magenta3", "deeppink1")) +
      # scale_color_brewer(palette = "Set2") +
      scale_color_manual(values = colorval) +
      # scale_color_viridis_d(direction = -1, option = "plasma") +
      labs(y = "Gain in Twitter followers over season", color = "Season") +
      theme_light() +
      # theme(legend.position = "none", panel.background = element_rect(fill = "lavenderblush"), axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10), axis.text.y = element_text(size = 8)) +
      theme(panel.background = element_rect(fill = "lavenderblush"), axis.title.y = element_blank()) +
      coord_flip()
}

blank = function(number, start, end) {
  ggplot(filter(twitter, season_number == number), aes(lubridate::as_date(datetime), followers_twitter)) +
    geom_blank() +
    labs(y = "Twitter followers", x = "Fridays over the course of the season", title = paste("Season", number, sep = " ")) +
    scale_x_date(date_labels = "%b \n%d", breaks = seq(from = start, to = end, by = "week")) +
    theme_light() +
    theme(legend.position = "none", panel.background = element_rect(fill = "lavenderblush"))
}

blank_diff = function(number) {
  ggplot(filter(twitter.diff, season_number == number), aes(lubridate::as_date(datetime), followers_twitter)) +
    geom_blank() +
    scale_color_gradientn(colors = c("deepskyblue2", "purple", "magenta3", "deeppink1")) +
    labs(y = "Gain in Twitter followers over season", x = "Contestants in order of season placement", title = paste("Season", number, sep = " ")) +
    theme_light() +
    theme(legend.position = "none", panel.background = element_rect(fill = "lavenderblush"), axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10), axis.text.y = element_blank()) +
    coord_flip()
}

blank_inter = function(number) {
  ggplot(filter(twitter.diff, season_number == number), aes(reorder(stringr::str_wrap(contestant_name, 15), twit_diff), twit_diff)) +
    geom_blank() +
    labs(y = "Gain in Twitter followers over season", x = "Contestants") +
    theme_light() +
    theme(panel.background = element_rect(fill = "lavenderblush"), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    coord_flip()
}

# User interface ----
ui <- fluidPage(
  titlePanel("RuPaul's Drag Race: Social Media Queens"),
  
  sidebarLayout(
    sidebarPanel(
      p(em("What happens on the show is only half the story.")),
      
      p(strong("Which queens most successfully translated a reality television run into an expanded online fanbase? Explore this and more below!")),
      
      # br(),
      
      helpText("Hover over the lines and points for more information, such as contestant age and placement."),
      
      br(),
      
      # helpText("Data available for seasons 4-10 only. If a contestant is not listed in the dropdown menus, they did not have Twitter data available during their season."),
      
      # br(),
      
      selectInput(inputId = "season", label = HTML("Select a season number<br/><span style=font-weight:normal><i>Data available for seasons 4-10 only</i></span style=font-weight:normal>"),
                  choices = list("Season 4" = 4, "Season 5" = 5,
                                 "Season 6" = 6, "Season 7" = 7,
                                 "Season 8" = 8, "Season 9" = 9, 
                                 "Season 10" = 10),
                  selected = 10
      ),
      
      conditionalPanel(
        condition = "input.season == 4",
        # https://dreamrs.github.io/shinyWidgets/index.html
        pickerInput(
          inputId = "conpick4", 
          label = HTML("Select/deselect contestants<br/><span style=font-weight:normal><i>Listed in order of season placement</i></span style=font-weight:normal>"), 
          choices = unique(twitter[twitter$season_number == 4, ]$contestant_name)[order(distinct(filter(twitter, season_number == 4), contestant_name, .keep_all = TRUE)$season_outcome)], 
          # selected = c("Sharon Needles", "Phi Phi O'Hara", "Latrice Royale"),
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        )
      ),
      
      conditionalPanel(
        condition = "input.season == 5",
        pickerInput(
          inputId = "conpick5", 
          label = HTML("Select/deselect contestants<br/><span style=font-weight:normal><i>Listed in order of season placement</i></span style=font-weight:normal>"), 
          choices = unique(twitter[twitter$season_number == 5, ]$contestant_name)[order(distinct(filter(twitter, season_number == 5), contestant_name, .keep_all = TRUE)$season_outcome)], 
          # selected = c("Jinkx Monsoon", "Detox", "Alyssa Edwards"),
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        )
      ),
      
      conditionalPanel(
        condition = "input.season == 6",
        pickerInput(
          inputId = "conpick6", 
          label = HTML("Select/deselect contestants<br/><span style=font-weight:normal><i>Listed in order of season placement</i></span style=font-weight:normal>"), 
          choices = unique(twitter[twitter$season_number == 6, ]$contestant_name)[order(distinct(filter(twitter, season_number == 6), contestant_name, .keep_all = TRUE)$season_outcome)], 
          # selected = c("Bianca Del Rio", "Courtney Act", "BenDeLaCreme"),
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        )
      ),
      
      conditionalPanel(
        condition = "input.season == 7",
        pickerInput(
          inputId = "conpick7", 
          label = HTML("Select/deselect contestants<br/><span style=font-weight:normal><i>Listed in order of season placement</i></span style=font-weight:normal>"), 
          choices = unique(twitter[twitter$season_number == 7, ]$contestant_name)[order(distinct(filter(twitter, season_number == 7), contestant_name, .keep_all = TRUE)$season_outcome)], 
          # selected = c("Violet Chachki", "Katya", "Trixie Mattel"),
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        )
      ),
      
      conditionalPanel(
        condition = "input.season == 8",
        pickerInput(
          inputId = "conpick8", 
          label = HTML("Select/deselect contestants<br/><span style=font-weight:normal><i>Listed in order of season placement</i></span style=font-weight:normal>"), 
          choices = unique(twitter[twitter$season_number == 8, ]$contestant_name)[order(distinct(filter(twitter, season_number == 8), contestant_name, .keep_all = TRUE)$season_outcome)], 
          # selected = c("Bob the Drag Queen", "Chi Chi DeVayne", "Thorgy Thor"),
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        )
      ),
      
      conditionalPanel(
        condition = "input.season == 9",
        pickerInput(
          inputId = "conpick9", 
          label = HTML("Select/deselect contestants<br/><span style=font-weight:normal><i>Listed in order of season placement</i></span style=font-weight:normal>"), 
          choices = unique(twitter[twitter$season_number == 9, ]$contestant_name)[order(distinct(filter(twitter, season_number == 9), contestant_name, .keep_all = TRUE)$season_outcome)], 
          # selected = c("Sasha Velour", "Nina Bo'nina Brown", "Farrah Moan"),
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        )
      ),
      
      conditionalPanel(
        condition = "input.season == 10",
        pickerInput(
          inputId = "conpick10", 
          label = HTML("Select/deselect contestants<br/><span style=font-weight:normal><i>Listed in order of season placement</i></span style=font-weight:normal>"), 
          choices = unique(twitter[twitter$season_number == 10, ]$contestant_name)[order(distinct(filter(twitter, season_number == 10), contestant_name, .keep_all = TRUE)$season_outcome)], 
          selected = c("Aquaria", "Miz Cracker", "Monique Heart"),
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        )
      ),
      
      # br(),
      
      radioButtons("switch", label="Select a plot type", 
                   choices=c("Single-season timeplot of changes in Twitter followers" = "time",
                             "Single-season comparison of gains in Twitter followers" = "gain",
                             "Interseason comparison of gains in Twitter followers: displays all contestants selected in all seasons" = "inter"),
                   selected = "time"),
      
      br(),
      
      helpText("Source: Data for Progress")
      
    ),
    
    mainPanel(plotlyOutput("plot"))
  )
)

# Server logic
server <- function(input, output) {
  
  choices <- reactive({
    c(input$conpick4, input$conpick5, input$conpick6, input$conpick7, input$conpick8, input$conpick9, input$conpick10)
  })
  
  selectedSeason <- reactive({
    twitter %>%
      filter(season_number == input$season)
  })
  
  selectedData <- reactive({
    twitter %>% 
      filter(season_number == input$season & contestant_name %in% choices()) 
  })
  
  selectedData_diff <- reactive({
    twitter.diff %>% 
      filter(season_number == input$season & contestant_name %in% choices()) 
  })
  
  selectedData_inter <- reactive({
    twitter.diff %>%
      filter(contestant_name %in% choices())
  })
  
  output$plot <- renderPlotly({
    if (input$switch == "time"){
      start <- lubridate::as_date(min(filter(filter(twitter, season_number == input$season), lubridate::wday(datetime, label = TRUE)=="Fri")$datetime))
      end <- lubridate::as_date(max(filter(filter(twitter, season_number == input$season), lubridate::wday(datetime, label = TRUE)=="Fri")$datetime))
      
      #choices <- c(input$conpick4, input$conpick5, input$conpick6, input$conpick7, input$conpick8, input$conpick9, input$conpick10)
      if (is.null(choices()) | isFALSE(any(choices() %in% filter(twitter, season_number == input$season)$contestant_name))){
        ggplotly(blank(input$season, start, end), 
                 height = 600
                 )
      }
      else {
        ggplotly(twitter_plots_time(selectedData(), selectedSeason(), input$season, start, end), 
                 height = 600,
                 tooltip = "text") 
      }
    }
    
    else{
      if (input$switch == "gain"){
        #choices <- c(input$conpick4, input$conpick5, input$conpick6, input$conpick7, input$conpick8, input$conpick9, input$conpick10)
        if (is.null(choices()) | isFALSE(any(choices() %in% filter(twitter.diff, season_number == input$season)$contestant_name))){
          ggplotly(blank_diff(input$season), 
                   height = 600
                   )
        }
        else {
          ggplotly(twitter_plots_diff(selectedData_diff(), input$season), 
                   # width = 550, height = 450,
                   height = 600,
                   tooltip = "text")
        }
      }
      
      else{
        if (is.null(choices()) | isFALSE(any(choices() %in% twitter.diff$contestant_name))){
          ggplotly(blank_inter(input$season), 
                   height = 600
                   )
        }
        else {
          ggplotly(twitter_plots_inter(selectedData_inter()),
                   height = 600,
                   tooltip = "text")
        }
      }
    }
    
  })
}

# Run app
shinyApp(ui, server)
