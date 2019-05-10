#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load packages:

suppressMessages(library(tidyverse))
library(sf)
library(mapview)
library(janitor)
library(USAboundaries)
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(lubridate)

# Get the missing and unidentified data from NamUS (5/8/2019):
missing_f <- read_csv("missing_f.csv") %>%
    clean_names() %>%
    separate(missing_age, c("age_no","unit"), sep = " ") %>% # NOTE: bad b/c removes "less than" ages
    select(-unit)

unidentified_f <- read_csv("unidentified_f.csv") %>%
    clean_names() %>%
    rename(state_name = state)

unclaimed_mf <- read_csv("unclaimed_mf.csv") %>%
    clean_names() %>%
    rename(county_name = county)

# Geographic data:
cities_geo <- read_csv("us_cities.csv") %>%
    rename(state = state_id)

# Join missing/unid and spatial data:
missing_by_city <- inner_join(missing_f, cities_geo)
unidentified_by_city <- inner_join(unidentified_f, cities_geo)

# Wrangle missing and unidentified and get the dates right!
miss_f <- missing_by_city %>%
    separate(race_ethnicity, c("first","second","third"), sep = ",") %>%
    unite(full_name, "first_name","last_name", sep = " ") %>%
    filter(age_no != "<") %>%
    mutate(age_no = as.numeric(age_no)) %>%
    mutate(date_last_contact = mdy(dlc)) %>%
    mutate(year = year(date_last_contact)) %>%
    mutate(year_correct = case_when(
        year > 2019 ~ year - 100,
        year <= 2019 ~ year
    )
    )

unid_f <- unidentified_by_city %>%
    separate(race_ethnicity, c("first","second","third"), sep = ",") %>%
    mutate(date_found = mdy(date_found)) %>%
    mutate(year_found = year(date_found)) %>%
    mutate(year_correct = case_when(
        year_found > 2019 ~ year_found - 100,
        year_found <= 2019 ~ year_found
    )
    )

# Define UI for application that draws a histogram
ui <- fluidPage(
    # shinythemes::themeSelector(),
    navbarPage("U.S. MISSING AND UNIDENTIFIED WOMEN",
               theme = shinythemes::shinytheme("cyborg"),

tabPanel("Search by race, age and time",

         p("Use this page to show locations for missing and unidentified women based on race/ethnicity, age, and year of last contact or when body was found. All data were retrieved from NamUS on May 8th, 2019, and do not include updates or new information since that date."),
    # theme = shinytheme("slate"),
    # # Application title
    # titlePanel("California missings, with surrounding states' unidentified"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Specify parameters:"),
            selectInput("race_choice",
                               label = h6("Race / Ethnicity of missing:"),
                               unique(miss_f$first),
                               selected = "Other"
            ),

            sliderInput("age_slider",
                        label = h6("Input age range of missing (yrs):"),
                        min = 0,
                        max = 100,
                        value = c(10,20)),

            sliderInput("age_slider_up",
                        label = h6("Input age range of unidentified (yrs):"),
                        min = 0, max = 100,
                        value = c(10,20)),

            sliderInput("year_missing",
                        label = h6("Select year(s) of last contact (inclusive):"),
                        min = 1900, max = 2019,
                        value = c(1900, 2019),
                        sep = ""),

            sliderInput("year_found",
                        label = h6("Select years(s) body found (inclusive):"),
                        min = 1900, max = 2019,
                        value = c(1900, 2019),
                        sep = "")


        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map")
        )
    )
    ),

tabPanel("By name",

    sidebarLayout(
        sidebarPanel(
            textInput("name_enter",
                      label = "Enter first and last name of missing (First Last):"),
            actionButton("run_name","Run")
        ),

        mainPanel(
            leafletOutput("name_map")
        )
    )
),

tabPanel("By case information",
         h2("testing again")
         )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Creating the reactive output ('map')
    output$map <- renderLeaflet({

        choose_options_miss <- miss_f %>%
            filter(first == input$race_choice |
                       second == input$race_choice) %>%
            filter(age_no >= input$age_slider[1] &
                       age_no <= input$age_slider[2]) %>%
            filter(year_correct >= input$year_missing[1] &
                       year_correct <= input$year_missing[2])

        choose_options_unid <- unid_f %>%
            filter(first %in% c(input$race_choice, "Uncertain") |
                       second %in% c(input$race_choice, "Uncertain")) %>%
            filter(age_from >= input$age_slider_up[1] &
                       age_to <= input$age_slider_up[2]) %>%
            filter(year_correct >= input$year_found[1] &
                       year_correct <= input$year_found[2])


        # Creating map
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(data = choose_options_miss,
                             color = "cyan",
                             stroke = FALSE,
                             fillOpacity = 0.5,
                             radius = 3,
                             popup = paste("Name:", choose_options_miss$full_name, "<br>",
                                                   "Last contact:", choose_options_miss$dlc, "<br>",
                                                   "Age reported missing:", choose_options_miss$age_no, "<br>",
                                                    "Case number:", choose_options_miss$case_number)
                             ) %>%
            addCircleMarkers(data = choose_options_unid,
                             color = "orange",
                             stroke = FALSE,
                             fillOpacity = 0.5,
                             radius = 3,
                             popup = paste("Case number:", choose_options_unid$case,"<br>",
                                 "Date discovered:", choose_options_unid$date_found, "<br>",
                                           "Race:", choose_options_unid$first, "<br>",
                                           "Minimum age:", choose_options_unid$age_from, "<br>")) %>%
            addProviderTiles("CartoDB.DarkMatter")
    })

    output$name_map <- renderLeaflet({

        # Select missings based on name input:
        name_select <- miss_f %>%
            filter(full_name == input$name_enter)

        #############################
        # Filter unidentified to include only matches (by race/ethnicity, age, and dlc)
        # THIS NEEDS TO BE UPDATED - should be and inclusive, or NAs
        #############################

        unid_matches_name <- unid_f %>%
            filter(first == name_select$first | first == "Uncertain") %>%
            filter(year_correct >= name_select$year_correct) %>%
            filter(age_from <= name_select$age_no | is.na(age_from)) %>%
            filter(age_to >= name_select$age_no | is.na(age_to))

        # Then add it to the map
        leaflet() %>%
            addTiles() %>%
            addCircleMarkers(data = name_select,
                             color = "cyan",
                             radius = 6,
                             stroke = FALSE,
                             fillOpacity = 0.5,
                             popup = paste("Name:", name_select$full_name, "<br>",
                                           "First race listed:", name_select$first,"<br>",
                                           "Last contact:", name_select$dlc, "<br>",
                                           "Age reported missing:", name_select$age_no, "<br>",
                                           "Case number:", name_select$case_number)
                             ) %>%
            addCircleMarkers(data = unid_matches_name,
                             color = "orange",
                             radius = 3,
                             stroke = FALSE,
                             fillOpacity = 0.5,
                             popup = paste("Case number:", unid_matches_name$case,"<br>",
                                           "Date discovered:", unid_matches_name$date_found, "<br>",
                                           "Race:", unid_matches_name$first, "<br>",
                                           "Minimum age:", unid_matches_name$age_from, "<br>",
                                           "Maximum age:", unid_matches_name$age_to
                             )) %>%
            addProviderTiles("CartoDB.DarkMatter")
    })
}

# Run the application
shinyApp(ui = ui, server = server)

