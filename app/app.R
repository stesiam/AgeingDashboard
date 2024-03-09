library(shiny)
library(bslib)
library(bsicons)
library(scales)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(echarts4r)
library(countrycode)

# remove here

# Setup -------------------------------------------------------------------

l = read_csv("data/l.csv")
pop = read_csv("data/pop.csv")
tfr = read_csv("data/tfr.csv")

tidy_l = l %>%
  tidyr::pivot_longer(.,cols = c(!country), names_to = "Year", values_to = "obsA") %>%
  drop_na(obsA) %>%
  mutate(
    Year = stringr::str_replace_all(Year,"[A-Z]", "")
  )

tidy_pop = pop %>%
  tidyr::pivot_longer(.,cols = c(!country), names_to = "Year", values_to = "obsB") %>%
  drop_na(obsB)

tidy_tfr = tfr %>%
  tidyr::pivot_longer(.,cols = c(!country), names_to = "Year", values_to = "obsC") %>%
  drop_na(obsC)
  

total = dplyr::full_join(tidy_l,tidy_pop,by = c("country", "Year"))

total1 = dplyr::full_join(total, tidy_tfr,by = c("country", "Year"))

total$iso2c = countrycode::countrycode(total$country,origin = "country.name", "iso2c")

# Function to create a dropdown option with flag and country name

dropdownOption <- function(country) {
  country_code <- countrycode(country, "country.name", "iso2c")
  flag_html <- flag(country_code)
  tagList(
    shiny::tags$img(src = flag_html, style = "width: 20px; height: 15px; margin-right: 5px;"),
    country
  )
}

ui <- page_navbar(
  theme =  bs_theme(version = 5),
  footer = "stesiam, 2024",
  window_title = "Ageing Dashboard",
  
    title = "Ageing Population",
    nav_spacer(),
    nav_item(
      tags$a(
        tags$span(
          bsicons::bs_icon("code-slash"), "Source code"
        ),
        href = "https://github.com/stesiam/AgeingDashboard",
        target = "_blank"
      )
    ),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "light")
    ),
    
  
  
  sidebar = sidebar(
    selectInput("country", "Select a Country:",
                choices = unique(total$country),
                selected = "Greece"
    ),
    hr(),
    sliderInput("year", "Select Year:", min = 1810, max = 2060, step = 5, value = 1950),
    hr(),
    selectInput("yeardiff", "Compare n-Years Before",
                choices = c("10Y", "20Y","50Y"),
                selected = "10Y"
    )
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Life Expectancy",
      value = textOutput("lifeExpectancy"),
      tags$div(class = "cl",
               textOutput("life_exp_change")),
      showcase = icon("person-cane")
    ),
    value_box(
      title = "Total Fertility Rate",
      value = textOutput("tfr"),
      tags$div(class = "cl",
               textOutput("tfr_change")),
      showcase = icon("baby")
    ),
    value_box(
      title = "Total Population",
      value = textOutput("totalPopulation"),
      showcase = icon("people-group")
    )
  ),
  layout_columns(
    navset_card_tab(
      full_screen = TRUE,
      title = "Historical Trends by Country",
      nav_panel(
        "Life Exp.",
        echarts4rOutput("plot", height = "260px")
      ),
      nav_panel(
        "TFR",
        echarts4rOutput("tfr_line", height = "260px")
      )
    ),
    card(full_screen = F,
         card_header("Map"),
         echarts4rOutput("map", height = "260px")
    )
 )
)


# Change ggplot2's default "gray" theme
theme_set(theme_bw(base_size = 16))



server <- function(input, output, session) {
  
  
  
  findYear = reactive({
    if (input$yeardiff == "10Y"){
      years = 10
    } else if (input$yeardiff == "20Y"){
      years = 20
    } else{
      years = 50
    }
   
    return(years) 
  })

  filtered_mean = reactive({
    
    # Filter data based on selected country
    filtered_data <- subset(total1,((country == input$country))) %>%
      subset(., (Year == input$year | Year == input$year - findYear())) %>%
      mutate(
        ObsAchange = (obsA[2] - obsA[1])/obsA[1],
        ObsCchange = (obsC[2] - obsC[1])/obsC[1]
      )
    
    # Calculate mean
    life_exp <- filtered_data$obsA[2]
    pop = filtered_data$obsB[2]
    tfr = filtered_data$obsC[2]
    life_exp_change = filtered_data$ObsAchange[2]
    tfr_change = filtered_data$ObsCchange[2]
    # Return mean value
    
  
    
    output = list(life_exp, pop, tfr, life_exp_change , tfr_change)
    return(output)
  })
  
  output$life_exp_change = renderText({
    paste0( round(filtered_mean()[[4]]*100,2), "%")
  })
  
  
  
  output$tfr_change = renderText({
    paste0( round(filtered_mean()[[5]]*100,2), "%")
  })
  
  
  output$lifeExpectancy <- renderText({
    round(filtered_mean()[[1]],2)
  })
  
  output$totalPopulation <- renderText({
    filtered_mean()[[2]]
  })
  
  output$tfr <- renderText({
    filtered_mean()[[3]]
  })
  
  observe({
    # This observer ensures that the reactive expression is re-run
    # whenever the value of input$country changes
    filtered_mean()
  })
  
  filter_country = reactive({
    filtered_data <- subset(total1, country == input$country)
    
    return(filtered_data)
  })
  
  output$plot = renderEcharts4r({
    filter_country() |>
      e_charts(Year) |> 
      e_line(obsA) |>
      e_legend(show = FALSE) |>
      e_axis_labels(x = "Years") |>
      e_title("Life Expectancy") |> 
      e_theme("infographic") |>  # theme
      e_tooltip(trigger = "axis", backgroundColor = "#becfff")
  })
  
  
output$tfr_line = renderEcharts4r({
  filter_country() |>
    e_charts(Year) |> 
    e_line(obsC) |>
    e_legend(show = FALSE) |>
    e_axis_labels(x = "Years") |>
    e_title("Total Fertility Rate") |> 
    e_theme("infographic") |>  # theme
    e_tooltip(trigger = "axis", backgroundColor = "#becfff")
})
  
  filter_year = reactive({
    filtered_data <- subset(total1, Year == input$year)
    
    return(filtered_data)
  })
  
  output$map = renderEcharts4r({
    filter_year() |> 
        e_charts(country) |> 
        e_map(obsA) |> 
        e_visual_map(obsA)
  })
  
  output$selected_country = renderText({
    print(input$country)
  })
  
  output$change = renderText({
    
  })
  
}


### Compare Year, Decade, 50Year



shinyApp(ui, server)