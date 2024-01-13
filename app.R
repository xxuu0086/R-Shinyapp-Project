# Monash University
# 2022 Semester 2
# ETC5523 Assignment 1
# Xianghe Xu

# Please show the code below to have a better view.












library(shiny)
library(maps)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(bslib)
library(leaflet)
library(shinyWidgets)
library(DT)
coffee <- readxl::read_xlsx("Data/week6_coffee_chains.xlsx")
coffee2 <-
  readxl::read_xlsx("Data/week6_coffee_chains.xlsx", sheet = 2)
coffee3 <-
  readxl::read_xlsx("Data/week6_coffee_chains.xlsx", sheet = 3)

# using iso3166 to check country code and country map name.

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),

  tabPanel(
    "Plot",


    column(width = 12,
           fixedRow(
             column(
               3,
               h5("Explore a new theme"),
               selectInput(
                 "theme",
                 label = "",
                 choices = list(
                   "Cerulean",
                   "Quartz",
                   "Litera",
                   "Flatly",
                   "Yeti",
                   "Spacelab",
                   "Slate",
                   "United",
                   "Solar"
                 ),
                 selected = "Cerulean"
               )
             ),
             column(6, titlePanel(h1(
               strong("Global Coffee Chain"), align = "center"
             ))),
             column(
               3,
               h6("Switch to dark mode", align = "right;"),
               materialSwitch(
                 inputId = "mode",
                 label = icon("moon"),
                 right = TRUE,
                 status = "success"
               )

             )
           )),

    navbarPage(
      "Coffee Chains |",
      position = ("static-top"),
      theme = bs_theme(version = 5, bootswatch = "cerulean"),
      hr(),

      tabPanel(
        "Starbucks",
        icon = icon("map-location-dot", class = "fa-2x", lib = "font-awesome"),
        sidebarLayout(
          sidebarPanel(
            "",
            selectInput(
              "country",
              label = "Select a country to view Starbucks coffeehouse locations",
              choices = list(
                "Andorra",
                "United Arab Emirates",
                "Argentina",
                "Austria",
                "Australia",
                "Aruba",
                "Azerbaijan",
                "Belgium",
                "Bulgaria",
                "Bahrain",
                "Brunei",
                "Bolivia",
                "Brazil",
                "Canada",
                "Switzerland",
                "Chile",
                "China",
                "Colombia",
                "Costa Rica",
                "Curacao" ,
                "Cyprus",
                "Czech Republic",
                "Germany",
                "Denmark",
                "Egypt" ,
                "Spain" ,
                "Finland",
                "France" ,

                "Greece",
                "Guatemala",
                "Hungary",

                "Ireland",
                "India" ,
                "Jordan",

                "Cambodia",
                "South Korea",
                "Kuwait" ,
                "Kazakhstan",
                "Lebanon",
                "Luxembourg",
                "Morocco",
                "Monaco",
                "Mexico",

                "Netherlands",
                "Norway",

                "Oman",
                "Panama",
                "Peru",

                "Poland",
                "Puerto Rico",
                "Portugal",
                "Qatar",
                "Romania",
                "Russia",
                "Saudi Arabia",
                "Sweden",
                "Singapore",
                "Slovakia",
                "El Salvador",
                "Thailand",
                "Turkey",
                "Trinidad",
                "USA",
                "Vietnam",
                "South Africa"
              ),
              selected = "Australia"
            )

            ,
            selectInput(
              "map_color",
              label = "Select a fill color for map",
              choices = list(
                "Green",
                "Blue",
                "Red",
                "Black",
                "White",
                "Yellow",
                "Purple",
                "Orange",
                "Gold",
                "Pink"
              ),
              selected = "Gold"
            ),
            sliderInput(
              "map_opacity",
              label = "Opacity of map fill",
              min = 0,
              max = 1,
              value = 0.2,
              step = 0.1
            )
          ),


          mainPanel(
            h5(textOutput("countrymap_desc")),
            h6(
              icon("magnifying-glass-plus", class = "fa-1x", lib = "font-awesome"),
              "Zoom in & Click map marks to check the store name"
            ),
            leafletOutput("CountryMap", width = 900, height = 500),
            br(),
            h6(
              "*Note: The choices of countries are based on the data-set (week6_coffee_chains.xlsx), some countries may not be listed due to the limitation of the data set,
                                      so it may not include the population of all coffeehouses for Starbucks"
            ),
            br(),
            br()
          )
        )
      ),

      tabPanel(
        "Dunkin' Donuts",
        icon = icon("map-location", class = "fa-2x", lib = "font-awesome"),
        sidebarLayout(
          sidebarPanel(
            "",
            selectInput(
              "country_dun",
              label = "Select a country to view Dunkin' Donuts coffeehouse locations",
              choices = list("USA"),
              selected = "USA"
            )

            ,
            selectInput(
              "map_color_dun",
              label = "Select a fill color for map",
              choices = list(
                "Green",
                "Blue",
                "Red",
                "Black",
                "White",
                "Yellow",
                "Purple",
                "Orange",
                "Gold",
                "Pink"
              ),
              selected = "Gold"
            ),
            sliderInput(
              "map_opacitys",
              label = "Opacity of map fill",
              min = 0,
              max = 1,
              value = 0.2,
              step = 0.1
            ),
            br(),
            br(),
            br(),
            h6(
              "*Note: Due to the limitation of the data set, the data set contains only USA Dunkin' Donuts data."
            )
          ),

          mainPanel(
            h5(textOutput("countrymap_desc_dun")),
            h6(
              icon("magnifying-glass-plus", class = "fa-1x", lib = "font-awesome"),
              "Zoom in & Click map marks to check the store name"
            ),
            leafletOutput("CountryMap_dun", width = 900, height = 500),
            br(),
            br()
          )
        )

      )
      ,
      tabPanel(
        "Chain Detail",
        icon = icon("table", class = "fa-2x", lib = "font-awesome"),
        sidebarLayout(
          "",
          mainPanel(
            h4(radioButtons(
              "detail",
              label = "Select a coffee chain.",
              choices = c("Starbucks",
                             "Tim Hortons",
                             "Dunkin' Donuts"),
              selected = "Starbucks"
            )),
            hr(),
            h6(
              icon("magnifying-glass", class = "fa-1x", lib = "font-awesome"),
              "Search coffee chain coffeehouse locations information "
            ),
            DT::dataTableOutput("table"),
            br(),
            br()
          )

        )
      ),
      tabPanel(
        "Summary",
        icon = icon("chart-column", class = "fa-2x", lib = "font-awesome"),
        column(width = 12,
               fixedRow(
                 h5(
                   icon("magnifying-glass", class = "fa-1x", lib = "font-awesome"),
                   "Compare the total number of coffeehouses in a country for different coffee chain:"
                 ),
                 br(),
                 column(
                   4,
                   selectInput(
                     "country_p",
                     label = "Select a country to view total of Starbucks coffeehouses in:",
                     choices = list(
                       "Andorra",
                       "United Arab Emirates",
                       "Argentina",
                       "Austria",
                       "Australia",
                       "Aruba",
                       "Azerbaijan",
                       "Belgium",
                       "Bulgaria",
                       "Bahrain",
                       "Brunei",
                       "Bolivia",
                       "Brazil",
                       "Canada",
                       "Switzerland",
                       "Chile",
                       "China",
                       "Colombia",
                       "Costa Rica",
                       "Curacao" ,
                       "Cyprus",
                       "Czech Republic",
                       "Germany",
                       "Denmark",
                       "Egypt" ,
                       "Spain" ,
                       "Finland",
                       "France" ,

                       "Greece",
                       "Guatemala",
                       "Hungary",

                       "Ireland",
                       "India" ,
                       "Jordan",

                       "Cambodia",
                       "South Korea",
                       "Kuwait" ,
                       "Kazakhstan",
                       "Lebanon",
                       "Luxembourg",
                       "Morocco",
                       "Monaco",
                       "Mexico",

                       "Netherlands",
                       "Norway",

                       "Oman",
                       "Panama",
                       "Peru",

                       "Poland",
                       "Puerto Rico",
                       "Portugal",
                       "Qatar",
                       "Romania",
                       "Russia",
                       "Saudi Arabia",
                       "Sweden",
                       "Singapore",
                       "Slovakia",
                       "El Salvador",
                       "Thailand",
                       "Turkey",
                       "Trinidad",
                       "USA",
                       "Vietnam",
                       "South Africa"
                     )
                     ,
                     selected = "Australia"
                   ),
                 ),

                 column(
                   4,
                   selectInput(
                     "country_p2",
                     label = "Select a country to view total of Tim Hortons coffeehouses in:",
                     choices = list("USA",
                                    "Canda"),
                     selected = "Canda"
                   ),
                 ),

                 column(
                   4,
                   selectInput(
                     "country_p3",
                     label = "Select a country to view total of Dunkin' Donuts coffeehouses in:",
                     choices = list("USA"),
                     selected = "USA"
                   ),
                 )
               )),

        column(width = 12,
               fixedRow(
                 column(4,
                        hr(),
                        mainPanel("",
                                  plotOutput("plot_coffee"))),

                 column(4,
                        hr(),
                        mainPanel("",
                                  plotOutput("plot_coffee2"))),

                 column(4,
                        hr(),
                        mainPanel("",
                                  plotOutput("plot_coffee3")))
               )),
        br(),
        br(),
        br()

      )
      ,

      navbarMenu(
        "More",
        icon = icon("circle-info", class = "fa-2x", lib = "font-awesome"),
        tabPanel("About chains",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       "coffee_brand",
                       label = "Choose a coffee / refreshment company for a brief background:",
                       choices = list(
                         "Starbucks",
                         "Teavana",
                         "Evolution Fresh",
                         "Coffee House Holdings",
                         "Tim Hortons",
                         "Dunkin' Donuts"
                       ),
                       selected = "Starbucks"
                     ),
                     h1(icon(
                       "mug-hot", class = "fa-3x", lib = "font-awesome"
                     ))
                   )
                   ,
                   mainPanel(
                     h1(textOutput("coffee_brand_o")),
                     hr(),
                     textOutput("coffee_brand_oo"),
                     br(),
                     img(
                       src = "coffee.png",
                       alt = "*coffee image",
                       width = "80%",
                       height = "50%",
                       style = "padding : 5px; background-color:#66B2FF;"
                     )
                   )
                 )),
        tabPanel("Data/Purpose/Creator",
                 fluidRow(column(
                   10,
                   div(class = "about",
                       uiOutput('about')
                       )
                 )
                 )
                 )

      )

    )

  )
  ,
  includeCSS("styles.css")
)

server <- function(input, output, session) {
  output$about <- renderUI({
    knitr::knit("about.Rmd", quiet = TRUE) %>%
      markdown::markdownToHTML(fragment.only = TRUE) %>%
      HTML()
  })

  output$plot_coffee3 <- renderPlot({
    countries_p3 <- switch (input$country_p3,
                            "USA" = "USA")

    coffee3 %>%
      select(e_country, biz_name) %>%
      group_by(e_country) %>%
      summarise("total" = n()) %>%
      filter(e_country == countries_p3) %>%
      ggplot(aes(x = e_country,
                 y = total)) +
      geom_col(fill = "#66B2FF") +
      theme(axis.text.x = element_blank()) +
      theme_bw() +
      labs(y = "Total number of coffeehouses",
           x = input$country_p3) +
      geom_text(size = 10, aes(label = total), vjust = 2)
  })



  output$plot_coffee2 <- renderPlot({
    countries_p2 <- switch (input$country_p2,
                            "USA" = "us",
                            "Canda" = "ca")


    coffee2 %>%
      select(country, address) %>%
      group_by(country) %>%
      summarise("total" = n()) %>%
      filter(country == countries_p2) %>%
      ggplot(aes(x = country,
                 y = total)) +
      geom_col(fill = "#66B2FF") +
      theme(axis.text.x = element_blank()) +
      theme_bw() +
      labs(y = "Total number of coffeehouses",
           x = input$country_p2) +
      geom_text(size = 10, aes(label = total), vjust = 2)
  })



  output$plot_coffee <- renderPlot({
    countries_p <- switch (
      input$country_p,
      "Andorra" = "AD",
      "United Arab Emirates" = "AE",
      "Argentina" = "AR",
      "Austria" = "AT",

      "Australia" = "AU",
      "Aruba" = "AW",
      "Azerbaijan" = "AZ",
      "Belgium" = "BE",
      "Bulgaria" = "BG",
      "Bahrain" = "BH",
      "Brunei" = "BN",
      "Bolivia" = "BO",
      "Brazil" = "BR",
      "Canada" = "CA",
      "Switzerland" = "CH",
      "Chile" = "CL",
      "China" = "CN",
      "Colombia" = "CO",
      "Costa Rica" = "CR",
      "Curacao" = "CW",
      "Cyprus" = "CY",
      "Czech Republic" = "CZ",
      "Germany" = "DE",
      "Denmark" = "DK",
      "Egypt" = "EG",
      "Spain" = "ES",
      "Finland" = "FI",
      "France" = "FR",

      "Greece" = "GR",
      "Guatemala" = "GT",
      "Hungary" = "HU",

      "Ireland" = "IE",
      "India" = "IN",
      "Jordan" = "JO",

      "Cambodia" = "KH",
      "South Korea" = "KR",
      "Kuwait" =  "KW",
      "Kazakhstan" = "KZ",
      "Lebanon" =  "LB",
      "Luxembourg" = "LU",
      "Morocco" = "MA",
      "Monaco" = "MC",
      "Mexico" =  "MX",

      "Netherlands" = "NL",
      "Norway" = "NO",

      "Oman" = "OM",
      "Panama" = "PA",
      "Peru"  = "PE",

      "Poland" = "PL",
      "Puerto Rico" = "PR",
      "Portugal" = "PT",
      "Qatar" =  "QA",
      "Romania" =  "RO",
      "Russia" = "RU",
      "Saudi Arabia" = "SA",
      "Sweden" = "SE",
      "Singapore" = "SG",
      "Slovakia" = "SK",
      "El Salvador" = "SV",
      "Thailand" = "TH",
      "Turkey" = "TR",
      "Trinidad" = "TT",
      "USA" = "US",
      "Vietnam" = "VN",
      "South Africa" =  "ZA"
    )

    coffee %>%
      select(Country, Brand) %>%
      group_by(Country) %>%
      summarise("total" = n()) %>%
      filter(Country == countries_p) %>%
      ggplot(aes(x = Country,
                 y = total)) +
      geom_col(fill = "#66B2FF") +
      theme(axis.text.x = element_blank()) +
      theme_bw() +
      labs(y = "Total number of coffeehouses",
           x = input$country_p) +
      geom_text(size = 10, aes(label = total), vjust = 2)
  })

  output$min_max <- renderText({
    paste("You have choosen a range that goes from",
          input$range[1],
          " to ",
          input$range[2])
  })

  output$countrymap_desc <- renderText({
    paste(
      "View Starbucks and subsidiary coffeehouses' locations and distribution in:",
      input$country
    )
  })

  output$countrymap_desc_dun <- renderText({
    paste(
      "View Dunkin' Donuts coffeehouses' locations and distribution in:",
      input$country_dun
    )
  })

  output$CountryMap <- renderLeaflet({
    table_optionz <- reactive({
      switch (
        input$detail,
        "Starbucks" = coffee,
        "Tim Hortons" = coffee2,
        "Dunkin' Donuts" = coffee3
      )
    })
    output$table <- DT::renderDataTable({
      DT::datatable(table_optionz())
    })





    # [Error in as_mapper(.f, ...) : argument ".f" is missing, with no default}
    # The The reason for the error is likely the conflict of packages that
    # have functions with similar names. One can avoid it by specifying the source
    # package for the function, i.e. typing things like maps::map() for map() function.

    Countrys = maps::map(
      database = "world",
      fill = TRUE,
      plot = FALSE,
      regions = input$country,
      exact = TRUE
    )

    countries <- switch (
      input$country,
      "Andorra" = "AD",
      "United Arab Emirates" = "AE",
      "Argentina" = "AR",
      "Austria" = "AT",
      "Australia" = "AU",
      "Aruba" = "AW",
      "Azerbaijan" = "AZ",
      "Belgium" = "BE",
      "Bulgaria" = "BG",
      "Bahrain" = "BH",
      "Brunei" = "BN",
      "Bolivia" = "BO",
      "Brazil" = "BR",
      "Canada" = "CA",
      "Switzerland" = "CH",
      "Chile" = "CL",
      "China" = "CN",
      "Colombia" = "CO",
      "Costa Rica" = "CR",
      "Curacao" = "CW",
      "Cyprus" = "CY",
      "Czech Republic" = "CZ",
      "Germany" = "DE",
      "Denmark" = "DK",
      "Egypt" = "EG",
      "Spain" = "ES",
      "Finland" = "FI",
      "France" = "FR",

      "Greece" = "GR",
      "Guatemala" = "GT",
      "Hungary" = "HU",

      "Ireland" = "IE",
      "India" = "IN",
      "Jordan" = "JO",

      "Cambodia" = "KH",
      "South Korea" = "KR",
      "Kuwait" =  "KW",
      "Kazakhstan" = "KZ",
      "Lebanon" =  "LB",
      "Luxembourg" = "LU",
      "Morocco" = "MA",
      "Monaco" = "MC",
      "Mexico" =  "MX",

      "Netherlands" = "NL",
      "Norway" = "NO",

      "Oman" = "OM",
      "Panama" = "PA",
      "Peru"  = "PE",

      "Poland" = "PL",
      "Puerto Rico" = "PR",
      "Portugal" = "PT",
      "Qatar" =  "QA",
      "Romania" =  "RO",
      "Russia" = "RU",
      "Saudi Arabia" = "SA",
      "Sweden" = "SE",
      "Singapore" = "SG",
      "Slovakia" = "SK",
      "El Salvador" = "SV",
      "Thailand" = "TH",
      "Turkey" = "TR",
      "Trinidad" = "TT",
      "USA" = "US",
      "Vietnam" = "VN",
      "South Africa" =  "ZA"
    )


    color1 <- switch (
      input$map_color,
      "Green" = "green",
      "Blue" = "blue",
      "Red" = "red",
      "Black" = "black",
      "White" = "white",
      "Yellow" = "yellow",
      "Purple" = "purple",
      "Orange" = "orange",
      "Gold" = "gold",
      "Pink" = "pink"
    )

    leaflet(Countrys) %>% addTiles() %>%
      fitBounds(Countrys$range[1],
                Countrys$range[3],
                Countrys$range[2],
                Countrys$range[4]) %>%
      addPolygons(
        fillOpacity = input$map_opacity,
        fillColor = color1 ,
        smoothFactor = 0.5,
        stroke = TRUE,
        weight = 1
      ) %>%
      addMarkers( ~ coffee$Longitude[coffee$Country == countries],
                  ~ coffee$Latitude[coffee$Country == countries],
                  label = coffee$`Store Name`[coffee$Country == countries])
  })

  output$CountryMap_dun <- renderLeaflet({
    # [Error in as_mapper(.f, ...) : argument ".f" is missing, with no default]
    # The The reason for the error is likely the conflict of packages that
    # have functions with similar names. One can avoid it by specifying the source
    # package for the function, i.e. typing things like maps::map() for map() function.
    # "Bahamas", "Japan", "UK", "Indonesia", "Malaysia", "New Zealand" and "Philippines" cannot find the mapname in maps::map()

    Countrys = maps::map(
      database = "world",
      fill = TRUE,
      plot = FALSE,
      regions = input$country_dun,
      exact = TRUE
    )

    countriess <- switch (input$country_dun,
                          "Canada" = "CA",
                          "USA" = "USA")

    color1 <- switch (
      input$map_color_dun,
      "Green" = "green",
      "Blue" = "blue",
      "Red" = "red",
      "Black" = "black",
      "White" = "white",
      "Yellow" = "yellow",
      "Purple" = "purple",
      "Orange" = "orange",
      "Gold" = "gold",
      "Pink" = "pink"
    )

    leaflet(Countrys) %>% addTiles() %>%
      fitBounds(Countrys$range[1],
                Countrys$range[3],
                Countrys$range[2],
                Countrys$range[4]) %>%
      addPolygons(
        fillOpacity = input$map_opacitys,
        fillColor = color1 ,
        smoothFactor = 0.5,
        stroke = TRUE,
        weight = 1
      ) %>%
      addMarkers( ~ coffee3$loc_LONG_poly[coffee3$e_country == countriess],
                  ~ coffee3$loc_LAT_poly[coffee3$e_country == countriess],
                  label = coffee3$biz_name[coffee3$e_country == countriess])
  })

  coffee_brand_brief <- reactive({
    switch (
      input$coffee_brand,
      "Starbucks" = "Starbucks Corporation is
                                          an American multinational chain of coffeehouses and roastery reserves
                                          headquartered in Seattle, Washington. It is the world's largest
                                          coffeehouse chain. As of November 2021, the company had 33,833
                                          stores in 80 countries, 15,444 of which were located in the United
                                          States.",
      "Teavana" = "Teavana Corporation is an
                                          American tea company, which previously
                                          had locations throughout the United States,
                                          Canada, Mexico, and the Middle East. Starbucks
                                          acquired Teavana in 2012, and in 2017, Starbucks
                                          announced it would close all Teavana locations by 2018.",
      "Evolution Fresh" = "Evolution Fresh,
                                          a subsidiary of Starbucks Corporation, is an American-based company
                                          producing fruit juices, fruit smoothies, gourmet soups, salads and
                                          signature bowls",
      "Coffee House Holdings" = "Coffee house holdings are the subsidiaries of
                                          Starbucks corporation.",
      "Tim Hortons" = "Tim Hortons is a Canadian multinational fast food
                                          restaurant chain, it is based in Toronto, Tim Hortons not only serves cakes, doughnuts
                                          and other fast food items, and also serves coffee. It is Canada's largest quick-service restaurant
                                          chain, with 4,949 restaurants in 15 countries as of March of 2022.",
      "Dunkin' Donuts" = "Dunkin' Donuts is an American multinational coffee and doughnut company,
                                          as well as a quick service restaurant. Dunkin' is one of the largest coffee shop and donut shop
                                          chains in the world, and it has approximately 12,900 locations in 42 countries."
    )
  })

  output$coffee_brand_o <- renderText({
    paste("A brief background about:", input$coffee_brand)
  })

  output$coffee_brand_oo <- renderText({
    coffee_brand_brief()
  })



  theme_name <- reactive({
    switch (
      input$theme,
      "Cerulean" = "cerulean",
      "Quartz" = "quartz",
      "Litera" = "litera",
      "Flatly" = "flatly",
      "Yeti" = "yeti",
      "Spacelab" = "spacelab",
      "Slate" = "slate",
      "United" = "united",
      "Solar" = "solar"
    )
  })

  observe(session$setCurrentTheme(if (isTRUE(input$mode)) {
    bs_theme(bootswatch = "superhero")
  } else {
    bs_theme(bootswatch = theme_name())
  }))


}

shinyApp(ui, server)
