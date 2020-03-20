

### Function to rename countries which are not in the
### format of the data used to plot map.
rename_countries <- function(data) {
  data[, 1][data[, 1] == "S. Korea", ] <- "Korea"
  data[, 1][data[, 1] == "USA", ] <- "United States"

  data[, 1] <- purrr::map(data[, 1], ~ if_else(. %in%
    c("UK", "Seychelles", "Channel Islands"),
  "United Kingdom", .
  ))

  data[, 1][data[, 1] == "Czechia", ] <- "Czech Rep."
  data[, 1][data[, 1] == "UAE", ] <- "United Arab Emirates"
  data[, 1][data[, 1] == "Bosnia and Herzegovina", ] <- "Bosnia and Herz."
  data[, 1][data[, 1] == "North Macedonia", ] <- "Macedonia"

  data[, 1] <- purrr::map(data[, 1], ~ if_else(. %in%
    c("Maldives", "Martinique", "French Guiana", "R?union", "Guadeloupe", "Mayotte"),
  "France", .
  ))

  data[, 1][data[, 1] == "Dominican Republic", ] <- "Dominican Rep."
  data[, 1][data[, 1] == "Faeroe Islands", ] <- "Faeroe Is."
  data[, 1][data[, 1] == "Ivory Coast", ] <- "C?te d'Ivoire"
  data[, 1][data[, 1] == "French Polynesia", ] <- "Fr. Polynesia"
  data[, 1][data[, 1] == "DRC", ] <- "Dem. Rep. Congo"
  data[, 1][data[, 1] == "Saint Martin", ] <- "St-Martin"
  data[, 1][data[, 1] == "Antigua and Barbuda", ] <- "Antigua and Barb."
  data[, 1][data[, 1] == "Cayman Islands", ] <- "Cayman Is."
  data[, 1][data[, 1] == "CAR", ] <- "Central African Rep."
  data[, 1][data[, 1] == "Equatorial Guinea", ] <- "Eq. Guinea"
  data[, 1][data[, 1] == "Vatican City", ] <- "Vatican"
  data[, 1][data[, 1] == "St. Barth", ] <- "St-Barth?lemy"
  data[, 1][data[, 1] == "St. Vincent Grenadines", ] <- "St. Vin. and Gren."
  data[, 1][data[, 1] == "Eswatini", ] <- "Swaziland"
  data[, 1][data[, 1] == "U.S. Virgin Islands", ] <- "U.S. Virgin Is."
  data
}

### Function to scrap data from the website given the url and world data (used to plot map).
### ("https://www.worldometers.info/coronavirus/#countries")
read_coro_data <- function(url, world) {
  html <- read_html(url)
  temp <- html %>%
    html_nodes(xpath = '//table[@id = "main_table_countries_today"]')

  title <- temp %>%
    html_nodes("th") %>%
    html_text(trim = T)
  list_data <- temp %>%
    html_nodes("td") %>%
    html_text(trim = T) %>%
    gsub(",", "", .)

  data <- matrix(list_data, ncol = 9, byrow = T) %>% as.tibble()
  colnames(data) <- title
  data[, 2:9] <- apply(data[, 2:9], 2, as.numeric)

  data <- rename_countries(data)
  data <- data %>%
    group_by(`Country,Other`) %>%
    summarise(
      TotalCases = sum(TotalCases),
      NewCases = sum(NewCases),
      TotalDeaths = sum(TotalDeaths),
      NewDeaths = sum(NewDeaths),
      TotalRecovered = sum(TotalRecovered),
      ActiveCases = sum(ActiveCases),
      "Serious,Critical" = sum(`Serious,Critical`)
    )

  #save and remove "total" row
  summ <- data[which(data[, 1] == "Total:"), ]
  data <- data[-which(data[, 1] == "Total:"), ]
  data <- data %>% rename("Country/Other" = "Country,Other")

  #geo_data contains data of number of cases
  #data_Map contains data to plot the map (use in leaflet)
  #filter the world data to only contain the countries in the website
  #rearrange countries in geo_data in the order of data_Map
  data_Map <- world[world$name %in% data$`Country/Other`, ]
  geo_data <- data[order(match(data$`Country/Other`, data_Map$name)), ]
  return(list(data_Map, geo_data, summ))
}

### Function to retrieve data from github ("https://github.com/CSSEGISandData/COVID-19")
read_daily_data <- function() {
  confirm <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
  death <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))
  recover <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"))

  r <- GET("https://covidtracking.com/api/states/daily")
  stop_for_status(r)
  json <- content(r, as = "text")

  state <- fromJSON(json) %>%
    mutate(date = ymd(date))

  r <- GET("https://covidtracking.com/api/states")
  stop_for_status(r)
  json <- content(r, as = "text")
  state_current <- fromJSON(json)

  state_map <- state_current %>%
    filter(state %in% datasets::state.abb)


  list(confirm, death, recover, state, state_map)
}


### Function to draw world map for page 1 that present information for all countries.
Draw_world_map <- function(data_Map, geo_data) {
  # set bin and color in map for both layer
  bins <- c(0, 100, 500, 1000, 5000, 10000, 20000, 80000, 100000)
  bins2 <- c(0, 1, 10, 50, 100, 500, 1000, 3000, 5000)
  pal <- colorBin("YlOrRd", domain = geo_data$TotalCases, bins = bins)
  pal2 <- colorBin("YlOrRd", domain = geo_data$TotalDeaths, bins = bins2)

  # set labels for both layer
  labels <- sprintf(
    "<strong>%s</strong> <br/> Total Cases: %g <sup></sup>",
    geo_data$`Country/Other`, geo_data$TotalCases
  ) %>% lapply(htmltools::HTML)
  labels2 <- sprintf(
    "<strong>%s</strong> <br/> Total Deaths: %g <sup></sup>",
    geo_data$`Country/Other`, geo_data$TotalDeaths
  ) %>% lapply(htmltools::HTML)

  #draw map with two layers of Confirmed and Deaths and add lengend
  map <- leaflet(data_Map) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~ pal(geo_data$TotalCases), weight = 2, opacity = 1, color = "white",
      dashArray = "3", fillOpacity = 0.7, highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"
      ),
      group = "Total Cases"
    ) %>%
    addLegend("bottomright",
      pal = pal, values = ~ geo_data$TotalCases,
      title = "Total Cases",
      group = "Total Cases"
    ) %>%
    addPolygons(
      fillColor = ~ pal2(geo_data$TotalDeaths), weight = 2, opacity = 1, color = "white",
      dashArray = "3", fillOpacity = 0.7, highlight = highlightOptions(
        weight = 5, color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels2, labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"
      ),
      group = "Total Deaths"
    ) %>%
    addLegend("bottomright",
      pal = pal2, values = ~ geo_data$TotalDeaths,
      title = "Total Deaths",
      group = "Total Deaths"
    ) %>%
    addLayersControl(
      baseGroups = c("Total Cases", "Total Deaths"),
      options = layersControlOptions(collapsed = F)
    ) %>%
    #hide Death layer
    hideGroup("Total Deaths") %>%
    addEasyButton(easyButton(
      icon = "fa-globe", title = "Back To Global",
      onClick = JS("function(btn, map){ map.setZoom(1); }")
    )) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Locate Me",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }")
    )) %>%
    setView(30, 30, 1) %>%
    # code from the following link
    # to fix the problem that map will not change legend with corresponding layer
    # https://stackoverflow.com/questions/52393310/is-it-possible-to-switch-between-multiple-legends-when-switching-between-base-gr
    htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }")

  map
}

### Function to draw countries map for page 4 "Typical Country Data" that present information for
### all state in China, US, Canada.
### Some state may not present in the map due to lack of data.
### Input is character string to indicate country name.
countrymap <- function(country_name) {
  #read in map data for given country
  if (country_name == "Canada") {
    country <- geojsonio::geojson_read(paste0(country_name, "-provinces.geojson"), what = "sp")
  } else {
    country <- geojsonio::geojson_read(paste0(country_name, "-provinces.json"), what = "sp")
  }

  #retrieve data from https://github.com/CSSEGISandData/COVID-19
  raw_confirm <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"))
  raw_death <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"))

  #the following code is to rename/reorder to clean the data from git to fit with map
  countrydata_confirm <- raw_confirm %>%
    filter(`Country/Region` == country_name) %>%
    select(1, ncol(raw_confirm)) %>%
    rename(
      Province = `Province/State`,
      TotalCases = colnames(raw_confirm)[ncol(raw_confirm)]
    )

  countrydata_death <- raw_death %>%
    filter(`Country/Region` == country_name) %>%
    select(ncol(raw_death)) %>%
    rename(TotalDeaths = colnames(raw_death)[ncol(raw_death)])

  countrydata <- cbind(countrydata_confirm, countrydata_death) %>%
    mutate(Province = as.character(Province)) %>%
    filter(!Province %in% c("Diamond Princess", "Grand Princess"))

  countrydata[, 1][countrydata[, 1] == "Inner Mongolia"] <- "Nei Mongol"
  countrydata[, 1][countrydata[, 1] == "Xinjiang"] <- "Xinjiang Uygur"
  countrydata[, 1][countrydata[, 1] == "Tibet"] <- "Xizang"
  countrydata[, 1][countrydata[, 1] == "Ningxia"] <- "Ningxia Hui"

  countrydata <- countrydata %>%
    filter(Province %in% country$PRO)

  n <- length(country$PRO)

  province_order <-
    sapply(1:n, function(i) {
      which(match(countrydata$Province, country$PRO[i]) == 1)
    }) %>% unlist()

  country_data <- countrydata[province_order, ] %>%
    mutate(
      TotalCases = as.numeric(TotalCases),
      TotalDeaths = as.numeric(TotalDeaths)
    )


  country_Map <- country[country$PRO %in% country_data$Province, ]
  #calculate labels for two layers
  labels_country <- sprintf(
    "<strong>%s</strong> <br/> Total Cases: %g <sup></sup>",
    country_data$Province, country_data$TotalCases
  ) %>% lapply(htmltools::HTML)
  labels_country2 <- sprintf(
    "<strong>%s</strong> <br/> Total Deaths: %g <sup></sup>",
    country_data$Province, country_data$TotalDeaths
  ) %>% lapply(htmltools::HTML)

  max_case <- max(country_data$TotalCases, na.rm = T)
  max_death <- max(country_data$TotalDeaths, na.rm = T)

  ### set bins and color
  if (country_name == "China") {
    bins_country <- c(0, 100, 300, 500, 1000, 10000, max_case)
    bins_country2 <- c(0, 2, 5, 10, 30, 1000, max_death)
  } else if (country_name == "US") {
    bins_country <- c(0, 50, 100, 200, 500, 1000, max_case)
    bins_country2 <- c(0, 2, 5, 10, 30, 50, max_death)
  } else if (country_name == "Canada") {
    bins_country <- c(0, 10, 50, 100, max_case)
    bins_country2 <- c(0, 1, 3, max_death)
  }

  pal_country <- colorBin("YlOrRd",
    domain = country_data$TotalCases,
    bins = bins_country
  )
  pal_country2 <- colorBin("YlOrRd",
    domain = country_data$TotalDeaths,
    bins = bins_country2
  )

  #draw country map with labels for two layers
  Country_Map <- leaflet(country_Map) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~ pal_country(country_data$TotalCases),
      weight = 2, opacity = 1, color = "white",
      dashArray = "3", fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels_country,
      labelOptions =
        labelOptions(
          style =
            list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
          textsize = "15px",
          direction = "auto"
        ),
      group = "Total Cases"
    ) %>%
    addLegend("bottomright",
      pal = pal_country, values = ~ country_data$TotalCases,
      title = "Total Cases",
      group = "Total Cases"
    ) %>%
    addPolygons(
      fillColor = ~ pal_country2(country_data$TotalDeaths),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels_country2,
      labelOptions = labelOptions(
        style = list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "15px",
        direction = "auto"
      ),
      group = "Total Deaths"
    ) %>%
    addLegend("bottomleft",
      pal = pal_country2, values = ~ country_data$TotalDeaths,
      title = "Total Deaths", labels = "Total Deaths",
      group = "Total Deaths"
    ) %>%
    addLayersControl(
      baseGroups = c("Total Cases", "Total Deaths"),
      options = layersControlOptions(collapsed = F)
    ) %>%
    hideGroup("Total Deaths") %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Locate Me",
      onClick = JS("function(btn, map){ map.locate({setView: (110,35,2.5)}); }")
    ))

  #set initial view for the map with given country
  if (country_name == "China") {
    Country_Map %>% setView(110, 35, 2.5)
  } else if (country_name == "US") {
    Country_Map %>% setView(-110, 45, 2.5)
  } else if (country_name == "Canada") {
    Country_Map %>% setView(-90, 60, 3)
  }
}

### Function to draw map for testing information with data from https://covidtracking.com/
### It is the map under page "More information", for "U.S. Testing"
USmap <- function(url) {
  US <- geojson_read("US-provinces.json", what = "sp")

  r <- GET(url)
  stop_for_status(r)
  json <- content(r, as = "text")
  state_current <- fromJSON(json)

  state_map <- state_current %>%
    filter(state %in% datasets::state.abb)

  state_map[, 1] <- state_map$state %>%
    map(~ state.name[grep(., state.abb)]) %>%
    unlist()

  USdata <- state_map

  US_data <- USdata[order(match(USdata$state, US$PRO)), ]

  US_Map <- US[US$PRO %in% US_data$state, ]

  #prepare labels
  labels_US <- sprintf(
    "<strong>%s</strong> <br/> Positive: %g <br/> Negetive: %g <br/> Pending: %g <br/>
    Death: %g <br/> Last Update Time: %s <sup></sup>",
    US_data$state, US_data$positive, US_data$negative,
    US_data$pending, US_data$death, US_data$lastUpdateEt
  ) %>% lapply(HTML)

  max_US <- max(US_data$positive, na.rm = T)
  #calculate map bins
  bins_US <- c(0, 50, 100, 200, 500, 1000, 2000, max_US)

  pal_US <- colorBin("YlOrRd", domain = US_data$positive, bins = bins_US)

  #draw map
  US_MAP <- leaflet(US_Map) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~ pal_US(US_data$positive),
      weight = 2, opacity = 1, color = "white",
      dashArray = "3", fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels_US,
      labelOptions =
        labelOptions(
          style =
            list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
          textsize = "15px",
          direction = "auto"
        )
    ) %>%
    addLegend("bottomleft",
      pal = pal_US, values = ~ US_data$positive,
      title = "Positive Testing"
    ) %>%
    setView(-110, 45, 2.5) %>%
    addEasyButton(easyButton(
      icon = "fa-globe", title = "Back To Global",
      onClick = JS("function(btn, map){ map.setZoom(3); }")
    )) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Locate Me",
      onClick = JS("function(btn, map){ map.locate({setView: (-110,45,2.5)}); }")
    ))

  US_MAP
}


### Function to retrieve data from "http://newsapi.org/v2/top-headlines" with given coutnry name
###  in page 5 "Breaking News"
read_news <- function(country_name) {
  r <- GET(
    "http://newsapi.org/v2/top-headlines",
    query = list(
      apiKey = "edc42fafbf144ee2aef125458f424ac0",
      country = country_name,
      q = "coronavirus",
      sortBy = "publishedAt"
    )
  )
  json <- content(r, as = "text", encoding = "UTF-8")
  response <- (fromJSON(json)$articles)[1:10, ]
  # format result
  response$publishedAt <- str_replace_all(response$publishedAt, "[TZ]", " ")
  response %>% select(title, description, url, publishedAt)
}

### Function to output html text for each news in page 5 "Breaking News"
row_new_html <- function(x) {
  x <- as.list(x)
  as.character(tags$div(
    tags$strong(tags$h4(tags$a(href = x$url, x$title))),
    tags$time(x$publishedAt), tags$br(),
    tags$p("Description: ", x$description), tags$hr()
  ))
}
