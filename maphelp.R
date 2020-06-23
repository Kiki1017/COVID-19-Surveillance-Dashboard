library(dplyr)
library(leaflet)
library(readr)
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(dplyr)
library(leaflet)
library(readr)
library(sf)
library(ggplot2)
map_breaks <- function(.data, var, newvar, classes, style, clean_labels = TRUE, dig_lab = 10){
      
      # save parameters to list
      paramList <- as.list(match.call())
      
      # quote input variables
      if (!is.character(paramList$var)) {
            ref <- rlang::enquo(var)
      } else if (is.character(paramList$var)) {
            ref <- rlang::quo(!! rlang::sym(var))
      }
      
      refQ <- rlang::quo_name(rlang::enquo(ref))
      
      if (!is.character(paramList$newvar)) {
            new <- rlang::enquo(newvar)
      } else if (is.character(paramList$newvar)) {
            new <- rlang::quo(!! rlang::sym(newvar))
      }
      
      newQ <- rlang::quo_name(rlang::enquo(new))
      
      # calculate breaks and categories
      breaks <- classInt::classIntervals(.data[[refQ]], n = classes, style = style)
      categories <- cut(.data[[refQ]], breaks = c(breaks$brks), include.lowest = TRUE, dig.lab = dig_lab)
      
      # create new variable
      .data <- dplyr::mutate(.data, !!newQ := categories)
      
      # clean labels
      if (clean_labels == TRUE){
            
            .data[[newQ]] %>%
                  forcats::fct_relabel(~ gsub(",", " - ", .x)) %>%
                  forcats::fct_relabel(~ gsub("\\(", "", .x)) %>%
                  forcats::fct_relabel(~ gsub("\\[", "", .x)) %>%
                  forcats::fct_relabel(~ gsub("\\]", "", .x)) -> .data[[newQ]]
            
      }
      
      # return result
      return(.data)
      
}

map_bins <- function(.data, var, classes = 5, style = "jenks", round = 0, dig_lab = 10){
      
      # calculate breaks
      breaks <- classInt::classIntervals(.data[[var]], n = classes, style = style)
      categories <- cut(.data[[var]], breaks = c(breaks$brks), include.lowest = TRUE, dig.lab = dig_lab)
      
      # parse categories
      categories <- levels(categories)
      # categories <- unique(categories)
      categories <- gsub("[][()]", "", categories)
      categories <- gsub(",", " ", categories)
      categories <- stringr::word(categories, 2)
      categories <- round(as.numeric(categories), digits = round)
      bins <- c(0, categories)
      
      # return output
      return(bins)
      
}
round_any <- function(x, accuracy, f=round){
      
      f(x/ accuracy) * accuracy
      
}

state_data <- read_csv("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/state/state_full.csv") %>%
      filter(state == "Missouri") %>%
      filter(report_date >= "2020-03-07") %>%
      arrange(desc(report_date))

state_test_data <- read_csv("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/state/state_testing.csv")

stl_metro_data <- read_csv("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/metro_all/metro_full.csv") %>%
      filter(short_name == "St. Louis") %>%
      filter(report_date >= "2020-03-07") %>%
      arrange(desc(report_date))

kc_metro_data <- read_csv("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/metro_all/metro_full.csv") %>%
      filter(short_name == "Kansas City") %>%
      filter(report_date >= "2020-03-07") %>%
      arrange(desc(report_date))

stl_city_data <- read_csv("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/county/county_full.csv") %>%
      filter(geoid == "29510") %>%
      filter(report_date >= "2020-03-18") %>%
      arrange(desc(report_date))

stl_county_data <- read_csv("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/county/county_full.csv") %>%
      filter(geoid == "29189") %>%
      filter(report_date >= "2020-03-07") %>%
      arrange(desc(report_date))

kc_city_data <- read_csv("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/county/county_full.csv") %>%
      filter(geoid == "29511") %>%
      filter(report_date >= "2020-03-20") %>%
      arrange(desc(report_date))

snapshot <- st_read("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/county/daily_snapshot_mo_xl.geojson",crs = 4326,
                    stringsAsFactors = FALSE) %>%
      mutate(icu_rate = ifelse(hospitals == 0, NA, icu_rate)) %>%
      mutate(avg_rate = (case_avg/total_pop)*1000)

zip_snapshot <- st_read("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/zip/daily_snapshot_city_county.geojson", stringsAsFactors = FALSE) 

stl_hosp <-  read_csv("https://raw.githubusercontent.com/slu-openGIS/covid_daily_viz/master/data/metro/stl_hospital.csv")

state_data %>%
      select(report_date, cases, new_cases, case_avg, deaths, new_deaths, deaths_avg) %>%
      mutate(
            case_avg = round(case_avg, digits = 2),
            deaths_avg = round(deaths_avg, digits = 2)
      ) %>%
      rename(
            `Report Date` = report_date,
            `Cumulative Cases` = cases,
            `New Cases` = new_cases,
            `Average New Cases` = case_avg,
            `Cumulative Deaths` = deaths,
            `New Deaths` = new_deaths,
            `Average New Deaths` = deaths_avg
      ) -> data_table

stl_metro_data %>%
      select(report_date, cases, new_cases, case_avg, deaths, new_deaths, deaths_avg) %>%
      mutate(
            case_avg = round(case_avg, digits = 2),
            deaths_avg = round(deaths_avg, digits = 2)
      ) %>%
      rename(
            `Report Date` = report_date,
            `Cumulative Cases` = cases,
            `New Cases` = new_cases,
            `Average New Cases` = case_avg,
            `Cumulative Deaths` = deaths,
            `New Deaths` = new_deaths,
            `Average New Deaths` = deaths_avg
      ) -> data_table

stl_city_data %>%
      select(report_date, cases, new_cases, case_avg, deaths, new_deaths, deaths_avg) %>%
      mutate(
            case_avg = round(case_avg, digits = 2),
            deaths_avg = round(deaths_avg, digits = 2)
      ) %>%
      rename(
            `Report Date` = report_date,
            `Cumulative Cases` = cases,
            `New Cases` = new_cases,
            `Average New Cases` = case_avg,
            `Cumulative Deaths` = deaths,
            `New Deaths` = new_deaths,
            `Average New Deaths` = deaths_avg
      ) -> data_table

stl_county_data %>%
      select(report_date, cases, new_cases, case_avg, deaths, new_deaths, deaths_avg) %>%
      mutate(
            case_avg = round(case_avg, digits = 2),
            deaths_avg = round(deaths_avg, digits = 2)
      ) %>%
      rename(
            `Report Date` = report_date,
            `Cumulative Cases` = cases,
            `New Cases` = new_cases,
            `Average New Cases` = case_avg,
            `Cumulative Deaths` = deaths,
            `New Deaths` = new_deaths,
            `Average New Deaths` = deaths_avg
      ) -> data_table

kc_metro_data %>%
      select(report_date, cases, new_cases, case_avg, deaths, new_deaths, deaths_avg) %>%
      mutate(
            case_avg = round(case_avg, digits = 2),
            deaths_avg = round(deaths_avg, digits = 2)
      ) %>%
      rename(
            `Report Date` = report_date,
            `Cumulative Cases` = cases,
            `New Cases` = new_cases,
            `Average New Cases` = case_avg,
            `Cumulative Deaths` = deaths,
            `New Deaths` = new_deaths,
            `Average New Deaths` = deaths_avg
      ) -> data_table


kc_city_data %>%
      select(report_date, cases, new_cases, case_avg, deaths, new_deaths, deaths_avg) %>%
      mutate(
            case_avg = round(case_avg, digits = 2),
            deaths_avg = round(deaths_avg, digits = 2)
      ) %>%
      rename(
            `Report Date` = report_date,
            `Cumulative Cases` = cases,
            `New Cases` = new_cases,
            `Average New Cases` = case_avg,
            `Cumulative Deaths` = deaths,
            `New Deaths` = new_deaths,
            `Average New Deaths` = deaths_avg
      ) -> data_table

      
bins <- map_bins(snapshot, var = "icu_rate", style = "fisher", classes = 5, dig_lab = 2,
                 round = 2)

pal <- colorBin("PuRd", domain = snapshot$icu_rate, bins = bins)

icu<-leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
            data = snapshot,
            color = "#444444", 
            weight = 1, 
            opacity = 1.0, 
            smoothFactor = 0.5,
            fillOpacity = 0.5,
            fillColor = ~pal(icu_rate),
            highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            popup = paste("<b>County:</b> ", snapshot$county, "<br>",
                          "<b>Hospital Count:</b> ", snapshot$hospitals, "<br>",
                          "<b>Baseline ICU Beds:</b> ", snapshot$icu_beds, "<br>",
                          "<b>Baseline ICU Bed Rate per 1,000 Residents:</b> ", round(snapshot$icu_rate, digits = 3))
      )  %>%
      addLegend(pal = pal, values = snapshot$icu_rate, opacity = .5, title = "ICU Rate")

bins <- map_bins(snapshot, var = "case_rate", style = "fisher", classes = 5, dig_lab = 2,
                 round = 2)
bins[length(bins)] <- round_any(x = max(snapshot$case_rate), accuracy = .01, f = ceiling)

pal <- colorBin("BuGn", domain = snapshot$case_rate, bins = bins)

case_rate<-leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
            data = snapshot,
            color = "#444444", 
            weight = 1, 
            opacity = 1.0, 
            smoothFactor = 0.5,
            fillOpacity = 0.75,
            fillColor = ~pal(case_rate),
            highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            popup = paste("<b>County:</b> ", snapshot$county, "<br>",
                          "<b>Current Cumulative Count:</b> ", snapshot$cases, "<br>",
                          "<b>Current Cumulative Rate per 1,000:</b> ", round(snapshot$case_rate, digits = 2), "<br>",
                          "<b>Hospital Count:</b> ", snapshot$hospitals, "<br>",
                          "<b>Baseline ICU Beds:</b> ", snapshot$icu_beds)
      )  %>%
      addLegend(pal = pal, values = snapshot$case_rate, opacity = .75, title = "Rate")


snapshot %>%
      select(state, county, cases, case_rate) %>%
      mutate(
            case_rate = round(case_rate, digits = 2)
      ) %>%
      rename(
            State = state,
            County = county,
            `Cumulative Cases` = cases,
            `Rate per 1,000 Residents` = case_rate
      ) %>%
      arrange(State, County) -> data_table
st_geometry(data_table) <- NULL



bins <- map_bins(snapshot, var = "avg_rate", style = "fisher", classes = 5, dig_lab = 2,
                 round = 2)
bins[length(bins)] <- round_any(x = max(snapshot$avg_rate), accuracy = .01, f = ceiling)

pal <- colorBin("RdPu", domain = snapshot$avg_rate, bins = bins)

avg_rate<-leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
            data = snapshot,
            color = "#444444", 
            weight = 1, 
            opacity = 1.0, 
            smoothFactor = 0.5,
            fillOpacity = 0.75,
            fillColor = ~pal(avg_rate),
            highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            popup = paste("<b>County:</b> ", snapshot$county, "<br>",
                          "<b>Current Cumulative Count:</b> ", snapshot$cases, "<br>",
                          "<b>7-day Average of New Cases:</b> ", round(snapshot$case_avg, digits = 2), "<br>",
                          "<b>Average New Cases per 1,000 Residents:</b> ", round(snapshot$avg_rate, digits = 2), "<br>",
                          "<b>Hospital Count:</b> ", snapshot$hospitals, "<br>",
                          "<b>Baseline ICU Beds:</b> ", snapshot$icu_beds)
      )  %>%
      addLegend(pal = pal, values = snapshot$avg_rate, opacity = .75, title = "7-day Average Rate")


snapshot %>%
      select(state, county, cases, case_avg, avg_rate) %>%
      mutate(
            case_avg = round(case_avg, digits = 2)
      ) %>%
      rename(
            State = state,
            County = county,
            `Cumulative Cases` = cases,
            `7-day Average New Cases` = case_avg,
            `Average New Cases per 1,000 Residents` = avg_rate
      ) %>%
      arrange(State, County) -> data_table
st_geometry(data_table) <- NULL


bins <- map_bins(snapshot, var = "mortality_rate", style = "fisher", classes = 5, dig_lab = 2,
                 round = 2)
bins[length(bins)] <- round_any(x = max(snapshot$mortality_rate), accuracy = .01, f = ceiling)

pal <- colorBin("YlGn", domain = snapshot$mortality_rate, bins = bins)

mort_rate<-leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
            data = snapshot,
            color = "#444444", 
            weight = 1, 
            opacity = 1.0, 
            smoothFactor = 0.5,
            fillOpacity = 0.75,
            fillColor = ~pal(mortality_rate),
            highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            popup = paste("<b>County:</b> ", snapshot$county, "<br>",
                          "<b>Current Mortality Count:</b> ", snapshot$deaths, "<br>",
                          "<b>Current Mortality Rate per 1,000:</b> ", round(snapshot$mortality_rate, digits = 2), "<br>",
                          "<b>Hospital Count:</b> ", snapshot$hospitals, "<br>",
                          "<b>Baseline ICU Beds:</b> ", snapshot$icu_beds)
      )  %>%
      addLegend(pal = pal, values = snapshot$mortality_rate, opacity = .75, title = "Rate")


snapshot %>%
      select(state, county, deaths, mortality_rate, case_fatality_rate) %>%
      mutate(
            mortality_rate = round(mortality_rate, digits = 2),
            case_fatality_rate = round(case_fatality_rate, digits = 2)
      ) %>%
      rename(
            State = state,
            County = county,
            `Cumulative Deaths` = deaths,
            `Rate per 1,000 Residents` = mortality_rate,
            `Case Fatality (%)` = case_fatality_rate
      ) %>%
      arrange(State, County) -> data_table
st_geometry(data_table) <- NULL

bins <- map_bins(zip_snapshot, var = "case_rate", style = "fisher", classes = 5, dig_lab = 2,
                 round = 2)
bins[1] <- round_any(x = min(zip_snapshot$case_rate, na.rm = TRUE), accuracy = .01, f = floor)
bins[length(bins)] <- round_any(x = max(zip_snapshot$case_rate, na.rm = TRUE), accuracy = .01, f = ceiling)

pal <- colorBin("BuGn", domain = zip_snapshot$case_rate, bins = bins)

demo<-leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
            data = zip_snapshot,
            color = "#444444", 
            weight = 1, 
            opacity = 1.0, 
            smoothFactor = 0.5,
            fillOpacity = 0.75,
            fillColor = ~pal(case_rate),
            highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
            popup = paste0("<b>Zip:</b> ", zip_snapshot$zip, " <br> ",
                           "<b>Current Cumulative Count:</b> ", zip_snapshot$cases, "<br>",
                           "<b>Current Cumulative Rate per 1,000:</b> ", round(zip_snapshot$case_rate, digits = 2), " <br> ",
                           "<b>Population Below Poverty Line:</b> ", round(zip_snapshot$pvty_pct, digits = 2), "% <br> ",
                           "<b>African American Population:</b> ", round(zip_snapshot$blk_pct, digits = 2), "% ")
      )  %>%
      addLegend(pal = pal, values = zip_snapshot$case_rate, opacity = .75, title = "Rate")

zip_snapshot %>%
      select(zip, cases, case_rate, pvty_pct, blk_pct) %>%
      mutate(
            confirmed_rate = round(case_rate, digits = 2),
            pvty_pct = round(pvty_pct, digits = 2),
            blk_pct = round(blk_pct, digits = 2)
      ) %>%
      rename(
            `Zip Code` = zip,
            `Cumulative Cases` = cases,
            `Rate per 1,000 Residents` = case_rate,
            `Poverty Rate (%)` = pvty_pct,
            `% African American` = blk_pct
      ) -> data_table
st_geometry(data_table) <- NULL


stl_hosp %>%
  arrange(desc(report_date)) %>%
  mutate(
    new_in_pt_avg = round(new_in_pt_avg, digits = 2),
    in_pt_avg = round(in_pt_avg, digits = 2),
    icu_avg = round(icu_avg, digits = 2),
    vent_avg = round(vent_avg, digits = 2)
  ) %>%
  rename(
    `Report Date` = report_date,
    `New Patients` = new_in_pt,
    `New Patients Average` = new_in_pt_avg,
    `Total Patients` = in_pt,
    `Total Patients Average` = in_pt_avg,
    `ICU Patients` = icu,
    `ICU Patients Average` = icu_avg,
    `Ventilated Patients` = vent,
    `Ventiled Patients Average` = vent_avg,
    `Cumulative Discharged Patients` = discharge,
    `Daily Discharged Patients` = new_discharge
  ) -> data_table
