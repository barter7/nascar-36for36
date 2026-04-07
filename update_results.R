#!/usr/bin/env Rscript
# update_results.R — Fetch 2026 NASCAR Cup Series race results
# Run by GitHub Actions after each race to update data/results.csv
#
# Primary source: nascaR.data package (updated weekly on CRAN)

library(dplyr)
library(readr)
library(stringr)

DATA_DIR <- "data"

# ---- Fetch from nascaR.data ----
fetch_nascaR_data <- function() {
  if (!requireNamespace("nascaR.data", quietly = TRUE)) {
    message("Installing nascaR.data from CRAN...")
    install.packages("nascaR.data", repos = "https://cloud.r-project.org")
  }

  tryCatch({
    cup <- nascaR.data::load_series("cup")

    results_2026 <- cup %>%
      filter(season == 2026) %>%
      select(
        race_number = race,
        car_number  = car,
        driver      = driver_name,
        finish_pos  = finish,
        points      = pts
      ) %>%
      mutate(
        car_number = as.integer(car_number),
        finish_pos = as.integer(finish_pos),
        points     = as.numeric(points)
      )

    if (nrow(results_2026) == 0) {
      message("No 2026 data found in nascaR.data")
      return(NULL)
    }

    message(sprintf("Fetched %d rows from nascaR.data for 2026", nrow(results_2026)))
    return(results_2026)
  }, error = function(e) {
    message("Error fetching from nascaR.data: ", e$message)
    return(NULL)
  })
}

# ---- Main update logic ----
update_results <- function() {
  results_file <- file.path(DATA_DIR, "results.csv")

  if (file.exists(results_file)) {
    existing <- read_csv(results_file, show_col_types = FALSE)
    if (nrow(existing) > 0) {
      last_race <- max(existing$race_number, na.rm = TRUE)
      message(sprintf("Existing results through race %d", last_race))
    } else {
      existing <- tibble(
        race_number = integer(), car_number = integer(),
        driver = character(), finish_pos = integer(), points = numeric()
      )
      last_race <- 0
    }
  } else {
    existing <- tibble(
      race_number = integer(), car_number = integer(),
      driver = character(), finish_pos = integer(), points = numeric()
    )
    last_race <- 0
    message("No existing results file, starting fresh")
  }

  new_data <- fetch_nascaR_data()

  if (!is.null(new_data) && nrow(new_data) > 0) {
    new_races <- new_data %>% filter(race_number > last_race)

    if (nrow(new_races) > 0) {
      updated <- bind_rows(existing, new_races) %>%
        arrange(race_number, finish_pos)
      write_csv(updated, results_file)
      message(sprintf("Added %d rows for races %d-%d",
                       nrow(new_races),
                       min(new_races$race_number),
                       max(new_races$race_number)))
    } else {
      message("No new races to add")
    }
  } else {
    message("Could not fetch new results. File unchanged.")
  }
}

if (!interactive()) {
  update_results()
}
