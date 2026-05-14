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
      filter(Season == 2026) %>%
      select(
        race_number = Race,
        car_number  = Car,
        driver      = Driver,
        finish_pos  = Finish,
        points      = Pts
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
      last_race <- 0
    }
  } else {
    last_race <- 0
    message("No existing results file, starting fresh")
  }

  new_data <- fetch_nascaR_data()

  if (!is.null(new_data) && nrow(new_data) > 0) {
    # Replace all results with fresh data from nascaR.data
    updated <- new_data %>% arrange(race_number, finish_pos)
    write_csv(updated, results_file)
    n_races <- n_distinct(updated$race_number)
    message(sprintf("Wrote %d rows for %d races (races 1-%d)",
                     nrow(updated), n_races, max(updated$race_number)))
  } else {
    message("Could not fetch new results. File unchanged.")
  }
}

if (!interactive()) {
  update_results()
}
