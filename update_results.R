#!/usr/bin/env Rscript
# update_results.R — Fetch 2026 NASCAR Cup Series race results into data/results.csv
# Run by GitHub Actions (twice daily) and only commits when the file changes.
#
# Source: nascaR.data (refreshed upstream every Monday morning).
#
# Fantasy scoring follows the NASCAR points formula, but NASCAR awards 0 Cup
# points to drivers not running for the Cup title (e.g. Austin Hill in the #33,
# Justin Allgaier in the #48). The fantasy game still scores them, so points are
# rebuilt from finish + stage positions rather than taken from `Pts` directly.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

DATA_DIR <- "data"
SEASON <- 2026

finish_points <- function(pos) {
  case_when(is.na(pos) ~ 0, pos == 1 ~ 55, TRUE ~ pmax(37 - pos, 1))
}

stage_points <- function(pos) {
  ifelse(!is.na(pos) & pos >= 1 & pos <= 10, 11 - pos, 0)
}

fetch_results <- function() {
  cup <- tryCatch(nascaR.data::load_series("cup"), error = function(e) {
    message("Error fetching from nascaR.data: ", conditionMessage(e))
    NULL
  })
  if (is.null(cup)) return(NULL)

  season <- cup %>% filter(Season == SEASON)
  if (nrow(season) == 0) {
    message("No ", SEASON, " data found in nascaR.data")
    return(NULL)
  }

  scored <- season %>%
    mutate(
      Finish = as.integer(Finish),
      Pts = as.numeric(Pts),
      across(c(S1, S2, S3), as.integer),
      fin_pts = finish_points(Finish),
      stg12 = stage_points(S1) + stage_points(S2),
      stg3 = stage_points(S3)
    )

  # Most races have two scoring stages; a few (e.g. the Coca-Cola 600) have
  # three. Decide per race whether S3 carries stage points by checking which
  # version reproduces official points for Cup-eligible drivers. The official
  # total can also include +1 for fastest lap, so a diff of 0 or 1 matches.
  s3_by_race <- scored %>%
    filter(Pts > 0) %>%
    group_by(Race) %>%
    summarize(
      match_without = mean((Pts - fin_pts - stg12) %in% c(0, 1)),
      match_with = mean((Pts - fin_pts - stg12 - stg3) %in% c(0, 1)),
      .groups = "drop"
    ) %>%
    mutate(use_s3 = match_with > match_without)

  scored <- scored %>%
    left_join(s3_by_race %>% select(Race, use_s3), by = "Race") %>%
    mutate(
      use_s3 = coalesce(use_s3, FALSE),
      stage_pts = stg12 + ifelse(use_s3, stg3, 0),
      calc_pts = fin_pts + stage_pts,
      ineligible = is.na(Pts) | Pts <= 0,
      points = ifelse(ineligible, calc_pts, Pts)
    )

  for (i in seq_len(nrow(s3_by_race))) {
    r <- s3_by_race[i, ]
    message(sprintf("Race %2d: formula matches %.0f%% of official points%s",
      r$Race, 100 * max(r$match_with, r$match_without),
      if (r$use_s3) " (3 scoring stages)" else ""))
  }
  fixed <- scored %>% filter(ineligible)
  message(sprintf("Rebuilt points for %d non-Cup-eligible entries (e.g. %s)",
    nrow(fixed), paste(head(unique(fixed$Driver), 4), collapse = ", ")))

  scored %>%
    transmute(
      race_number = as.integer(Race),
      car_number = as.integer(Car),
      driver = Driver,
      finish_pos = Finish,
      start_pos = as.integer(Start),
      laps_led = as.integer(Led),
      stage_pts = as.integer(stage_pts),
      points = as.integer(points),
      calc = as.integer(ineligible)
    ) %>%
    arrange(race_number, finish_pos)
}

update_results <- function() {
  results_file <- file.path(DATA_DIR, "results.csv")
  new_data <- fetch_results()

  if (is.null(new_data) || nrow(new_data) == 0) {
    message("Could not fetch new results. File unchanged.")
    return(invisible())
  }

  if (file.exists(results_file)) {
    existing <- read_csv(results_file, show_col_types = FALSE)
    if (nrow(existing) > 0 &&
        max(new_data$race_number) < max(existing$race_number, na.rm = TRUE)) {
      message("Fetched data has fewer races than existing file; keeping existing.")
      return(invisible())
    }
  }

  write_csv(new_data, results_file, na = "")
  message(sprintf("Wrote %d rows for %d races (through race %d)",
    nrow(new_data), n_distinct(new_data$race_number), max(new_data$race_number)))
}

if (!interactive()) {
  update_results()
}
