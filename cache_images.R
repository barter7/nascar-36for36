#!/usr/bin/env Rscript
# Download and cache all driver images locally so the Shiny app
# doesn't need to fetch them from nascar.com on every page load.
#
# Run once:  Rscript cache_images.R

library(readr)

drivers <- read_csv("data/drivers.csv", show_col_types = FALSE)

# Create directories
dir.create("www/img/headshots", recursive = TRUE, showWarnings = FALSE)
dir.create("www/img/numbers", recursive = TRUE, showWarnings = FALSE)
dir.create("www/img/manufacturers", recursive = TRUE, showWarnings = FALSE)

# --- Headshots ---
for (i in seq_len(nrow(drivers))) {
  d <- drivers[i, ]
  dest <- sprintf("www/img/headshots/%s.png", d$car_number)
  if (!file.exists(dest) && !is.na(d$headshot_url) && nchar(d$headshot_url) > 0) {
    message(sprintf("Downloading headshot for #%s %s ...", d$car_number, d$driver))
    tryCatch(download.file(d$headshot_url, dest, mode = "wb", quiet = TRUE),
             error = function(e) message("  FAILED: ", e$message))
    Sys.sleep(0.5)
  }
}

# --- Car number badges (from cf.nascar.com) ---
for (i in seq_len(nrow(drivers))) {
  d <- drivers[i, ]
  dest <- sprintf("www/img/numbers/%s.png", d$car_number)
  if (!file.exists(dest)) {
    badge_url <- sprintf("https://cf.nascar.com/data/images/carbadges/1/%s.png", d$car_number)
    message(sprintf("Downloading number badge for #%s ...", d$car_number))
    tryCatch(download.file(badge_url, dest, mode = "wb", quiet = TRUE),
             error = function(e) message("  FAILED: ", e$message))
    Sys.sleep(0.5)
  }
}

# --- Manufacturer logos ---
mfr_logos <- list(
  Toyota    = "https://www.nascar.com/wp-content/uploads/sites/7/2020/04/06/Toyota-180x180.png",
  Chevrolet = "https://www.nascar.com/wp-content/uploads/sites/7/2025/03/04/Chevrolet_2025-330x140.png",
  Ford      = "https://www.nascar.com/wp-content/uploads/sites/7/2026/02/18/Ford-Racing-Logo-300-100.png"
)

for (mfr in names(mfr_logos)) {
  dest <- sprintf("www/img/manufacturers/%s.png", tolower(mfr))
  if (!file.exists(dest)) {
    message(sprintf("Downloading %s logo ...", mfr))
    tryCatch(download.file(mfr_logos[[mfr]], dest, mode = "wb", quiet = TRUE),
             error = function(e) message("  FAILED: ", e$message))
  }
}

# --- NASCAR logo ---
dest <- "www/img/nascar_logo.svg"
if (!file.exists(dest)) {
  tryCatch(
    download.file(
      "https://www.nascar.com/wp-content/uploads/sites/7/2024/11/19/NASCAR_FullColor_onBlack_CMYK-6.svg",
      dest, mode = "wb", quiet = TRUE
    ),
    error = function(e) message("  FAILED: ", e$message)
  )
}

message("Done! All images cached in www/img/")
