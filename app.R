############################################################################
# NASCAR 36 for 36 - 2026 Season Tracker
# Shiny App for tracking a 4-person pick'em league
# Dark racing theme with driver headshots, car numbers, manufacturer logos
############################################################################

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
if (requireNamespace("png", quietly = TRUE)) library(png)

# ---- Configuration ----
PARTICIPANTS <- c("Mike", "Matt", "Brian", "Tom")
PARTICIPANT_COLORS <- c(
  "Mike"  = "#E41A1C",
  "Matt"  = "#377EB8",
  "Brian" = "#4DAF4A",
  "Tom"   = "#FF7F00"
)

# Manufacturer logo URLs (used as fallback if local cache missing)
MFR_LOGOS <- list(
  Toyota    = "https://www.nascar.com/wp-content/uploads/sites/7/2020/04/06/Toyota-180x180.png",
  Chevrolet = "https://www.nascar.com/wp-content/uploads/sites/7/2025/03/04/Chevrolet_2025-330x140.png",
  Ford      = "https://www.nascar.com/wp-content/uploads/sites/7/2026/02/18/Ford-Racing-Logo-300-100.png"
)

# Car number badge URL pattern (consistent for all cars)
car_badge_url <- function(car_number) {
  paste0("https://cf.nascar.com/data/images/carbadges/1/", car_number, ".png")
}

# ---- Image helpers ----
# Prefer local cached image; fall back to remote URL
headshot_src <- function(car_number, headshot_url) {
  local <- paste0("img/headshots/", car_number, ".png")
  if (file.exists(file.path("www", local))) local else headshot_url
}

number_src <- function(car_number, number_url = NA) {
  local <- paste0("img/numbers/", car_number, ".png")
  if (file.exists(file.path("www", local))) local else car_badge_url(car_number)
}

mfr_src <- function(manufacturer) {
  local <- paste0("img/manufacturers/", tolower(manufacturer), ".png")
  if (file.exists(file.path("www", local))) local else MFR_LOGOS[[manufacturer]]
}

# ---- Load data ----
load_schedule <- function(year = 2026) {
  f <- if (year == 2025) "data/schedule_2025.csv" else "data/schedule.csv"
  if (!file.exists(f)) {
    # 2025 doesn't have a schedule — create a generic one
    return(tibble(race_num = 1:36, date = as.Date(NA),
                  track = paste("Race", 1:36), track_short = paste0("R", 1:36)))
  }
  read_csv(f, show_col_types = FALSE) %>%
    mutate(date = as.Date(date), race_num = as.integer(race_num))
}

load_drivers <- function() {
  read_csv("data/drivers.csv", show_col_types = FALSE)
}

load_results <- function(year = 2026) {
  f <- if (year == 2025) "data/results_2025.csv" else "data/results.csv"
  results <- read_csv(f, show_col_types = FALSE)
  if (nrow(results) == 0) return(results)
  results %>%
    mutate(
      race_number = as.integer(race_number),
      car_number  = as.integer(car_number),
      finish_pos  = as.integer(finish_pos),
      points      = as.numeric(points)
    )
}

load_picks <- function(year = 2026) {
  f <- if (year == 2025) "data/picks_2025.csv" else "data/picks.csv"
  read_csv(f, show_col_types = FALSE)
}

# ---- Transform picks from wide to long ----
picks_to_long <- function(picks_wide) {
  picks_wide %>%
    pivot_longer(cols = starts_with("race_"), names_to = "race_num", values_to = "car_number") %>%
    mutate(
      race_number = as.integer(str_extract(race_num, "\\d+")),
      car_number  = as.integer(car_number)
    ) %>%
    filter(!is.na(car_number)) %>%
    select(participant, race_number, car_number)
}

# ---- Join picks to results ----
compute_scores <- function(picks_long, results) {
  if (nrow(results) == 0 || nrow(picks_long) == 0) {
    return(tibble(
      participant = character(), race_number = integer(),
      car_number = integer(), driver = character(),
      finish_pos = integer(), points = numeric()
    ))
  }
  picks_long %>%
    inner_join(
      results %>% select(race_number, car_number, driver, finish_pos, points),
      by = c("race_number", "car_number")
    )
}

# ---- Cumulative standings ----
compute_standings <- function(scores, schedule) {
  if (nrow(scores) == 0) return(tibble())
  race_points <- scores %>%
    group_by(participant, race_number) %>%
    summarise(points = sum(points, na.rm = TRUE), .groups = "drop")
  race_points %>%
    arrange(participant, race_number) %>%
    group_by(participant) %>%
    mutate(cumulative_points = cumsum(points)) %>%
    ungroup() %>%
    left_join(schedule %>% select(race_num, track_short),
              by = c("race_number" = "race_num"))
}

# ---- Value over driver's season average ----
compute_value <- function(scores, results) {
  if (nrow(scores) == 0 || nrow(results) == 0) return(tibble())
  driver_avg <- results %>%
    group_by(car_number, driver) %>%
    summarise(avg_points = mean(points, na.rm = TRUE), .groups = "drop")
  scores %>%
    left_join(driver_avg, by = c("car_number", "driver")) %>%
    mutate(value = points - avg_points)
}

# ---- Weekly rankings ----
compute_weekly_ranks <- function(scores) {
  if (nrow(scores) == 0) return(tibble())
  scores %>%
    group_by(race_number) %>%
    mutate(weekly_rank = rank(-points, ties.method = "min")) %>%
    ungroup()
}

# ---- Build driver card HTML ----
build_driver_card <- function(car_number, driver, team, manufacturer,
                               headshot_url = NA, number_url = NA,
                               highlight = FALSE, used = FALSE, extra_info = "",
                               badge_html = "") {
  hs <- if (!is.na(headshot_url) && nchar(headshot_url) > 0) {
    headshot_src(car_number, headshot_url)
  } else ""
  ns <- number_src(car_number)
  ms <- mfr_src(manufacturer)

  border_color <- if (highlight) "#FFD700" else if (used) "#555" else "#333"
  opacity <- if (used) "0.5" else "1"

  # Full-bleed driver photo as card background
  bg_style <- if (nchar(hs) > 0) {
    sprintf("background-image:url('%s');background-size:cover;background-position:top center;", hs)
  } else {
    "background:#1a1a2e;"
  }

  # Car number badge (top-left) — use image if available, else styled text
  number_html <- if (nchar(ns) > 0) {
    sprintf('<img src="%s" alt="#%s" style="height:35px;filter:drop-shadow(0 1px 3px rgba(0,0,0,0.8));" onerror="this.style.display=\'none\';this.nextElementSibling.style.display=\'block\'"><span style="display:none;font-family:Orbitron,sans-serif;font-size:18px;font-weight:bold;color:#FFD700;text-shadow:0 1px 4px rgba(0,0,0,0.9);">#%s</span>', ns, car_number, car_number)
  } else {
    sprintf('<span style="font-family:Orbitron,sans-serif;font-size:18px;font-weight:bold;color:#FFD700;text-shadow:0 1px 4px rgba(0,0,0,0.9);">#%s</span>', car_number)
  }

  # Manufacturer logo (top-right)
  mfr_html <- if (!is.null(ms) && !is.na(ms) && nchar(ms) > 0) {
    sprintf('<img src="%s" alt="%s" style="height:22px;filter:drop-shadow(0 1px 2px rgba(0,0,0,0.8));">', ms, manufacturer)
  } else ""

  # Fallback for no headshot — show big number
  no_photo_html <- if (nchar(hs) == 0) {
    sprintf('<div style="display:flex;align-items:center;justify-content:center;height:100%%;font-family:Orbitron,sans-serif;font-size:48px;font-weight:bold;color:#FFD700;text-shadow:0 2px 8px rgba(0,0,0,0.5);">#%s</div>', car_number)
  } else ""

  badge_div <- if (nchar(badge_html) > 0) {
    sprintf('<div style="position:absolute;top:-12px;left:50%%;transform:translateX(-50%%);z-index:10;display:flex;gap:2px;">%s</div>', badge_html)
  } else ""

  info_div <- if (nchar(extra_info) > 0) {
    sprintf('<div style="background:#12121f;padding:5px 8px;font-size:12px;color:#FFD700;text-align:center;font-family:Rajdhani,sans-serif;font-weight:600;">%s</div>', extra_info)
  } else ""

  sprintf(
    '<div style="display:inline-block;width:155px;margin:14px 6px 6px;position:relative;border:2px solid %s;border-radius:10px;overflow:visible;opacity:%s;vertical-align:top;box-shadow:0 4px 12px rgba(0,0,0,0.5);">
      %s
      <div style="position:relative;width:100%%;height:190px;border-radius:8px 8px 0 0;overflow:hidden;%s">
        %s
        <div style="position:absolute;top:6px;left:6px;">%s</div>
        <div style="position:absolute;top:6px;right:6px;">%s</div>
        <div style="position:absolute;bottom:0;left:0;right:0;background:linear-gradient(transparent, rgba(0,0,0,0.85));padding:8px 8px 6px;">
          <div style="font-family:Rajdhani,sans-serif;font-size:14px;font-weight:700;color:#fff;line-height:1.2;">%s</div>
          <div style="font-size:11px;color:#bbb;">%s</div>
        </div>
      </div>
      %s
    </div>',
    border_color, opacity,
    badge_div,
    bg_style,
    no_photo_html,
    number_html,
    mfr_html,
    driver,
    team,
    info_div
  )
}

# ===========================================================================
# UI
# ===========================================================================
ui <- page_navbar(
  title = tags$span(
    style = "font-family:Orbitron,sans-serif;font-weight:700;letter-spacing:2px;display:flex;align-items:center;gap:10px;",
    "NASCAR 36 for 36",
    tags$div(style = "display:inline-flex;gap:4px;",
      actionButton("year_2026", "2026", class = "btn-sm",
        style = "background:#FFD700;color:#000;border:none;font-family:Orbitron,sans-serif;font-size:0.6em;padding:3px 8px;border-radius:4px;"),
      actionButton("year_2025", "2025", class = "btn-sm",
        style = "background:#444;color:#aaa;border:none;font-family:Orbitron,sans-serif;font-size:0.6em;padding:3px 8px;border-radius:4px;")
    )
  ),
  theme = bs_theme(
    version = 5,
    bg = "#0d0d1a",
    fg = "#e0e0e0",
    primary = "#FFD700",
    secondary = "#333",
    "navbar-bg" = "#0d0d1a",
    "card-bg" = "#161625",
    "card-border-color" = "#333"
  ),
  header = tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&family=Rajdhani:wght@400;600;700&display=swap"
    ),
    tags$style(HTML("
      body { background-color: #0d0d1a; font-family: Rajdhani, sans-serif; }
      .navbar { border-bottom: 2px solid #FFD700; }
      .navbar-toggler { display: none !important; }
      .navbar-collapse { display: flex !important; }
      .nav-link { font-family: Orbitron, sans-serif; font-size: 0.65em; letter-spacing: 0.5px; padding: 6px 8px !important; }
      .card { border-radius: 12px; box-shadow: 0 2px 12px rgba(0,0,0,0.4); }
      .card-header { font-family: Orbitron, sans-serif; font-size: 0.95em;
                     letter-spacing: 1px; color: #FFD700; background: #1a1a2e;
                     border-bottom: 1px solid #333; }
      .stat-card { text-align: center; padding: 8px 4px; }
      .stat-card .stat-value { font-family: Orbitron, sans-serif; font-size: 1.1em; font-weight: bold; }
      .stat-card .stat-label { font-size: 0.75em; color: #888; }
      .table { color: #e0e0e0; }
      .table thead th { color: #FFD700; font-family: Orbitron, sans-serif; font-size: 0.8em; }
      .sidebar { background: #12121f; border-right: 1px solid #333; }
      .form-select, .form-control { background: #1e1e2e; color: #e0e0e0; border-color: #444; }
    "))
  ),

  # ---- Standings Tab ----
  nav_panel("Standings",
    layout_sidebar(fillable = FALSE, sidebar = NULL,
      card(card_header("Overall Standings"), tableOutput("standings_table")),
      card(card_header("Points by Stage"), tableOutput("stage_table")),
      div(style = "display:flex;gap:8px;flex-wrap:nowrap;overflow-x:auto;",
        div(style = "flex:1;min-width:0;", uiOutput("leader_card")),
        div(style = "flex:1;min-width:0;", uiOutput("most_points_week_card")),
        div(style = "flex:1;min-width:0;", uiOutput("best_value_card")),
        div(style = "flex:1;min-width:0;", uiOutput("races_completed_card"))
      ),
      card(card_header("Stage Winners"),
        div(style = "display:flex;gap:8px;flex-wrap:nowrap;overflow-x:auto;margin-bottom:8px;",
          div(style = "flex:1;min-width:0;", uiOutput("stage1_card")),
          div(style = "flex:1;min-width:0;", uiOutput("stage2_card")),
          div(style = "flex:1;min-width:0;", uiOutput("stage3_card"))
        ),
        div(style = "display:flex;gap:8px;flex-wrap:nowrap;overflow-x:auto;",
          div(style = "flex:1;min-width:0;", uiOutput("stage4_card")),
          div(style = "flex:1;min-width:0;", uiOutput("stage5_card")),
          div(style = "flex:1;min-width:0;", uiOutput("stage6_card"))
        )
      ),
      card(card_header("Cumulative Points"), plotOutput("cumulative_chart", height = "400px"))
    )
  ),

  # ---- Roster Tab ----
  nav_panel("Roster",
    layout_sidebar(fillable = FALSE, sidebar = NULL,
      card(card_header("2026 NASCAR Cup Series Roster"),
           uiOutput("roster_cards"))
    )
  ),

  # ---- Weekly Results Tab ----
  nav_panel("Weekly Results",
    layout_sidebar(fillable = FALSE, sidebar = NULL,
      uiOutput("race_buttons"),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Picks & Points"), uiOutput("weekly_cards")),
        card(card_header("Weekly Points"), plotOutput("weekly_bar", height = "300px"))
      )
    )
  ),

  # ---- Pick History Tab ----
  nav_panel("Pick History",
    layout_sidebar(fillable = FALSE, sidebar = NULL,
      card(card_header("All Picks - Car # & Points Earned"),
           div(style = "overflow-x: auto;", uiOutput("picks_grid_visual"))),
      card(card_header("Value Over Average (Points vs Driver Season Avg)"),
           div(style = "overflow-x: auto;", tableOutput("value_grid")))
    )
  ),

  # ---- Rankings Tab ----
  nav_panel("Rankings",
    layout_sidebar(fillable = FALSE, sidebar = NULL,
      card(card_header("Position Over Time"),
           plotOutput("rankings_chart", height = "400px")),
      card(card_header("Weekly Rank (1 = Best Pick That Week)"),
           div(style = "overflow-x: auto;", tableOutput("rank_grid")))
    )
  ),

  # ---- Drivers Used Tab ----
  nav_panel("Drivers Used",
    layout_sidebar(fillable = FALSE, sidebar = NULL,
      div(style = "display:flex;justify-content:center;gap:10px;margin-bottom:16px;flex-wrap:wrap;",
        actionButton("pick_Mike", "Mike", class = "btn-sm",
          style = "background:#E41A1C;color:#fff;border:none;font-family:Orbitron,sans-serif;font-size:0.85em;padding:6px 18px;"),
        actionButton("pick_Matt", "Matt", class = "btn-sm",
          style = "background:#377EB8;color:#fff;border:none;font-family:Orbitron,sans-serif;font-size:0.85em;padding:6px 18px;"),
        actionButton("pick_Brian", "Brian", class = "btn-sm",
          style = "background:#4DAF4A;color:#fff;border:none;font-family:Orbitron,sans-serif;font-size:0.85em;padding:6px 18px;"),
        actionButton("pick_Tom", "Tom", class = "btn-sm",
          style = "background:#FF7F00;color:#fff;border:none;font-family:Orbitron,sans-serif;font-size:0.85em;padding:6px 18px;")
      ),
      layout_columns(col_widths = c(6, 6),
        card(card_header(uiOutput("used_header_text")), uiOutput("used_drivers_cards")),
        card(card_header(uiOutput("avail_header_text")), uiOutput("available_drivers_cards"))
      )
    )
  )
)

# ===========================================================================
# Server
# ===========================================================================
server <- function(input, output, session) {

  # ---- Year toggle ----
  selected_year <- reactiveVal(2026)

  observeEvent(input$year_2026, selected_year(2026))
  observeEvent(input$year_2025, selected_year(2025))

  schedule <- reactive(load_schedule(selected_year()))
  drivers  <- reactive(load_drivers())
  results  <- reactive(load_results(selected_year()))
  picks_wide <- reactive(load_picks(selected_year()))

  picks_long <- reactive({
    pw <- picks_wide()
    if (is.null(pw) || nrow(pw) == 0) return(tibble())
    picks_to_long(pw)
  })

  scores <- reactive(compute_scores(picks_long(), results()))
  standings <- reactive(compute_standings(scores(), schedule()))
  value_scores <- reactive(compute_value(scores(), results()))
  weekly_ranks <- reactive(compute_weekly_ranks(scores()))

  completed_races <- reactive({
    res <- results()
    if (nrow(res) == 0) return(integer(0))
    sort(unique(res$race_number))
  })

  selected_race <- reactiveVal(NULL)

  output$race_buttons <- renderUI({
    cr <- completed_races()
    sched <- schedule()
    if (length(cr) == 0) return(tags$p(style = "color:#888;", "No races completed yet"))
    # Auto-select latest race if none selected
    if (is.null(selected_race()) || !selected_race() %in% cr) selected_race(max(cr))
    btns <- lapply(cr, function(r) {
      label <- sched$track_short[match(r, sched$race_num)]
      if (is.na(label)) label <- paste0("R", r)
      is_active <- identical(as.integer(r), as.integer(selected_race()))
      bg <- if (is_active) "#FFD700" else "#2a2a3a"
      fg <- if (is_active) "#000" else "#aaa"
      actionButton(
        paste0("race_btn_", r), paste0(r, ". ", label),
        class = "btn-sm",
        style = sprintf("background:%s;color:%s;border:1px solid #444;font-family:Rajdhani,sans-serif;font-size:0.8em;padding:4px 8px;margin:2px;", bg, fg)
      )
    })
    div(style = "display:flex;flex-wrap:wrap;justify-content:center;margin-bottom:12px;", btns)
  })

  observe({
    cr <- completed_races()
    lapply(cr, function(r) {
      observeEvent(input[[paste0("race_btn_", r)]], { selected_race(r) }, ignoreInit = TRUE)
    })
  })

  # ---- Summary cards ----
  output$leader_card <- renderUI({
    sc <- scores()
    if (nrow(sc) == 0)
      return(card(class = "stat-card",
        div(class = "stat-value", "\u2014"),
        div(class = "stat-label", "Current Leader")))
    totals <- sc %>% group_by(participant) %>%
      summarise(total = sum(points, na.rm = TRUE)) %>% arrange(desc(total))
    leader <- totals$participant[1]; pts <- totals$total[1]
    card(class = "stat-card",
      div(class = "stat-value", style = paste0("color:", PARTICIPANT_COLORS[leader]), leader),
      div(class = "stat-label", paste0(pts, " points")))
  })

  output$most_points_week_card <- renderUI({
    sc <- scores()
    if (nrow(sc) == 0)
      return(card(class = "stat-card",
        div(class = "stat-value", "\u2014"),
        div(class = "stat-label", "Best Single Race")))
    best <- sc %>% arrange(desc(points)) %>% slice(1)
    track <- schedule()$track_short[match(best$race_number, schedule()$race_num)]
    card(class = "stat-card",
      div(class = "stat-value", best$points),
      div(class = "stat-label", paste0(best$participant, " @ ", track)))
  })

  output$best_value_card <- renderUI({
    vs <- value_scores()
    if (nrow(vs) == 0)
      return(card(class = "stat-card",
        div(class = "stat-value", "\u2014"),
        div(class = "stat-label", "Best Value Pick")))
    best <- vs %>% arrange(desc(value)) %>% slice(1)
    card(class = "stat-card",
      div(class = "stat-value", paste0("+", round(best$value, 1))),
      div(class = "stat-label", paste0(best$participant, " - #", best$car_number)))
  })

  output$races_completed_card <- renderUI({
    card(class = "stat-card",
      div(class = "stat-value", paste0(length(completed_races()), "/36")),
      div(class = "stat-label", "Races Run"))
  })

  # ---- Standings table ----
  output$standings_table <- renderTable({
    sc <- scores()
    if (nrow(sc) == 0)
      return(data.frame(Message = "No results yet. Waiting for race data and picks."))
    sc %>%
      group_by(participant) %>%
      summarise(
        `Total Points` = sum(points, na.rm = TRUE),
        Races = n_distinct(race_number),
        `Avg Pts` = round(mean(points, na.rm = TRUE)),
        Best = max(points, na.rm = TRUE),
        Worst = min(points, na.rm = TRUE)
      ) %>%
      arrange(desc(`Total Points`)) %>%
      mutate(Rank = row_number(), Gap = max(`Total Points`) - `Total Points`) %>%
      select(Rank, Participant = participant, `Total Points`, Gap, Races, `Avg Pts`, Best, Worst)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c", digits = 0)

  # ---- Stage definitions ----
  STAGES <- list(
    "Stage 1" = 1:6, "Stage 2" = 7:12, "Stage 3" = 13:18,
    "Stage 4" = 19:24, "Stage 5" = 25:30, "Stage 6" = 31:36
  )

  # ---- Points by Stage table ----
  output$stage_table <- renderTable({
    sc <- scores()
    if (nrow(sc) == 0) return(data.frame(Message = "No data yet"))

    stage_pts <- lapply(names(STAGES), function(stg) {
      races <- STAGES[[stg]]
      sc %>% filter(race_number %in% races) %>%
        group_by(participant) %>%
        summarise(pts = sum(points, na.rm = TRUE), .groups = "drop") %>%
        rename(!!stg := pts)
    })

    tbl <- stage_pts[[1]]
    for (i in 2:length(stage_pts)) {
      tbl <- tbl %>% full_join(stage_pts[[i]], by = "participant")
    }
    tbl[is.na(tbl)] <- 0
    tbl <- tbl %>%
      mutate(Total = rowSums(across(starts_with("Stage")), na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      rename(Participant = participant)
    tbl
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c", digits = 0)

  # ---- Stage winner/leader cards ----
  build_stage_card <- function(stage_num) {
    renderUI({
      sc <- scores()
      races <- STAGES[[stage_num]]
      completed <- intersect(races, completed_races())
      n_completed <- length(completed)
      n_total <- length(races)

      card_style <- "text-align:center;padding:10px 6px;border-radius:10px;white-space:nowrap;min-height:120px;display:flex;flex-direction:column;justify-content:center;"

      if (n_completed == 0) {
        div(style = paste0(card_style, "background:#2a1015;border:2px solid #8B0000;"),
          div(style = "font-family:Orbitron,sans-serif;font-size:0.75em;color:#888;", paste0("Stage ", stage_num)),
          div(style = "font-family:Orbitron,sans-serif;font-size:0.9em;color:#666;margin-top:4px;", "TBD"),
          div(style = "font-size:0.7em;color:#555;margin-top:2px;", paste0("0/", n_total, " races"))
        )
      } else {
        stage_sc <- sc %>% filter(race_number %in% completed)
        leader <- stage_sc %>%
          group_by(participant) %>%
          summarise(pts = sum(points, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(pts)) %>% slice(1)

        is_complete <- n_completed == n_total
        label <- if (is_complete) "Winner" else "Leader"
        bg <- if (is_complete) "#0a2e0a" else "#2e2a05"
        border <- if (is_complete) "#228B22" else "#DAA520"
        name_color <- if (is_complete) "#4ADE80" else "#FFD700"

        div(style = sprintf("%sbackground:%s;border:2px solid %s;", card_style, bg, border),
          div(style = "font-family:Orbitron,sans-serif;font-size:0.75em;color:#888;", paste0("Stage ", stage_num)),
          div(style = sprintf("font-family:Rajdhani,sans-serif;font-size:1em;font-weight:700;color:%s;margin-top:4px;", name_color), leader$participant),
          div(style = "font-size:0.8em;color:#ccc;", paste0(leader$pts, " pts")),
          div(style = "font-size:0.65em;color:#888;margin-top:2px;", paste0(label, " (", n_completed, "/", n_total, ")"))
        )
      }
    })
  }

  output$stage1_card <- build_stage_card(1)
  output$stage2_card <- build_stage_card(2)
  output$stage3_card <- build_stage_card(3)
  output$stage4_card <- build_stage_card(4)
  output$stage5_card <- build_stage_card(5)
  output$stage6_card <- build_stage_card(6)

  # ---- Cumulative chart (base R with car number labels) ----
  output$cumulative_chart <- renderPlot({
    sc <- scores()
    sched <- schedule()
    if (nrow(sc) == 0) return(NULL)

    # Keep car_number for labels
    race_pts <- sc %>%
      group_by(participant, race_number) %>%
      summarise(points = sum(points, na.rm = TRUE),
                car_number = first(car_number), .groups = "drop") %>%
      arrange(participant, race_number) %>%
      group_by(participant) %>%
      mutate(cumulative_points = cumsum(points)) %>%
      ungroup() %>%
      mutate(race_number = as.integer(race_number))

    sched_lookup <- sched %>%
      mutate(race_num = as.integer(race_num)) %>%
      select(race_num, track_short)

    all_races <- sort(unique(race_pts$race_number))
    track_names <- sched_lookup$track_short[match(all_races, sched_lookup$race_num)]
    y_max <- max(race_pts$cumulative_points, na.rm = TRUE) * 1.15

    par(bg = "#161625", fg = "#e0e0e0", col.axis = "#aaaaaa", col.lab = "#e0e0e0",
        col.main = "#e0e0e0", mar = c(5, 4, 3, 1))

    plot(NULL, xlim = range(all_races), ylim = c(0, y_max),
         xlab = "", ylab = "Cumulative Points", xaxt = "n",
         main = "")
    axis(1, at = all_races, labels = track_names, las = 2, cex.axis = 0.85)
    grid(col = "#2a2a3a", lty = 1)

    # Try to load car badge images
    badge_cache <- list()
    for (car in unique(race_pts$car_number)) {
      local_path <- file.path("www", "img", "numbers", paste0(car, ".png"))
      if (file.exists(local_path)) {
        tryCatch({
          badge_cache[[as.character(car)]] <- png::readPNG(local_path)
        }, error = function(e) NULL)
      }
    }

    colors <- PARTICIPANT_COLORS
    badge_size <- diff(range(all_races)) * 0.04
    y_badge <- y_max * 0.03

    for (p in unique(race_pts$participant)) {
      d <- race_pts %>% filter(participant == p) %>% arrange(race_number)
      lines(d$race_number, d$cumulative_points, col = colors[p], lwd = 2.5)

      # Draw car badges or numbered circles at each point
      for (j in seq_len(nrow(d))) {
        car_key <- as.character(d$car_number[j])
        x <- d$race_number[j]
        y <- d$cumulative_points[j]
        if (car_key %in% names(badge_cache)) {
          tryCatch({
            rasterImage(badge_cache[[car_key]],
                        x - badge_size, y - y_badge,
                        x + badge_size, y + y_badge)
          }, error = function(e) {
            points(x, y, col = colors[p], pch = 19, cex = 1.5)
            text(x, y, d$car_number[j], cex = 0.5, col = "#ffffff", font = 2)
          })
        } else {
          # Fallback: colored circle with car number text
          points(x, y, col = colors[p], pch = 19, cex = 2)
          text(x, y, d$car_number[j], cex = 0.45, col = "#ffffff", font = 2)
        }
      }
    }

    legend("topleft", legend = names(colors), col = colors, lwd = 2.5, pch = 19,
           bg = "#1a1a2e", text.col = "#e0e0e0", border = "#333333", cex = 0.9)
  }, bg = "#161625")

  # ---- Weekly results with driver cards ----
  output$weekly_cards <- renderUI({
    req(selected_race())
    rn <- as.integer(selected_race())
    sc <- scores() %>% filter(race_number == rn)
    drv <- drivers()
    if (nrow(sc) == 0) return(tags$p(style = "color:#888;", "No data for this race"))

    sc <- sc %>% arrange(desc(points)) %>%
      left_join(drv %>% select(car_number, team, manufacturer, headshot_url, number_url),
                by = "car_number")

    cards <- lapply(seq_len(nrow(sc)), function(i) {
      r <- sc[i, ]
      info <- paste0("P", r$finish_pos, " | ", r$points, " pts | ", r$participant)
      HTML(build_driver_card(
        r$car_number, r$driver, r$team, r$manufacturer,
        r$headshot_url, r$number_url,
        highlight = (i == 1), extra_info = info
      ))
    })
    div(style = "display:flex;flex-wrap:wrap;justify-content:center;", cards)
  })

  output$weekly_bar <- renderPlot({
    req(selected_race())
    rn <- as.integer(selected_race())
    sc <- scores() %>% filter(race_number == rn)
    if (nrow(sc) == 0) return(NULL)
    ggplot(sc, aes(x = reorder(participant, -points), y = points, fill = participant)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = points), vjust = -0.5, size = 5, fontface = "bold", color = "#e0e0e0") +
      scale_fill_manual(values = PARTICIPANT_COLORS) +
      labs(x = NULL, y = "Points") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "#161625", color = NA),
        panel.background = element_rect(fill = "#161625", color = NA),
        text = element_text(color = "#e0e0e0"),
        axis.text = element_text(color = "#aaa"),
        panel.grid.major = element_line(color = "#2a2a3a"),
        panel.grid.minor = element_blank()
      ) +
      coord_cartesian(ylim = c(0, max(sc$points, na.rm = TRUE) * 1.15))
  })

  # ---- Pick history grid ----
  output$picks_grid_visual <- renderUI({
    sc <- scores(); sched <- schedule()
    if (nrow(sc) == 0) return(tags$p(style = "color:#888;", "No data yet"))

    all_races <- sort(unique(sc$race_number))
    race_labels <- sched$track_short[match(all_races, sched$race_num)]
    race_labels[is.na(race_labels)] <- paste0("R", all_races[is.na(race_labels)])

    # Header row
    header_cells <- c(
      '<th style="padding:4px 6px;color:#FFD700;font-family:Orbitron,sans-serif;font-size:0.7em;position:sticky;left:0;background:#161625;z-index:1;">Player</th>',
      sapply(seq_along(all_races), function(i) {
        sprintf('<th style="padding:4px 6px;color:#FFD700;font-family:Orbitron,sans-serif;font-size:0.65em;text-align:center;min-width:55px;">%s</th>', race_labels[i])
      }),
      '<th style="padding:4px 6px;color:#FFD700;font-family:Orbitron,sans-serif;font-size:0.7em;text-align:center;">Total</th>'
    )

    rows <- lapply(PARTICIPANTS, function(p) {
      p_scores <- sc %>% filter(participant == p)
      total <- sum(p_scores$points, na.rm = TRUE)

      cells <- sapply(all_races, function(r) {
        row <- p_scores %>% filter(race_number == r)
        if (nrow(row) == 0) {
          '<td style="padding:4px;text-align:center;color:#555;">-</td>'
        } else {
          badge_url <- car_badge_url(row$car_number[1])
          sprintf('<td style="padding:4px;text-align:center;"><img src="%s" style="height:28px;" onerror="this.outerHTML=\'<span style=color:#FFD700;font-weight:bold>#%s</span>\'"><div style="font-size:0.75em;color:#ccc;">%s</div></td>',
                  badge_url, row$car_number[1], row$points[1])
        }
      })

      sprintf('<tr><td style="padding:4px 6px;color:%s;font-weight:bold;font-family:Rajdhani,sans-serif;position:sticky;left:0;background:#161625;z-index:1;">%s</td>%s<td style="padding:4px 6px;text-align:center;color:#FFD700;font-weight:bold;">%s</td></tr>',
              PARTICIPANT_COLORS[p], p, paste(cells, collapse = ""), total)
    })

    HTML(sprintf(
      '<table style="border-collapse:collapse;width:100%%;"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>',
      paste(header_cells, collapse = ""),
      paste(rows, collapse = "")
    ))
  })

  output$value_grid <- renderTable({
    vs <- value_scores(); sched <- schedule()
    if (nrow(vs) == 0) return(data.frame(Message = "No data yet"))
    wide <- vs %>%
      left_join(sched %>% select(race_num, track_short), by = c("race_number" = "race_num")) %>%
      mutate(label = round(value, 1)) %>%
      select(participant, track_short, label) %>%
      pivot_wider(names_from = track_short, values_from = label, values_fn = list(label = first))
    totals <- vs %>% group_by(participant) %>%
      summarise(`Total Value` = round(sum(value, na.rm = TRUE), 1))
    wide %>% left_join(totals, by = "participant") %>% rename(Participant = participant)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

  # ---- Rankings ----
  # Helper to build full cumulative standings with gaps filled
  full_standings <- reactive({
    sc <- scores(); sched <- schedule()
    if (nrow(sc) == 0) return(tibble())
    all_races <- sort(unique(sc$race_number))
    all_participants <- unique(sc$participant)
    race_pts <- sc %>%
      group_by(participant, race_number) %>%
      summarise(points = sum(points, na.rm = TRUE), .groups = "drop")
    full_grid <- expand.grid(
      participant = all_participants, race_number = all_races,
      stringsAsFactors = FALSE
    ) %>% as_tibble()
    full_grid %>%
      left_join(race_pts, by = c("participant", "race_number")) %>%
      mutate(points = ifelse(is.na(points), 0, points)) %>%
      arrange(participant, race_number) %>%
      group_by(participant) %>%
      mutate(cumulative_points = cumsum(points)) %>%
      ungroup() %>%
      left_join(sched %>% select(race_num, track_short),
                by = c("race_number" = "race_num"))
  })

  output$rankings_chart <- renderPlot({
    st <- full_standings()
    if (nrow(st) == 0) return(NULL)
    ranked <- st %>% group_by(race_number) %>%
      mutate(position = rank(-cumulative_points, ties.method = "min")) %>% ungroup()

    sched <- schedule() %>% mutate(race_num = as.integer(race_num))
    all_races <- sort(unique(ranked$race_number))
    track_names <- sched$track_short[match(all_races, sched$race_num)]

    par(bg = "#161625", fg = "#e0e0e0", col.axis = "#aaaaaa", col.lab = "#e0e0e0",
        col.main = "#e0e0e0", mar = c(5, 4, 3, 1))

    plot(NULL, xlim = range(all_races), ylim = c(4.5, 0.5),
         xlab = "", ylab = "Position", xaxt = "n", yaxt = "n", main = "")
    axis(1, at = all_races, labels = track_names, las = 2, cex.axis = 0.85)
    axis(2, at = 1:4, labels = c("1st", "2nd", "3rd", "4th"), las = 1)
    grid(col = "#2a2a3a", lty = 1)

    colors <- PARTICIPANT_COLORS
    for (p in unique(ranked$participant)) {
      d <- ranked %>% filter(participant == p) %>% arrange(race_number)
      lines(d$race_number, d$position, col = colors[p], lwd = 2.5)
      points(d$race_number, d$position, col = colors[p], pch = 19, cex = 1.5)
    }

    legend("bottomleft", legend = names(colors), col = colors, lwd = 2.5, pch = 19,
           bg = "#1a1a2e", text.col = "#e0e0e0", border = "#333333", cex = 0.9)
  }, bg = "#161625")

  output$rank_grid <- renderTable({
    wr <- weekly_ranks(); sched <- schedule()
    if (nrow(wr) == 0) return(data.frame(Message = "No data yet"))
    wide <- wr %>%
      left_join(sched %>% select(race_num, track_short), by = c("race_number" = "race_num")) %>%
      select(participant, track_short, weekly_rank) %>%
      pivot_wider(names_from = track_short, values_from = weekly_rank, values_fn = list(weekly_rank = first))
    avg <- wr %>% group_by(participant) %>%
      summarise(`Avg Rank` = round(mean(weekly_rank, na.rm = TRUE), 2))
    wide %>% left_join(avg, by = "participant") %>% rename(Participant = participant)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

  # ---- Drivers Used: button-based participant selection ----
  selected_participant <- reactiveVal("Mike")

  observeEvent(input$pick_Mike, selected_participant("Mike"))
  observeEvent(input$pick_Matt, selected_participant("Matt"))
  observeEvent(input$pick_Brian, selected_participant("Brian"))
  observeEvent(input$pick_Tom, selected_participant("Tom"))

  output$used_header_text <- renderUI({
    tags$span(paste0(selected_participant(), " — Drivers Used"))
  })
  output$avail_header_text <- renderUI({
    tags$span(paste0(selected_participant(), " — Still Available"))
  })

  output$used_drivers_cards <- renderUI({
    p <- selected_participant()
    pl <- picks_long(); drv <- drivers(); sched <- schedule(); sc <- scores()
    if (nrow(pl) == 0) return(tags$p(style = "color:#888;", "No picks yet"))

    used <- pl %>%
      filter(participant == p) %>%
      left_join(drv, by = "car_number") %>%
      left_join(sched %>% select(race_num, track_short), by = c("race_number" = "race_num")) %>%
      left_join(sc %>% select(participant, race_number, car_number, points),
                by = c("participant", "race_number", "car_number")) %>%
      arrange(race_number)

    if (nrow(used) == 0) return(tags$p(style = "color:#888;", "No picks yet"))

    cards <- lapply(seq_len(nrow(used)), function(i) {
      r <- used[i, ]
      info <- paste0("R", r$race_number, " ", r$track_short,
                     if (!is.na(r$points)) paste0(" | ", r$points, " pts") else "")
      HTML(build_driver_card(
        r$car_number, r$driver, r$team, r$manufacturer,
        r$headshot_url, r$number_url,
        used = TRUE, extra_info = info
      ))
    })
    div(style = "display:flex;flex-wrap:wrap;justify-content:center;", cards)
  })

  output$available_drivers_cards <- renderUI({
    p <- selected_participant()
    used_cars <- picks_long() %>%
      filter(participant == p) %>%
      pull(car_number) %>% unique()
    drv <- drivers()
    available <- drv %>% filter(!car_number %in% used_cars) %>% arrange(car_number)

    if (nrow(available) == 0) return(tags$p(style = "color:#888;", "All drivers used!"))

    cards <- lapply(seq_len(nrow(available)), function(i) {
      r <- available[i, ]
      HTML(build_driver_card(
        r$car_number, r$driver, r$team, r$manufacturer,
        r$headshot_url, r$number_url
      ))
    })
    div(style = "display:flex;flex-wrap:wrap;justify-content:center;", cards)
  })

  # ---- Roster Tab ----
  # Participant initials and colors for pick badges
  PARTICIPANT_INITIALS <- c("Mike" = "MT", "Matt" = "MD", "Brian" = "BM", "Tom" = "TM")

  output$roster_cards <- renderUI({
    drv <- drivers(); res <- results(); pl <- picks_long()

    # Compute average points per car across all races
    driver_avg <- if (nrow(res) > 0) {
      res %>%
        group_by(car_number) %>%
        summarise(avg_pts = round(mean(points, na.rm = TRUE), 1),
                  races = n(), .groups = "drop")
    } else {
      tibble(car_number = integer(), avg_pts = numeric(), races = integer())
    }

    # Who picked each car and when
    pick_map <- if (nrow(pl) > 0) {
      pl %>% select(participant, car_number) %>% distinct()
    } else {
      tibble(participant = character(), car_number = integer())
    }

    drv <- drv %>%
      left_join(driver_avg, by = "car_number") %>%
      mutate(avg_pts = ifelse(is.na(avg_pts), 0, avg_pts),
             races = ifelse(is.na(races), 0, races)) %>%
      arrange(desc(avg_pts))

    cards <- lapply(seq_len(nrow(drv)), function(i) {
      r <- drv[i, ]
      # Build 4 pick badges — always show all 4 circles, filled when picked
      pickers <- pick_map %>% filter(car_number == r$car_number) %>% pull(participant)
      badge_html <- paste(sapply(PARTICIPANTS, function(p) {
        if (p %in% pickers) {
          sprintf('<span style="display:inline-block;width:24px;height:24px;border-radius:50%%;background:%s;color:#fff;font-size:10px;font-weight:bold;font-family:Orbitron,sans-serif;line-height:24px;text-align:center;margin:0 2px;box-shadow:0 1px 3px rgba(0,0,0,0.5);">%s</span>',
                  PARTICIPANT_COLORS[p], PARTICIPANT_INITIALS[p])
        } else {
          '<span style="display:inline-block;width:24px;height:24px;border-radius:50%;background:#2a2a3a;border:1px solid #444;margin:0 2px;"></span>'
        }
      }), collapse = "")

      info <- if (r$races > 0) {
        paste0(r$avg_pts, " avg pts (", r$races, " races)")
      } else "No race data"

      HTML(build_driver_card(
        r$car_number, r$driver, r$team, r$manufacturer,
        r$headshot_url, r$number_url,
        extra_info = info,
        badge_html = badge_html
      ))
    })
    div(style = "display:flex;flex-wrap:wrap;justify-content:center;", cards)
  })
}

shinyApp(ui, server)
