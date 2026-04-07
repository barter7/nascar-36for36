############################################################################
# NASCAR 36 for 36 — 2026 Season Tracker
# Shiny App for tracking a 4-person pick'em league
############################################################################

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(googlesheets4)

# ---- Configuration ----
PARTICIPANTS <- c("Mike", "Matt", "Brian", "Tom")
PARTICIPANT_COLORS <- c(
  "Mike"  = "#E41A1C",
  "Matt"  = "#377EB8",
  "Brian" = "#4DAF4A",
  "Tom"   = "#FF7F00"
)

# Google Sheet ID for picks (public, no auth needed)
# Set to NULL to use local CSV only
PICKS_SHEET_ID <- NULL

# ---- Load data ----
load_schedule <- function() {
  read_csv("data/schedule.csv", show_col_types = FALSE) %>%
    mutate(date = as.Date(date))
}

load_drivers <- function() {
  read_csv("data/drivers.csv", show_col_types = FALSE)
}

load_results <- function() {
  results <- read_csv("data/results.csv", show_col_types = FALSE)
  if (nrow(results) == 0) return(results)
  results %>%
    mutate(
      race_number = as.integer(race_number),
      car_number  = as.integer(car_number),
      finish_pos  = as.integer(finish_pos),
      points      = as.numeric(points)
    )
}

load_picks <- function() {
  picks <- NULL

  # Try Google Sheet first
  if (!is.null(PICKS_SHEET_ID)) {
    tryCatch({
      gs4_deauth()
      picks <- read_sheet(PICKS_SHEET_ID, sheet = 1) %>% as.data.frame()
    }, error = function(e) {
      message("Could not read Google Sheet: ", e$message)
    })
  }

  # Fall back to local CSV
  if (is.null(picks)) {
    picks <- read_csv("data/picks.csv", show_col_types = FALSE)
  }

  picks
}

# ---- Transform picks from wide to long ----
picks_to_long <- function(picks_wide) {
  picks_wide %>%
    pivot_longer(
      cols = starts_with("race_"),
      names_to = "race_num",
      values_to = "car_number"
    ) %>%
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

# ===========================================================================
# UI
# ===========================================================================
ui <- page_navbar(
  title = "NASCAR 36 for 36 — 2026",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1a1a2e",
    "navbar-bg" = "#1a1a2e"
  ),
  header = tags$head(tags$style(HTML("
    body { background-color: #f8f9fa; }
    .card { border-radius: 12px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
    .stat-card { text-align: center; padding: 20px; }
    .stat-card .stat-value { font-size: 2.5em; font-weight: bold; }
    .stat-card .stat-label { font-size: 0.9em; color: #666; }
  "))),

  # ---- Standings Tab ----
  nav_panel("Standings",
    layout_sidebar(fillable = FALSE, sidebar = NULL,
      layout_columns(col_widths = c(3, 3, 3, 3),
        uiOutput("leader_card"),
        uiOutput("most_points_week_card"),
        uiOutput("best_value_card"),
        uiOutput("races_completed_card")
      ),
      card(card_header("Overall Standings"), tableOutput("standings_table")),
      card(card_header("Cumulative Points"), plotOutput("cumulative_chart", height = "400px"))
    )
  ),

  # ---- Weekly Results Tab ----
  nav_panel("Weekly Results",
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(
        selectInput("race_select", "Select Race", choices = NULL, selected = NULL)
      ),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Picks & Points"), tableOutput("weekly_table")),
        card(card_header("Weekly Points"), plotOutput("weekly_bar", height = "300px"))
      )
    )
  ),

  # ---- Pick History Tab ----
  nav_panel("Pick History",
    layout_sidebar(fillable = FALSE, sidebar = NULL,
      card(card_header("All Picks — Car # & Points Earned"),
           div(style = "overflow-x: auto;", tableOutput("picks_grid"))),
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
    layout_sidebar(fillable = FALSE,
      sidebar = sidebar(
        selectInput("participant_select", "Participant",
                    choices = PARTICIPANTS, selected = PARTICIPANTS[1])
      ),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Drivers Already Used"), tableOutput("used_drivers")),
        card(card_header("Drivers Still Available"), tableOutput("available_drivers"))
      )
    )
  )
)

# ===========================================================================
# Server
# ===========================================================================
server <- function(input, output, session) {

  schedule <- reactive(load_schedule())
  drivers  <- reactive(load_drivers())
  results  <- reactive(load_results())
  picks_wide <- reactive(load_picks())

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

  # Update race selector

  observe({
    cr <- completed_races()
    sched <- schedule()
    if (length(cr) > 0) {
      choices <- cr
      names(choices) <- paste0("Race ", cr, " - ", sched$track_short[match(cr, sched$race_num)])
      updateSelectInput(session, "race_select", choices = choices, selected = max(cr))
    }
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
      div(class = "stat-label", "Races Completed"))
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
        `Avg Pts` = round(mean(points, na.rm = TRUE), 1),
        Best = max(points, na.rm = TRUE),
        Worst = min(points, na.rm = TRUE)
      ) %>%
      arrange(desc(`Total Points`)) %>%
      mutate(Rank = row_number(),
             Gap = max(`Total Points`) - `Total Points`) %>%
      select(Rank, Participant = participant, `Total Points`, Gap, Races, `Avg Pts`, Best, Worst)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

  # ---- Cumulative chart ----

  output$cumulative_chart <- renderPlot({
    st <- standings()
    if (nrow(st) == 0) return(NULL)
    ggplot(st, aes(x = race_number, y = cumulative_points,
                   color = participant, group = participant)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
      scale_color_manual(values = PARTICIPANT_COLORS) +
      scale_x_continuous(breaks = unique(st$race_number), labels = unique(st$track_short)) +
      labs(x = NULL, y = "Cumulative Points", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            legend.position = "top", panel.grid.minor = element_blank())
  })

  # ---- Weekly results ----

  output$weekly_table <- renderTable({
    req(input$race_select)
    rn <- as.integer(input$race_select)
    sc <- scores() %>% filter(race_number == rn)
    if (nrow(sc) == 0) return(data.frame(Message = "No data for this race"))
    sc %>% arrange(desc(points)) %>% mutate(Rank = row_number()) %>%
      left_join(drivers() %>% select(car_number, team), by = "car_number") %>%
      select(Rank, Participant = participant, `Car #` = car_number,
             Driver = driver, Team = team, Finish = finish_pos, Points = points)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

  output$weekly_bar <- renderPlot({
    req(input$race_select)
    rn <- as.integer(input$race_select)
    sc <- scores() %>% filter(race_number == rn)
    if (nrow(sc) == 0) return(NULL)
    ggplot(sc, aes(x = reorder(participant, -points), y = points, fill = participant)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = points), vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = PARTICIPANT_COLORS) +
      labs(x = NULL, y = "Points") +
      theme_minimal(base_size = 14) + theme(legend.position = "none") +
      coord_cartesian(ylim = c(0, max(sc$points, na.rm = TRUE) * 1.15))
  })

  # ---- Pick history grid ----

  output$picks_grid <- renderTable({
    sc <- scores(); sched <- schedule()
    if (nrow(sc) == 0) return(data.frame(Message = "No data yet"))
    wide <- sc %>%
      left_join(sched %>% select(race_num, track_short), by = c("race_number" = "race_num")) %>%
      mutate(label = paste0("#", car_number, " (", points, ")")) %>%
      select(participant, track_short, label) %>%
      pivot_wider(names_from = track_short, values_from = label)
    totals <- sc %>% group_by(participant) %>% summarise(Total = sum(points, na.rm = TRUE))
    wide %>% left_join(totals, by = "participant") %>% rename(Participant = participant)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

  output$value_grid <- renderTable({
    vs <- value_scores(); sched <- schedule()
    if (nrow(vs) == 0) return(data.frame(Message = "No data yet"))
    wide <- vs %>%
      left_join(sched %>% select(race_num, track_short), by = c("race_number" = "race_num")) %>%
      mutate(label = round(value, 1)) %>%
      select(participant, track_short, label) %>%
      pivot_wider(names_from = track_short, values_from = label)
    totals <- vs %>% group_by(participant) %>%
      summarise(`Total Value` = round(sum(value, na.rm = TRUE), 1))
    wide %>% left_join(totals, by = "participant") %>% rename(Participant = participant)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

  # ---- Rankings ----

  output$rankings_chart <- renderPlot({
    st <- standings()
    if (nrow(st) == 0) return(NULL)
    ranked <- st %>% group_by(race_number) %>%
      mutate(position = rank(-cumulative_points, ties.method = "min")) %>% ungroup()
    ggplot(ranked, aes(x = race_number, y = position, color = participant, group = participant)) +
      geom_line(linewidth = 1.2) + geom_point(size = 3) +
      scale_y_reverse(breaks = 1:4, labels = c("1st", "2nd", "3rd", "4th")) +
      scale_color_manual(values = PARTICIPANT_COLORS) +
      scale_x_continuous(breaks = unique(ranked$race_number), labels = unique(ranked$track_short)) +
      labs(x = NULL, y = "Position", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
            legend.position = "top", panel.grid.minor = element_blank())
  })

  output$rank_grid <- renderTable({
    wr <- weekly_ranks(); sched <- schedule()
    if (nrow(wr) == 0) return(data.frame(Message = "No data yet"))
    wide <- wr %>%
      left_join(sched %>% select(race_num, track_short), by = c("race_number" = "race_num")) %>%
      select(participant, track_short, weekly_rank) %>%
      pivot_wider(names_from = track_short, values_from = weekly_rank)
    avg <- wr %>% group_by(participant) %>%
      summarise(`Avg Rank` = round(mean(weekly_rank, na.rm = TRUE), 2))
    wide %>% left_join(avg, by = "participant") %>% rename(Participant = participant)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

  # ---- Drivers used ----

  output$used_drivers <- renderTable({
    req(input$participant_select)
    pl <- picks_long(); drv <- drivers(); sched <- schedule(); sc <- scores()
    if (nrow(pl) == 0) return(data.frame(Message = "No picks yet"))
    pl %>%
      filter(participant == input$participant_select) %>%
      left_join(drv, by = "car_number") %>%
      left_join(sched %>% select(race_num, track_short), by = c("race_number" = "race_num")) %>%
      left_join(sc %>% select(participant, race_number, car_number, points),
                by = c("participant", "race_number", "car_number")) %>%
      arrange(race_number) %>%
      select(`Race` = race_number, Track = track_short, `Car #` = car_number,
             Driver = driver, Team = team, Points = points)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")

  output$available_drivers <- renderTable({
    req(input$participant_select)
    used_cars <- picks_long() %>%
      filter(participant == input$participant_select) %>%
      pull(car_number) %>% unique()
    available <- drivers() %>% filter(!car_number %in% used_cars) %>%
      arrange(car_number) %>% select(`Car #` = car_number, Driver = driver, Team = team)
    if (nrow(available) == 0) return(data.frame(Message = "All drivers used!"))
    available
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "c")
}

shinyApp(ui, server)
