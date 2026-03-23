# hefml_cbb_pipeline.R
# End-to-end college basketball modeling pipeline with improved feature engineering
# and conference/average-opponent indexing.

suppressPackageStartupMessages({
  library(data.table)
library(dplyr)
  library(httr)
  library(jsonlite)
  library(zoo)
  library(catboost)
  library(quantregForest)
  library(caret)
})

# ---------------------- configuration -----------------------
set.seed(2025)
base_url <- "https://api.collegebasketballdata.com/"
# Fallback to provided API key so the script can run out-of-the-box if the
# environment variable is not set. Replace with your own key for production.
api_key  <- Sys.getenv(
  "HEFML_CBB_API_KEY",
  ""
)
if (!nzchar(api_key)) {
  warning("Set HEFML_CBB_API_KEY in the environment for authenticated API calls.")
}

# Seasons to train on and predict
historical_seasons <- 2018:2025
future_season      <- 2026

# ---------------------- helpers -----------------------------
get_auth <- function() {
  if (!nzchar(api_key)) return(NULL)
  add_headers(`Authorization` = paste("Bearer", api_key))
}

fetch_json <- function(url) {
  resp <- GET(url, get_auth())
  if (status_code(resp) != 200) {
    stop("API returned status ", status_code(resp), " for ", url)
  }
  fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
}

fetch_games <- function(season, status = "final") {
  url <- paste0(base_url, "games?season=", season, "&status=", status)
  tryCatch(fetch_json(url), error = function(e) {
    message("Games fetch failed for season ", season, ": ", e$message)
    NULL
  })
}

fetch_lines <- function(season) {
  url <- paste0(base_url, "lines?season=", season,"&startDateRange=2026-01-12T03%3A20%3A53%2B0000")
  tryCatch(fetch_json(url), error = function(e) {
    message("Lines fetch failed for season ", season, ": ", e$message)
    NULL
  })
}

flatten_lines <- function(lines_df) {
  # lines_df has a list-column named lines (NULL or a list of provider rows)
  stopifnot("lines" %in% names(lines_df))

  # Make sure purrr/tibble exist
  if (!requireNamespace("purrr", quietly = TRUE)) stop("install.packages('purrr')")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("install.packages('tibble')")

  out <- purrr::map2_dfr(
    .x = lines_df$lines,
    .y = seq_len(nrow(lines_df)),
    .f = function(li, i) {
      base <- lines_df[i, setdiff(names(lines_df), "lines"), drop = FALSE]

      # No providers for this game
      if (is.null(li) || length(li) == 0) return(NULL)

      li_df <- tryCatch(as.data.frame(li, stringsAsFactors = FALSE), error = function(e) NULL)
      if (is.null(li_df) || nrow(li_df) == 0) return(NULL)

      # Replicate base row to match provider rows, then cbind
      base_rep <- base[rep(1, nrow(li_df)), , drop = FALSE]
      merged <- cbind(base_rep, li_df)

      # Standardize expected columns (some APIs omit MLs etc.)
      for (nm in c("provider","spread","overUnder","homeMoneyline","awayMoneyline","spreadOpen","overUnderOpen")) {
        if (!nm %in% names(merged)) merged[[nm]] <- NA
      }

      tibble::as_tibble(merged)
    }
  )

  out
}

fetch_team_stats <- function(season) {
  url <- paste0(base_url, "stats/team/season?season=", season)
  tryCatch(fetch_json(url), error = function(e) {
    message("Team stats fetch failed for season ", season, ": ", e$message)
    NULL
  })
}

bind_safely <- function(lst) {
  if (length(lst) == 0) return(data.table())
  rbindlist(lst, fill = TRUE, use.names = TRUE)
}

# ---------------------- Elo machinery -----------------------
calculate_final_elo <- function(games_dt, k = 20) {
  dt <- copy(as.data.table(games_dt))
  setorder(dt, startDate)
  teams <- unique(c(dt$homeTeam, dt$awayTeam))
  elo <- rep(1500, length(teams)); names(elo) <- teams
  for (i in seq_len(nrow(dt))) {
    home <- dt$homeTeam[i]; away <- dt$awayTeam[i]
    elo_h <- elo[home]; elo_a <- elo[away]
    exp_h <- 1 / (1 + 10^((elo_a - elo_h - 100) / 400))
    if (!is.na(dt$homePoints[i]) && !is.na(dt$awayPoints[i])) {
      result <- ifelse(dt$homePoints[i] > dt$awayPoints[i], 1, 0)
      elo[home] <- elo_h + k * (result - exp_h)
      elo[away] <- elo_a + k * ((1 - result) - (1 - exp_h))
    }
  }
  elo
}

update_elo <- function(games_dt, k = 20) {
  dt <- copy(as.data.table(games_dt))
  setorder(dt, startDate)
  teams <- unique(c(dt$homeTeam, dt$awayTeam))
  elo <- rep(1500, length(teams)); names(elo) <- teams
  for (i in seq_len(nrow(dt))) {
    home <- dt$homeTeam[i]; away <- dt$awayTeam[i]
    elo_h <- elo[home]; elo_a <- elo[away]
    exp_h <- 1 / (1 + 10^((elo_a - elo_h - 100) / 400))
    if (!is.na(dt$homePoints[i]) && !is.na(dt$awayPoints[i])) {
      result <- ifelse(dt$homePoints[i] > dt$awayPoints[i], 1, 0)
      elo[home] <- elo_h + k * (result - exp_h)
      elo[away] <- elo_a + k * ((1 - result) - (1 - exp_h))
    }
    dt$elo_diff[i] <- elo[home] - elo[away]
  }
  dt
}

# ---------------------- feature engineering ------------------
cap_rest <- function(x, max_days = 6) pmin(pmax(x, 0), max_days)

compute_auc <- function(labels, scores) {
  ord <- order(scores)
  pos <- labels[ord] == 1
  n_pos <- sum(pos)
  n_neg <- length(labels) - n_pos
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  rank_sum <- sum(which(pos))
  (rank_sum - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

compute_validation_metrics <- function(valid_dt) {
  dt <- valid_dt[!is.na(money_prob) & !is.na(moneyline)]
  if (!nrow(dt)) return(data.table(metric = character(), value = numeric()))

  eps <- 1e-15
  probs <- pmin(pmax(dt$money_prob, eps), 1 - eps)
  labels <- dt$moneyline
  preds <- as.integer(probs >= 0.5)

  logloss <- -mean(labels * log(probs) + (1 - labels) * log(1 - probs))
  acc <- mean(preds == labels)
  auc <- compute_auc(labels, probs)

  data.table(metric = c("logloss", "accuracy", "auc"), value = c(logloss, acc, auc))
}

compute_avg_opp_index <- function(games_dt, power_lookup) {
  dt <- as.data.table(games_dt)
  teams <- unique(c(dt$homeTeam, dt$awayTeam))
  lookup <- data.table(team = names(power_lookup), power = as.numeric(power_lookup))
  hg <- dt[, .(team = homeTeam, opp = awayTeam)]
  ag <- dt[, .(team = awayTeam, opp = homeTeam)]
  both <- rbind(hg, ag)
  both <- lookup[both, on = .(team)]
  both <- lookup[both, on = .(team = opp), nomatch = 0L]
  both[, .(avg_opp_index = mean(i.power, na.rm = TRUE)), by = team]
}

engineer_features <- function(game_data, team_stats, power_lookup = NULL,
                              avg_opp_lookup = NULL, conf_power_lookup = NULL,
                              update_elo_flag = TRUE) {
  gd <- as.data.table(game_data)
  ts <- as.data.table(team_stats)
  ts[, conference := if ("conference" %in% names(ts)) conference else "Unknown"]

  has_scores <- "homePoints" %in% names(gd) && "awayPoints" %in% names(gd)

  if (update_elo_flag) {
    gd <- update_elo(gd)
  }

  # merge conference and stats
  gd[ts, on = .(season = season, homeTeam = team), `:=`(
    home_conf = i.conference,
    home_pace = i.pace,
    home_trueShooting = i.teamStats.trueShooting,
    home_efg = i.teamStats.fourFactors.effectiveFieldGoalPct,
    home_tov = i.teamStats.fourFactors.turnoverRatio,
    home_orb = i.teamStats.fourFactors.offensiveReboundPct,
    home_ftr = i.teamStats.fourFactors.freeThrowRate,
    home_pts_total = i.teamStats.points.total,
    home_pts_paint = i.teamStats.points.inPaint,
    home_pts_off_to = i.teamStats.points.offTurnovers,
    home_pts_fb = i.teamStats.points.fastBreak
  )]
  gd[ts, on = .(season = season, awayTeam = team), `:=`(
    away_conf = i.conference,
    away_pace = i.pace,
    away_trueShooting = i.teamStats.trueShooting,
    away_efg = i.teamStats.fourFactors.effectiveFieldGoalPct,
    away_tov = i.teamStats.fourFactors.turnoverRatio,
    away_orb = i.teamStats.fourFactors.offensiveReboundPct,
    away_ftr = i.teamStats.fourFactors.freeThrowRate,
    away_pts_total = i.teamStats.points.total,
    away_pts_paint = i.teamStats.points.inPaint,
    away_pts_off_to = i.teamStats.points.offTurnovers,
    away_pts_fb = i.teamStats.points.fastBreak
  )]

  # rest days and momentum
  team_game_dates <- rbind(gd[, .(team = homeTeam, date = startDate)],
                           gd[, .(team = awayTeam, date = startDate)])
  setorder(team_game_dates, team, date)
  team_game_dates[, rest_days := cap_rest(as.numeric(difftime(date, shift(date), units = "days"))), by = team]
  team_game_dates[, rest_days := fifelse(is.na(rest_days), 0, rest_days)]
  team_game_dates[, b2b := rest_days <= 1]

  gd[team_game_dates, on = .(homeTeam = team, startDate = date), `:=`(
    home_rest_days = i.rest_days,
    home_b2b = i.b2b
  )]
  gd[team_game_dates, on = .(awayTeam = team, startDate = date), `:=`(
    away_rest_days = i.rest_days,
    away_b2b = i.b2b
  )]

  if (has_scores) {
    team_games <- melt(gd[, .(startDate, homePoints, awayPoints, homeTeam, awayTeam)],
                       id.vars = c("startDate", "homePoints", "awayPoints"),
                       measure.vars = c("homeTeam", "awayTeam"),
                       value.name = "team", variable.name = "side")
    team_games[, score_diff := fifelse(side == "homeTeam", homePoints - awayPoints, awayPoints - homePoints)]
    setorder(team_games, team, startDate)
    team_games[, momentum_ewma := zoo::rollapply(score_diff, width = 5,
                                                FUN = function(v) stats::filter(v, 0.7, method = "recursive")[length(v)],
                                                fill = NA, align = "right"), by = team]
    gd[team_games, on = .(homeTeam = team, startDate), home_momentum := i.momentum_ewma]
    gd[team_games, on = .(awayTeam = team, startDate), away_momentum := i.momentum_ewma]
  } else {
    gd[, `:=`(home_momentum = NA_real_, away_momentum = NA_real_)]
  }

  league_mean_power <- if (!is.null(power_lookup)) mean(as.numeric(power_lookup), na.rm = TRUE) else 0
  gd[, home_power := if (!is.null(power_lookup)) power_lookup[homeTeam] else NA_real_]
  gd[, away_power := if (!is.null(power_lookup)) power_lookup[awayTeam] else NA_real_]
  gd[, power_diff := fifelse(
    is.na(home_power - away_power),
    if ("elo_diff" %in% names(gd)) elo_diff else 0,
    home_power - away_power
  )]
  gd[, `:=`(
    home_vs_avg = fifelse(is.na(home_power), 0, home_power - league_mean_power),
    away_vs_avg = fifelse(is.na(away_power), 0, away_power - league_mean_power)
  )]

  if (!is.null(avg_opp_lookup)) {
    avg_opp_lookup <- avg_opp_lookup[!is.na(avg_opp_index)]
    gd[avg_opp_lookup, on = .(homeTeam = team), home_avg_opp := i.avg_opp_index]
    gd[avg_opp_lookup, on = .(awayTeam = team), away_avg_opp := i.avg_opp_index]
  }
  gd[, avg_opp_gap := fifelse(is.na(home_avg_opp - away_avg_opp), 0, home_avg_opp - away_avg_opp)]

  if (!is.null(conf_power_lookup)) {
    gd[conf_power_lookup, on = .(home_conf = conference), home_conf_power := i.conf_power]
    gd[conf_power_lookup, on = .(away_conf = conference), away_conf_power := i.conf_power]
    gd[, conf_power_diff := fifelse(is.na(home_conf_power - away_conf_power), 0, home_conf_power - away_conf_power)]
  } else {
    gd[, conf_power_diff := 0]
  }

  gd[, `:=`(
    eff_diff = fifelse(is.na(home_pts_total - away_pts_total), 0, home_pts_total - away_pts_total),
    pace = fifelse(is.na((home_pace + away_pace) / 2), 0, (home_pace + away_pace) / 2),
    momentum_diff = fifelse(is.na(home_momentum - away_momentum), 0, home_momentum - away_momentum),
    fatigue = pmin(fifelse(is.na(home_rest_days), 0, home_rest_days), fifelse(is.na(away_rest_days), 0, away_rest_days)),
    efg_diff = fifelse(is.na(home_efg - away_efg), 0, home_efg - away_efg),
    ts_diff = fifelse(is.na(home_trueShooting - away_trueShooting), 0, home_trueShooting - away_trueShooting),
    tov_diff = fifelse(is.na(home_tov - away_tov), 0, home_tov - away_tov),
    orb_diff = fifelse(is.na(home_orb - away_orb), 0, home_orb - away_orb),
    ftr_diff = fifelse(is.na(home_ftr - away_ftr), 0, home_ftr - away_ftr),
    paint_diff = fifelse(is.na(home_pts_paint - away_pts_paint), 0, home_pts_paint - away_pts_paint),
    fastbreak_diff = fifelse(is.na(home_pts_fb - away_pts_fb), 0, home_pts_fb - away_pts_fb),
    off_to_diff = fifelse(is.na(home_pts_off_to - away_pts_off_to), 0, home_pts_off_to - away_pts_off_to),
    b2b_flag = fifelse(home_b2b | away_b2b, 1L, 0L)
  )]

  if (has_scores) {
    gd[, `:=`(moneyline = fifelse(homePoints > awayPoints, 1L, 0L),
              spread = homePoints - awayPoints)]
  }

  gd
}

# ---------------------- modeling -----------------------------
run_pipeline <- function() {
  message("Fetching data…")
  games_hist <- bind_safely(lapply(historical_seasons, fetch_games, status = "final"))
  team_stats <- bind_safely(lapply(historical_seasons, fetch_team_stats))
  future_games <- fetch_games(future_season, status = "scheduled")

  if (nrow(games_hist) == 0 || nrow(team_stats) == 0) {
    stop("No data returned from API; cannot continue.")
  }

  message("Computing Elo and power indices…")
  final_elo <- calculate_final_elo(games_hist)
  power_lookup <- final_elo - mean(final_elo, na.rm = TRUE)
  avg_opp_lookup <- compute_avg_opp_index(games_hist, power_lookup)
  conf_lookup <- data.table(team_stats)[, .(conf_power = mean(power_lookup[team], na.rm = TRUE)), by = conference]

  message("Engineering features for historical games…")
  hist_features <- engineer_features(games_hist, team_stats, power_lookup, avg_opp_lookup, conf_lookup, TRUE)
  hist_features <- hist_features[!is.na(moneyline)]

  message("Engineering features for future games…")
  future_ts <- bind_safely(list(fetch_team_stats(future_season), team_stats[season == max(historical_seasons)]))
  future_dt <- as.data.table(future_games)
  future_dt[, `:=`(homePoints = NA_real_, awayPoints = NA_real_)]
  future_features <- engineer_features(future_dt, future_ts, power_lookup, avg_opp_lookup, conf_lookup, FALSE)

  features <- c("power_diff", "home_vs_avg", "away_vs_avg", "avg_opp_gap", "conf_power_diff",
               "eff_diff", "pace", "momentum_diff", "fatigue", "efg_diff", "ts_diff",
               "tov_diff", "orb_diff", "ftr_diff", "paint_diff", "fastbreak_diff", "off_to_diff",
               "b2b_flag")
  message("Time-based train/validation split…")
  cutoff_season <- max(historical_seasons)
  train_dt <- hist_features[season < cutoff_season]
  valid_dt <- hist_features[season == cutoff_season]

  cat_train <- catboost.load_pool(as.matrix(train_dt[, ..features]), label = train_dt$moneyline)
  cat_valid <- catboost.load_pool(as.matrix(valid_dt[, ..features]), label = valid_dt$moneyline)

  message("Training CatBoost (moneyline)…")
  cat_model <- catboost.train(cat_train, params = list(
    iterations = 300,
    depth = 4,
    learning_rate = 0.05,
    loss_function = "Logloss",
    eval_metric = "AUC",
    random_seed = 2025,
    od_type = "Iter",
    od_wait = 40,
    verbose = 50
  ), test_pool = cat_valid)

  message("Training Quantile Regression Forest (spread)…")
  qrf_model <- quantregForest(
    x = as.data.frame(train_dt[, ..features]),
    y = train_dt$spread,
    ntree = 400,
    mtry = max(2, floor(length(features) / 3)),
    nodesize = 5
  )

  message("Scoring validation and future games…")
  valid_pool <- catboost.load_pool(as.matrix(valid_dt[, ..features]))
  future_pool <- catboost.load_pool(as.matrix(future_features[, ..features]))

  valid_dt[, money_prob := catboost.predict(cat_model, valid_pool, prediction_type = "Probability")]
  valid_dt[, spread_pred := predict(qrf_model, as.data.frame(valid_dt[, ..features]), what = mean)]

  future_predictions <- copy(future_features)
  future_predictions[, `:=`(
    money_prob = catboost.predict(cat_model, future_pool, prediction_type = "Probability"),
    spread_pred = predict(qrf_model, as.data.frame(future_features[, ..features]), what = mean),
    spread_lower = predict(qrf_model, as.data.frame(future_features[, ..features]), what = 0.1),
    spread_upper = predict(qrf_model, as.data.frame(future_features[, ..features]), what = 0.9)
  )]

  saveRDS(cat_model, file = "cat_model_moneyline.rds")
  saveRDS(qrf_model, file = "qrf_model_spread.rds")
  saveRDS(hist_features, file = "historical_features.rds")
  saveRDS(future_predictions, file = "future_predictions.rds")
  list(valid = valid_dt, future = future_predictions, cat_model = cat_model, qrf_model = qrf_model)
}

save_pipeline_results <- function(pipeline_result, output_dir = "data") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (!is.null(pipeline_result$valid)) {
    fwrite(pipeline_result$valid, file.path(output_dir, "validation_predictions.csv"))
    metrics <- compute_validation_metrics(pipeline_result$valid)
    fwrite(metrics, file.path(output_dir, "validation_metrics.csv"))
  }

  if (!is.null(pipeline_result$future)) {
    fwrite(pipeline_result$future, file.path(output_dir, "future_predictions.csv"))
  }


  invisible(pipeline_result)
}

if (interactive()) {
  run_pipeline()
}

predictions_2025 <- readRDS("future_predictions.rds")

conference_power_rankings <- predictions_2025 %>%
  select(homeConference, awayConference, home_conf_power, away_conf_power) %>%
  transmute(
    conf = homeConference,
    power = home_conf_power
  ) %>%
  bind_rows(
    predictions_2025 %>%
      transmute(
        conf = awayConference,
        power = away_conf_power
      )
  ) %>%
  group_by(conf) %>%
  summarise(
    avg_conf_power = mean(power, na.rm = TRUE),
    n_games = n()
  ) %>%
  arrange(desc(avg_conf_power))

conference_power_rankings

Sys.setenv(GEMINI_API_KEY = "")

# -------------------------
# BOT PRE-SITE (Gemini) using your working Bovada lines pipeline
# -------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(httr2)
})

Sys.setenv(GEMINI_API_KEY = "")

filter_ny_today_plus1 <- function(df, start_col = "startDate") {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!(start_col %in% names(df))) return(df[0, , drop = FALSE])

  today_ny <- as.Date(Sys.time(), tz = "America/New_York")
  end_ny   <- today_ny

  df |>
    dplyr::mutate(
      startNY = as.Date(as.POSIXct(.data[[start_col]], tz = "UTC"), tz = "America/New_York")
    ) |>
    dplyr::filter(startNY >= today_ny, startNY <= end_ny)
}

# 1) Build Bovada lines for today (your working functions)
lines_raw <- fetch_lines(2026)
lines_long <- flatten_lines(lines_raw)
lines_bov <- lines_long |>
  filter(provider == "Bovada")

# 2) Reduce to 1 row per game (Bovada only) and rename to market_* fields
# (If Bovada ever duplicates, keep the first)
bov_market <- lines_bov %>%
  arrange(startDate) %>%
  group_by(gameId) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  transmute(
    gameId,
    startDate,
    homeTeamId, awayTeamId,
    homeTeam, awayTeam,
    market_spread = as.numeric(spread),
    market_total  = as.numeric(overUnder),
    spread_open   = as.numeric(spreadOpen),
    total_open    = as.numeric(overUnderOpen),
    market_provider = provider
  )

# 3) Join onto predictions (use robust keys)
# Try gameId via sourceId first; otherwise fall back to startDate+teamIds
pred2 <- predictions_2025 %>%
  mutate(
    gameId = suppressWarnings(as.integer(sourceId))
  )

joined <- pred2 %>%
  left_join(bov_market, by = "gameId") %>%
  mutate(
    # fallback join if gameId is missing / not matching
    market_spread = if_else(is.na(market_spread),
                            as.numeric(NA), market_spread)
  )

# If you want the fallback join when gameId doesn't work:
missing_market <- mean(is.na(joined$market_spread))
if (missing_market > 0.2) {
  joined <- pred2 %>%
    left_join(
      bov_market %>% select(startDate, homeTeamId, awayTeamId, market_spread, market_total, spread_open, total_open, market_provider),
      by = c("startDate", "homeTeamId", "awayTeamId")
    )
}

# 4) Compute edges
joined <- joined %>%
  mutate(
    market_spread = as.numeric(market_spread),
    spread_pred   = as.numeric(spread_pred),
    edge_spread   = spread_pred - market_spread,
    abs_edge_spread = abs(edge_spread),
    line_move_spread = market_spread - spread_open
  )

# 5) Build Gemini slate JSON (top N)
slate_json <- joined %>%
  filter(!is.na(market_spread), !is.na(spread_pred)) %>%
  mutate(
    # Model outputs (already margins)
    model_margin  = as.numeric(spread_pred),
    model_lower   = as.numeric(spread_lower),
    model_upper   = as.numeric(spread_upper),

    # Convert market line -> implied margin
    market_margin = -as.numeric(market_spread),

    # True edge: model vs market margin
    edge_spread = model_margin - market_margin
  ) %>%
  arrange(desc(abs(edge_spread))) %>%
  slice_head(n = 40) %>%
  transmute(
    startDate,
    awayTeam,
    homeTeam,

    market_provider,

    # Market (what bettors see)
    spread_open,
    market_spread,
    market_margin,

    # Model
    model_margin,
    model_lower,
    model_upper,

    # Edge
    edge_spread,

    # Extras
    money_prob,
    neutralSite,
    conferenceGame
  ) %>%
  (\(df) jsonlite::toJSON(
    list(
      date = as.character(Sys.Date()),
      n_games = nrow(df),
      games = df
    ),
    auto_unbox = TRUE,
    digits = 6,
    null = "null"
  ))()

`%||%` <- function(a, b) if (is.null(a)) b else a

gemini_bot_text <- function(slate_json,
                            model = "gemini-2.5-flash",
                            api_key = Sys.getenv("GEMINI_API_KEY"),
                            max_output_tokens = 2200,
                            thinkingConfig = list(thinkingBudget = 0),
                            temperature = 0.4,
                            debug = FALSE) {

  if (!nzchar(api_key)) stop("Missing GEMINI_API_KEY in environment.")

  sys <- "TITLE
College Basketball Full-Slate Market Classification (Bookmaker + Model Logic)

ROLE & OBJECTIVE
You are acting as a senior sportsbook oddsmaker and quantitative market analyst.
Your job is to evaluate EVERY game on today’s college basketball slate, infer the
correct side mechanically, and classify each game based on edge strength and
market confidence.

You are classifying price quality, not predicting vibes.

────────────────────────────────
HARD INFERENCE RULES (MANDATORY)
────────────────────────────────

WIN-SIDE DETERMINATION (DO NOT OVERRIDE)

1) Moneyline Probability
• If moneyline_prob ≥ 50.0% → HOME team is the model side
• If moneyline_prob < 50.0% → AWAY team is the model side

2) Spread Prediction Direction
• If spread_pred > 0 → HOME team favored
• If spread_pred < 0 → AWAY team favored

3) Side Selection Constraint
• Selected team MUST align with BOTH signals
• If signals conflict → downgrade confidence (never upgrade)
• Never flip sides for narrative or intuition

────────────────────────────────
MODEL INTERPRETATION
────────────────────────────────

• spread_pred ≈ median expected margin
• Market spreads reflect bias, liquidity, and anchoring
• Small edges can be valid if consistent
• Large edges require structural confidence

You may infer volatility, efficiency, and pricing sharpness like a professional book.

────────────────────────────────
CORE RULES (MANDATORY)
────────────────────────────────

1) Analyze EVERY game provided (no limits, no filtering)
2) Each game appears EXACTLY ONCE
3) One team per game
4) No parlays, promos, or bankroll framing
5) No dramatics or fear language

────────────────────────────────
BET CLASSIFICATION BUCKETS
────────────────────────────────

Assign EACH game to exactly one:

MUST BET
• Clear mispricing
• Durable edge
• Strong model alignment

GOOD BET
• Solid edge
• Minor uncertainty
• Acceptable at standard limits

OKAY BET
• Thin edge
• Market near fair
• Lean only

AVOID
• Efficient pricing
• Inflated or noisy
• No actionable edge

────────────────────────────────
OUTPUT FORMAT (STRICT)
────────────────────────────────

ALL PICKS

MUST BET
• Team A @ Team B: Team Name spread_pred
• Team A @ Team B: TTeam Name spread_pred

GOOD BET
• Team A @ Team B: TTeam Name spread_pred

OKAY BET
• Team A @ Team B: TTeam Name spread_pred

AVOID
• Team A @ Team B: TTeam Name spread_pred

Rules:
• Only team name + spread_pred
• No inline explanations
• No market spreads unless needed for sign clarity
* Must classify all availale games for that day. 

────────────────────────────────
INFERRED BETS MODULE (SEPARATE)
────────────────────────────────

PURPOSE
After ALL PICKS are complete, identify a SMALL SET of additional betting angles
that are logically implied by the model outputs or market structure.

These are speculative, non-primary, and must NOT alter or replace any pick above.

RULES
1) Maximum 10 inferred bets
2) Must NOT contradict selected sides
3) May include totals, alt spreads, derivatives
4) No parlays
5) Do NOT invent markets
6) Clearly labeled as speculative

RANKING
• Rank inferred bets by confidence (High / Medium / Low)
• Confidence reflects structural support, not payout appeal

INFERRED BETS OUTPUT FORMAT (STRICT)

INFERRED BETS (NON-PRIMARY, RANKED)

1) Confidence: High
   Game: Team A vs Team B
   Angle: Team A alt -6.5
   Rationale: Model median and distribution tails support margin clearance

2) Confidence: Medium
   Game: Team C vs Team D
   Angle: Under 143.5
   Rationale: Implied pace and margin suggest suppressed scoring

3) Confidence: Low
   Game: Team E vs Team F
   Angle: Team F first half +4.5
   Rationale: Narrow distribution favors early competitiveness

If no inferred bets meet criteria, state:
“No additional inferred bets meet structural criteria.”

────────────────────────────────
FINAL LINE (EXACTLY ONE SENTENCE)
────────────────────────────────

End with one neutral sentence summarizing overall slate quality."

  # IMPORTANT: Gemini expects camelCase keys here
  body <- list(
    systemInstruction = list(parts = list(list(text = sys))),
    contents = list(list(role = "user", parts = list(list(text = slate_json)))),
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = max_output_tokens
    )
  )


  url <- paste0(
    "https://generativelanguage.googleapis.com/v1beta/models/",
    model,
    ":generateContent"
  )

  req <- httr2::request(url) |>
    httr2::req_headers(
      `x-goog-api-key` = api_key,
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body)

  out <- req |> httr2::req_perform() |> httr2::resp_body_json()


  if (debug) {
    message("finishReason: ", out$candidates[[1]]$finishReason %||% "NA")
    if (!is.null(out$usageMetadata)) print(out$usageMetadata)
    message("parts: ", length(out$candidates[[1]]$content$parts))
  }

  parts <- out$candidates[[1]]$content$parts
  text <- paste0(vapply(parts, function(p) p$text %||% "", character(1)), collapse = "")

  text
}
bot_text <- gemini_bot_text(slate_json, max_output_tokens = 15000, debug = TRUE)
cat(bot_text)

source("build_hefml_hoops_site_with_grid_v17_sticky_nav_only.R")

build_hefml_hoops_site(
  pred_data   = joined,   # includes market_spread + edge now
  output_file = "hefml_hoops_1_14.html",
  bot_text    = bot_text
)
