# ------------------------------------------------------------------
# HefML Hoops – build HTML cards page from pred_data
# ------------------------------------------------------------------
# Usage after pred_data is created:
#   source("build_hefml_hoops_site.R")
#   build_hefml_hoops_site(pred_data,
#                          output_file = "HefML_Hoops_cards_tint_glow.html")
# ------------------------------------------------------------------

build_hefml_hoops_site <- function(pred_data,
                                   output_file = "HefML_Hoops_cards_tint_glow.html") {

  # --- required cols ------------------------------------------------
  needed <- c("startDate", "homeTeam", "awayTeam",
              "money_prob", "spread_lower", "spread_pred", "spread_upper")
  missing <- setdiff(needed, names(pred_data))
  if (length(missing) > 0) {
    stop("pred_data is missing columns: ", paste(missing, collapse = ", "))
  }

  # --- helpers ------------------------------------------------------
  html_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    x <- gsub(">", "&gt;",  x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x
  }

  # tiny basketball icon used in header + date
  ball_icon <- '<svg viewBox="0 0 24 24" width="16" height="16" aria-hidden="true"><circle cx="12" cy="12" r="10" fill="#ff9640" stroke="#f5b27a" stroke-width="1.5"/><path d="M2,12 H22 M12,2 V22 M5,7 C10,12 14,13 19,17 M5,17 C10,13 14,12 19,7" stroke="#2b1406" stroke-width="1.5" fill="none"/></svg>'

  # --- time handling ------------------------------------------------
  dt_utc <- as.POSIXct(pred_data$startDate,
                       format = "%Y-%m-%dT%H:%M:%OSZ",
                       tz = "UTC")

  pred_data$start_et <- format(
    dt_utc, tz = "America/New_York",
    format = "%b %d, %Y %I:%M %p ET"
  )

  # month key for ordering; month label for display
  pred_data$month_key   <- format(dt_utc, tz = "America/New_York", format = "%Y-%m")
  pred_data$month_label <- format(dt_utc, tz = "America/New_York", format = "%B %Y")

  # sort all rows by datetime
  ord <- order(dt_utc)
  pred_data <- pred_data[ord, , drop = FALSE]

  # unique months in chronological order
  month_keys   <- unique(pred_data$month_key)
  month_labels <- vapply(
    month_keys,
    function(k) pred_data$month_label[pred_data$month_key == k][1],
    character(1)
  )

  # --- conference power / ranking info (used for chips + table) ----
  needed_conf_cols <- c("homeConference", "awayConference",
                        "home_conf_power", "away_conf_power")

  conf_info <- list(conf_tbl = NULL, rank_lookup = NULL)

  if (all(needed_conf_cols %in% names(pred_data))) {
    conf_home <- data.frame(
      conf  = as.character(pred_data$homeConference),
      power = as.numeric(pred_data$home_conf_power),
      stringsAsFactors = FALSE
    )
    conf_away <- data.frame(
      conf  = as.character(pred_data$awayConference),
      power = as.numeric(pred_data$away_conf_power),
      stringsAsFactors = FALSE
    )
    conf_df <- rbind(conf_home, conf_away)

    # drop missing / blank conference names or powers
    conf_df <- conf_df[!is.na(conf_df$conf) & conf_df$conf != "" &
                       !is.na(conf_df$power), , drop = FALSE]

    if (nrow(conf_df) > 0) {
      avg_power <- tapply(conf_df$power, conf_df$conf,
                          function(x) mean(x, na.rm = TRUE))
      n_games   <- tapply(conf_df$power, conf_df$conf,
                          function(x) sum(!is.na(x)))

      conf_tbl <- data.frame(
        conference     = names(avg_power),
        avg_conf_power = as.numeric(avg_power),
        n_games        = as.integer(n_games[names(avg_power)]),
        stringsAsFactors = FALSE
      )

      # rank by average power, desc; break ties by name
      conf_tbl <- conf_tbl[order(-conf_tbl$avg_conf_power,
                                 conf_tbl$conference), , drop = FALSE]

      # keep top 32 only
      if (nrow(conf_tbl) > 32) {
        conf_tbl <- conf_tbl[1:32, , drop = FALSE]
      }

      rank_lookup <- seq_len(nrow(conf_tbl))
      names(rank_lookup) <- conf_tbl$conference

      conf_info$conf_tbl <- conf_tbl
      conf_info$rank_lookup <- rank_lookup
    }
  }

  rank_to_tier <- function(rank_val) {
    if (is.na(rank_val)) return(list(label = "", cls = ""))
    if (rank_val <= 5)   return(list(label = "Top 5",  cls = "tier-top5"))
    if (rank_val <= 10)  return(list(label = "Top 10", cls = "tier-top10"))
    if (rank_val <= 15)  return(list(label = "Top 15", cls = "tier-top15"))
    if (rank_val <= 20)  return(list(label = "Top 20", cls = "tier-top20"))
    list(label = "21+", cls = "tier-21plus")
  }

  # --- HTML HEAD / STYLES / FILTER SCRIPT --------------------------
  html_head <- paste(
'<!doctype html><html><head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover"/>
<title>HefML Hoops — Fav Tint + Glow</title>
<style>
:root{--bg:#000;--card:rgba(255,255,255,.06);--txt:#eaf0ff;--muted:#a3b2d7;--r:14px;
      --good:#16a34a;--bad:#dc2626;--chip:#0d1016;--chipBd:#2b2f39;
      --home:#fbbf24;--away:#60a5fa;--glow:#ff9f1a}
*{box-sizing:border-box}
body{margin:0;background:#000;color:var(--txt);font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Inter,Arial}
.container{max-width:980px;margin:0 auto;padding:12px}
.headerbar{display:flex;align-items:center;gap:8px;padding:12px 0}
.headerbar .title{display:flex;align-items:center;gap:8px;font-weight:900;font-size:22px;letter-spacing:.02em}
.headerbar svg{flex:none}
.card{background:var(--card);border:1px solid rgba(255,255,255,.08);border-radius:14px;margin:10px 0}
.inner{padding:10px}
/* logo grid at top */
.logo-grid-card{margin:10px 0;}
.logo-grid-title{font-size:12px;color:var(--muted);margin-bottom:4px;}
.logo-grid-title b{color:var(--txt);}
.logo-grid-wrapper{position:relative;height:800px;margin-top:8px;overflow:visible;}
.logo-grid{position:relative;width:100%;height:100%;border-radius:12px;
           border:1px solid rgba(255,255,255,.12);
           background:
             radial-gradient(circle at 0 0,rgba(96,165,250,.35),transparent 55%),
             radial-gradient(circle at 100% 100%,rgba(248,250,252,.18),transparent 55%);
           overflow:visible;}
.logo-grid::before{content:"";position:absolute;left:50%;top:0;bottom:0;width:1px;
                   background:linear-gradient(to bottom,rgba(148,163,184,.1),
                                              rgba(148,163,184,.6),
                                              rgba(148,163,184,.1));}
.logo-grid::after{content:"";position:absolute;top:50%;left:0;right:0;height:1px;
                  background:linear-gradient(to right,rgba(148,163,184,.1),
                                             rgba(148,163,184,.6),
                                             rgba(148,163,184,.1));}
.team-logo{position:absolute;transform:translate(-50%,-50%);
           border-radius:999px;overflow:hidden;
           box-shadow:0 0 0 1px rgba(15,23,42,.7),0 10px 20px rgba(15,23,42,.9);
           display:flex;align-items:center;justify-content:center;
           background:#020617;}
.team-logo img{width:100%;height:auto;object-fit:contain;}
.logo-axis-label{position:absolute;font-size:10px;color:var(--muted);
                 text-shadow:0 1px 2px #000;pointer-events:none;}
.logo-axis-label.x{left:50%;bottom:4px;transform:translateX(-50%);}
.logo-axis-label.y{top:50%;left:4px;transform:translateY(-50%) rotate(-90deg);
                   transform-origin:left center;}


/* month filter */
.filter-row{display:flex;align-items:center;gap:8px;margin:0 0 8px 0;font-size:12px;color:var(--muted);}
.filter-row select{background:#05060a;color:var(--txt);border-radius:999px;border:1px solid rgba(255,255,255,.22);padding:4px 10px;font-size:12px}

/* Chips */
.chip{display:inline-block;padding:1px 6px;border-radius:999px;background:var(--chip);
      border:1px solid var(--chipBd);font-weight:700;font-variant-numeric:tabular-nums}
.chip-good{color:#34d399;background:rgba(20,40,24,.55);border-color:#194f2b}
.chip-bad{color:#f87171;background:rgba(56,20,20,.55);border-color:#5a1f1f}
.chip-pos{color:var(--away)} .chip-neg{color:var(--home)} /* pos=away fav, neg=home fav */
.conf-chip{margin-left:6px;font-size:11px;padding:1px 7px;border:1px solid rgba(255,255,255,.12);
           color:#d9e6ff;background:rgba(255,255,255,.04);}
.tier-top5{background:rgba(22,163,74,.18);border-color:rgba(22,163,74,.4);color:#b2f5cc}
.tier-top10{background:rgba(74,222,128,.12);border-color:rgba(22,163,74,.35);color:#d0fadf}
.tier-top15{background:rgba(234,179,8,.12);border-color:rgba(234,179,8,.35);color:#fef9c3}
.tier-top20{background:rgba(249,115,22,.12);border-color:rgba(249,115,22,.35);color:#fed7aa}
.tier-21plus{background:rgba(220,38,38,.12);border-color:rgba(220,38,38,.35);color:#fecaca}

/* Game cards */
.gcard{background:rgba(255,255,255,.06);border:1px solid rgba(255,255,255,.08);
       border-radius:12px;padding:8px 10px;margin:8px 0;position:relative}
.gcard.fav-home{box-shadow:0 0 0 2px rgba(251,191,36,.35),0 0 18px rgba(251,191,36,.18) inset}
.gcard.fav-away{box-shadow:0 0 0 2px rgba(96,165,250,.35),0 0 18px rgba(96,165,250,.18) inset}
.gcard.hotglow{outline:2px solid rgba(255,159,26,.25); outline-offset:-2px}
.gcard .row1{display:flex;justify-content:space-between;align-items:center;gap:8px;margin-bottom:6px}
.gcard .date{font-size:12px;color:var(--muted);line-height:1.1;display:flex;align-items:center;gap:6px}
.gcard .row2{display:flex;justify-content:space-between;gap:8px;align-items:flex-start;margin-bottom:4px}
.gcard .teams{flex:1 1 auto;min-width:0}
.gcard .home,.gcard .away{font-size:13px;line-height:1.15;word-break:break-word}
.fav{color:#fff}
.gcard .row3{display:flex;justify-content:space-between;align-items:center;gap:8px}
.gcard .labels{display:flex;gap:8px}
.gcard .labels .lbl{font-size:10px;color:var(--muted);text-transform:uppercase;letter-spacing:.06em}
.gcard .spreads{display:flex;gap:6px;align-items:center}

/* Team sections */
.team-details{background:rgba(255,255,255,.04);border:1px solid rgba(255,255,255,.1);
              border-radius:12px;margin:10px 0;overflow:hidden}
.team-details > summary{cursor:pointer;padding:10px 12px;font-weight:800;list-style:none}
.team-details > summary::-webkit-details-marker{display:none}

.month-block{margin:10px 0}

/* responsive tweaks */
@media (max-width:400px){
  .container{padding:10px}
  .gcard .home,.gcard .away{font-size:13px}
  .chip{padding:0 5px;font-size:11px}
}
</style>
<script>
document.addEventListener("DOMContentLoaded", function(){
  var sel = document.getElementById("monthFilter");
  if(!sel) return;
  sel.addEventListener("change", function(){
    var val = this.value;
    document.querySelectorAll(".month-block").forEach(function(el){
      if(val === "ALL" || el.getAttribute("data-month") === val){
        el.style.display = "";
      } else {
        el.style.display = "none";
      }
    });
  });
});
</script>
</head><body>

<div class="container">
  <div class="headerbar">
    <div class="title">
      <svg viewBox="0 0 24 24" width="18" height="18" aria-hidden="true"><circle cx="12" cy="12" r="10" fill="#ff9640" stroke="#f5b27a" stroke-width="1.5"/><path d="M2,12 H22 M12,2 V22 M5,7 C10,12 14,13 19,17 M5,17 C10,13 14,12 19,7" stroke="#2b1406" stroke-width="1.5" fill="none"/></svg>
      <span>HefML Hoops</span>
    </div>
  </div>
</div>

<div class="container">
  <div class="card"><div class="inner">
    <div class="filter-row">
      <span>Filter by month:</span>
      <select id="monthFilter">
        <option value="ALL">All</option>',
    sep = "\n"
  )

  # add month options into the select
  month_options <- paste(
    sprintf('        <option value="%s">%s</option>', month_keys, month_labels),
    collapse = "\n"
  )

  html_head <- paste0(
    html_head, "\n", month_options,
'
      </select>
    </div>
  </div></div>
</div>

<div class="container">
'
  )

  html_tail <- "\n</div>\n</body>\n</html>\n"

  # --- builder for a single game card ------------------------------
  build_card <- function(row) {
    money <- as.numeric(row[["money_prob"]])
    low   <- as.numeric(row[["spread_lower"]])
    mid   <- as.numeric(row[["spread_pred"]])
    high  <- as.numeric(row[["spread_upper"]])

    home <- html_escape(as.character(row[["homeTeam"]]))
    away <- html_escape(as.character(row[["awayTeam"]]))
    date <- as.character(row[["start_et"]])

    conf_chip <- function(conf_name) {
      if (is.null(conf_info$rank_lookup) || length(conf_name) == 0 || is.na(conf_name) || conf_name == "") return("")
      rk <- conf_info$rank_lookup[[as.character(conf_name)]]
      tier <- rank_to_tier(if (is.null(rk)) NA_integer_ else rk)
      cls <- paste("conf-chip", tier$cls)
      label <- if (nzchar(tier$label)) paste0(" • ", tier$label) else ""
      sprintf('<span class="%s">%s%s</span>', cls, html_escape(conf_name), label)
    }

    home_conf_html <- conf_chip(row[["homeConference"]])
    away_conf_html <- conf_chip(row[["awayConference"]])

    fav_home <- !is.na(money) && money >= 0.5
    gcard_cls <- if (fav_home) "gcard fav-home" else "gcard fav-away"
    if (!is.na(money) && (money >= 0.85 || money <= 0.15)) {
      gcard_cls <- paste(gcard_cls, "hotglow")
    }

    money_cls <- if (!is.na(money) && money >= 0.5) "chip chip-good" else "chip chip-bad"
    money_txt <- if (is.na(money)) "–" else sprintf("%.1f%%", money * 100)

    spread_chip <- function(val) {
      if (is.na(val)) {
        '<span class="chip">nan</span>'
      } else {
        cls <- if (val >= 0) "chip chip-pos" else "chip chip-neg"
        sprintf('<span class="%s">%s</span>', cls,
                format(round(val, 1), nsmall = 1))
      }
    }

    home_html <- if (fav_home) {
      sprintf('<div class="home"><b class="fav">%s</b>%s</div>', home, home_conf_html)
    } else {
      sprintf('<div class="home">%s%s</div>', home, home_conf_html)
    }
    away_html <- if (!fav_home) {
      sprintf('<div class="away"><b class="fav">%s</b>%s</div>', away, away_conf_html)
    } else {
      sprintf('<div class="away">%s%s</div>', away, away_conf_html)
    }

    paste0(
      '\n<div class="', gcard_cls, '">',
      '\n  <div class="row1">',
      '\n    <div class="date">', ball_icon, ' <span>', date, '</span></div>',
      '\n    <div class="money"><span class="', money_cls, '">', money_txt, '</span></div>',
      '\n  </div>',
      '\n  <div class="row2">',
      '\n    <div class="teams">',
      '\n      ', home_html,
      '\n      ', away_html,
      '\n    </div>',
      '\n  </div>',
      '\n  <div class="row3">',
      '\n    <div class="labels"><span class="lbl">low</span><span class="lbl">pred</span><span class="lbl">up</span></div>',
      '\n    <div class="spreads">',
      spread_chip(low), spread_chip(mid), spread_chip(high),
      '</div>',
      '\n  </div>',
      '\n</div>\n'
    )
  }


  # --- logo grid section (favored margin vs SOS) --------------------
  logo_section <- ""

  # attempt to build logo grid if mbb_team_info.rda is available
  if (file.exists("mbb_team_info.rda")) {
    mbb_obj_name <- load("mbb_team_info.rda")
    mbb_team_info <- get(mbb_obj_name)
    if (!("espn" %in% names(mbb_team_info)) ||
        !("team_logo_espn" %in% names(mbb_team_info))) {
      warning("mbb_team_info.rda found but missing espn/team_logo_espn columns; skipping logo grid.")
    } else {
      # build long team-game view
      home_df <- data.frame(
        team        = as.character(pred_data$homeTeam),
        opponent    = as.character(pred_data$awayTeam),
        is_home     = TRUE,
        spread_pred = pred_data$spread_pred,
        money_prob  = pred_data$money_prob,
        stringsAsFactors = FALSE
      )
      away_df <- data.frame(
        team        = as.character(pred_data$awayTeam),
        opponent    = as.character(pred_data$homeTeam),
        is_home     = FALSE,
        spread_pred = pred_data$spread_pred,
        money_prob  = pred_data$money_prob,
        stringsAsFactors = FALSE
      )
      games_long <- rbind(home_df, away_df)

      # which side is favored in this game's view?
      # spread_pred > 0 => home favored; spread_pred < 0 => away favored
      games_long$team_is_fav <- (games_long$is_home & games_long$spread_pred > 0) |
                                (!games_long$is_home & games_long$spread_pred < 0)

      # predicted margin from this team's perspective
      # home perspective = spread_pred; away perspective = -spread_pred
      games_long$team_margin_pred <- ifelse(
        games_long$is_home,
        games_long$spread_pred,
        -games_long$spread_pred
      )

      # team win probability and opponent win prob
      # money_prob is home win prob; away win prob = 1 - money_prob
      games_long$team_win_prob <- ifelse(
        games_long$is_home,
        games_long$money_prob,
        1 - games_long$money_prob
      )
      games_long$opp_win_prob <- 1 - games_long$team_win_prob

# --- schedule-level favored counts -------------------------
# Per team: how many times THEY are favored
fav_counts <- tapply(
  games_long$team_is_fav,
  games_long$team,
  function(x) sum(x, na.rm = TRUE)
)

# For each row, map opponent's favored-count
games_long$opp_fav_total <- fav_counts[games_long$opponent]

# Per team: total times opponents on their schedule are favored
schedule_opp_fav_total <- tapply(
  games_long$opp_fav_total,
  games_long$team,
  function(x) sum(x, na.rm = TRUE)
)
# ------------------------------------------------------------

      # keep only games where this team is favored and inputs are not NA
      fav <- subset(
        games_long,
        team_is_fav & !is.na(team_margin_pred) & !is.na(opp_win_prob)
      )

      if (nrow(fav) > 0) {
        # aggregate per team
        avg_margin <- tapply(fav$team_margin_pred, fav$team, mean, na.rm = TRUE)
        sos        <- tapply(fav$opp_win_prob,      fav$team, mean, na.rm = TRUE)
        games_fav  <- tapply(fav$team_margin_pred,  fav$team, length)

        team_grid <- data.frame(
          team               = names(avg_margin),
          games_favored      = as.integer(games_fav[names(avg_margin)]),
          avg_margin_favored = as.numeric(avg_margin),
          sos_moneyprob      = as.numeric(sos[names(avg_margin)]),
          stringsAsFactors   = FALSE
        )

        # attach logos from mbb_team_info (espn column)
        idx_match <- match(team_grid$team, mbb_team_info$espn)
        team_grid$team_logo_espn <- mbb_team_info$team_logo_espn[idx_match]

        # drop teams without any logo URL
        team_grid <- subset(team_grid, !is.na(team_logo_espn) & team_logo_espn != "")

        if (nrow(team_grid) > 0) {
          # normalize axes to 0–100 for grid positioning, with padding
          xr <- range(team_grid$avg_margin_favored, na.rm = TRUE)
          yr <- range(team_grid$sos_moneyprob,      na.rm = TRUE)

          pad <- 8
          span <- 100 - 2 * pad

          if (diff(xr) == 0) {
            team_grid$x_pct <- 50
          } else {
            team_grid$x_pct <- pad + (team_grid$avg_margin_favored - xr[1]) / diff(xr) * span
          }

          if (diff(yr) == 0) {
            team_grid$y_pct <- 50
          } else {
            team_grid$y_pct <- pad + (team_grid$sos_moneyprob - yr[1]) / diff(yr) * span
          }

          # build logo divs
          logo_divs <- character(nrow(team_grid))
          for (i in seq_len(nrow(team_grid))) {
            tm    <- html_escape(team_grid$team[i])
            logo  <- team_grid$team_logo_espn[i]
            x     <- sprintf("%.1f", team_grid$x_pct[i])
            # invert y to use CSS bottom
            y     <- sprintf("%.1f", 100 - team_grid$y_pct[i])
            margin <- sprintf("%.1f", team_grid$avg_margin_favored[i])
            sos_pct <- sprintf("%.1f", team_grid$sos_moneyprob[i] * 100)

            logo_divs[i] <- paste0(
              '<div class="team-logo" style="left:', x, '%;bottom:', y, '%;width:4%;height:auto;" ',
              'title="', tm,
              '&#10;Fav margin: ', margin,
              '&#10;Opp win prob: ', sos_pct, '%">',
              '<img src="', logo, '" alt="', tm, '" loading="lazy"/>',
              '</div>'
            )
          }

          logo_section <- paste0(
            '\n  <div class="card logo-grid-card"><div class="inner">',
            '\n    <div class="logo-grid-title"><b>Model Landscape: Favored Margin vs Strength of Schedule</b><br/><small>Each logo is a team positioned by how strongly the model favors them and how tough their opponents are when they are favored.</small></div>',
            '\n    <div class="logo-grid-wrapper">',
            '\n      <div class="logo-grid">',
            paste(logo_divs, collapse = ""),
            '\n        <div class="logo-axis-label x">Times team is favored</div>',
            '\n        <div class="logo-axis-label y">Opponents favored on schedule \u2191</div>',
            '\n      </div>',
            '\n    </div>',
            '\n  </div></div>\n'
          )
        }
      }
    }
  }

  # --- conference power table (ESPN-style top 32) -------------------
  conf_section <- ""

  if (!is.null(conf_info$conf_tbl)) {
    conf_tbl <- conf_info$conf_tbl

    rows <- character(nrow(conf_tbl))
    for (i in seq_len(nrow(conf_tbl))) {
      rk        <- i
      conf_name <- html_escape(conf_tbl$conference[i])
      games     <- conf_tbl$n_games[i]
      power_val <- sprintf("%.3f", conf_tbl$avg_conf_power[i])

      rows[i] <- paste0(
        '\n          <tr>',
        '<td class="conf-rank">', rk, '.</td>',
        '<td class="conf-name">', conf_name, '</td>',
        '<td class="conf-games">', games, '</td>',
        '<td class="conf-power">', power_val, '</td>',
        '</tr>'
      )
    }

    conf_section <- paste0(
      '\n  <div class="card conf-card"><div class="inner">',
      '\n    <div class="conf-title"><b>Conference Power Index</b><br/><small>Top 32 conferences by average model power (home/away combined).</small></div>',
      '\n    <table class="conf-table">',
      '\n      <thead><tr>',
      '<th class="conf-rank">#</th>',
      '<th>Conference</th>',
      '<th class="conf-games">Games</th>',
      '<th class="conf-power">Avg Power</th>',
      '</tr></thead>',
      '\n      <tbody>',
      paste(rows, collapse = ""),
      '\n      </tbody>',
      '\n    </table>',
      '\n  </div></div>\n'
    )
  }

  # --- month sections (chronological) -------------------------------
  month_blocks <- character(length(month_keys))

  for (i in seq_along(month_keys)) {
    mk <- month_keys[i]
    ml <- month_labels[i]
    dfm <- pred_data[pred_data$month_key == mk, , drop = FALSE]

    cards_html <- paste(apply(dfm, 1, build_card), collapse = "")

    month_blocks[i] <- paste0(
      '\n  <div class="month-block" data-month="', mk, '">',
      '\n    <div class="card"><div class="inner"><b>', ml, '</b>',
      cards_html,
      '\n    </div></div>',
      '\n  </div>\n'
    )
  }

  # --- team schedules section --------------------------------------
  # list of all unique team names (home + away)
  all_teams <- sort(unique(c(
    as.character(pred_data$homeTeam),
    as.character(pred_data$awayTeam)
  )))

  team_section <- c(
    '\n  <div class="card"><div class="inner"><b>Schedules by Team</b><br/><small>Tap a team to expand.</small>'
  )

  for (tm in all_teams) {
    rows <- pred_data[pred_data$homeTeam == tm | pred_data$awayTeam == tm, , drop = FALSE]
    if (!nrow(rows)) next

    team_cards <- paste(apply(rows, 1, build_card), collapse = "")

    block <- paste0(
      '\n<details class="team-details">',
      '\n  <summary>', html_escape(tm), '</summary>',
      '\n  <div class="card"><div class="inner">',
      team_cards,
      '\n  </div></div>',
      '\n</details>\n'
    )
    team_section <- c(team_section, block)
  }

  team_section <- c(team_section, "\n  </div></div>\n")

  # --- compose final html ------------------------------------------
  full_html <- paste0(
    html_head,
    logo_section,
    conf_section,
    paste(month_blocks, collapse = ""),
    paste(team_section, collapse = ""),
    html_tail
  )

  writeLines(full_html, con = output_file, useBytes = TRUE)
  invisible(output_file)
}