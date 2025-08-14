library(shiny)
library(dplyr)
library(tidyr)
library(reactable)
library(plotly)

playerPageUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("theme_styles")),
    
    div(class = "themed-player-page",
        
        div(style = "margin-bottom: 20px; width: 300px;",
            selectInput(ns("selected_player"), "Select Player:", choices = NULL)
        ),
        
        uiOutput(ns("player_header")),
        
        radioButtons(ns("pie_toggle"), "Select Type:", choices = c("Offense", "Defense")),
        
        plotlyOutput(ns("playtype_pie")),
        
        plotlyOutput(ns("college_pie")),
        
        
        selectInput(ns("position_filter"), "Filter NBA Players by Position:", choices = NULL, multiple = TRUE),
        selectInput(
          ns("comparison_stat_type"),
          "Select Stat Type:",
          choices = c("Basic Stats", "P&R Ball Handler", "Spot Up", "Transition", "Isolation", "Post", "Cut", "Off Screen", "Handoffs", "Offensive Rebounds", "P&R Roll Man")
        ),
        DT::DTOutput(ns("prospect_table")),
        DT::DTOutput(ns("nba_team_table")),
        
        
        h3("Team Fit Weaknesses"),
        reactableOutput(ns("team_weaknesses")),
        
        h3("Player Stat Fit Table"),
        reactableOutput(ns("player_fit_table")),
        
        h3("Pick Assessment"),
        uiOutput(ns("player_breakdown"))
    )
  )
}




playerPageServer <- function(id, final_draft_synergy_percentiled, team_playtype_percentiled,
                             nba_full_combined_percentiled, nba_data, theme_colors, player_headshots, 
                             player_breakdowns, fit_function) {
  
  moduleServer(id, function(input, output, session) {
    apply_percentile_coloring <- function(table, df) {
      pctile_cols <- grep("%tile$", names(df), value = TRUE)
      if (length(pctile_cols) == 0) return(table)
      
      DT::formatStyle(
        table,
        columns = pctile_cols,
        backgroundColor = DT::styleInterval(
          c(20, 40, 60, 80),
          c("darkred", "orange", "gold", "yellowgreen", "green")
        ),
        color = 'white'
      )
    }
    clean_colnames <- function(cols) {
      playtype_clean <- c(
        "pnr_roll" = "P&R Roll Man",    
        "cut" = "Cut",
        "iso" = "Isolation",
        "pnr" = "P&R Ball Handler",
        "post" = "Post",
        "spot" = "Spot Up",
        "offscreen" = "Off Screen",
        "handoff" = "Handoff",
        "offreb" = "Off Rebounds",
        "transition" = "Transition"
      )
      
      new_cols <- cols
      
      new_cols <- gsub("^Pos\\.x$", "Pos", new_cols)
      
      new_cols <- gsub("_pctile$", " %tile", new_cols)
      
      for (ptype in names(playtype_clean)) {
        new_cols <- gsub(ptype, playtype_clean[[ptype]], new_cols, fixed = TRUE)
      }
      
      new_cols <- gsub("_", " ", new_cols)
      
      return(new_cols)
    }
    
    
    clean_stat_label <- function(stat) {
      if (stat == "player_height") return("Player Height")
      if (stat == "player_weight") return("Player Weight")
      if (stat == "Team_3PT") return("Team 3PT%")  
      
      stat <- gsub("_score$", " score", stat)
      stat <- gsub("_pct$", " %", stat)
      stat <- gsub("_pct", " %", stat)
      stat <- gsub("_per_", "/", stat)
      stat <- gsub("_", " ", stat)
      stat <- gsub("\\.x$", "", stat)
      stat <- gsub("usg bpm", "USG% * BPM", stat, ignore.case = TRUE)
      
      stat <- gsub("^(.)", "\\U\\1", stat, perl = TRUE)
      
      return(stat)
    }
    
    
    
     ns <- session$ns
     draft_order_players <- c(
       "Cooper Flagg", "Dylan Harper", "VJ Edgecombe", "Kon Knueppel", "Ace Bailey", "Tre Johnson",
       "Jeremiah Fears", "Egor Demin", "Collin Murray-Boyles", "Khaman Maluach", "Cedric Coward",
       "Noa Essengue", "Derik Queen", "Carter Bryant", "Thomas Sorber", "Hansen Yang", "Joan Beringer",
       "Walter Clayton Jr", "Nolan Traore", "Kasparas Jakucionis", "Will Riley", "Drake Powell",
       "Asa Newell", "Nique Clifford", "Jase Richardson", "Ben Saraf", "Danny Wolf", "Hugo Gonzalez",
       "Liam McNeeley", "Yanic Konan Niederhauser", "Rasheer Fleming", "Noah Penda", "Sion James",
       "Ryan Kalkbrenner", "Johni Broome", "Adou Thiero", "Chaz Lanier", "Kam Jones", "Alijah Martin",
       "Micah Peavy", "Koby Brea", "Maxime Raynaud", "Jamir Watkins", "Brooks Barnhizer", "Rocco Zikarsky",
       "Amari Williams", "Bogoljub Markovic", "Javon Small", "Tyrese Proctor", "Kobe Sanders",
       "Mohamed Diawara", "Alex Toohey", "John Tonje", "Taelon Peter", "Lachlan Olbrich", "Will Richard",
       "Max Shulga", "Saliou Niang", "Jahmai Mashack"
     )
     
     available_players <- unique(final_draft_synergy_percentiled$Player)
     
     ordered_choices <- draft_order_players[draft_order_players %in% available_players]
     
    
    observe({
      updateSelectInput(session, "selected_player", choices = ordered_choices, selected = ordered_choices[1])
    })
    
    player_row <- reactive({
      final_draft_synergy_percentiled %>% filter(Player == input$selected_player)
    })
    
    theme_row <- reactive({
      theme_colors %>% filter(Player == input$selected_player)
    })
    

    
    drafted_team <- reactive({
      theme_row()$NBA_Team[1]
    })
    
    
    player_headshot <- reactive({
      headshot <- player_headshots %>% filter(Player == input$selected_player)
      if (nrow(headshot) == 0) return(NULL)
      headshot$Picture[1]
    })
    
    
    output$theme_styles <- renderUI({
      theme <- theme_row()
      req(theme)
      
      primary <- theme$Primary_Color[1]
      secondary <- theme$Secondary_Color[1]
      
      tags$style(HTML(paste0("
    /* ---- Full Page Themed Background ---- */
    .container-fluid, .main-panel, .themed-player-page, body, html {
      background: linear-gradient(to bottom, ", primary, ", ", secondary, ") !important;
      color: white;
      width: 100%;
      min-height: 100vh;
      padding: 0;
      margin: 0;
    }

    /* ---- Reactable Table Header ---- */
    .reactable .rt-thead.-header {
      background-color: ", primary, " !important;
      color: white !important;
      font-weight: bold;
    }

    /* ---- Reactable Striped Rows ---- */
    .reactable .rt-tr.-even {
      background-color: ", secondary, ";
    }

    /* ---- DT Table Header ---- */
    table.dataTable thead {
      background-color: ", primary, " !important;
      color: white !important;
      font-weight: bold;
    }

    /* ---- DT Table Body ---- */
    table.dataTable {
      background-color: transparent !important;
      color: white;
      width: 100% !important;
    }

    /* ---- DT Table Body Cells ---- */
    table.dataTable tbody td {
      background-color: #212529;
      color: white;
    }

    /* ---- DT Pagination Buttons ---- */
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      background-color: ", secondary, " !important;
      color: white !important;
    }

    /* ---- Tighten Up Form Controls ---- */
    .themed-player-page .form-group {
      margin-bottom: 10px;
    }

    /* ---- Reduce Unwanted Body Margins ---- */
    body {
      margin: 0;
      padding: 0;
    }
  ")))
    })
    
    
    
    
    
    
 
    output$player_header <- renderUI({
      row <- player_row()
      theme <- theme_row()
      req(nrow(row) > 0, nrow(theme) > 0)
      
      height_cm <- as.numeric(row$player_height[1])
      height_in <- round(height_cm / 2.54)
      height_ft <- floor(height_in / 12)
      height_inch <- height_in %% 12
      height_str <- paste0(height_ft, "'", height_inch, "\"")
      
      weight_kg <- as.numeric(row$player_weight[1])
      if (row$Player[1] == "Egor Demin") {
        weight_lb <- 190
      } else {
        weight_lb <- round(weight_kg * 2.20462)
      }
      
      div(
        style = paste0("
      text-align: center; 
      margin-bottom: 30px;
    "),
        
        tags$h1(
          row$Player[1],
          style = "font-size: 36px; font-weight: 700; margin-bottom: 20px;"
        ),
        
        if (!is.null(player_headshot())) {
          tags$img(
            src = player_headshot(),
            style = "height: 250px; border-radius: 12px; margin-bottom: 20px;"
          )
        },
        
        tags$p(
          paste(
            "Height:", height_str, "|",
            "Weight:", weight_lb, "lbs |",
            "Team:", row$Team.x[1], "|",
            "Pos:", row$Pos[1]
          ),
          style = "font-size: 16px; font-weight: 500; color: white; margin-bottom: 10px;"
        )
      )
    })
    
    
    output$playtype_pie <- renderPlotly({
      req(input$pie_toggle, drafted_team())
      team_row <- team_playtype_percentiled %>% filter(Team == drafted_team())
      if (nrow(team_row) == 0) return(NULL)
      
      if (input$pie_toggle == "Offense") {
        time_cols <- grep("^%Time_", names(team_row), value = TRUE)
        time_cols <- time_cols[!grepl("_def|_pctile", time_cols)]
        playtypes <- gsub("%Time_", "", time_cols)
        
        ppp_cols <- paste0("PPP_", playtypes)
        ppp_pct_cols <- paste0("PPP_", playtypes, "_pctile")
        fg_cols <- paste0("FG%_", playtypes)
        fg_pct_cols <- paste0("FG%_", playtypes, "_pctile")
        three_cols <- paste0("3 FG%_", playtypes)
        three_pct_cols <- paste0("3 FG%_", playtypes, "_pctile")
        ft_cols <- paste0("%FT_", playtypes)
        ft_pct_cols <- paste0("%FT_", playtypes, "_pctile")
        to_cols <- paste0("TO%_", playtypes)
        to_pct_cols <- paste0("TO%_", playtypes, "_pctile")
        
      } else {
        time_cols <- grep("^%Time_.*_def$", names(team_row), value = TRUE)
        playtypes <- gsub("_def$", "", gsub("%Time_", "", time_cols))
        
        ppp_cols <- paste0("PPP_", playtypes, "_def")
        ppp_pct_cols <- paste0("PPP_", playtypes, "_def_pctile")
        fg_cols <- paste0("FG%_", playtypes, "_def")
        fg_pct_cols <- paste0("FG%_", playtypes, "_def_pctile")
        three_cols <- paste0("3 FG%_", playtypes, "_def")
        three_pct_cols <- paste0("3 FG%_", playtypes, "_def_pctile")
        ft_cols <- paste0("%FT_", playtypes, "_def")
        ft_pct_cols <- paste0("%FT_", playtypes, "_def_pctile")
        to_cols <- paste0("TO%_", playtypes, "_def")
        to_pct_cols <- paste0("TO%_", playtypes, "_def_pctile")
      }
      
      get_safe_val <- function(col_name) {
        if (col_name %in% names(team_row)) {
          val <- as.numeric(team_row[[col_name]])
          if (is.na(val)) return(NA_real_) else return(val)
        } else {
          return(NA_real_)
        }
      }
      
      size_vals <- as.numeric(team_row[1, time_cols])
      ppp_vals <- unlist(lapply(ppp_cols, get_safe_val))
      ppp_pct_vals <- unlist(lapply(ppp_pct_cols, get_safe_val))
      fg_vals <- unlist(lapply(fg_cols, get_safe_val))
      fg_pct_vals <- unlist(lapply(fg_pct_cols, get_safe_val))
      three_vals <- unlist(lapply(three_cols, get_safe_val))
      three_pct_vals <- unlist(lapply(three_pct_cols, get_safe_val))
      ft_vals <- unlist(lapply(ft_cols, get_safe_val))
      ft_pct_vals <- unlist(lapply(ft_pct_cols, get_safe_val))
      to_vals <- unlist(lapply(to_cols, get_safe_val))
      to_pct_vals <- unlist(lapply(to_pct_cols, get_safe_val))
      
      color_vals <- sapply(ppp_pct_vals, function(pct) {
        if (is.na(pct)) return("gray")
        if (pct <= 20) return("darkred")
        else if (pct <= 40) return("orange")
        else if (pct <= 60) return("gold")
        else if (pct <= 80) return("yellowgreen")
        else return("green")
      })
      
      clean_names <- c(
        "cut" = "Cut",
        "iso" = "Isolation",
        "pnr" = "P&R Ball Handler",
        "post" = "Post",
        "pnr_roll" = "P&R Roll Man",
        "spot" = "Spot Up",
        "offscreen" = "Off Screen",
        "handoff" = "Handoffs",
        "offreb" = "Offensive Rebounds",
        "transition" = "Transition"
      )
      
      labels <- sapply(playtypes, function(p) {
        clean_names[[p]] %||% p
      })
      
      hover_text <- mapply(function(play, ppp, ppp_pct, fg, fg_pct, three, three_pct, to, to_pct, ft, ft_pct, time) {
        txt <- paste0(
          "<b>", clean_names[[play]] %||% play, "</b><br>",
          "Time Share: ", round(time * 100, 1), "%<br>",
          "PPP: ", ifelse(is.na(ppp), "-", round(ppp, 3)), "<br>",
          "PPP Percentile: ", ifelse(is.na(ppp_pct), "-", paste0(round(ppp_pct), " percentile")), "<br>"
        )
        if (!is.na(fg_pct)) txt <- paste0(txt, "FG%: ", round(fg * 100, 1), "% (", round(fg_pct), " percentile)<br>")
        if (!is.na(three_pct)) txt <- paste0(txt, "3PT%: ", round(three * 100, 1), "% (", round(three_pct), " percentile)<br>")
        if (!is.na(to_pct)) txt <- paste0(txt, "TO%: ", round(to * 100, 1), "% (", round(to_pct), " percentile)<br>")
        if (!is.na(ft_pct)) txt <- paste0(txt, "%FT: ", round(ft * 100, 1), "% (", round(ft_pct), " percentile)<br>")
        return(txt)
      }, playtypes, ppp_vals, ppp_pct_vals, fg_vals, fg_pct_vals, three_vals, three_pct_vals,
      to_vals, to_pct_vals, ft_vals, ft_pct_vals, size_vals, SIMPLIFY = TRUE)
      
      
      legend_text <- paste0(
        "<b>Efficiency Legend:</b><br>",
        "<span style='color:darkred'>\u25CF 0–20: Bad</span><br>",
        "<span style='color:orange'>\u25CF 21–40: Below Avg</span><br>",
        "<span style='color:gold'>\u25CF 41–60: Neutral</span><br>",
        "<span style='color:yellowgreen'>\u25CF 61–80: Good</span><br>",
        "<span style='color:green'>\u25CF 81–100: Elite</span>"
      )
      
      plot_ly(
        labels = labels,
        values = size_vals,
        type = "pie",
        text = paste0(labels, ": ", round(size_vals * 100, 1), "%"),
        textinfo = "text",
        hoverinfo = "text",
        textposition = "inside",
        insidetextorientation = "radial",
        hovertemplate = paste0(hover_text, "<extra></extra>"),
        marker = list(colors = color_vals)
      ) %>% layout(
        title = list(
          text = paste(input$pie_toggle, "Play Type Breakdown for", drafted_team()),
          font = list(color = "white", size = 20)
        ),
        annotations = list(
          x = 1.35,
          y = 0.5,
          text = legend_text,
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          align = "left",
          font = list(size = 12, color = "white")
        ),
        margin = list(l = 0, r = 400, t = 50, b = 0),
        showlegend = FALSE,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)',
        hoverlabel = list(
          font = list(size = 8, color = "white")
        )
      )
      
      
      
      
    })
    
    
    output$college_pie <- renderPlotly({
      req(input$selected_player)
      
      college_row <- final_draft_synergy_percentiled %>% filter(Player == input$selected_player)
      req(nrow(college_row) > 0)
      
      time_cols <- grep("^%Time_", names(college_row), value = TRUE)
      time_cols <- time_cols[!grepl("_def|_pctile", time_cols)]
      playtypes <- gsub("%Time_", "", time_cols)
      
      ppp_cols <- paste0("PPP_", playtypes)
      ppp_pct_cols <- paste0("PPP_", playtypes, "_pctile")
      
      clean_names <- c(
        "cut" = "Cut",
        "iso" = "Isolation",
        "pnr" = "P&R Ball Handler",
        "post" = "Post",
        "pnr_roll" = "PNR Roll",
        "spot" = "Spot Up",
        "offscreen" = "Off Screen",
        "handoff" = "Handoffs",
        "offreb" = "Off Rebounds",
        "transition" = "Transition"
      )
      
      size_vals <- as.numeric(college_row[1, time_cols])
      ppp_vals <- as.numeric(college_row[1, ppp_cols])
      ppp_pct_vals <- as.numeric(college_row[1, ppp_pct_cols])
      
      size_vals[is.na(size_vals)] <- 0
      ppp_vals[is.na(ppp_vals)] <- NA
      ppp_pct_vals[is.na(ppp_pct_vals)] <- NA
      
      labels <- sapply(playtypes, function(p) clean_names[[p]] %||% p)
      
      color_vals <- sapply(ppp_pct_vals, function(pct) {
        if (is.na(pct)) return("gray")
        if (pct <= 20) "darkred"
        else if (pct <= 40) "orange"
        else if (pct <= 60) "gold"
        else if (pct <= 80) "yellowgreen"
        else "green"
      })
      
      hover_text <- mapply(function(label, time, ppp, pct) {
        paste0(
          "<b>", label, "</b><br>",
          "Time Share: ", round(time * 100, 1), "%<br>",
          "Efficiency (PPP): ", ifelse(is.na(ppp), "-", round(ppp, 3)), "<br>",
          "Efficiency Percentile: ", ifelse(is.na(pct), "-", paste0(round(pct), "%"))
        )
      }, labels, size_vals, ppp_vals, ppp_pct_vals, SIMPLIFY = TRUE)
      
      legend_text <- paste0(
        "<b>Efficiency Legend:</b><br>",
        "<span style='color:darkred'>\u25CF 0–20: Bad</span><br>",
        "<span style='color:orange'>\u25CF 21–40: Below Average</span><br>",
        "<span style='color:gold'>\u25CF 41–60: Neutral</span><br>",
        "<span style='color:yellowgreen'>\u25CF 61–80: Good</span><br>",
        "<span style='color:green'>\u25CF 81–100: Elite</span>"
      )
      
      valid_rows <- size_vals > 0
      size_vals <- size_vals[valid_rows]
      ppp_vals <- ppp_vals[valid_rows]
      ppp_pct_vals <- ppp_pct_vals[valid_rows]
      labels <- labels[valid_rows]
      hover_text <- hover_text[valid_rows]
      color_vals <- color_vals[valid_rows]
      
      plot_ly(
        labels = labels,
        values = size_vals,
        type = "pie",
        text = paste0(labels, ": ", round(size_vals * 100, 1), "%"),
        textinfo = "text",
        textposition = "inside",
        insidetextorientation = "radial",
        hoverinfo = "text",
        hovertext = hover_text,
        marker = list(colors = color_vals),
        textfont = list(size = 12)
      ) %>% layout(
        title = list(
          text = paste(input$selected_player, "- College Offense Play Type Breakdown"),
          font = list(color = "white", size = 20)
        ),
        annotations = list(
          x = 1.35,
          y = 0.5,
          text = legend_text,
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          align = "left",
          font = list(size = 12, color = "white")
        ),
        margin = list(l = 0, r = 400, t = 50, b = 0),
        showlegend = FALSE,
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
      
      
      
    })
    
    
    
  
    
    observe({
      nba_players <- nba_full_combined_percentiled %>% filter(Team.x == drafted_team())
      pos_choices <- unique(nba_players$Pos.x)
      updateSelectInput(session, "position_filter", choices = pos_choices, selected = pos_choices)
    })
    output$prospect_table <- DT::renderDT({
      req(input$comparison_stat_type)
      
      player <- player_row()
      
      if (input$comparison_stat_type == "Basic Stats") {
        stat_cols <- c("PTS", "AST", "ORB", "TRB", "PF", "TOV", "BLK", "STL", "OWS", "DWS", "TS%", "3P%", "FT%")
      } else {
        playtype <- switch(input$comparison_stat_type,
                           "P&R Ball Handler" = "pnr",
                           "Spot Up" = "spot",
                           "Transition" = "transition",
                           "Isolation" = "iso",
                           "Post" = "post",
                           "Cut" = "cut",
                           "Off Screen" = "offscreen",
                           "Handoffs" = "handoff",
                           "Offensive Rebounds" = "offreb",
                           "P&R Roll Man" = "pnr_roll")
        stat_cols <- c(paste0("%Time_", playtype),
                       paste0("PPP_", playtype),
                       paste0("FG%_", playtype),
                       paste0("3 FG%_", playtype),
                       paste0("%SF_", playtype),
                       paste0("%FT_", playtype),
                       paste0("TO%_", playtype))
      }
      
      pctile_cols <- paste0(stat_cols, "_pctile")
      all_cols <- c(stat_cols, pctile_cols)
      
      prospect <- player %>%
        mutate(Pos.x = Pos) %>%
        select(Player, Pos.x, any_of(all_cols))
      
      for (col in names(prospect)) {
        if (col == "PTS") {
          prospect[[col]] <- round(as.numeric(prospect[[col]]), 2)
        } else if (grepl("%|FG|TO%|SF|FT|TS", col) & !grepl("_pctile$", col) & col != "PTS") {
          prospect[[col]] <- round(as.numeric(prospect[[col]]) * 100, 2)
        } 
        else if (!grepl("_pctile$", col) & is.numeric(prospect[[col]]) & col != "PTS") {
          prospect[[col]] <- round(as.numeric(prospect[[col]]), 2)
        }
      }
      
      reordered_cols <- c("Player", "Pos.x")
      for (s in stat_cols) {
        if (s %in% names(prospect)) {
          reordered_cols <- c(reordered_cols, s)
          pct_col <- paste0(s, "_pctile")
          if (pct_col %in% names(prospect)) reordered_cols <- c(reordered_cols, pct_col)
        }
      }
      prospect <- prospect[, reordered_cols]
      print(names(prospect))
      colnames(prospect) <- clean_colnames(colnames(prospect))
      
      DT::datatable(
        prospect,
        options = list(
          dom = 't',
          ordering = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE,
          fixedColumns = list(leftColumns = 2), 
          columnDefs = list(
            list(width = '150px', targets = 0),
            list(width = '50px', targets = 1),
            list(width = '60px', targets = 2:3),
            list(width = '60px', targets = 4:5),
            list(width = '60px', targets = 6:7),
            list(width = '60px', targets = 8:9),
            list(width = '60px', targets = 10:11),
            list(width = '60px', targets = 12:13),
            list(width = '60px', targets = 14:15)
          )
        ),
        escape = FALSE,
        class = 'compact stripe nowrap',
        width = "100%",
        extensions = 'FixedColumns'
      ) %>%
        apply_percentile_coloring(prospect) %>%
        DT::formatStyle(
          columns = setdiff(names(prospect), grep("_pctile$", names(prospect), value = TRUE)),
          color = 'white'
        )
      
      
      
      
    })
    
    
    
    
    output$nba_team_table <- DT::renderDT({
      req(input$position_filter, input$comparison_stat_type)
      
      nba_players <- nba_full_combined_percentiled %>%
        filter(Team.x == drafted_team(), Pos.x %in% input$position_filter)
      
      if (input$comparison_stat_type == "Basic Stats") {
        stat_cols <- c("PTS", "AST", "ORB", "TRB", "PF", "TOV", "BLK", "STL", "OWS", "DWS", "TS%", "3P%", "FT%")
      } else {
        playtype <- switch(input$comparison_stat_type,
                           "P&R Ball Handler" = "pnr",
                           "Spot Up" = "spot",
                           "Transition" = "transition",
                           "Isolation" = "iso",
                           "Post" = "post",
                           "Cut" = "cut",
                           "Off Screen" = "offscreen",
                           "Handoffs" = "handoff",
                           "Offensive Rebounds" = "offreb",
                           "P&R Roll Man" = "pnr_roll")
        stat_cols <- c(paste0("%Time_", playtype),
                       paste0("PPP_", playtype),
                       paste0("FG%_", playtype),
                       paste0("3 FG%_", playtype),
                       paste0("%SF_", playtype),
                       paste0("%FT_", playtype),
                       paste0("TO%_", playtype))
      }
      
      pctile_cols <- paste0(stat_cols, "_pctile")
      all_cols <- c(stat_cols, pctile_cols)
      
      nba_subset <- nba_players %>%
        select(Player, Pos.x, any_of(all_cols))
      
      for (col in names(nba_subset)) {
        if (col == "PTS") {
          nba_subset[[col]] <- round(as.numeric(nba_subset[[col]]), 2)
        } 
        else if (grepl("%|FG|TO%|SF|FT|TS", col) & !grepl("_pctile$", col)) {
          nba_subset[[col]] <- round(as.numeric(nba_subset[[col]]) * 100, 2)
        } 
        else if (!grepl("_pctile$", col) & is.numeric(nba_subset[[col]])) {
          nba_subset[[col]] <- round(as.numeric(nba_subset[[col]]), 2)
        }
      }
      
      
      reordered_cols <- c("Player", "Pos.x")
      for (s in stat_cols) {
        if (s %in% names(nba_subset)) {
          reordered_cols <- c(reordered_cols, s)
          pct_col <- paste0(s, "_pctile")
          if (pct_col %in% names(nba_subset)) reordered_cols <- c(reordered_cols, pct_col)
        }
      }
      nba_subset <- nba_subset[, reordered_cols]
      colnames(nba_subset) <- clean_colnames(colnames(nba_subset))
      
      DT::datatable(
        nba_subset,
        options = list(
          pageLength = nrow(nba_subset),
          autoWidth = TRUE,
          dom = 't',
          paging = FALSE,
          scrollY = "500px",
          scrollX = TRUE, 
          fixedColumns = list(leftColumns = 2), 
          columnDefs = list(
           
          )
        ),
        escape = FALSE,
        class = 'compact stripe nowrap',
        width = "100%",
        extensions = 'FixedColumns'
      )%>%apply_percentile_coloring(nba_subset) %>%
        DT::formatStyle(
          columns = setdiff(names(nba_subset), grep("_pctile$", names(nba_subset), value = TRUE)),
          color = 'white'
        )
      
      
      
    })
    
    
    
    
    output$team_weaknesses <- renderReactable({
      result <- fit_function(drafted_team(), "2024-25", nba_data, final_draft_synergy_percentiled)
      req(result, !is.null(result$Team_Worst_Stats))
      
      theme <- theme_row()
      
      validateColor <- function(color, fallback) {
        if (!isTruthy(color) || is.na(color[1]) || !is.character(color) || !grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", color[1])) fallback else color[1]
      }
      
      validateRowColor <- function(color) {
        if (tolower(color) %in% c("#ffffff", "white")) {
          "#212529"
        } else {
          color
        }
      }
      
      primary <- validateColor(as.character(theme$Primary_Color[1]), "#000000")
      secondary_raw <- validateColor(as.character(theme$Secondary_Color[1]), "#000000")
      secondary <- validateRowColor(secondary_raw)
      
      df <- result$Team_Worst_Stats %>%
        select(Stat, RawValue, Percentile) %>%
        mutate(
          Percentile = round(Percentile * 100, 0),
          Stat = sapply(Stat, clean_stat_label),
          RawValue = ifelse(
            grepl("FG %|3PT%|eFG %|TS %|2P %|FT %", Stat),
            round(RawValue * 100, 2),
            round(RawValue, 2)
          )
        )
      
      colnames(df) <- c("Stat", "Raw Value", "Percentile")
      
      reactable(
        df,
        bordered = TRUE,
        highlight = TRUE,
        striped = TRUE,
        defaultPageSize = nrow(df),
        showPageSizeOptions = FALSE,
        columns = list(
          Stat = colDef(name = "Stat"),
          `Raw Value` = colDef(name = "Raw Value"),
          Percentile = colDef(
            name = "Percentile",
            style = function(value) {
              color <- if (is.na(value)) {
                "white"
              } else if (value <= 20) {
                "darkred"
              } else if (value <= 40) {
                "orange"
              } else if (value <= 60) {
                "gold"
              } else if (value <= 80) {
                "yellowgreen"
              } else {
                "green"
              }
              list(color = "white", background = color, fontWeight = "bold")
            }
          )
        ),
        theme = reactableTheme(
          headerStyle = list(backgroundColor = primary, color = "white", fontWeight = "bold"),
          borderColor = secondary,
          stripedColor = secondary,
          highlightColor = secondary,
          style = list(backgroundColor = "#212529", color = "white", fontSize = "22px", fontFamily = "Arial")
        )
      )
    })
    
    
    
    
    
    
    
    output$player_fit_table <- renderReactable({
      result <- fit_function(drafted_team(), "2024-25", nba_data, final_draft_synergy_percentiled)
      req(result, !is.null(result$Player_Stat_Table))
      
      theme <- theme_row()
      
      validateColor <- function(color, fallback) {
        if (!isTruthy(color) || is.na(color[1]) || !is.character(color) || !grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", color[1])) fallback else color[1]
      }
      
      validateRowColor <- function(color) {
        if (tolower(color) %in% c("#ffffff", "white")) "#212529" else color
      }
      
      primary <- validateColor(as.character(theme$Primary_Color[1]), "#000000")
      secondary_raw <- validateColor(as.character(theme$Secondary_Color[1]), "#000000")
      secondary <- validateRowColor(secondary_raw)
      
      player_name <- player_row()$Player[1]
      df <- result$Player_Stat_Table %>%
        filter(Player == player_name) %>%
        select(Stat, RawValue, Percentile)
      
      shooting_stats_display <- c("FG%", "2P%", "3P%", "FT%", "TS%", "eFG%")
      
      df <- df %>%
        mutate(
          Percentile = round(Percentile, 0),
          RawValue = ifelse(
            Stat %in% shooting_stats_display,
            round(RawValue * 100, 2),
            round(RawValue, 2)
          ),
          Stat = sapply(Stat, clean_stat_label)
        ) %>%
        filter(Stat != "2P%")  
      
      
      colnames(df) <- c("Stat", "Raw Value", "Percentile")
      
      reactable(
        df,
        bordered = TRUE,
        highlight = TRUE,
        striped = TRUE,
        defaultPageSize = nrow(df),
        showPageSizeOptions = FALSE,
        columns = list(
          Stat = colDef(name = "Stat"),
          `Raw Value` = colDef(name = "Raw Value"),
          Percentile = colDef(
            name = "Percentile",
            style = function(value) {
              if (is.na(value)) return(NULL)
              background <- if (value <= 20) {
                "darkred"
              } else if (value <= 40) {
                "orange"
              } else if (value <= 60) {
                "gold"
              } else if (value <= 80) {
                "yellowgreen"
              } else {
                "green"
              }
              list(background = background, color = "white", fontWeight = "bold")
            }
          )
        ),
        theme = reactableTheme(
          headerStyle = list(
            backgroundColor = primary,
            color = "white",
            fontWeight = "bold"
          ),
          borderColor = secondary,
          stripedColor = secondary,
          highlightColor = secondary,
          style = list(
            backgroundColor = "#212529",
            color = "white",
            fontSize = "22px",
            fontFamily = "Arial"
          )
        )
      )
    })
    
    
    
    
    
    
    
    
    output$player_breakdown <- renderUI({
      blurb <- player_breakdowns %>% filter(Player == input$selected_player)
      if (nrow(blurb) == 0) return(NULL)
      
      div(class = "pick-assessment-container",
          tags$h2("Pick Assessment"),
          HTML(blurb$Analysis[1])
      )
    })
    
  })
}

