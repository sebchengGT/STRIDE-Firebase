# --- SERVER LOGIC FOR PLANTILLA POSITIONS ---

# --- Shared global drilldown state ---
plantilla_drill_state <- reactiveVal(list(region = NULL))
plantilla_trigger <- reactiveVal(0)
last_preset_selection <- reactiveVal(character(0))

# --- NEW: Create a master list of all locations ---
# Ensure dfGMIS is loaded in your global environment
all_locations <- distinct(dfGMIS, GMIS.Region, GMIS.Division)

# --- SERVER Code ---

# 1. Define Defaults
req(all_available_positions) # Ensures this variable exists
default_plantilla_selection <- character(0)

# --- Observer for Plantilla Position Presets (SINGLE SELECTION ENFORCED) ---
observeEvent(input$plantilla_presets, {
  
  req(all_available_positions)
  
  # 1. Get the current (new) input from UI
  current_selection <- input$plantilla_presets
  
  # 2. Get the previous state
  prev_selection <- last_preset_selection()
  
  # 3. Determine the actual selection
  final_selection <- character(0)
  
  # Calculate what was JUST added (The "Difference")
  newly_clicked <- setdiff(current_selection, prev_selection)
  
  if (length(newly_clicked) > 0) {
    # CASE A: A new box was clicked. Enforce single selection.
    final_selection <- newly_clicked[1] 
  } else {
    # CASE B: No new box was added (user unchecked).
    if (length(current_selection) > 0) {
      final_selection <- tail(current_selection, 1)
    } else {
      final_selection <- character(0)
    }
  }
  
  # 4. Update the UI if needed
  if (!identical(current_selection, final_selection)) {
    updateCheckboxGroupInput(session, "plantilla_presets", selected = final_selection)
  }
  
  # 5. Save state
  last_preset_selection(final_selection)
  
  # --- LOGIC TO SELECT POSITIONS ---
  positions_to_select <- default_plantilla_selection
  
  if (length(final_selection) > 0) {
    preset <- final_selection 
    
    if (preset == "Teacher") {
      teacher_positions <- all_available_positions[
        all_available_positions %in% c("Teacher I", "Teacher II", "Teacher III")
      ]
      positions_to_select <- c(positions_to_select, teacher_positions)
    } else {
      matched_positions <- all_available_positions[
        grepl(preset, all_available_positions, ignore.case = TRUE)
      ]
      positions_to_select <- c(positions_to_select, matched_positions)
    }
  }
  
  unique_final <- unique(positions_to_select)
  
  updatePickerInput(
    session,
    inputId = "selected_positions",
    selected = unique_final
  )
  
}, ignoreNULL = FALSE, ignoreInit = TRUE)


# --- Main Observer for Generating Cards ---
observe({
  req(input$selected_positions)
  
  # 1. Render the Layout (Cards)
  output$dynamic_positions_ui <- renderUI({
    req(input$selected_positions)
    
    cards <- lapply(input$selected_positions, function(pos) {
      plot_id <- paste0("plot_", gsub(" ", "_", pos))
      vbox_id <- paste0("vbox_", gsub(" ", "_", pos))
      
      card(
        full_screen = TRUE,
        class = "shadow-sm p-2",
        card_header(h4(pos, class = "m-0 pt-1")),
        card_body(
          
          # Total Title (Center)
          fluidRow(
            column(12, uiOutput(vbox_id))
          ),
          
          # Filled / Unfilled Boxes
          fluidRow(
            column(
              width = 6, 
              bslib::value_box(
                class = "no-scroll-card",
                max_height = "120px",
                title = tags$h6("Total Filled", style = "color:#FFFFFF; font-weight: bold; margin-bottom: 0;"),
                value = uiOutput(paste0("filled_count_ui_", gsub(" ", "_", pos))),
                theme = "primary", 
                full_screen = FALSE
              )
            ),
            column(
              width = 6, 
              bslib::value_box(
                class = "no-scroll-card",
                max_height = "120px",
                title = tags$h6("Total Unfilled", style = "color: #FFFFFF; font-weight: bold; margin-bottom: 0;"),
                value = uiOutput(paste0("unfilled_count_ui_", gsub(" ", "_", pos))),
                theme = "danger",
                full_screen = FALSE
              )
            )
          ),
          
          # Plotly Output
          plotlyOutput(plot_id, height = "600px")
        )
      )
    })
    
    do.call(
      layout_column_wrap,
      c(list(width = 1/3, heights_equal = "row"), cards)
    )
  })
  
  # 2. Generate Logic for Each Selected Position
  lapply(input$selected_positions, function(pos) {
    
    clean_pos <- gsub(" ", "_", pos)
    plot_id <- paste0("plot_", clean_pos)
    vbox_id <- paste0("vbox_", clean_pos)
    source_id <- paste0("drilldown_source_", clean_pos)
    
    # Reactive Data for this specific position
    df_sub <- reactive({
      pos_scaffold <- all_locations %>% mutate(Position = pos)
      
      actual_data <- dfGMIS %>% 
        filter(Position == pos) %>%
        select(Position, GMIS.Region, GMIS.Division, Total.Filled, Total.Unfilled)
      
      clean_data <- pos_scaffold %>%
        left_join(actual_data, by = c("Position", "GMIS.Region", "GMIS.Division")) %>%
        mutate(
          Total.Filled = replace_na(Total.Filled, 0),
          Total.Unfilled = replace_na(Total.Unfilled, 0)
        ) %>%
        filter(
          GMIS.Region != "<not available>", 
          GMIS.Division != "<not available>"
        )
      
      # Sanitize Text
      clean_data$GMIS.Region <- iconv(clean_data$GMIS.Region, to = "UTF-8", sub = " ")
      clean_data$GMIS.Division <- iconv(clean_data$GMIS.Division, to = "UTF-8", sub = " ")
      
      return(clean_data)
    })
    
    # --- Value Boxes ---
    get_filtered_data <- function() {
      trigger <- plantilla_trigger()
      state <- plantilla_drill_state()
      if (is.null(state)) state <- list(region = NULL)
      
      d <- df_sub()
      title_txt <- "Total Positions"
      if (!is.null(state$region)) {
        d <- d %>% filter(GMIS.Region == state$region)
        title_txt <- paste("Total Positions in", state$region)
      }
      return(list(data = d, title = title_txt))
    }
    
    output[[vbox_id]] <- renderUI({
      res <- get_filtered_data()
      total <- sum(res$data$Total.Filled + res$data$Total.Unfilled, na.rm = TRUE)
      
      card(
        class = "border-2 border-primary-subtle",
        card_body(
          class = "text-center p-2",
          h5(paste(res$title, "-", pos), class = "card-title mb-1"),
          h3(formatC(total, format = "d", big.mark = ","), class = "card-text")
        )
      )
    })
    
    output[[paste0("filled_count_ui_", clean_pos)]] <- renderUI({
      res <- get_filtered_data()
      total <- sum(res$data$Total.Filled, na.rm = TRUE)
      tags$h4(format(total, big.mark = ","), style = "font-size: 21.6px; font-weight: bold; margin: 0; padding-top: 5px; color:white;")
    })
    
    output[[paste0("unfilled_count_ui_", clean_pos)]] <- renderUI({
      res <- get_filtered_data()
      total <- sum(res$data$Total.Unfilled, na.rm = TRUE)
      tags$h4(format(total, big.mark = ","), style = "font-size: 21.6px; font-weight: bold; margin: 0; padding-top: 5px; color: white;")
    })
    
    # --- PLOTLY CHART ---
    output[[plot_id]] <- renderPlotly({
      trigger <- plantilla_trigger()
      state <- plantilla_drill_state()
      if (is.null(state)) state <- list(region = NULL)
      
      df <- df_sub()
      df <- df %>% filter(GMIS.Region != "<not available>", GMIS.Division != "<not available>")
      
      # Determine Drilldown Level
      if (is.null(state$region)) {
        plot_data <- df %>%
          group_by(GMIS.Region) %>%
          summarise(
            Filled = sum(Total.Filled, na.rm = TRUE),
            Unfilled = sum(Total.Unfilled, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          tidyr::pivot_longer(cols = c(Filled, Unfilled), names_to = "Type", values_to = "Count")
        y_formula <- ~GMIS.Region
      } else {
        plot_data <- df %>%
          filter(GMIS.Region == state$region) %>%
          group_by(GMIS.Division) %>%
          summarise(
            Filled = sum(Total.Filled, na.rm = TRUE),
            Unfilled = sum(Total.Unfilled, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          tidyr::pivot_longer(cols = c(Filled, Unfilled), names_to = "Type", values_to = "Count")
        y_formula <- ~GMIS.Division
      }
      
      if (nrow(plot_data) == 0) return(plot_ly() %>% layout(title = "No Data"))
      
      # Prepare Data for Totals (Text at end)
      y_var_name <- all.vars(y_formula)  
      
      data_for_totals <- if (is.null(state$region)) {
        df 
      } else {
        df %>% filter(GMIS.Region == state$region) 
      }
      
      total_counts <- data_for_totals %>%
        group_by(!!sym(y_var_name)) %>%
        summarise(
          TotalFilled = sum(Total.Filled, na.rm = TRUE),
          TotalUnfilled = sum(Total.Unfilled, na.rm = TRUE),
          TotalCount = TotalFilled + TotalUnfilled,
          FillingRate = ifelse(TotalCount == 0, 0, TotalFilled / TotalCount), 
          .groups = "drop"
        )
      
      max_val <- max(total_counts$TotalCount, na.rm = TRUE)
      final_max <- if (max_val <= 0) 10 else (max_val * 1.4) 
      
      color_map <- c("Filled" = "#007BFF", "Unfilled" = "#FF0000")
      
      plot_ly(
        data = plot_data,
        y = y_formula,
        x = ~Count,
        color = ~Type,
        colors = color_map,
        type = 'bar',
        orientation = 'h',
        source = source_id,
        text = ~Count,
        texttemplate = '%{x:,.0f}',
        textposition = 'inside' 
      ) %>%
        layout(
          barmode = 'stack',
          xaxis = list(
            title = "Number of Positions", 
            range = c(0, final_max),
            tickformat = ',.0f'
          ),
          yaxis = list(
            title = "", 
            categoryorder = "total descending",
            autorange = "reversed"
          ),
          # --- FIXED: LEGEND MOVED TO BOTTOM ---
          legend = list(
            orientation = 'h',      # Horizontal
            xanchor = 'center',     # Center horizontally
            x = 0.5,                # Middle of the plot
            y = -0.1,               # Below the x-axis
            yanchor = 'top'         # Anchor top of legend to that y position
          ),
          margin = list(b = 60)     # Extra bottom margin to fit the legend
          # -------------------------------------
        ) %>%
        add_text(
          data = total_counts,
          y = as.formula(paste0("~`", y_var_name, "`")),
          x = ~TotalCount,
          text = ~paste0(
            "<span style='color: #000000; font-weight: bold;'>", 
            formatC(TotalCount, format = "d", big.mark = ","),
            " (",
            scales::percent(FillingRate, accuracy = 1),
            ")",
            "</span>"
          ),
          textposition = "middle right", 
          showlegend = FALSE,
          inherit = FALSE,
          hoverinfo = 'none'
        )
    })
    
    # --- DRILLDOWN HANDLER ---
    observeEvent(event_data("plotly_click", source = source_id), {
      d <- event_data("plotly_click", source = source_id)
      if (is.null(d)) return()
      
      clicked_location <- d$y 
      if (is.null(clicked_location) || length(clicked_location) == 0) return()
      
      current_state <- isolate(plantilla_drill_state())
      if (is.null(current_state)) current_state <- list(region = NULL)
      
      if (is.null(current_state$region)) {
        plantilla_drill_state(list(region = as.character(clicked_location)))
        plantilla_trigger(plantilla_trigger() + 1)
      }
    })
  })
})

# --- GLOBAL BACK BUTTON ---
observeEvent(input$btn_back_drilldown, {
  state <- isolate(plantilla_drill_state())
  if (!is.null(state$region)) {
    plantilla_drill_state(list(region = NULL))
    plantilla_trigger(plantilla_trigger() + 1)
  }
})

# =========================================================
# --- PLANTILLA REPORT GENERATOR (Sorted Descending) ---
# =========================================================

output$generate_report_plantilla <- downloadHandler(
  filename = function() {
    paste("Plantilla_Report_", Sys.Date(), ".html", sep = "")
  },
  content = function(file) {
    id <- showNotification("Generating Plantilla Report...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    selected_pos <- input$selected_positions
    state <- plantilla_drill_state()
    if (is.null(state)) state <- list(region = NULL)
    
    final_report_data <- data.frame()
    
    group_col <- "GMIS.Region"
    current_level <- "Region"
    
    if (!is.null(state$region)) {
      group_col <- "GMIS.Division"
      current_level <- "Division"
    }
    
    # 1. LOOP TO GATHER DATA
    for (pos in selected_pos) {
      d <- dfGMIS %>% filter(Position == pos)
      
      if (!is.null(state$region)) {
        d <- d %>% filter(GMIS.Region == state$region)
      }
      
      summ <- d %>%
        filter(GMIS.Region != "<not available>", GMIS.Division != "<not available>") %>% 
        group_by(.data[[group_col]]) %>%
        summarise(
          Filled = sum(Total.Filled, na.rm=TRUE),
          Unfilled = sum(Total.Unfilled, na.rm=TRUE)
        ) %>%
        rename(Category = .data[[group_col]])
      
      f_rows <- summ %>% 
        select(Category, Value = Filled) %>% 
        mutate(Metric = paste(pos, "(Filled)"))
      
      u_rows <- summ %>% 
        select(Category, Value = Unfilled) %>% 
        mutate(Metric = paste(pos, "(Unfilled)"))
      
      final_report_data <- bind_rows(final_report_data, f_rows, u_rows)
    }
    
    # --- 2. CRITICAL CHANGE: FORCE SORT ORDER ---
    # We calculate the total value per Category to determine the rank
    rank_df <- final_report_data %>%
      group_by(Category) %>%
      summarise(Total = sum(Value, na.rm = TRUE)) %>%
      arrange(Total) # Use 'Total' for ascending logic if using coord_flip, or 'desc(Total)' otherwise
    
    # Convert Category to a Factor with specific levels. 
    # This forces ggplot to plot them in this exact order.
    final_report_data$Category <- factor(final_report_data$Category, levels = rank_df$Category)
    
    all_metrics <- unique(final_report_data$Metric)
    metric_names_list <- setNames(as.list(all_metrics), all_metrics)
    
    params_list <- list(
      data = final_report_data,
      metrics = all_metrics,
      state = list(
        level = current_level,
        region = state$region,
        division = NULL,
        municipality = NULL
      ),
      metric_names = metric_names_list
    )
    
    rmarkdown::render(
      tempReport,
      output_file = file,
      params = params_list,
      envir = new.env(parent = globalenv())
    )
  }
)