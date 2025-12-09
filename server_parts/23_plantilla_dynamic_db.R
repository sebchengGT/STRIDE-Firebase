# --- SERVER LOGIC FOR PLANTILLA POSITIONS ---

# --- Shared global drilldown state ---
plantilla_drill_state <- reactiveVal(list(region = NULL))
plantilla_trigger <- reactiveVal(0)
# Add this near line 7 with your other reactiveVals
last_preset_selection <- reactiveVal(character(0))

# --- NEW: Create a master list of all locations ---
# Ensure dfGMIS is loaded in your global environment
all_locations <- distinct(dfGMIS, GMIS.Region, GMIS.Division)

# --- SERVER Code ---

# 1. Define Defaults
req(all_available_positions) # Ensures this variable exists
default_plantilla_selection <- character(0)

# --- Observer for Plantilla Position Presets (SINGLE SELECTION ENFORCED) ---
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
    # CASE A: A new box was clicked. 
    # We enforce single selection by taking ONLY the new item.
    final_selection <- newly_clicked[1] 
  } else {
    # CASE B: No new box was added (meaning the user unchecked the active box).
    # If standard behavior is allowing uncheck -> empty, leave it empty.
    # If you want to prevent unchecking, set it back to prev_selection.
    if (length(current_selection) > 0) {
      # This handles edge cases where setdiff might miss depending on race conditions
      final_selection <- tail(current_selection, 1)
    } else {
      final_selection <- character(0)
    }
  }
  
  # 4. Update the UI if the selection needs to be forced (Standardizing to 1 item)
  # We only update if the UI state (current) is different from our calculated final
  if (!identical(current_selection, final_selection)) {
    updateCheckboxGroupInput(session, "plantilla_presets", selected = final_selection)
  }
  
  # 5. Save this state for the next click
  last_preset_selection(final_selection)
  
  # --- LOGIC TO SELECT POSITIONS (Copied from your original code) ---
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
  
  # Update the Picker Input
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
          
          # --- CHANGE: Increased height from 450px to 600px ---
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
    
    # Create unique IDs
    clean_pos <- gsub(" ", "_", pos)
    plot_id <- paste0("plot_", clean_pos)
    vbox_id <- paste0("vbox_", clean_pos)
    source_id <- paste0("drilldown_source_", clean_pos)
    
    # Reactive Data for this specific position
    # Reactive Data for this specific position
    # Reactive Data for this specific position
    df_sub <- reactive({
      # Scaffold ensures we have rows even if counts are 0
      pos_scaffold <- all_locations %>% mutate(Position = pos)
      
      actual_data <- dfGMIS %>% 
        filter(Position == pos) %>%
        select(Position, GMIS.Region, GMIS.Division, Total.Filled, Total.Unfilled)
      
      # Join data
      clean_data <- pos_scaffold %>%
        left_join(actual_data, by = c("Position", "GMIS.Region", "GMIS.Division")) %>%
        mutate(
          Total.Filled = replace_na(Total.Filled, 0),
          Total.Unfilled = replace_na(Total.Unfilled, 0)
        ) %>%
        # --- FIX: Remove "<not available>" entries ---
        filter(
          GMIS.Region != "<not available>", 
          GMIS.Division != "<not available>"
        )
      
      # --- FIX: Sanitize Text to prevent "Invalid Input String" Errors ---
      clean_data$GMIS.Region <- iconv(clean_data$GMIS.Region, to = "UTF-8", sub = " ")
      clean_data$GMIS.Division <- iconv(clean_data$GMIS.Division, to = "UTF-8", sub = " ")
      
      return(clean_data)
    })
    
    # --- Value Boxes (Main, Filled, Unfilled) ---
    
    # A. Helper function to calculate totals based on drilldown state
    get_filtered_data <- function() {
      trigger <- plantilla_trigger() # dependency
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
    
    # B. Render Main Total Box
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
    
    # C. Render Filled Count
    output[[paste0("filled_count_ui_", clean_pos)]] <- renderUI({
      res <- get_filtered_data()
      total <- sum(res$data$Total.Filled, na.rm = TRUE)
      tags$h4(format(total, big.mark = ","), style = "font-size: 21.6px; font-weight: bold; margin: 0; padding-top: 5px; color:white;")
    })
    
    # D. Render Unfilled Count
    output[[paste0("unfilled_count_ui_", clean_pos)]] <- renderUI({
      res <- get_filtered_data()
      total <- sum(res$data$Total.Unfilled, na.rm = TRUE)
      tags$h4(format(total, big.mark = ","), style = "font-size: 21.6px; font-weight: bold; margin: 0; padding-top: 5px; color: white;")
    })
    
    # --- PLOTLY CHART ---
# --- PLOTLY CHART (Fixed Text Position) ---
    output[[plot_id]] <- renderPlotly({
      trigger <- plantilla_trigger()
      state <- plantilla_drill_state()
      if (is.null(state)) state <- list(region = NULL)
      
      df <- df_sub()
      
      # Determine Drilldown Level
      if (is.null(state$region)) {
        # Level 1: Region
        group_var <- "GMIS.Region"
        plot_data_raw <- df
        y_formula <- ~GMIS.Region
      } else {
        # Level 2: Division
        group_var <- "GMIS.Division"
        plot_data_raw <- df %>% filter(GMIS.Region == state$region)
        y_formula <- ~GMIS.Division
      }
      
      # Summarize for Plotting (Bars)
      plot_data <- plot_data_raw %>%
        group_by(.data[[group_var]]) %>%
        summarise(
          Filled = sum(Total.Filled, na.rm = TRUE),
          Unfilled = sum(Total.Unfilled, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        tidyr::pivot_longer(cols = c(Filled, Unfilled), names_to = "Type", values_to = "Count")
      
      if (nrow(plot_data) == 0) return(plot_ly())
      
      # Summarize for Totals (Text Labels)
      total_counts <- plot_data_raw %>%
        group_by(.data[[group_var]]) %>%
        summarise(
          TotalFilled = sum(Total.Filled, na.rm = TRUE),
          TotalUnfilled = sum(Total.Unfilled, na.rm = TRUE),
          TotalCount = TotalFilled + TotalUnfilled,
          .groups = "drop"
        )
      
      # Fix Axis Range
      # We multiply by 1.4 to give extra whitespace on the right for the text
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
        textposition = 'inside', 
        insidetextanchor = 'middle'
      ) %>%
        layout(
          barmode = 'stack',
          xaxis = list(
            title = "Number of Positions", 
            range = c(0, final_max),
            tickformat = ',.0f'
          ),
          yaxis = list(title = "", categoryorder = "total descending", autorange = "reversed"),
          legend = list(orientation = 'h', x = 0.5, y = 1.05, xanchor = 'center')
        ) %>%
        # --- FIX: "middle right" forces text to the right of the x-coordinate ---
        add_text(
          data = total_counts,
          y = as.formula(paste0("~`", group_var, "`")),
          x = ~TotalCount,
          text = ~formatC(TotalCount, format = "d", big.mark = ","),
          # CHANGE HERE: "middle right" places the text to the right of the data point
          textposition = "middle right", 
          textfont = list(color = 'black', size = 12, weight = "bold"), 
          showlegend = FALSE,
          inherit = FALSE,
          hoverinfo = 'none'
        )
    })
    
    # --- DRILLDOWN HANDLER (UPDATED) ---
    # We use a unique ID (source_id) to ensure we only catch clicks for this specific plot
    # --- DRILLDOWN HANDLER (FIXED) ---
    # We use a unique ID (source_id) to ensure we only catch clicks for this specific plot
    observeEvent(event_data("plotly_click", source = source_id), {
      
      # 1. Get Event Data
      d <- event_data("plotly_click", source = source_id)
      if (is.null(d)) return()
      
      # 2. Extract the Location (Y-axis)
      # CRITICAL: We strictly grab 'y' (the location name) and ignore 'curveNumber' or color.
      # This ensures the drilldown works based on location, regardless of whether you click Filled or Unfilled.
      clicked_location <- d$y 
      
      # Safety Check: Ensure the click captured a valid category/location
      if (is.null(clicked_location) || length(clicked_location) == 0) return()
      
      # 3. Get Current State
      current_state <- isolate(plantilla_drill_state())
      if (is.null(current_state)) current_state <- list(region = NULL)
      
      # 4. Execute Drilldown Logic
      # Only drill down if we are currently at the top level (Region is NULL)
      if (is.null(current_state$region)) {
        
        # Update State: Set the region to the clicked location
        plantilla_drill_state(list(region = as.character(clicked_location)))
        
        # Trigger re-render of all cards
        plantilla_trigger(plantilla_trigger() + 1)
      }
    })
  })
})

# --- GLOBAL BACK BUTTON ---
observeEvent(input$btn_back_drilldown, {
  state <- isolate(plantilla_drill_state())
  
  # Only trigger if we are actually drilled down
  if (!is.null(state$region)) {
    plantilla_drill_state(list(region = NULL))
    plantilla_trigger(plantilla_trigger() + 1)
  }
})

# --- REPORT GENERATOR (Unchanged) ---
# ... (Keep your existing report generator code here) ...