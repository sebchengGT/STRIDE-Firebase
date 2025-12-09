# --- 1. VALIDATE METADATA ---
if (!exists("col_info_adv_static")) {
  stop("FATAL ERROR: 'col_info_adv_static' object not found...")
} else {
  print("--- ADVANCED ANALYTICS: Server logic is loading. Metadata found. ---")
}


# --- 2. Dynamic Filter Management ---
active_filter_ids <- reactiveVal(c()) 
adv_filter_counter <- reactiveVal(0) 

observeEvent(input$add_adv_filter_btn, {
  new_id <- isolate(adv_filter_counter()) + 1
  adv_filter_counter(new_id)
  ui_id <- paste0("adv_filter_row_", new_id)
  
  choices_with_prompt <- c("Select a column..." = "", adv_analytics_choices)
  
  insertUI(
    selector = "#adv_filter_container",
    where = "beforeEnd",
    ui = div(
      id = ui_id,
      class = "adv-filter-group",
      style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
      actionButton(paste0("adv_remove_", new_id), "Remove", 
                   icon = icon("times"), class = "btn-danger btn-sm float-end"),
      selectInput(paste0("adv_col_", new_id), 
                  label = paste("Filter", new_id, ": Select Column"),
                  choices = choices_with_prompt), 
      uiOutput(paste0("adv_filter_val_ui_", new_id))
    )
  )
  active_filter_ids(c(isolate(active_filter_ids()), new_id))
  
  local({
    current_filter_id <- new_id
    
    output[[paste0("adv_filter_val_ui_", current_filter_id)]] <- renderUI({
      
      col_name <- input[[paste0("adv_col_", current_filter_id)]]
      req(col_name, col_name != "") 
      
      print(paste("--- Generating UI for filter", current_filter_id, "---"))
      print(paste("Selected col_name:", col_name))
      
      col_type_vec <- col_info_adv_static$type[col_info_adv_static$Raw_Name == col_name]
      
      if (length(col_type_vec) == 0) {
        return(tags$p(style = "color: #dc3545; font-weight: bold;", paste("Error: Metadata for column '", col_name, "' not found.")))
      }
      
      col_type <- col_type_vec[1]
      
      if (!col_name %in% names(uni)) {
        return(tags$p(style = "color: #dc3545; font-weight: bold;", paste("Error: Data for column '", col_name, "' not found.")))
      }
      
      col_data <- uni[[col_name]] 
      
      if (col_type == "Numeric") {
        min_val <- min(col_data, na.rm = TRUE)
        max_val <- max(col_data, na.rm = TRUE)
        if (is.infinite(min_val) || is.infinite(max_val)) {
          tags$p("No valid numeric data.", style = "color: #dc3545;")
        } else {
          fluidRow(
            column(6, numericInput(paste0("adv_num_min_", current_filter_id), "Min:", value = min_val)),
            column(6, numericInput(paste0("adv_num_max_", current_filter_id), "Max:", value = max_val))
          )
        }
      } else if (col_type %in% c("Categorical", "Binary")) {
        choices <- sort(unique(col_data[!is.na(col_data)]))
        if (length(choices) == 0) {
          tags$p("No valid categories.", style = "color: #dc3545;")
        } else {
          pickerInput(
            inputId = paste0("adv_select_", current_filter_id),
            label = "Select Value(s):",
            choices = choices,
            selected = choices,
            multiple = TRUE, 
            options = list(`actions-box` = TRUE, `live-search` = TRUE, `selected-text-format` = "count > 3")
          )
        }
      } else {
        return(tags$p(style = "color: #ffc107; font-weight: bold;", paste("Warning: No UI defined for column type '", col_type, "'.")))
      }
    })
    
    observeEvent(input[[paste0("adv_remove_", current_filter_id)]], {
      removeUI(selector = paste0("#adv_filter_row_", current_filter_id))
      active_filter_ids(setdiff(isolate(active_filter_ids()), current_filter_id))
    })
  }) 
}) 

# --- 3. Filter Data and Manage State ---

# --- 3a. Add state management for the interactive drilldown ---
adv_drill_state <- reactiveVal(list(level = "Overall", filters = list()))

# --- 3b. Define the FINAL drilldown hierarchy ---
drill_levels <- c("Overall", "Region", "Division", "Municipality", "Legislative.District", "District")

col_name_map <- c(
  "Overall" = "Overall", 
  "Region" = "Region",
  "Division" = "Division",
  "Municipality" = "Municipality",
  "Legislative.District" = "Legislative.District",
  "District" = "District"
)

# --- 3c. 'Apply' button now filters data AND resets drilldown to "Overall" ---
# --- MODIFIED: REMOVED GLOBAL DRILL STATE DEPENDENCY ---
filtered_data_adv <- eventReactive(input$adv_analytics_run, {
  
  print("--- ADVANCED ANALYTICS: 'Apply' button clicked. INDEPENDENT MODE. ---")
  
  # 1. Start with the full dataset (Ignoring main dashboard drilldown)
  data_filtered <- uni 
  
  # 2. Reset the local drilldown state to "Overall"
  start_level <- "Overall"
  adv_drill_state(list(level = start_level, filters = list()))
  print(paste("--- ADVANCED ANALYTICS: Local Drilldown reset to:", start_level, "---"))
  
  # 3. Apply ONLY dynamic filters from this panel
  filter_ids_to_apply <- isolate(active_filter_ids())
  if (length(filter_ids_to_apply) > 0) {
    for (id in filter_ids_to_apply) {
      col_name <- isolate(input[[paste0("adv_col_", id)]])
      if (is.null(col_name)) next 
      col_type <- col_info_adv_static$type[col_info_adv_static$Raw_Name == col_name]
      
      if (col_type == "Numeric") {
        min_val <- isolate(input[[paste0("adv_num_min_", id)]])
        max_val <- isolate(input[[paste0("adv_num_max_", id)]])
        if (is.null(min_val) || is.null(max_val)) next 
        data_filtered <- data_filtered %>%
          filter(!is.na(!!sym(col_name))) %>% 
          filter(!!sym(col_name) >= min_val, !!sym(col_name) <= max_val)
      } else if (col_type %in% c("Categorical", "Binary")) {
        selected_choices <- isolate(input[[paste0("adv_select_", id)]])
        if (is.null(selected_choices)) next 
        data_filtered <- data_filtered %>%
          filter(!!sym(col_name) %in% selected_choices)
      }
    }
  }
  
  print("--- ADVANCED ANALYTICS: Base filtering complete (Independent). ---")
  return(data_filtered)
})

# --- 3d. Create a reactive for the drilldown data ---
drilled_data_and_level <- reactive({
  data <- filtered_data_adv()
  state <- adv_drill_state()
  
  if (length(state$filters) > 0) {
    print("--- ADVANCED ANALYTICS: Applying drilldown filters... ---")
    for (col_name in names(state$filters)) {
      filter_value <- state$filters[[col_name]]
      print(paste("     ... filtering", col_name, "==", filter_value))
      data <- data %>% filter(!!sym(col_name) == !!filter_value)
    }
  }
  return(list(data = data, level = state$level))
})

# --- 3e. Create a reactive for the plot data ---
plot_data_r <- reactive({
  req(drilled_data_and_level())
  info <- drilled_data_and_level()
  data_to_plot <- info$data
  drill_level <- info$level
  
  last_level_name <- drill_levels[length(drill_levels)]
  
  if (drill_level == last_level_name) {
    group_col_name <- col_name_map[drill_level]
  } else {
    current_index <- match(drill_level, drill_levels)
    next_level <- drill_levels[current_index + 1]
    group_col_name <- col_name_map[next_level]
  }
  
  group_col_sym <- sym(group_col_name)
  
  if (nrow(data_to_plot) == 0) return(NULL) 
  
  plot_data <- data_to_plot %>%
    group_by(!!group_col_sym) %>%
    summarise(School_Count = n(), .groups = 'drop') %>%
    rename(Group = !!group_col_sym) %>%
    filter(!is.na(Group)) %>% 
    arrange(desc(School_Count)) %>% 
    head(25) 
  
  return(plot_data)
})


# --- 4. Plot, Controls, and Click Logic ---
# --- 4a. UI for "Reset" AND "Back" buttons ---
output$adv_drill_controls_ui <- renderUI({
  
  state <- adv_drill_state()
  start_level <- "Overall"
  
  show_reset <- state$level != start_level || length(state$filters) > 0
  show_back <- length(state$filters) > 0 
  
  div(style = "display: flex; gap: 10px; margin-bottom: 10px;",
      if (show_reset) {
        actionButton("adv_drill_reset_btn", "Reset Drilldown View", 
                     icon = icon("undo"), class = "btn-secondary btn-sm")
      },
      if (show_back) {
        actionButton("adv_drill_back_btn", "Go Back One Level", 
                     icon = icon("arrow-left"), class = "btn-info btn-sm")
      }
  )
})

# --- 4b. Observer for the "Reset" button ---
observeEvent(input$adv_drill_reset_btn, {
  print("--- ADVANCED ANALYTICS: Drilldown reset button clicked. ---")
  start_level <- "Overall"
  adv_drill_state(list(level = start_level, filters = list()))
})

# --- 4c. Observer for the "Back" button ---
observeEvent(input$adv_drill_back_btn, {
  print("--- ADVANCED ANALYTICS: Drilldown 'Back' button clicked. ---")
  current_state <- isolate(adv_drill_state())
  if (length(current_state$filters) == 0) return()
  
  current_level <- current_state$level
  current_index <- match(current_level, drill_levels)
  if (current_index == 1) return() 
  prev_level <- drill_levels[current_index - 1]
  
  new_filters <- current_state$filters
  new_filters[[length(new_filters)]] <- NULL
  
  print(paste("--- Going back to level:", prev_level))
  adv_drill_state(list(level = prev_level, filters = new_filters))
})


# --- 4d. Render the interactive plot ---
output$advanced_drilldown_plot <- renderPlot({
  
  req(drilled_data_and_level())
  
  plot_data <- plot_data_r()
  drill_level <- drilled_data_and_level()$level
  
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    print("--- ADVANCED ANALYTICS: No data to plot. ---")
    return(
      ggplot() + 
        labs(title = "No Data to Display",
             subtitle = "No schools match all selected filters. Try broadening your search.") +
        theme_minimal()
    )
  }
  
  current_index <- match(drill_level, drill_levels)
  
  if (current_index == length(drill_levels)) {
    title_text <- paste("School Count by", drill_level, "(Top 25)")
    subtitle_text <- "End of drilldown. See table and map below."
  } else {
    next_level <- drill_levels[current_index + 1]
    title_text <- paste("School Count by", next_level, "(Top 25)")
    subtitle_text <- paste("Click a bar to drill down into a", next_level)
  }
  
  ggplot(plot_data, aes(x = School_Count, y = reorder(Group, School_Count))) +
    geom_col(fill = "#007bff") + 
    geom_text(aes(label = School_Count), hjust = -0.1, size = 3.5) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "Number of Schools",
      y = if(current_index < length(drill_levels)) drill_levels[current_index+1] else drill_level
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, .1))) + 
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10), 
          axis.text.x = element_text(size = 10))
  
}, res = 96)


# --- 4e. Observer for plot clicks ---
observeEvent(input$adv_plot_click, {
  
  print("--- PLOT CLICK DETECTED! ---") 
  
  plot_data_for_click <- plot_data_r()
  if (is.null(plot_data_for_click) || nrow(plot_data_for_click) == 0) return()
  
  current_state <- adv_drill_state()
  current_index <- match(current_state$level, drill_levels)
  
  if (is.na(current_index) || current_index == length(drill_levels)) return()
  
  plot_data_sorted_asc <- plot_data_for_click %>% arrange(School_Count)
  clicked_y_index <- round(input$adv_plot_click$y)
  
  if (clicked_y_index < 1 || clicked_y_index > nrow(plot_data_sorted_asc)) return()
  
  clicked_bar_data <- plot_data_sorted_asc[clicked_y_index, ]
  clicked_x_value <- input$adv_plot_click$x
  bar_x_max <- clicked_bar_data$School_Count
  
  if (clicked_x_value < 0 || clicked_x_value > (bar_x_max * 1.1)) return()
  
  clicked_value <- as.character(clicked_bar_data$Group)
  print(paste("--- DEBUG: Click is valid. Group:", clicked_value, "---"))
  
  next_level <- drill_levels[current_index + 1]
  col_to_filter <- col_name_map[next_level] 
  
  new_filters <- current_state$filters
  new_filters[[col_to_filter]] <- clicked_value
  
  print(paste("--- ADVANCED ANALYTICS: Drilling down to", next_level, 
              "where", col_to_filter, "=", clicked_value, "---"))
  
  new_state_list <- list(level = next_level, filters = new_filters)
  adv_drill_state(new_state_list)
})


# --- 5. Data Table and Map ---

# --- 5a. Render the Filtered Data Table ---
output$advanced_data_table <- DT::renderDataTable({
  
  req(drilled_data_and_level())
  print("--- ADVANCED ANALYTICS: Rendering data table. ---")
  
  data_from_drilldown <- drilled_data_and_level()$data
  
  if (!"SchoolID" %in% names(data_from_drilldown)) {
    data_to_show <- data_from_drilldown %>% select(School.Name, everything())
    cols_to_freeze <- 1
  } else {
    data_to_show <- data_from_drilldown %>% select(School.Name, SchoolID, everything())
    cols_to_freeze <- 2
  }
  
  datatable(
    data_to_show,
    selection = 'single', 
    extensions = 'FixedColumns', 
    options = list(
      scrollX = TRUE,  
      pageLength = 10, 
      autoWidth = FALSE, 
      fixedColumns = list(leftColumns = cols_to_freeze),
      scrollCollapse = TRUE
    ),
    rownames = FALSE,
    filter = 'top' 
  )
})

# --- 5b. Render the Leaflet Map ---
output$advanced_school_map <- renderLeaflet({
  
  print("--- MAP RENDER: Starting... ---")
  
  req(drilled_data_and_level())
  data_for_map <- drilled_data_and_level()$data
  
  lat_col <- "Latitude" 
  lon_col <- "Longitude"
  
  if (!all(c(lat_col, lon_col) %in% names(data_for_map))) {
    return(leaflet() %>% addTiles() %>% 
             addControl(paste("MAP ERROR: Columns", lat_col, "or", lon_col, "not found."), 
                        position = "topright", className = "map-error-box"))
  }
  
  data_for_map_filtered <- data_for_map %>%
    mutate(
      !!sym(lat_col) := as.numeric(!!sym(lat_col)),
      !!sym(lon_col) := as.numeric(!!sym(lon_col))
    ) %>%
    filter(!is.na(!!sym(lat_col)) & !is.na(!!sym(lon_col))) 
  
  if (nrow(data_for_map_filtered) == 0) {
    return(leaflet() %>% addTiles() %>% 
             addControl("No schools with valid coordinates match your filters.", 
                        position = "topright"))
  }
  
  data_for_map_filtered <- data_for_map_filtered %>%
    mutate(
      map_label_html = paste(
        "<strong>", School.Name, "</strong><hr>",
        "<strong>School ID:</strong>", SchoolID, "<br>",
        "<strong>Region:</strong>", Region, "<br>",
        "<strong>Division:</strong>", Division, "<br>",
        "<strong>Municipality:</strong>", Municipality
      )
    )
  
  data_for_map_filtered$map_label_html <- lapply(
    data_for_map_filtered$map_label_html, 
    htmltools::HTML
  )
  
  leaflet(data = data_for_map_filtered) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
    addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Road Map") %>%
    addMeasure(position = "topright", primaryLengthUnit = "kilometers", primaryAreaUnit = "sqmeters") %>% 
    addAwesomeMarkers(
      lng = as.formula(paste0("~", lon_col)),
      lat = as.formula(paste0("~", lat_col)),
      label = ~map_label_html,
      icon = icon("graduation-cap"),
      clusterOptions = markerClusterOptions()
    ) %>%
    addLayersControl(baseGroups = c("Satellite","Road Map"))
})


# --- 5c. Observer for table row selection ---
observeEvent(input$advanced_data_table_rows_selected, {
  
  req(input$advanced_data_table_rows_selected)
  selected_row_index <- input$advanced_data_table_rows_selected
  
  lat_col <- "Latitude" 
  lon_col <- "Longitude"
  
  tryCatch({
    data_from_table <- drilled_data_and_level()$data
    
    if (!all(c(lat_col, lon_col) %in% names(data_from_table))) {
      return()
    }
    
    selected_row_data <- data_from_table[selected_row_index, ]
    selected_lat <- as.numeric(selected_row_data[[lat_col]])
    selected_lon <- as.numeric(selected_row_data[[lon_col]])
    
    if (is.na(selected_lat) || is.na(selected_lon)) {
      return()
    }
    
    leafletProxy("advanced_school_map") %>%
      clearPopups() %>% 
      setView(
        lng = selected_lon,
        lat = selected_lat,
        zoom = 15
      )
    
  }, error = function(e) {
    print(paste("--- MAP SETVIEW ERROR:", e$message, "---"))
  })
})