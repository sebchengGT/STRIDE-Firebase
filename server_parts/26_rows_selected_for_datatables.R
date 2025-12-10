# Rows Selected for Tables

# --- 2. The "Run" Button Observer ---
# This observer's ONLY job is to filter data and update the data_filtered object.

observeEvent(input$LMSTable_rows_selected, {
  
  RegRCT <- input$resource_map_region
  SDORCT1 <- input$Resource_SDO
  
  mainreactLMS <- LMS %>%
    filter(LMS == 1) %>%
    left_join(buildablecsv %>% select(SCHOOL.ID,OTHER.REMARKS..Buildable.Space..), by = c("School_ID" = "SCHOOL.ID")) %>% 
    filter(Region == RegRCT) %>% filter(Division == SDORCT1)
  
  df1 <- reactive({
    if (is.null(input$LMSMapping_bounds)) {
      mainreactLMS
    } else {
      bounds <- input$LMSMapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(mainreactLMS,
             Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
  
  row_selected = df1()[input$LMSTable_rows_selected,]
  
  leafletProxy("LMSMapping") %>%
    flyTo(
      lng = row_selected$Longitude, 
      lat = row_selected$Latitude, 
      zoom = 15,
      options = leafletOptions(duration = 0.5)
    )
})

# --- NOTE: TextTable (Quick Search) Observer REMOVED ---
# It conflicted with the new logic in 22_quick_school_search.R


# --- 2. Congestion Table Selection ---
observeEvent(input$CongestTable_rows_selected, {
  
  RegRCT <- input$resource_map_region
  SDORCT1 <- input$Resource_SDO
  DistRCT1 <- input$Resource_LegDist
  
  mainreactNTP <- uni %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Legislative.District == DistRCT1)
  
  dfreact_cong <- reactive({
    if (is.null(input$CongestMapping_bounds)) {
      mainreactNTP
    } else {
      bounds <- input$CongestMapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(mainreactNTP,
             Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
  
  row_selected = dfreact_cong()[input$CongestTable_rows_selected,]
  
  leafletProxy("CongestMapping") %>%
    flyTo(
      lng = row_selected$Longitude, 
      lat = row_selected$Latitude, 
      zoom = 15,
      options = leafletOptions(duration = 0.5)
    )
})

# --- 3. Classroom Table Selection ---
observeEvent(input$CLTable_rows_selected, {
  
  RegRCT <- input$resource_map_region
  SDORCT3 <- input$Resource_SDO
  DistRCT3 <- input$Resource_LegDist
  
  mainreact1x <- uni %>% filter(Region == RegRCT) %>% filter(Division == SDORCT3) %>% filter(Legislative.District == DistRCT3) %>% arrange(desc(SBPI))
  
  CL1 <- reactive({
    if (is.null(input$CLMapping_bounds)) {
      mainreact1x
    } else {
      bounds <- input$CLMapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(mainreact1x,
             Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
  
  row_selected = CL1()[input$CLTable_rows_selected,]
  
  leafletProxy("CLMapping") %>%
    flyTo(
      lng = row_selected$Longitude, 
      lat = row_selected$Latitude, 
      zoom = 15,
      options = leafletOptions(duration = 0.5)
    )
})

# --- 4. Facilities Table Selection ---
observeEvent(input$FacTable_rows_selected, {
  
  RegRCT <- input$resource_map_region
  SDORCT1 <- input$Resource_SDO
  
  mainreactEFD <- EFDMP %>% 
    filter(!is.na(Old.Region), Old.Region != "") %>% 
    filter(!is.na(Latitude), !is.na(Longitude)) %>% 
    mutate(Latitude = as.numeric(Latitude),
           Allocation = dollar(Allocation, prefix = "₱")) %>% 
    distinct(SchoolID, FundingYear, Allocation, Category, .keep_all = TRUE) %>%
    arrange(FundingYear) %>% 
    filter(Region == input$resource_map_region) %>%
    filter(Division == input$Resource_SDO) %>%
    filter(Category %in% input$EFD_Type)
  
  dfreact_fac <- reactive({
    if (is.null(input$FacMapping_bounds)) {
      mainreactEFD
    } else {
      bounds <- input$FacMapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(mainreactEFD,
             Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
  
  row_selected = dfreact_fac()[input$FacTable_rows_selected,]
  
  leafletProxy("FacMapping") %>%
    flyTo(
      lng = row_selected$Longitude, 
      lat = row_selected$Latitude, 
      zoom = 15,
      options = leafletOptions(duration = 0.5)
    )
})

# --- 5. AO2 Table Selection ---
observeEvent(input$AO2Table_rows_selected, {
  
  RegRCT <- input$resource_map_region
  SDORCT2 <- input$Resource_SDO
  DistRCT2 <- input$Resource_LegDist
  
  Ao2Filter <- uni %>% filter(Region == RegRCT) %>% filter(Division == SDORCT2) %>% filter(Legislative.District == DistRCT2)
  
  xy1 <- reactive({
    if (is.null(input$AO2Mapping_bounds)) {
      Ao2Filter
    } else {
      bounds <- input$AO2Mapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(Ao2Filter,
             Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
  
  row_selected = xy1()[input$AO2Table_rows_selected,]
  
  leafletProxy("AO2Mapping") %>%
    flyTo(
      lng = row_selected$Longitude, 
      lat = row_selected$Latitude, 
      zoom = 15,
      options = leafletOptions(duration = 0.5)
    )
})

# --- 6. Teacher Shortage Table Selection ---
observeEvent(input$TeacherShortage_Table_rows_selected, {
  
  RegRCT <- input$resource_map_region
  SDORCT1 <- input$Resource_SDO
  DistRCT1 <- input$Resource_LegDist
  Lev <- input$resource_map_level
  
  
  mainreact1 <- df %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Legislative.District == DistRCT1) %>% filter(Level == Lev)
  
  df1 <- reactive({
    if (is.null(input$TeacherShortage_Mapping_bounds)) {
      mainreact1
    } else {
      bounds <- input$TeacherShortage_Mapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(mainreact1,
             Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
  
  row_selected = df1()[input$TeacherShortage_Table_rows_selected,]
  
  leafletProxy("TeacherShortage_Mapping") %>%
    flyTo(
      lng = row_selected$Longitude, 
      lat = row_selected$Latitude, 
      zoom = 15,
      options = leafletOptions(duration = 0.5)
    )
  
  # Update side panel details if any
  output$d <- renderValueBox({
    valueBox(tags$p(strong(row_selected$TeacherShortage), style = "font-size: 65%;"), 
             subtitle = tags$p(strong("School Teacher Shortage"), style = "font-size: 60%;"), 
             color = "red")
  })
  
  output$TeacherShortage_Assessment <- renderUI({
    # Add safe checks for values
    total_excess <- sum(df1()$TeacherExcess, na.rm = TRUE)
    p(HTML(paste(strong(row_selected$School.Name),
                 "is located in the Division of", strong(row_selected$Division),
                 ". The said school has a shortage of ", strong(row_selected$TeacherShortage),
                 "teacher/s and a total excess of", strong(total_excess),
                 "teacher/s captured within the map and table above")), 
      style = "font-family: Century Gothic; font-size: 15px; color: #111111;")
  })
  
  # Update School Data Tables
  EDtable <- row_selected %>% select(School.Name,Kinder,G1,G2,G3,G4,G5,G6,G7,G8,G9,G10,G11,G12)
  Loctable <- row_selected %>% select(School.Name,Province,Municipality,District, Barangay,Street.Address)
  Spectable <- row_selected %>% select(School.Name,English,Mathematics,Science,Biological.Sciences,Physical.Sciences) 
  
  observeEvent(input$SelectSchoolData, {
    selectSD = input$SelectSchoolData
    
    # Replaced get_dt_dom with 'Bfrtip' here too if needed, though these are simple renders
    if (selectSD == "Enrolment Data") {
      output$SchoolData <- DT::renderDT(EDtable, rownames = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-left', targets ="_all"))))}
    else {if (selectSD == "School Location") {
      output$SchoolData <- DT::renderDT(Loctable, rownames = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-left', targets ="_all"))))}
      else {if  (selectSD == "Specialization") {
        output$SchoolData <- DT::renderDT(Spectable, rownames = FALSE, options = list(scrollX = TRUE, columnDefs = list(list(className = 'dt-left', targets ="_all"))))}
      }}})
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# --- 7. SHS List Table Selection ---
observeEvent(input$SHSListTable_rows_selected, {
  
  RegRCT <- input$resource_map_region
  SDORCT1 <- input$Resource_SDO
  DistRCT1 <- input$Resource_LegDist
  
  region_selected <- IndALL %>% filter(Region == RegRCT) %>% arrange(Distance)
  
  mainreact1 <- df %>% filter(Region == RegRCT) %>% filter(Division == SDORCT1) %>% filter(Legislative.District == DistRCT1) %>% filter(Level == "SHS") %>% distinct(SchoolID, .keep_all = TRUE)
  
  df1 <- reactive({
    if (is.null(input$SHSMapping_bounds)) {
      mainreact1
    } else {
      bounds <- input$SHSMapping_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(mainreact1,
             Latitude >= latRng[1] & Latitude <= latRng[2] & Longitude >= lngRng[1] & Longitude <= lngRng[2])
    }
  })
  
  row_selected = df1()[input$SHSListTable_rows_selected,]
  rowschool = row_selected$School.Name
  
  leafletProxy("SHSMapping") %>%
    flyTo(
      lng = row_selected$Longitude, 
      lat = row_selected$Latitude, 
      zoom = 15,
      options = leafletOptions(duration = 0.5)
    )
  
  SHSIndustries <- region_selected %>% filter(School.Name %in% rowschool) %>% select("Company","Sector","Distance") %>% rename("Distance in KM" = Distance)
  
  output$dataTableSHS <- DT::renderDT({
    data_to_display <- SHSIndustries
    
    if (is.null(data_to_display) || nrow(data_to_display) == 0) {
      return(DT::datatable(
        data.frame("Message" = "Select a school to view nearby industries."),
        options = list(dom = 't', scrollX = TRUE),
        rownames = FALSE
      ))
    }
    
    DT::datatable(
      data_to_display,
      extensions = c("Buttons", "FixedHeader", "FixedColumns"),
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        fixedHeader = TRUE,
        fixedColumns = list(leftColumns = 2),
        pageLength = 10,
        dom = 'Bfrtip', # <--- FIXED HERE
        buttons = list('csv','excel','pdf','print'),
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      filter = 'top',     
      selection = 'multiple',
      rownames = FALSE
    )
  }, server = FALSE)
  
  output$AccoCount <- renderValueBox({
    valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Manufacturing and Engineering")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
  
  output$ProfCount <- renderValueBox({
    valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Hospitality and Tourism")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
  
  output$WastCount <- renderValueBox({
    valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Public Administration")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
  
  output$TranCount <- renderValueBox({
    valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Professional/Private Services")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
  
  output$WholCount <- renderValueBox({
    valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Business and Finance")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
  
  output$WholCount2 <- renderValueBox({
    valueBox(tags$p(strong(sum(SHSIndustries$Sector == "Agriculture and Agri-business")), style = "font-size: 100%; text-align: center;"), subtitle = NULL)})
  
  rowselected_SHStable <- df %>% filter(SchoolID == row_selected$SchoolID) %>% slice(1) %>% select(Region,Division,Modified.COC,School.Name,SchoolID,SHS.Enrolment,SHS.Packages) %>% rename("School ID" = SchoolID,"Schools Division Office" = Division,"Modified Curricular Offering" = Modified.COC,"School Name" = School.Name,"Delivered Learning Packages (2018-2025)"=SHS.Packages) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
    cols = everything(),    # Pivot all columns selected in details_to_pivot
    names_to = "Profile Item",     # Name of the new column holding the original column names
    values_to = "Data")     # Name of the new column holding the original values
  
  rowselected_SHStablespec <- df %>%  filter(SchoolID == row_selected$SchoolID) %>% slice(1) %>% select(English,Mathematics,Science,Biological.Sciences,Physical.Sciences,General.Ed,Araling.Panlipunan,TLE,MAPEH,Filipino,ESP,Agriculture,ECE,SPED) %>% rename("Biological Sciences" = Biological.Sciences,"Physical Sciences" = Physical.Sciences,"General Education" = General.Ed,"Araling Panlipunan" = Araling.Panlipunan,"Early Chilhood Education" = ECE) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
    cols = everything(),    # Pivot all columns selected in details_to_pivot
    names_to = "Profile Item",     # Name of the new column holding the original column names
    values_to = "Data")     # Name of the new column holding the original values
  
  rowselected_SHStablespec2 <- df %>% select(English,Mathematics,Science,Biological.Sciences,Physical.Sciences,General.Ed,Araling.Panlipunan,TLE,MAPEH,Filipino,ESP,Agriculture,ECE,SPED) %>% rename("Biological Sciences" = Biological.Sciences,"Physical Sciences" = Physical.Sciences,"General Education" = General.Ed,"Araling Panlipunan" = Araling.Panlipunan,"Early Chilhood Education" = ECE) %>% mutate(dplyr::across(tidyr::everything(), as.character)) %>% pivot_longer(
    cols = everything(),    # Pivot all columns selected in details_to_pivot
    names_to = "Profile",     # Name of the new column holding the original column names
    values_to = "Data") %>% mutate(Data = as.numeric(Data)) %>% group_by(Profile) %>% summarise(Data = sum(Data, na.rm = TRUE))
  
  output$SHSTablex <- renderTable({
    # Pass the pivoted data frame directly
    rowselected_SHStable
  },
  rownames = FALSE, # Don't show automatic row names
  colnames = TRUE,  # Show column names (Field, Value)
  hover = TRUE,     # Add hover effect to rows (optional styling)
  bordered = TRUE)
  
  output$PilotSpec <- renderTable({
    # Pass the pivoted data frame directly
    rowselected_SHStablespec
  },
  rownames = FALSE, # Don't show automatic row names
  colnames = TRUE,  # Show column names (Field, Value)
  hover = TRUE,     # Add hover effect to rows (optional styling)
  bordered = TRUE)
  
})