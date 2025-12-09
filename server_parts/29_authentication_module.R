# ==========================================================
# --- AUTHENTICATION MODULE: FIXED VISIBILITY & ANIMATION ---
# ==========================================================

authentication_server <- function(input, output, session, 
                                  user_status, 
                                  authenticated_user, 
                                  f, 
                                  user_database, 
                                  should_show_tour) { 
  ns <- session$ns
  
  # --- 0. STATE TRACKERS ---
  auth_mode <- reactiveVal("login") 
  login_type <- reactiveVal("deped") 
  
  # --- 1. SWITCH MODES ---
  observeEvent(input$btn_register, { auth_mode("register") })
  observeEvent(input$btn_login, { auth_mode("login") })
  observeEvent(input$btn_back_to_login, { auth_mode("login") })
  observeEvent(input$js_switch_to_login, { auth_mode("login") })
  
  # --- 2. TRANSITION LOGIC (The Smooth Animation) ---
  observeEvent(input$user_type_switch, {
    login_type(input$user_type_switch)
    
    # Use jQuery to smoothly fade between the two specific DIVs
    if (input$user_type_switch == "guest") {
      shinyjs::runjs(sprintf("$('#%s').fadeOut(200, function(){ $('#%s').fadeIn(300); });", 
                             ns("deped_panel"), ns("guest_panel")))
    } else {
      shinyjs::runjs(sprintf("$('#%s').fadeOut(200, function(){ $('#%s').fadeIn(300); });", 
                             ns("guest_panel"), ns("deped_panel")))
    }
  }, ignoreInit = TRUE)
  
  
  # --- 3. MAIN AUTH PAGE RENDERER ---
  output$auth_page <- renderUI({
    
    # CSS for Custom Switch Colors
    switch_css <- tags$style(HTML("
      /* Container Styling */
      .auth-toggle-group .btn-group {
         border: 1px solid #ddd;
         border-radius: 50px;
         overflow: hidden;
         box-shadow: 0 2px 5px rgba(0,0,0,0.05);
         width: 100%; /* Full width */
      }
      .auth-toggle-group .btn { 
         font-weight: 700; 
         font-size: 0.85rem; 
         border: none; 
         color: #666; /* Default text color */
         background-color: #f8f9fa;
         transition: all 0.3s;
      }
      
      /* Active State: DepEd (Left Button) */
      .auth-toggle-group .btn.active[data-value='deped'] {
         background-color: #003366 !important; 
         color: white !important;
         box-shadow: inset 0 3px 5px rgba(0,0,0,0.2);
      }
      
      /* Active State: Guest (Right Button) */
      .auth-toggle-group .btn.active[data-value='guest'] {
         background-color: #d68910 !important; 
         color: white !important;
         box-shadow: inset 0 3px 5px rgba(0,0,0,0.2);
      }
      
      /* Fix hover states */
      .auth-toggle-group .btn:hover {
         background-color: #e2e6ea;
      }
    "))
    
    # Left Side (Logo)
    left_side_ui <- div(
      class = "login-left",
      div(
        class = "login-text-box text-center",
        div(class = "login-left-logos", tags$img(src = "logo1.png", class = "left-logo")),
        h2(HTML('<img src="Stridelogo1.png" class="stride-logo-i" alt="I Logo">'), class = "stride-logo-text mt-3"),
        p(class = "slogan-mid", "Education in Motion!"),
        div(class = "slogan-bottom-row", span(class = "slogan-left", "Data Precision."), span(class = "slogan-right", "Smart Decision."))
      )
    )
    
    # Dynamic Right Side
    if (auth_mode() == "login") {
      # === LOGIN VIEW ===
      div(class = "login-container", switch_css, left_side_ui,
          div(class = "login-right",
              div(class = "login-card",
                  
                  # 1. HEADER
                  p(class = "slogan-login-top", "Welcome to STRIDE!"),
                  div(class = "slogan-login-bottom", span("Select your login type:")),
                  
                  # 2. SWITCH BUTTON
                  # Note: We assign values 'deped' and 'guest' which match the CSS data-value selectors above
                  div(class = "auth-toggle-group mb-4 d-flex justify-content-center",
                      shinyWidgets::radioGroupButtons(
                        inputId = ns("user_type_switch"),
                        label = NULL,
                        choices = c("DEPED" = "deped", "GUEST" = "guest"),
                        selected = "deped",
                        justified = TRUE,
                        size = "sm",
                        status = "default" # We override this with custom CSS
                      )
                  ),
                  
                  # 3. FORM CONTAINER
                  div(style = "position: relative; min-height: 280px;",
                      
                      # --- A. DEPED FORM (Visible) ---
                      div(id = ns("deped_panel"),
                          textInput(ns("login_user"), NULL, placeholder = "Email"),
                          tags$div(class = "input-group mb-2",
                                   tags$input(id = ns("login_pass"), type = "password", class = "form-control", placeholder = "Password"),
                                   tags$span(class = "input-group-text toggle-password", `data-target` = ns("login_pass"), HTML('<i class="fa fa-eye" aria-hidden="true"></i>'))
                          ),
                          actionButton(ns("do_login"), "Sign In", class = "btn-login w-100 btn-enter-login"),
                          # Google Login Button
                          div(style = "margin-top: 15px; margin-bottom: 15px;",
                              actionButton(ns("google_login_btn"), 
                                label = HTML('
                                  <div style="display: flex; align-items: center; justify-content: center;">
                                    <svg version="1.1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 48 48" style="height: 20px; margin-right: 12px; display: block;">
                                      <path fill="#EA4335" d="M24 9.5c3.54 0 6.71 1.22 9.21 3.6l6.85-6.85C35.9 2.38 30.47 0 24 0 14.62 0 6.51 5.38 2.56 13.22l7.98 6.19C12.43 13.72 17.74 9.5 24 9.5z"></path>
                                      <path fill="#4285F4" d="M46.98 24.55c0-1.57-.15-3.09-.38-4.55H24v9.02h12.94c-.58 2.96-2.26 5.48-4.78 7.18l7.73 6c4.51-4.18 7.09-10.36 7.09-17.65z"></path>
                                      <path fill="#FBBC05" d="M10.53 28.59c-.48-1.45-.76-2.99-.76-4.59s.27-3.14.76-4.59l-7.98-6.19C.92 16.46 0 20.12 0 24c0 3.88.92 7.54 2.56 10.78l7.97-6.19z"></path>
                                      <path fill="#34A853" d="M24 48c6.48 0 11.93-2.13 15.89-5.81l-7.73-6c-2.15 1.45-4.92 2.3-8.16 2.3-6.26 0-11.57-4.22-13.47-9.91l-7.98 6.19C6.51 42.62 14.62 48 24 48z"></path>
                                    </svg>
                                    <span style="font-family: Roboto, arial, sans-serif; font-weight: 500;">Sign in with Google</span>
                                  </div>
                                '), 
                                class = "btn w-100", 
                                style = "background-color: #ffffff; color: #3c4043; border: 1px solid #dadce0; box-shadow: 0 1px 2px rgba(0,0,0,0.1);"
                              )
                          ),
                          br(),
                          actionLink(ns("btn_register"), "Create an account", class = "register-link"),
                          
                          div(class = "login-logos-bottom mt-3", tags$img(src = "logo2.png", class = "bottom-logo"), tags$img(src = "HROD LOGO1.png", class = "bottom-logo"), tags$img(src = "logo3.png", class = "bottom-logo"))
                      ),
                      
                      # --- B. GUEST FORM (Hidden by standard CSS) ---
                      # FIX: Removed shinyjs::hidden(), used standard style="display:none"
                      div(id = ns("guest_panel"), style = "display: none;",
                          textInput(ns("guest_name"), "Full Name", placeholder = "Enter your name"),
                          textInput(ns("guest_email"), "Email Address", placeholder = "e.g., juan@gmail.com"),
                          textInput(ns("guest_org"), "Organization", placeholder = "Affiliation"),
                          textAreaInput(ns("guest_purpose"), "Purpose", placeholder = "Reason for visit...", rows = 2),
                          
                          actionButton(ns("do_guest_enter"), "Enter Dashboard", 
                                       class = "btn-login w-100", 
                                       style = "background-color: #e0a800 !important; border-color: #e0a800 !important; margin-top: 10px;"),
                          
                          div(class = "login-logos-bottom mt-3", tags$img(src = "logo2.png", class = "bottom-logo"), tags$img(src = "HROD LOGO1.png", class = "bottom-logo"))
                      )
                  )
              )
          )
      )
    } else {
      # === REGISTER VIEW (UNCHANGED) ===
      div(class = "login-container",
          div(class = "login-left",
              div(class = "login-text-box",
                  h2("Create a STRIDE Account"),
                  p("Register your DepEd account to access STRIDE dashboards.")
              )
          ),
          div(class = "register-wrapper d-flex gap-4 align-items-start",
              
              conditionalPanel(
                condition = paste0("['Engineer II','Engineer III','Engineer IV','Engineer V','Human Resources Management Officer I'].includes(input['", ns("position"), "'])"),
                div(class = "engineer-panel card p-3",
                    h4("Engineer / HR Information"),
                    textInput(ns("first_name"), "First Name"),
                    textInput(ns("middle_name"), "Middle Name"),
                    textInput(ns("last_name"), "Last Name"),
                    numericInput(ns("age"), "Age", value = NA, min = 18, max = 100, step = 1),
                    dateInput(ns("birthday"), "Birthday", format = "yyyy-mm-dd"),
                    textInput(ns("address"), "Address"),
                    selectInput(ns("region"), "Region", choices = sort(unique(uni$Region))),
                    uiOutput(ns("division_ui")),
                    uiOutput(ns("district_ui")),
                    uiOutput(ns("school_ui"))
                )
              ),
              
              div(class = "login-right flex-grow-1",
                  div(class = "login-card",
                      selectInput(ns("govlev"), "Select Station:", choices = c("— Select an Option —" = "", "Central Office", "Regional Office", "Schools Division Office", "School")),
                      uiOutput(ns("station_specific_ui")),
                      uiOutput(ns("position_ui")),
                      textInput(ns("reg_user"), NULL, placeholder = "DepEd Email (@deped.gov.ph)"),
                      tags$div(class = "input-group mb-2",
                               tags$input(id = ns("reg_pass"), type = "password", class = "form-control", placeholder = "Password"),
                               tags$span(class = "input-group-text toggle-password", `data-target` = ns("reg_pass"), HTML('<i class="fa fa-eye" aria-hidden="true"></i>'))
                      ),
                      tags$div(class = "input-group mb-2",
                               tags$input(id = ns("reg_pass_confirm"), type = "password", class = "form-control", placeholder = "Confirm Password"),
                               tags$span(class = "input-group-text toggle-password", `data-target` = ns("reg_pass_confirm"), HTML('<i class="fa fa-eye" aria-hidden="true"></i>'))
                      ),
                      actionButton(ns("do_register"), "Register Account", class = "btn-login w-100 btn-enter-register"),
                      br(),
                      actionLink(ns("btn_back_to_login"), "Back to Login", class = "register-link"),
                      div(class = "login-logos-bottom", tags$img(src = "HROD LOGO1.png", class = "bottom-logo"))
                  )
              )
          )
      )
    }
  })
  
  # --- 4. DYNAMIC UI RENDERING (Dropdowns) ---
  output$station_specific_ui <- renderUI({
    req(input$govlev)
    if (input$govlev == "School") {
      tagList(textInput(ns("school_id_main"), "School ID:"), tags$small("Enter your School ID (6 digits).", class = "text-muted"))
    } else if (input$govlev %in% c("Central Office", "Regional Office", "Schools Division Office")) {
      tagList(textInput(ns("office_name"), "Office Name:"), tags$small("Enter Bureau/Division. Do not abbreviate!", class = "text-muted"))
    } else NULL
  })
  
  output$position_ui <- renderUI({
    req(input$govlev)
    all_positions <- tryCatch({
      dfGMISPosCat <- read.csv("GMIS-Apr2025-PosCat.csv")
      unique(dfGMISPosCat$Position)
    }, error = function(e) { c("Teacher I", "School Principal I", "Engineer II") })
    final_positions <- sort(unique(c(all_positions, "Others (COS)", "Technical Assistant")))
    selectInput(ns("position"), "Position:", choices = final_positions)
  })
  
  observeEvent(input$region, {
    req(input$region)
    divisions <- sort(unique(uni$Division[uni$Region == input$region]))
    updateSelectInput(session, "division", choices = divisions)
  })
  output$division_ui <- renderUI({ req(input$region); selectInput(ns("division"), "Division", choices = sort(unique(uni$Division[uni$Region == input$region]))) })
  output$district_ui <- renderUI({ req(input$division); selectInput(ns("district"), "Legislative District", choices = sort(unique(uni$Legislative.District[uni$Division == input$division]))) })
  output$school_ui <- renderUI({ req(input$district); selectInput(ns("school_id_eng"), "School ID (6-digit)", choices = sort(unique(uni$SchoolID[uni$Legislative.District == input$district]))) })
  
  
  # --- 5. LOGIN HANDLERS ---
  observeEvent(input$do_login, {
    req(input$login_user, input$login_pass)
    f$sign_in(input$login_user, input$login_pass)
  })
  observeEvent(input$google_login_btn, { session$sendCustomMessage("firebase-google-auth", "trigger") })
  
  # GUEST LOGIN HANDLER
  observeEvent(input$do_guest_enter, {
    if (is.null(input$guest_name) || input$guest_name == "") { showNotification("Please enter your name.", type = "error"); return() }
    if (is.null(input$guest_email) || input$guest_email == "" || !grepl("@", input$guest_email)) { showNotification("Please enter a valid email.", type = "error"); return() }
    if (is.null(input$guest_purpose) || input$guest_purpose == "") { showNotification("Please state your purpose.", type = "error"); return() }
    
    guest_data <- list(Name = input$guest_name, Email = input$guest_email, Organization = ifelse(is.null(input$guest_org), "", input$guest_org), Purpose = input$guest_purpose, Timestamp = as.character(Sys.time()), User_Type = "Guest")
    f$save_guest_data(guest_data)
    
    user_status("authenticated")
    authenticated_user("guest_user@stride")
    should_show_tour(TRUE)
    session$sendCustomMessage("showLoader", "Entering Guest Mode...")
  })
  
  # --- 6. REGISTRATION HANDLER ---
  observeEvent(input$do_register, {
    reg_user <- input$reg_user
    reg_pass <- input$reg_pass
    if (is.null(reg_user) || reg_user == "") { showNotification("❌ Please enter email.", type = "error"); return() }
    if (is.null(reg_pass) || reg_pass == "") { showNotification("❌ Please enter password.", type = "error"); return() }
    if (input$reg_pass != input$reg_pass_confirm) { showNotification("❌ Passwords do not match!", type = "error"); return() }
    
    current_db <- user_database()
    if (!is.null(current_db) && nrow(current_db) > 0) {
      existing_emails <- tolower(trimws(current_db$Email_Address))
      if (tolower(trimws(reg_user)) %in% existing_emails) {
        showNotification("❌ This email is already registered. Please log in.", type = "error", duration = 5)
        return()
      }
    }
    
    final_school_id <- ""
    if (!is.null(input$school_id_main) && nzchar(input$school_id_main)) final_school_id <- input$school_id_main
    else if (!is.null(input$school_id_eng) && nzchar(input$school_id_eng)) final_school_id <- input$school_id_eng
    
    new_user <- list(
      Email_Address = reg_user, Station = input$govlev, Position = input$position,
      Office_Name = ifelse(is.null(input$office_name), "", input$office_name),
      School_ID = final_school_id,
      Engineer_Selected_School_ID = ifelse(is.null(input$school_id_eng), "", input$school_id_eng),
      First_Name = ifelse(is.null(input$first_name), "", input$first_name),
      Last_Name = ifelse(is.null(input$last_name), "", input$last_name),
      Region = ifelse(is.null(input$region), "", input$region),
      Division = ifelse(is.null(input$division), "", input$division),
      Registration_Date = as.character(Sys.time())
    )
    
    f$create_user(reg_user, reg_pass)
    f$save_user_data(new_user)
    should_show_tour(TRUE)
    showNotification("Processing registration...", type = "warning")
  })
}