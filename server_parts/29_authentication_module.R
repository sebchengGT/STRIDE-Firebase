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
  
  # 0.1 OTP STATE TRACKER
  otp_state <- reactiveValues(
    generated_code = NULL,
    verified = FALSE
  )
  
  # --- 1. SWITCH MODES ---
  observeEvent(input$btn_register, { auth_mode("register") })
  observeEvent(input$btn_login, { auth_mode("login") })
  observeEvent(input$btn_back_to_login, { auth_mode("login") })
  observeEvent(input$js_switch_to_login, { auth_mode("login") })
  
  # --- 2. TRANSITION LOGIC ---
  observeEvent(input$user_type_switch, {
    login_type(input$user_type_switch)
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
    
    # CSS Styles
    switch_css <- tags$style(HTML("
      .auth-toggle-group .btn-group { border: 1px solid #ddd; border-radius: 50px; overflow: hidden; width: 100%; }
      .auth-toggle-group .btn { font-weight: 700; font-size: 0.85rem; border: none; color: #666; background-color: #f8f9fa; }
      .auth-toggle-group .btn.active[data-value='deped'] { background-color: #003366 !important; color: white !important; }
      .auth-toggle-group .btn.active[data-value='guest'] { background-color: #d68910 !important; color: white !important; }
    "))
    
    # Left Side UI
    left_side_ui <- div(class = "login-left",
                        div(class = "login-text-box text-center",
                            div(class = "login-left-logos", tags$img(src = "logo1.png", class = "left-logo")),
                            h2(HTML('<img src="Stridelogo1.png" class="stride-logo-i" alt="I Logo">'), class = "stride-logo-text mt-3"),
                            p(class = "slogan-mid", "Education in Motion!"),
                            div(class = "slogan-bottom-row", span(class = "slogan-left", "Data Precision."), span(class = "slogan-right", "Smart Decision."))
                        )
    )
    
    # Google Button Label (Moved here for safety)
    google_btn_label <- HTML('<div style="display: flex; align-items: center; justify-content: center;"><span style="font-family: Roboto, arial, sans-serif; font-weight: 500;">Sign in with Google</span></div>')
    
    # === LOGIC FOR SHOWING LOGIN vs REGISTER ===
    if (auth_mode() == "login") {
      # === LOGIN VIEW ===
      div(class = "login-container", switch_css, left_side_ui,
          div(class = "login-right",
              div(class = "login-card",
                  p(class = "slogan-login-top", "Welcome to STRIDE!"),
                  div(class = "slogan-login-bottom", span("Select your login type:")),
                  
                  # Switch Buttons
                  div(class = "auth-toggle-group mb-4 d-flex justify-content-center",
                      shinyWidgets::radioGroupButtons(inputId = ns("user_type_switch"), label = NULL, choices = c("DEPED" = "deped", "GUEST" = "guest"), selected = "deped", justified = TRUE, size = "sm", status = "default")
                  ),
                  
                  # Form Container
                  div(style = "position: relative; min-height: 280px;",
                      
                      # DEPED PANEL
                      div(id = ns("deped_panel"),
                          textInput(ns("login_user"), NULL, placeholder = "Email"),
                          tags$div(class = "input-group mb-2",
                                   tags$input(id = ns("login_pass"), type = "password", class = "form-control", placeholder = "Password"),
                                   tags$span(class = "input-group-text toggle-password", `data-target` = ns("login_pass"), HTML('<i class="fa fa-eye" aria-hidden="true"></i>'))
                          ),
                          actionButton(ns("do_login"), "Sign In", class = "btn-login w-100 btn-enter-login"),
                          div(style = "margin-top: 15px; margin-bottom: 15px;",
                              actionButton(ns("google_login_btn"), label = google_btn_label, class = "btn w-100", style = "background-color: #ffffff; color: #3c4043; border: 1px solid #dadce0;")
                          ),
                          br(),
                          actionLink(ns("btn_register"), "Create an account", class = "register-link"),
                          div(class = "login-logos-bottom mt-3", tags$img(src = "logo2.png", class = "bottom-logo"), tags$img(src = "HROD LOGO1.png", class = "bottom-logo"), tags$img(src = "logo3.png", class = "bottom-logo"))
                      ),
                      
                      # GUEST PANEL
                      div(id = ns("guest_panel"), style = "display: none;",
                          textInput(ns("guest_name"), "Full Name", placeholder = "Enter your name"),
                          textInput(ns("guest_email"), "Email Address", placeholder = "e.g., juan@gmail.com"),
                          textInput(ns("guest_org"), "Organization", placeholder = "Affiliation"),
                          textAreaInput(ns("guest_purpose"), "Purpose", placeholder = "Reason for visit...", rows = 2),
                          actionButton(ns("do_guest_enter"), "Enter Dashboard", class = "btn-login w-100", style = "background-color: #e0a800 !important; border-color: #e0a800 !important; margin-top: 10px;"),
                          div(class = "login-logos-bottom mt-3", tags$img(src = "logo2.png", class = "bottom-logo"), tags$img(src = "HROD LOGO1.png", class = "bottom-logo"))
                      )
                  )
              )
          )
      )
    } else {
      # === REGISTER VIEW (WITH OTP) ===
      div(class = "login-container",
          div(class = "login-left",
              div(class = "login-text-box", h2("Create a STRIDE Account"), p("Register your DepEd account to access STRIDE dashboards."))
          ),
          div(class = "register-wrapper d-flex gap-4 align-items-start",
              
              # Engineer Panel (Conditional)
              conditionalPanel(
                condition = paste0("['Engineer II','Engineer III','Engineer IV','Engineer V','Human Resources Management Officer I'].includes(input['", ns("position"), "'])"),
                div(class = "engineer-panel card p-3", h4("Engineer / HR Information"), textInput(ns("first_name"), "First Name"), textInput(ns("middle_name"), "Middle Name"), textInput(ns("last_name"), "Last Name"), numericInput(ns("age"), "Age", value = NA, min = 18, max = 100, step = 1), dateInput(ns("birthday"), "Birthday", format = "yyyy-mm-dd"), textInput(ns("address"), "Address"), selectInput(ns("region"), "Region", choices = sort(unique(uni$Region))), uiOutput(ns("division_ui")), uiOutput(ns("district_ui")), uiOutput(ns("school_ui")))
              ),
              
              # Main Registration Form
              div(class = "login-right flex-grow-1",
                  div(class = "login-card",
                      selectInput(ns("govlev"), "Select Station:", choices = c("— Select an Option —" = "", "Central Office", "Regional Office", "Schools Division Office", "School")),
                      uiOutput(ns("station_specific_ui")),
                      uiOutput(ns("position_ui")),
                      
                      # OTP Section
                      tags$div(class = "mb-2",
                               textInput(ns("reg_user"), NULL, placeholder = "DepEd Email (@deped.gov.ph)", width = "100%"),
                               actionButton(ns("btn_send_otp"), "Send OTP Verification Code", class = "btn-info w-100", style = "margin-top: 5px; font-size: 12px;")
                      ),
                      uiOutput(ns("otp_panel")), 
                      
                      # Password Fields
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
  
  # --- 4. DYNAMIC UI & DROPDOWNS ---
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
    selectInput(ns("position"), "Position:", choices = c("", final_positions), selected = "")
  })
  
  observeEvent(input$region, {
    req(input$region)
    divisions <- sort(unique(uni$Division[uni$Region == input$region]))
    updateSelectInput(session, "division", choices = divisions)
  })
  output$division_ui <- renderUI({ req(input$region); selectInput(ns("division"), "Division", choices = sort(unique(uni$Division[uni$Region == input$region]))) })
  output$district_ui <- renderUI({ req(input$division); selectInput(ns("district"), "Legislative District", choices = sort(unique(uni$Legislative.District[uni$Division == input$division]))) })
  output$school_ui <- renderUI({ req(input$district); selectInput(ns("school_id_eng"), "School ID (6-digit)", choices = sort(unique(uni$SchoolID[uni$Legislative.District == input$district]))) })
  
  # --- 5. OTP LOGIC HANDLERS ---
  # --- 5. OTP LOGIC HANDLERS (Fixed & Automated) ---
  observeEvent(input$btn_send_otp, {
    req(input$reg_user)
    
    # 1. Validate Email
    if(!grepl("@deped.gov.ph", input$reg_user, fixed = TRUE)){
      showNotification("Please use a valid DepEd email (@deped.gov.ph).", type = "error")
      return()
    }
    
    # 2. Generate Code
    new_code <- floor(runif(1, 100000, 999999))
    otp_state$generated_code <- as.character(new_code)
    otp_state$verified <- FALSE 
    
    showNotification("Sending OTP...", type = "warning", duration = 2)
    
    # 3. Send Email using the Saved Credentials File
    tryCatch({
      email_body <- blastula::compose_email(
        body = blastula::md(paste0("## STRIDE Dashboard Verification\n\nYour verification code is: **", new_code, "**\n\nIf you did not request this, please ignore this email."))
      )
      
      blastula::smtp_send(
        email_body,
        to = input$reg_user,
        from = "sebastian.cheng@deped.gov.ph", 
        subject = "STRIDE Dashboard OTP Code",
        credentials = blastula::creds_file("gmail_creds") # <--- READS THE SAVED FILE
      )
      
      showNotification(paste("OTP Sent to", input$reg_user), type = "message")
      print(paste("DEBUG OTP:", new_code)) 
      
    }, error = function(e) {
      # Detailed error printing
      print(paste("SMTP Error:", e$message))
      showNotification("Failed to send email. Check Console for details.", type = "error")
    })
  })
  
  output$otp_panel <- renderUI({
    req(otp_state$generated_code)
    if(otp_state$verified) {
      tags$div(class = "alert alert-success mt-2", style = "padding: 5px; font-size: 12px;", icon("check"), "Email Verified")
    } else {
      tagList(
        textInput(ns("otp_input"), NULL, placeholder = "Enter 6-digit Code"),
        actionButton(ns("btn_verify_otp"), "Verify Code", class = "btn-warning w-100 mb-2")
      )
    }
  })
  
  observeEvent(input$btn_verify_otp, {
    req(input$otp_input)
    
    if(input$otp_input == otp_state$generated_code) {
      otp_state$verified <- TRUE
      
      # FIX: Changed 'success' to 'message' to prevent crash
      showNotification("Email Verified Successfully!", type = "message") 
      
      shinyjs::disable("reg_user") 
      shinyjs::hide("btn_send_otp") 
    } else {
      showNotification("Invalid Code. Please try again.", type = "error")
    }
  })
  
  # --- 6. LOGIN & REGISTRATION HANDLERS ---
  observeEvent(input$do_login, {
    req(input$login_user, input$login_pass)
    f$sign_in(input$login_user, input$login_pass)
  })
  
  observeEvent(input$google_login_btn, { session$sendCustomMessage("firebase-google-auth", "trigger") })
  
  observeEvent(input$do_guest_enter, {
    if (is.null(input$guest_name) || input$guest_name == "") { showNotification("Please enter your name.", type = "error"); return() }
    if (is.null(input$guest_email) || input$guest_email == "") { showNotification("Please enter a valid email.", type = "error"); return() }
    
    guest_data <- list(Name = input$guest_name, Email = input$guest_email, Organization = ifelse(is.null(input$guest_org), "", input$guest_org), Purpose = input$guest_purpose, Timestamp = as.character(Sys.time()), User_Type = "Guest")
    f$save_guest_data(guest_data)
    
    user_status("authenticated")
    authenticated_user("guest_user@stride")
    should_show_tour(TRUE)
    session$sendCustomMessage("showLoader", "Entering Guest Mode...")
  })
  
  observeEvent(input$do_register, {
    # OTP Check
    if(!otp_state$verified) {
      showNotification("⚠ Please verify your DepEd email via OTP first.", type = "error")
      return()
    }
    
    reg_user <- input$reg_user
    reg_pass <- input$reg_pass
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