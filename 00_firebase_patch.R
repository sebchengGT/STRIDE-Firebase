# ==========================================================
# 00_firebase_patch.R - FIXED SYNTAX
# ==========================================================
library(R6)

Firebase <- R6::R6Class("Firebase",
                        public = list(
                          config = NULL,
                          
                          initialize = function(config) {
                            print("PATCH: Firebase Class Initialized")
                            self$config <- config
                          },
                          
                          sign_in = function(email, password) {
                            session <- shiny::getDefaultReactiveDomain()
                            if (!is.null(session)) {
                              session$sendCustomMessage("firebase-sign_in", list(email = email, password = password))
                            }
                          },
                          
                          create_user = function(email, password) {
                            session <- shiny::getDefaultReactiveDomain()
                            session$sendCustomMessage("firebase-create_user", list(email = email, password = password))
                          },
                          
                          save_user_data = function(data) {
                            session <- shiny::getDefaultReactiveDomain()
                            if (is.data.frame(data)) data <- as.list(data)
                            session$sendCustomMessage("firebase-save-user", data)
                          },
                          
                          # --- Function to save GUEST data ---
                          save_guest_data = function(data) {
                            print("PATCH: save_guest_data called")
                            session <- shiny::getDefaultReactiveDomain()
                            if (is.data.frame(data)) data <- as.list(data)
                            session$sendCustomMessage("firebase-save-guest", data)
                          },
                          
                          sign_out = function() {
                            session <- shiny::getDefaultReactiveDomain()
                            session$sendCustomMessage("firebase-sign_out", list())
                          },
                          
                          get_signed_in = function() { 
                            shiny::getDefaultReactiveDomain()$input$fire_signed_in 
                          },
                          
                          get_created = function() { 
                            shiny::getDefaultReactiveDomain()$input$fire_created 
                          }
                        )
)