# server_parts/35_quick_tour.R

# 1. Define the Tour Steps (Unchanged)
stride_tour_steps <- data.frame(
  element = c(
    "#STRIDE2_navbar",
    "a[data-value='dashboard_menu']", 
    "a[data-value='quick_search_tab']",
    "a[data-value='resource_mapping_tab']",
    "#quick_start_btn" 
  ),
  intro = c(
    "<strong>Welcome to STRIDE!</strong><br>This platform unifies school data, human resources, and infrastructure planning into one strategic view.",
    "<strong>Interactive Dashboards</strong><br>Access the <em>Education Resource Dashboard</em> here. <br><br>Use it to visualize <strong>Teacher Shortages</strong>, <strong>Classroom Gaps</strong>, and <strong>Enrolment Trends</strong>.",
    "<strong>Quick School Search</strong><br>Need data on a specific school?<br><br>Use this tab to search by <strong>School Name or ID</strong>.",
    "<strong>Resource Mapping</strong><br>Perform geospatial analysis here.<br><br>Select a resource type to see distribution across the map.",
    "<strong>Need Help?</strong><br>Click here anytime to open the <strong>User Manual</strong>, view the <strong>Glossary</strong>, or check the <strong>FAQs</strong>."
  ),
  position = c("bottom", "right", "bottom", "bottom", "left")
)

# 2. Trigger Logic (UPDATED)
# Now listens specifically for the 'should_show_tour' flag
observeEvent(should_show_tour(), {
  
  # Only run if the flag is TRUE
  req(should_show_tour())
  
  # Delay slightly to ensure UI is ready
  shinyjs::delay(1500, {
    rintrojs::introjs(
      session, 
      options = list(
        steps = stride_tour_steps,
        "nextLabel" = "Next >",
        "prevLabel" = "< Back",
        "skipLabel" = "Skip Tour",
        "doneLabel" = "Get Started",
        "showStepNumbers" = "false",
        "showBullets" = "false" 
      )
    )
  })
  
  # Reset the flag so it doesn't run again on reload/re-login within session
  should_show_tour(FALSE)
})