
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)

# ==========================================
# 1. PRE-LOAD MODEL ARTIFACTS & DATA LOOKUPS
# ==========================================

# Maintain the structural Town-to-Cluster routing framework
town_cluster_lookup <- tibble(
  town = c("ANG MO KIO", "BEDOK", "BISHAN", "BUKIT BATOK", "BUKIT MERAH", 
           "BUKIT PANJANG", "BUKIT TIMAH", "CENTRAL AREA", "CHOA CHU KANG", 
           "CLEMENTI", "GEYLANG", "HOUGANG", "JURONG EAST", "JURONG WEST", 
           "KALLANG/WHAMPOA", "MARINE PARADE", "PASIR RIS", "PUNGGOL", 
           "QUEENSTOWN", "SEMBAWANG", "SENG KANG", "SERANGOON", "TAMPINES", 
           "TOA PAYOH", "WOODLANDS", "YISHUN"),
  town_group = c("3", "3", "2", "1", "2", "1", "2", "2", "1", "3", "3", "1", 
                 "1", "1", "3", "2", "1", "1", "2", "1", "1", "3", "1", "3", "1", "1")
)

# Hardcoded fallback metrics generated from your backend analytics pipeline
app_start_psf_data <- tibble(
  town_group           = c("1","1","1","1","2","2","2","2","3","3","3","3"),
  flat_type            = c("2_ROOM","3_ROOM","4_ROOM","5_ROOM","2_ROOM","3_ROOM","4_ROOM","5_ROOM","2_ROOM","3_ROOM","4_ROOM","5_ROOM"),
  predicted_start_psf  = c(782, 603, 581, 556, 640, 564, 929, 778, 574, 624, 729, 774),
  floor_area_sqf       = c(500, 750, 1022, 1200, 500, 750, 1022, 1200, 500, 750, 1022, 1200)
)

app_growth_data <- tibble(
  town_group            = c("1","1","1","1","2","2","2","2","3","3","3","3"),
  flat_type_clean       = c("2_ROOM","3_ROOM","4_ROOM","5_ROOM","2_ROOM","3_ROOM","4_ROOM","5_ROOM","2_ROOM","3_ROOM","4_ROOM","5_ROOM"),
  central_growth_annual = c(0.0213, 0.0143, -0.0320, -0.0339, -0.0221, -0.0871, 0.0164, 0.0067, 0.0434, -0.0216, 0.0188, -0.0095)
)

rules <- list(
  horizon_years = 5,
  ltv           = 0.75,
  loan_years    = 25
)

# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  
  tags$head(
    tags$style(HTML("
      .hero-banner {
        width: 100%;
        height: auto;
        max-height: 350px;
        object-fit: cover;
        border-radius: 6px;
        margin-bottom: 25px;
      }
      .sidebar-panel {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
    "))
  ),
  
  div(
    img(src = "hdbimage.jpg", class = "hero-banner")
  ),
  
  titlePanel("HDB Financial Strategy Engine for Singles"),
  p("Empirical wealth forecasting and cash flow feasibility mapping across multiple housing configurations."),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      tags$h4("Step 1: Property Configurations"),
      
      selectizeInput(
        inputId = "selected_towns",
        label   = "Select Target Towns (Choose one or multiple):",
        choices = c("ANG MO KIO", "BEDOK", "BISHAN", "BUKIT MERAH", "CLEMENTI", 
                    "PUNGGOL", "SENGKANG", "WOODLANDS"), 
        selected = "PUNGGOL",
        multiple = TRUE,
        options  = list(placeholder = 'Select towns...')
      ),
      
      checkboxGroupInput(
        inputId  = "selected_room_types",
        label    = "Select Flat Sizes to Compare:",
        choices  = c("2-Room" = "2_ROOM", "3-Room" = "3_ROOM", 
                     "4-Room" = "4_ROOM", "5-Room" = "5_ROOM"),
        selected = "4_ROOM",
        inline   = TRUE
      ),
      
      hr(),
      tags$h4("Step 2: Financial & Budget Bounds"),
      
      sliderInput(
        inputId = "monthly_budget",
        label   = "Maximum Comfortable Monthly Housing Budget ($/month):",
        min     = 1500,
        max     = 7000,
        value   = 3500,
        step    = 100,
        pre     = "$"
      ),
      
      numericInput(
        inputId = "cash_injection",
        label   = "Available Cash / CPF Downpayment (For Resale Path):",
        value   = 150000,
        min     = 0,
        step    = 5000
      ),
      
      hr(),
      tags$h4("Step 3: Macro Controls"),
      
      numericInput(
        inputId = "base_rent",
        label   = "Current Monthly Rental Baseline ($):",
        value   = 2200,
        min     = 1000,
        step    = 100
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Wealth Projection Matrix", 
                 br(),
                 plotOutput("networth_plot", height = "500px"),
                 br(),
                 tags$h4("Granular Financial Breakdown Table"),
                 tableOutput("summary_table")),
        
        tabPanel("Strategic Recommendations", 
                 br(),
                 textOutput("recommendation_text"))
      )
    )
  )
)

# =====================================================================
# PART 3: DYNAMIC SERVER CONTROLLER REGIME (server)
# =====================================================================
server <- function(input, output, session) {
  
  monthly_payment <- function(principal, annual_rate, years) {
    r <- annual_rate / 12; n <- years * 12
    if (r == 0) principal / n else principal * r / (1 - (1 + r)^(-n))
  }
  
  results_data <- reactive({
    req(input$selected_towns, input$selected_room_types)
    
    sim_grid <- crossing(
      town      = input$selected_towns,
      flat_type = input$selected_room_types
    ) %>%
      left_join(town_cluster_lookup, by = "town")
    
    interest_rate <- 0.035
    h <- rules$horizon_years
    
    simulation_output <- sim_grid %>%
      rowwise() %>%
      do({
        current_town  <- .$town
        current_group <- .$town_group
        current_ft    <- .$flat_type
        
        psf_row <- app_start_psf_data %>% 
          filter(town_group == current_group, flat_type == current_ft)
        
        growth_row <- app_growth_data %>% 
          filter(town_group == current_group, flat_type_clean == current_ft)
        
        psf_val         <- if(nrow(psf_row) > 0) psf_row$predicted_start_psf else 600
        floor_area_sqf  <- if(nrow(psf_row) > 0) psf_row$floor_area_sqf else 1000
        growth_rate     <- if(nrow(growth_row) > 0) growth_row$central_growth_annual else 0.02
        
        calculated_resale_price <- psf_val * floor_area_sqf
        
        # B. RUN PATHWAY 1: OWNERSHIP (RESALE)
        min_downpayment_resale <- calculated_resale_price * (1 - rules$ltv)
        down_resale <- max(min_downpayment_resale, input$cash_injection)
        
        loan_resale <- max(0, calculated_resale_price - down_resale)
        pmt_resale  <- monthly_payment(loan_resale, interest_rate, rules$loan_years)
        
        r_mo <- interest_rate/12; n_mo <- rules$loan_years * 12; k_mo <- h * 12
        bal_resale <- if(loan_resale == 0) 0 else loan_resale * ((1+r_mo)^n_mo - (1+r_mo)^k_mo) / ((1+r_mo)^n_mo - 1)
        val_5y_resale <- calculated_resale_price * (1 + growth_rate)^h
        
        # APPROACH A: Net Worth position is the raw property asset equity (Value - Debt)
        resale_equity <- val_5y_resale - bal_resale
        
        # C. RUN PATHWAY 2: OWNERSHIP (BTO)
        bto_price_start <- 380000 
        bto_growth_rate <- 0.021
        min_downpayment_bto <- bto_price_start * (1 - rules$ltv)
        
        down_bto <- max(min_downpayment_bto, min(input$cash_injection, bto_price_start))
        loan_bto <- max(0, bto_price_start - down_bto)
        pmt_bto  <- monthly_payment(loan_bto, interest_rate, rules$loan_years)
        
        bal_bto <- if(loan_bto == 0) 0 else loan_bto * ((1+r_mo)^n_mo - (1+r_mo)^k_mo) / ((1+r_mo)^n_mo - 1)
        val_5y_bto <- bto_price_start * (1 + bto_growth_rate)^h
        
        # APPROACH A: Net Worth position is the raw property asset equity (Value - Debt)
        bto_equity <- val_5y_bto - bal_bto
        
        # D. RUN PATHWAY 3: RENTAL
        rent_total <- 0
        for (yr in 1:h) {
          rent_total <- rent_total + (input$base_rent * 12) * (1.035)^(yr - 1)
        }
        
        # Package everything back cleanly into a multi-row structure
        tibble(
          town            = rep(current_town, 3),
          flat_type       = rep(current_ft, 3),
          label           = paste0(current_town, " (", current_ft, ")"),
          path            = c("BTO Purchase", "Resale Purchase", "Renting"),
          net_worth_5y    = c(bto_equity, resale_equity, -rent_total), 
          monthly_housing = c(pmt_bto, pmt_resale, input$base_rent),
          initial_price   = c(bto_price_start, calculated_resale_price, NA_real_),
          final_val       = c(val_5y_bto, val_5y_resale, 0),
          growth_rate     = c(bto_growth_rate, growth_rate, 0)
        )
      }) %>%
      ungroup()
    
    simulation_output <- simulation_output %>%
      mutate(
        budget_violator = monthly_housing > input$monthly_budget,
        display_name    = if_else(budget_violator, paste(label, "⚠️ Unaffordable"), label)
      )
    
    return(simulation_output)
  })
  
  output$networth_plot <- renderPlot({
    df <- results_data()
    
    ggplot(df, aes(x = display_name, y = net_worth_5y, fill = path)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_manual(values = c("BTO Purchase" = "#2ecc71", "Resale Purchase" = "#3498db", "Renting" = "#e74c3c")) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
      labs(
        x        = "Housing Alternative Matrix Rows", 
        y        = "Projected 5-Year Net Worth Accumulation",
        title    = "Cross-Comparison Simulation Grid Output",
        fill     = "Strategic Strategy Path"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1, face = "bold"),
        legend.position = "top"
      )
  })
  
  output$summary_table <- renderTable({
    results_data() %>%
      mutate(
        `Configuration Label`  = display_name,
        `Pathway Option`       = path,
        `Monthly Payment`      = scales::dollar(monthly_housing),
        `Initial Asset Price`  = if_else(is.na(initial_price), "-", scales::dollar(initial_price)),
        `Growth Path Vector`   = if_else(path == "Renting", "-", scales::percent(growth_rate, accuracy = 0.01)),
        `Projected Net Worth`  = scales::dollar(net_worth_5y)
      ) %>%
      select(`Configuration Label`, `Pathway Option`, `Monthly Payment`, `Initial Asset Price`, `Growth Path Vector`, `Projected Net Worth`)
  }, align = "c")
  
  output$recommendation_text <- renderText({
    df <- results_data() %>% filter(budget_violator == FALSE)
    
    if (nrow(df) == 0) {
      return("CRITICAL WARNING STRATEGY GATEWAY: Every selected HDB ownership configuration choice crosses your active monthly payment comfort ceiling constraint slider. Please expand your threshold criteria or lower target flat sizes.")
    }
    
    winner <- df %>% arrange(desc(net_worth_5y)) %>% slice(1)
    
    paste0("Operational Feasibility Sweep Complete. Within your stated monthly cost budget envelope of ", 
           scales::dollar(input$monthly_budget), ", the strategy maximizing capital generation over 5 years is the ", 
           toupper(winner$path), " pathway tracking a ", winner$flat_type, " configuration inside ", winner$town, 
           ". This delivers a 5-year equity footprint outcome of ", scales::dollar(winner$net_worth_5y), 
           ". Configurations that break your comfortable cash flow bounds have been automatically flagged with warning markers.")
  })
}

shinyApp(ui = ui, server = server)