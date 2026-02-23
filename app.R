# FishMIP Southern Projections Explorer
# Shiny app for visualizing fish biomass projections in Antarctic waters
# Author: Adapted from FAO report Shiny app structure

# Load required libraries ----------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(arrow)
library(cmocean)
library(scales)
library(sf)
library(CCAMLRGIS)
library(ggiraph)
library(rnaturalearthdata)

options(sass.cache = FALSE)

# Loading input data ---------------------------------------------------------
ant_sectors <- read_csv("data/antarctic_sectors_keys.csv")


## DBPM outputs ------------------------------------------------------------
# Biomass
dbpm_biomass_sf <- read_csv(
  "data/dbpm_mean_tot-exp-bio_all_regions_simask_1deg_1961-2010.csv") |>
  left_join(ant_sectors, by = "region") |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# Catches
dbpm_catches_sf <- read_csv(
  "data/dbpm_mean_catch_all_regions_simask_1deg_1961-2010.csv") |>
  # Map FAO regions to CCAMLR names for consistency
  left_join(ant_sectors, by = "region") |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
# Simulated vs observed catches time series data
sim_obs_catches <- read_parquet(
  "data/simulated_observed_catches_all.parquet") |> 
  # Map FAO regions to CCAMLR names for consistency
  left_join(ant_sectors, by = "region")
 

# Mizer outputs -----------------------------------------------------------
# Regional model outputs for Prydz Bay
prydz_bay_data <- read_csv(
  "data/mizer_biomass_yield_timeseries_summary_per_year_tonnes.csv")


# Ensemble outputs --------------------------------------------------------
# Spatial data
maps_data <- read_csv(
  "data/ensemble_perc_change_fish_bio_all-ssp_mid-end-century_all-reg.csv")
# Load summary statistics table
summary_stats <- read_csv(
  "data/ensemble_perc_change_summ-stats_all-ssp_mid-end-century_all-reg.csv")

# Time series data
ts_data <- read_csv(
  "data/mean_ensemble_perc_change_fish_bio_timeseries_all-ssp_all-reg_1950-2100.csv")

# Load CCAMLR spatial data
CCAMLR_Areas <- load_ASDs()
EEZs <- load_EEZs()
ANT <- load_Coastline()

# Get spatial region lists (now includes subregions)
region_list <- maps_data |>
  distinct(region_name) |>
  pull(region_name)

# Get subregion list
subregion_list <- maps_data |>
  distinct(subregion) |>
  drop_na() |>
  pull()

# Get MPA list
mpa_list <- maps_data |>
  distinct(mpa) |>
  drop_na() |>
  arrange(mpa) |> 
  pull()


map_files <- list.files("data/projection_maps/", full.names = T)

# Supporting functions -------------------------------------------------------
# Custom color palette
scale_fill_custom <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                              option = "D", values = NULL, space = "Lab",
                              na.value = "transparent", guide = "colourbar",
                              aesthetics = "fill") {
  continuous_scale(aesthetics,
                   palette = gradient_n_pal(
                     c(cmocean("matter", start = 0.1, end = 0.8, 
                               direction = -1)(123),
                       cmocean("delta", start = 0.49, end = 0.5)(20),
                       cmocean("deep", start = 0.1, end = 0.8)(123)), 
                     values, space),
                   na.value = na.value, guide = guide, ...)
}

# UI Definition --------------------------------------------------------------
ui <- navbarPage(
  title = NULL,
  theme = bs_theme(bootswatch = "lux", font_scale = 1.1),
  
  header = tags$head(
    tags$style(HTML("
      .header-panel {
        background-color: #2c3e50;
        padding: 20px;
        margin-bottom: 0px;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .header-left {
        flex: 1;
      }
      .logo-container {
        display: flex;
        align-items: center;
        gap: 20px;
        margin-bottom: 15px;
      }
      .title-main {
        color: white;
        font-size: 1.8em;
        font-weight: bold;
        margin: 0;
        line-height: 1.2;
      }
      .title-sub {
        color: white;
        font-size: 1.4em;
        font-weight: normal;
        margin: 5px 0 0 0;
      }
      .header-right {
        display: flex;
        align-items: center;
        margin-left: 30px;
      }
      .navbar {
        margin-bottom: 20px;
      }
    ")),
    # Header with logos and titles
    div(
      class = "header-panel",
      div(
        class = "header-left",
        div(
          class = "logo-container",
          img(src = "FishMIP_white_no-bg_logo.png", height = 70)
        ),
        h1(class = "title-main", "Southern Ocean Projections Explorer"),
        h2(class = "title-sub", 
           "Results from the Southern Ocean Marine Ecosystem Model Ensemble (SOMEME)")
      ),
      div(
        class = "header-right",
        img(src = "ACEAS_logo_white.png", height = 120, width = 120)
      )
    )
  ),
  
  # Navbar Tab 1: Future Projections
  tabPanel(
    "Fish Biomass Projections",
    titlePanel("Projected fish biomass change"),
    br(),
    "Explore mean changes in fish biomass estimated by FishMIP ensemble",
    " (including 10 marine ecosystem models) relative to the reference period ",
    "(2005-2014) for Antarctic waters.",
    br(), br(),
    
    sidebarLayout(
      sidebarPanel(
        h4(strong("Select region and scenario:")),
        radioButtons(
          "region_type",
          "Choose region type",
          choiceNames = c("CCAMLR Statistical Areas", "CCAMLR Subdivisions", 
                          "CCAMLR Marine Protected Areas", 
                          "Exclusive Economic Zones"),
          choiceValues = c("CCAMLR", "CCAMLR_sub", "CCAMLR_mpa", "EEZ"),
          selected = "CCAMLR"
        ),
        
        selectInput(
          "selected_region",
          "Choose your area of interest",
          choices = NULL
        ),
        
        radioButtons(
          "map_scenario",
          "Choose emissions scenario",
          choiceNames = c("SSP1-2.6 (low emissions)", 
                          "SSP5-8.5 (high emissions)"),
          choiceValues = c("ssp126", "ssp585"),
          selected = "ssp126"
        ),
        
        radioButtons(
          "map_decade",
          "Choose projection period",
          choiceNames = c("2041-2050 (medium term)", "2091-2100 (long term)"),
          choiceValues = c("2041-2050", "2091-2100"),
          selected = "2041-2050"
        ),
        
        br(),
        p("Click 'Download' to get the data used to create plots."),
        downloadButton("download_map", "Download")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Map",
            br(),
            p("Mean change in fish biomass for the selected scenario and time ",
              "period."),
            girafeOutput("plot_map", height = "600px")
          ),
          tabPanel("Time series",
            br(),
            p("Mean percentage change in fish biomass (1950-2100) relative to ",
              "the historical reference period (2005-2014) derived from the ",
              "FishMIP model ensemble under two emissions scenarios: SSP1-2.6 ", 
              "and SSP5-8.5. Shaded areas show the standard deviation across ",
              "the 10 marine ecosystem models included in the ensemble."),
            br(),
            girafeOutput("plot_ts", height = "500px")
          )
        )
      )
    )
  ),
  
  # Navbar Tab 3: Model Evaluation
  navbarMenu(
    "Model Evaluation",
    
    tabPanel(
      "Global Models",
      titlePanel("Global Model Evaluation"),
      br(),
      
      sidebarLayout(
        sidebarPanel(
          h4(strong("Select model, region, and variable:")),
          
          selectInput(
            "global_model",
            "Choose FishMIP marine ecosystem model",
            choices = c("Ensemble", "DBPM", "BOATS", "DBEM", "EcoOcean",
                        "FEISTY"),
            selected = "Ensemble"
          ),
          
          selectInput(
            "global_model_region",
            "Choose CCAMLR Statistical Area",
            choices = ant_sectors$region_name,
            selected = "Atlantic"
          ),
          
          selectInput(
            "global_model_variable",
            "Choose variable",
            choices = c("Catches", "Biomass"),
            selected = "Biomass"
          ),
          
          br(),
          p("Select a global marine ecosystem model, CCAMLR Statistical area,",
            " and variable to view evaluation metrics.")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Map",
              br(),
              conditionalPanel(
                condition = 
                  "input.global_model == 'DBPM' && (input.global_model_variable == 'Biomass' || input.global_model_variable == 'Catches')",
                p(textOutput("global_model_description")),
                girafeOutput("plot_global_model_map", height = "600px")
              ),
              conditionalPanel(
                condition = 
                  "input.global_model != 'DBPM' || (input.global_model_variable != 'Biomass' && input.global_model_variable != 'Catches')",
                p("Data for this model/variable combination is not yet available."),
                p(em("Under development."))
              )
            ),
            tabPanel("Time series",
              br(),
              conditionalPanel(
                condition = "input.global_model == 'DBPM' && input.global_model_variable == 'Catches'",
                p("Time series of simulated vs observed catches for DBPM model",
                  " (1961-2010)."),
                selectInput(
                  "obs_source",
                  "Observation source:",
                  choices = c("Watson" = "obs_watson", "CCAMLR" = "obs_ccamlr",
                              "Pauly" = "obs_pauly"),
                  selected = "obs_watson"
                ),
                selectInput(
                  "model_config",
                  "Model configuration:",
                  choices = c("Original (1°)" = "1_org", "0.25°" = "025_org", 
                              "Calibrated" = "calib_org", "SI run" = "1_si"),
                  selected = "1_org"
                ),
                girafeOutput("plot_sim_obs_ts", height = "500px")
              ),
              conditionalPanel(
                condition = 
                  "input.global_model != 'DBPM' || input.global_model_variable != 'Catches'",
                p("Time series evaluation is currently only available for DBPM", 
                  " catches."),
                p(em("Additional model/variable combinations are under ", 
                     "development."))
              )
            ),
            tabPanel("Performance Metrics",
              br(),
              conditionalPanel(
                condition = 
                  "input.global_model == 'DBPM' && input.global_model_variable == 'Catches'",
                p("Statistical performance metrics comparing DBPM simulated ",
                  "catches against observations."),
                selectInput(
                  "perf_obs_source",
                  "Observation source:",
                  choices = c("Watson" = "obs_watson", "CCAMLR" = "obs_ccamlr", 
                              "Pauly" = "obs_pauly"),
                  selected = "obs_watson"
                ),
                selectInput(
                  "perf_model_config",
                  "Model configuration:",
                  choices = c("Original (1°)" = "1_org", "0.25°" = "025_org", 
                              "Calibrated" = "calib_org", "SI run" = "1_si"),
                  selected = "1_org"
                ),
                br(),
                fluidRow(
                  column(6, 
                    h4("Overall Performance"),
                    tableOutput("performance_table")
                  ),
                  column(6,
                    h4("Scatter Plot"),
                    girafeOutput("performance_scatter", height = "400px")
                  )
                ),
                br(),
                fluidRow(
                  column(12,
                    h4("Residual Analysis"),
                    girafeOutput("residual_plot", height = "350px")
                  )
                )
              ),
              conditionalPanel(
                condition = 
                  "input.global_model != 'DBPM' || input.global_model_variable != 'Catches'",
                p("Performance metrics are currently only available for DBPM ",
                  "catches."),
                p(em("Additional model/variable combinations are under ",
                     "development."))
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      "Regional Models",
      titlePanel("Regional Model Evaluation"),
      br(),
      
      sidebarLayout(
        sidebarPanel(
          h4(strong("Select regional model, region, and variables:")),
          
          selectInput(
            "regional_model",
            "Choose regional model",
            choices = c("Prydz Bay mizer"),
            selected = "Prydz Bay mizer"
          ),
          
          selectInput(
            "regional_region",
            "Choose region",
            choices = c("Prydz Bay"),
            selected = "Prydz Bay"
          ),
          
          selectInput(
            "regional_variable",
            "Choose variable",
            choices = c("Yield (Catches)", "Biomass"),
            selected = "Biomass"
          ),
          
          selectInput(
            "regional_species",
            "Choose functional group/species",
            choices = NULL,
            multiple = TRUE,
            selected = NULL
          ),
          
          br(),
          p("Select up to five (5) functional groups to compare in the time ",
            "series plot."),
          br(),
          downloadButton("download_regional_ts", "Download Data")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Time series",
              br(),
              p("Time series showing model outputs from 1950-2010 for the ",
                "selected functional groups."),
              p("The solid line shows the median value, with shaded ribbons ",
                "showing the 25th-75th percentile range."),
              br(),
              girafeOutput("plot_regional_ts", height = "600px")
            ),
            tabPanel("Summary Statistics",
              br(),
              p("Summary statistics for the selected functional groups and ",
                "variable."),
              br(),
              tableOutput("regional_summary_table")
            )
          )
        )
      )
    )
  ),
  
  # Navbar Tab 4: About
  tabPanel(
    "About",
    titlePanel("About This Tool"),
    
    fluidRow(
      column(
        12,
        h3("Summary Statistics"),
        p("Download comprehensive summary statistics for all regions, ",
          "scenarios, and time periods."),
        downloadButton("download_summary", "Download Summary Statistics"),
        br(), br(),
        
        h3("About this tool"),
        p("This interactive tool visualizes fish biomass change projections ",
          "under differentclimate scenarios for Antarctic waters. Projections ",
          "are based on the FishMIP (Fisheries and Marine Ecosystem Model ",
          "Intercomparison Project) ensemble of 10 marine ecosystem models."),
        br(),
        
        h4("Data Sources"),
        tags$ul(
          tags$li("Fish biomass projections: FishMIP ensemble (10 models)"),
          tags$li("Spatial boundaries: CCAMLRGIS package (CCAMLR Stastitical areas and EEZs)"),
          tags$li("Climate scenarios: SSP1-2.6 (low emissions) and SSP5-8.5 (high emissions)"),
          tags$li("Reference period: 2005-2014 mean")
        ),
        br(),
        
        h4("How to cite"),
        p("When using data or visualizations from this tool, please cite:"),
        tags$ul(
          tags$li("FishMIP Southern Projections Explorer: Interactive tool for Antarctic fish biomass projections"),
          tags$li("FishMIP collaboration and contributing modeling groups"),
          tags$li("CCAMLRGIS package for spatial data")
        ),
        br(),
        
        h4("Contact & Feedback"),
        p("For questions or feedback about this tool, please contact the ",
          "FishMIP team."),
        br(),
        
        h4("Acknowledgments"),
        p("This tool was developed to support understanding of climate change ",
          "impacts on Antarctic marine ecosystems. We acknowledge the FishMIP ",
          "community and all contributing modelling groups.")
      )
    )
  )
)

# Server Logic ---------------------------------------------------------------
server <- function(input, output, session) {
  
  ## Fish biomass projections ------------------------------------------------
  # Update region choices based on region type
  observe({
    if(input$region_type == "CCAMLR"){
      choices <- ant_sectors$region_name
    }else if(input$region_type == "CCAMLR_sub"){
      # Use spatially-matched subregions from the data
      choices <- subregion_list
    }else if(input$region_type == "CCAMLR_mpa"){
      # Use MPA list
      choices <- mpa_list
    }else{
      # Get unique EEZ names (exclude CCAMLR regions)
      choices <- region_list[!region_list %in% ant_sectors$region_name]
    }
    updateSelectInput(session, "selected_region", choices = choices)
  })

  # Reactive data for maps
  map_data <- reactive({
    req(input$selected_region, input$map_scenario, input$map_decade)

    # Filter data
    # filtered <- maps_data |>
    #   filter(
    #     scenario == input$map_scenario,
    #     decade == input$map_decade
    #   )

    # Further filter by region type
    filtered <- map_files |>
      str_subset(paste0(
        str_replace_all(str_to_lower(input$selected_region), " ", "-"), 
        "_", input$map_decade, "_", input$map_scenario))
    
    # Find file with mean values and load data 
    proj_rast <- filtered |>
      str_subset("mean-cvmask.*shp") |>
      read_sf()

    # Find file with CV mask and load data
    proj_cv <- filtered |>
      str_subset("cv_en.*shp") |>
      read_sf()

    return(list(proj_rast = proj_rast,
                proj_cv = proj_cv))
  })

  # Render map
  output$plot_map <- renderGirafe({
    req(map_data())

    # Get data ready for plotting maps 
    mean_cv_shp <- map_data()$proj_rast
    cv_ras <- mean_cv_shp |> 
      drop_na(cv_mask)
    cv_mask <- map_data()$proj_cv

    p <- ggplot()+
      geom_sf_interactive(data = mean_cv_shp, 
                          aes(fill = mean, tooltip = tooltip), color = NA)+
      geom_sf(data = cv_ras, aes(fill = cv_mask), color = NA, alpha = 0.6,
              show.legend = F)+
      scale_fill_binned(limits = c(-50, 50), n.breaks = 8,
                        type = scale_fill_custom, oob = oob_squish,
                        name = "% change in fish biomass")+
      geom_sf(data = cv_mask, color = "grey40", shape = 4, size = 0.2, 
              show.legend = F)+
      geom_sf(data = ANT, fill = "grey99", colour = "grey60")+
      geom_sf(data = CCAMLR_Areas, fill = NA, colour = "red", lwd = 0.5)+
      geom_sf(data = EEZs, fill = NA, colour = "orange", lwd = 0.5)+
      geom_sf(data = countries50, fill = "grey60", colour = "grey60")+
      coord_sf(crs = "+proj=ortho +lat_0=-90 +lon_0=0",
               ylim = c(-4000000, 4000000), xlim = c(-4000000, 4100000))+
      labs(title = paste0(
        input$selected_region, " - ",
        ifelse(input$map_scenario == "ssp126", "SSP1-2.6", "SSP5-8.5"), " - ",
        input$map_decade),
        caption = "Stippling indicates high intermodel spread (CV > 1)")+
      theme_bw()+
      theme(panel.border = element_rect(colour = NA),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.caption = element_text(size = 10, hjust = 0.5),
            legend.position = "bottom", axis.title = element_blank())+
      guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, 
                                   barwidth = 20, barheight = 2))

    girafe(code = print(p), width_svg = 8, height_svg = 8) |>
      girafe_options(
        opts_zoom(max = 5),
        opts_toolbar(hidden = c("zoom_rect")),
        opts_hover(css = "stroke: gray1; stroke-width: 2px"),
        opts_tooltip(opacity = 0.8),
        opts_selection(type = "none", only_shiny = TRUE)
      )
  })
  # 
  # # Download map data
  # output$download_map <- downloadHandler(
  #   filename = function() {
  #     region_clean <- gsub("[, ]", "_", input$selected_region)
  #     paste0(
  #       "fishmip_", region_clean, "_",
  #       input$map_scenario, "_",
  #       input$map_decade, ".csv"
  #     )
  #   },
  #   content = function(file) {
  #     req(input$selected_region, input$map_scenario, input$map_decade)
  #     
  #     data_to_download <- maps_data |>
  #       filter(
  #         scenario == input$map_scenario,
  #         decade == input$map_decade
  #       )
  #     
  #     if (input$region_type == "CCAMLR") {
  #       data_to_download <- data_to_download |> 
  #         filter(region == input$selected_region)
  #     } else {
  #       data_to_download <- data_to_download |> 
  #         filter(region_name == input$selected_region)
  #     }
  #     
  #     data_to_download <- data_to_download |>
  #       select(-tooltip, -rowid)
  #     
  #     write_csv(data_to_download, file)
  #   }
  # )
  # 
  # Reactive data for time series
  ts_plot_data <- reactive({
    req(input$selected_region)

    # For CCAMLR regions, use the selected region directly
    # For other types, try to match with available time series data
    if (input$region_type == "CCAMLR") {
      ts_data |>
        filter(region_name == input$selected_region)
    } else {
      # For other region types, show time series for the parent CCAMLR region if available
      # Otherwise return empty data
      ts_data |>
        filter(region_name == input$selected_region)
    }
  })

  # Render time series plot
  output$plot_ts <- renderGirafe({
    req(ts_plot_data())

    p <- ggplot(
      ts_plot_data(),
      aes(x = year, y = mean_change, colour = scenario, group = scenario)
    ) +
      geom_point_interactive(
        aes(tooltip = tooltip),
        size = 0.1,
        hover_nearest = TRUE
      ) +
      geom_line(linewidth = 0.5) +
      geom_hline(
        yintercept = 0,
        color = "grey80",
        linewidth = 0.65,
        linetype = 2
      ) +
      geom_vline(
        xintercept = 2015,
        color = "grey80",
        linewidth = 0.65
      ) +
      geom_ribbon(
        aes(
          ymin = mean_change - sd_change,
          ymax = mean_change + sd_change,
          fill = scenario
        ),
        alpha = 0.3,
        color = NA
      ) +
      scale_color_manual(
        values = c("historical" = "black", "ssp126" = "#33bbee", "ssp585" = "#ee3377"),
        name = "Scenarios",
        labels = c("Historical", "SSP1-2.6", "SSP5-8.5")
      ) +
      scale_fill_manual(
        values = c("historical" = "black", "ssp126" = "#33bbee", "ssp585" = "#ee3377"),
        name = "Scenarios",
        labels = c("Historical", "SSP1-2.6", "SSP5-8.5")
      ) +
      scale_x_continuous(breaks = seq(1950, 2100, 10)) +
      labs(
        title = paste("Fish biomass change:", input$selected_region),
        y = "Change in exploitable fish biomass (%)"
      ) +
      guides(color = guide_legend(nrow = 1, title.position = "left")) +
      theme_classic() +
      theme(
        legend.position = "top",
        legend.justification = "center",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11),
        axis.text.x = element_text(angle = 45, vjust = 0.765, hjust = 0.65, size = 10),
        axis.text.y = element_text(size = 10)
      )

    girafe(ggobj = p, height_svg = 5) |>
      girafe_options(
        opts_selection(type = "none", only_shiny = TRUE)
      )
  })
  # 
  # # Download time series data
  # output$download_ts <- downloadHandler(
  #   filename = function() {
  #     region_clean <- gsub("[, ]", "_", input$selected_region)
  #     paste0("fishmip_timeseries_", region_clean, "_1950-2100.csv")
  #   },
  #   content = function(file) {
  #     data_to_download <- ts_plot_data() |>
  #       select(-tooltip)
  #     write_csv(data_to_download, file)
  #   }
  # )
  # 
  # # Download summary statistics
  # output$download_summary <- downloadHandler(
  #   filename = function() {
  #     paste0("fishmip_summary_statistics_", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     write_csv(summary_stats, file)
  #   }
  # )
  # 
  # # Render description text for global model
  # output$global_model_description <- renderText({
  #   var_label <- ifelse(input$global_model_variable == "Biomass", "biomass", "catches")
  #   paste("Mean", var_label, "from DBPM model (1961-2010 time-averaged).")
  # })
  # 
  # # Reactive data for global model evaluation maps
  # global_model_data <- reactive({
  #   req(input$global_model, input$global_model_region, input$global_model_variable)
  #   
  #   # DBPM data available for both biomass and catches
  #   if (input$global_model == "DBPM") {
  #     if (input$global_model_variable == "Biomass" && !is.null(dbpm_biomass_sf)) {
  #       # Filter biomass data by selected region
  #       filtered <- dbpm_biomass_sf |>
  #         filter(region == input$global_model_region)
  #       return(filtered)
  #     } else if (input$global_model_variable == "Catches" && !is.null(dbpm_catches_sf)) {
  #       # Filter catches data by selected region
  #       filtered <- dbpm_catches_sf |>
  #         filter(region == input$global_model_region)
  #       return(filtered)
  #     }
  #   }
  #   
  #   return(NULL)
  # })
  # 
  # # Reactive data for sim vs obs catches time series
  # sim_obs_ts_data <- reactive({
  #   req(input$global_model_region, input$obs_source, input$model_config)
  #   
  #   if (is.null(sim_obs_catches)) return(NULL)
  #   
  #   # Parse model config (res_run format)
  #   config_parts <- strsplit(input$model_config, "_")[[1]]
  #   res_val <- config_parts[1]
  #   run_val <- config_parts[2]
  #   
  #   # Filter data
  #   filtered <- sim_obs_catches |>
  #     filter(
  #       region_display == input$global_model_region,
  #       res == res_val,
  #       run == run_val,
  #       source == input$obs_source
  #     )
  #   
  #   return(filtered)
  # })
  # 
  # # Render simulated vs observed catches time series
  # output$plot_sim_obs_ts <- renderGirafe({
  #   ts_data <- sim_obs_ts_data()
  #   
  #   if (is.null(ts_data) || nrow(ts_data) == 0) {
  #     return(NULL)
  #   }
  #   
  #   # Prepare data for plotting
  #   plot_data <- ts_data |>
  #     select(year, vals, obs, pseudo) |>
  #     pivot_longer(cols = c(vals, obs, pseudo), names_to = "type", values_to = "catch") |>
  #     mutate(
  #       type_label = case_when(
  #         type == "vals" ~ "Simulated (DBPM)",
  #         type == "obs" ~ "Observed",
  #         type == "pseudo" ~ "Pseudo-observation"
  #       )
  #     )
  #   
  #   # Create plot
  #   p <- ggplot(plot_data, aes(x = year, y = catch, color = type_label, group = type_label)) +
  #     geom_line(linewidth = 0.8) +
  #     geom_point(size = 1.5, alpha = 0.6) +
  #     scale_color_manual(
  #       values = c(
  #         "Simulated (DBPM)" = "#2E86AB",
  #         "Observed" = "#A23B72",
  #         "Pseudo-observation" = "#F18F01"
  #       ),
  #       name = "Data type"
  #     ) +
  #     scale_x_continuous(breaks = seq(1960, 2010, 10)) +
  #     labs(
  #       title = paste0("DBPM vs Observed Catches - ", input$global_model_region),
  #       subtitle = paste0("Model: ", gsub("_", " - ", input$model_config), " | Obs: ", 
  #                        gsub("obs_", "", input$obs_source)),
  #       x = "Year",
  #       y = "Mean catch density (tonnes/km²)"
  #     ) +
  #     theme_classic() +
  #     theme(
  #       legend.position = "top",
  #       plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 11),
  #       axis.title = element_text(size = 11),
  #       axis.text = element_text(size = 10),
  #       legend.text = element_text(size = 10)
  #     )
  #   
  #   girafe(ggobj = p, height_svg = 5) |>
  #     girafe_options(
  #       opts_selection(type = "none", only_shiny = TRUE),
  #       opts_tooltip(opacity = 0.8)
  #     )
  # })
  # 
  # # Reactive data for performance metrics
  # perf_data <- reactive({
  #   req(input$global_model_region, input$perf_obs_source, input$perf_model_config)
  #   
  #   if (is.null(sim_obs_catches)) return(NULL)
  #   
  #   # Parse model config
  #   config_parts <- strsplit(input$perf_model_config, "_")[[1]]
  #   res_val <- config_parts[1]
  #   run_val <- config_parts[2]
  #   
  #   # Filter data and remove NAs
  #   filtered <- sim_obs_catches |>
  #     filter(
  #       region_display == input$global_model_region,
  #       res == res_val,
  #       run == run_val,
  #       source == input$perf_obs_source,
  #       !is.na(vals),
  #       !is.na(obs)
  #     )
  #   
  #   return(filtered)
  # })
  # 
  # # Calculate performance metrics
  # performance_metrics <- reactive({
  #   data <- perf_data()
  #   
  #   if (is.null(data) || nrow(data) == 0) {
  #     return(NULL)
  #   }
  #   
  #   # Calculate metrics
  #   n <- nrow(data)
  #   sim <- data$vals
  #   obs <- data$obs
  #   
  #   # Correlation
  #   cor_val <- cor(sim, obs, use = "complete.obs")
  #   
  #   # R-squared
  #   ss_res <- sum((obs - sim)^2)
  #   ss_tot <- sum((obs - mean(obs))^2)
  #   r_squared <- 1 - (ss_res / ss_tot)
  #   
  #   # RMSE
  #   rmse <- sqrt(mean((sim - obs)^2))
  #   
  #   # MAE
  #   mae <- mean(abs(sim - obs))
  #   
  #   # Bias
  #   bias <- mean(sim - obs)
  #   
  #   # Normalized RMSE
  #   nrmse <- rmse / mean(obs) * 100
  #   
  #   # Create results dataframe
  #   metrics <- data.frame(
  #     Metric = c("Number of observations", "Correlation (r)", "R²", 
  #                "RMSE", "MAE", "Bias", "Normalized RMSE (%)"),
  #     Value = c(
  #       as.character(n),
  #       round(cor_val, 3),
  #       round(r_squared, 3),
  #       round(rmse, 4),
  #       round(mae, 4),
  #       round(bias, 4),
  #       round(nrmse, 2)
  #     ),
  #     stringsAsFactors = FALSE
  #   )
  #   
  #   return(metrics)
  # })
  # 
  # # Render performance table
  # output$performance_table <- renderTable({
  #   metrics <- performance_metrics()
  #   
  #   if (is.null(metrics)) {
  #     return(data.frame(Metric = "No data available", Value = ""))
  #   }
  #   
  #   metrics
  # }, striped = TRUE, hover = TRUE, bordered = TRUE)
  # 
  # # Render performance scatter plot
  # output$performance_scatter <- renderGirafe({
  #   data <- perf_data()
  #   
  #   if (is.null(data) || nrow(data) == 0) {
  #     return(NULL)
  #   }
  #   
  #   # Get metrics for subtitle
  #   metrics <- performance_metrics()
  #   r2_val <- metrics$Value[metrics$Metric == "R²"]
  #   rmse_val <- metrics$Value[metrics$Metric == "RMSE"]
  #   
  #   # Create scatter plot
  #   p <- ggplot(data, aes(x = obs, y = vals)) +
  #     geom_point_interactive(aes(tooltip = paste0("Year: ", year, "\nObs: ", round(obs, 3), "\nSim: ", round(vals, 3))),
  #                           color = "#2E86AB", size = 2.5, alpha = 0.6) +
  #     geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  #     geom_smooth(method = "lm", se = TRUE, color = "#A23B72", fill = "#A23B72", alpha = 0.2) +
  #     labs(
  #       title = "Simulated vs Observed Catches",
  #       subtitle = paste0("R² = ", r2_val, ", RMSE = ", rmse_val),
  #       x = "Observed catches (tonnes/km²)",
  #       y = "Simulated catches (tonnes/km²)"
  #     ) +
  #     theme_classic() +
  #     theme(
  #       plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 10),
  #       axis.title = element_text(size = 10),
  #       axis.text = element_text(size = 9)
  #     )
  #   
  #   girafe(ggobj = p, height_svg = 4) |>
  #     girafe_options(
  #       opts_selection(type = "none", only_shiny = TRUE),
  #       opts_tooltip(opacity = 0.8)
  #     )
  # })
  # 
  # # Render residual plot
  # output$residual_plot <- renderGirafe({
  #   data <- perf_data()
  #   
  #   if (is.null(data) || nrow(data) == 0) {
  #     return(NULL)
  #   }
  #   
  #   # Calculate residuals
  #   plot_data <- data |>
  #     mutate(
  #       residual = vals - obs,
  #       fitted = vals
  #     )
  #   
  #   p <- ggplot(plot_data, aes(x = fitted, y = residual)) +
  #     geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  #     geom_point_interactive(aes(tooltip = paste0("Year: ", year, "\nFitted: ", round(fitted, 3), "\nResidual: ", round(residual, 3))),
  #                           color = "#2E86AB", size = 2.5, alpha = 0.6) +
  #     geom_smooth(method = "loess", se = TRUE, color = "#A23B72", fill = "#A23B72", alpha = 0.2) +
  #     labs(
  #       title = "Residual Analysis",
  #       subtitle = "Residuals should be randomly distributed around zero",
  #       x = "Fitted values (tonnes/km²)",
  #       y = "Residuals (tonnes/km²)"
  #     ) +
  #     theme_classic() +
  #     theme(
  #       plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 10),
  #       axis.title = element_text(size = 10),
  #       axis.text = element_text(size = 9)
  #     )
  #   
  #   girafe(ggobj = p, height_svg = 3.5) |>
  #     girafe_options(
  #       opts_selection(type = "none", only_shiny = TRUE),
  #       opts_tooltip(opacity = 0.8)
  #     )
  # })
  # 
  # # Update regional species choices based on selected variable
  # observe({
  #   if (!is.null(prydz_bay_data) && !is.null(input$regional_variable)) {
  #     # Get variable type
  #     var_type <- ifelse(input$regional_variable == "Yield (Catches)", "Yield", "Biomass")
  #     
  #     # Filter species list by variable
  #     available_species <- prydz_bay_data |>
  #       filter(variable == var_type) |>
  #       distinct(Species) |>
  #       arrange(Species) |>
  #       pull(Species)
  #     
  #     # Update species dropdown (limit selection to 5)
  #     updateSelectInput(session, "regional_species", 
  #                      choices = available_species,
  #                      selected = head(available_species, 3))  # Default to first 3
  #   }
  # })
  # 
  # # Reactive data for regional model time series
  # regional_ts_data <- reactive({
  #   req(input$regional_variable, input$regional_species)
  #   
  #   if (is.null(prydz_bay_data)) return(NULL)
  #   
  #   # Get variable type
  #   var_type <- ifelse(input$regional_variable == "Yield (Catches)", "Yield", "Biomass")
  #   
  #   # Filter data (only show 1950 onwards)
  #   filtered <- prydz_bay_data |>
  #     filter(
  #       variable == var_type,
  #       Species %in% input$regional_species,
  #       Year >= 1950
  #     )
  #   
  #   return(filtered)
  # })
  # 
  # # Render regional model time series plot
  # output$plot_regional_ts <- renderGirafe({
  #   data <- regional_ts_data()
  #   
  #   if (is.null(data) || nrow(data) == 0) {
  #     return(NULL)
  #   }
  #   
  #   # Limit to 5 species for readability
  #   if (length(unique(data$Species)) > 5) {
  #     data <- data |>
  #       filter(Species %in% head(unique(data$Species), 5))
  #   }
  #   
  #   # Get variable label
  #   var_label <- ifelse(input$regional_variable == "Yield (Catches)", "Yield", "Biomass")
  #   
  #   # Create faceted plot (one panel per species)
  #   p <- ggplot(data, aes(x = Year, y = median_t)) +
  #     geom_ribbon(
  #       aes(ymin = q25_t, ymax = q75_t),
  #       alpha = 0.3,
  #       fill = "#2E86AB"
  #     ) +
  #     geom_line(color = "#2E86AB", linewidth = 0.8) +
  #     facet_wrap(~ Species, scales = "free_y", ncol = 2) +
  #     scale_x_continuous(breaks = seq(1950, 2010, 10)) +
  #     scale_y_continuous(labels = scales::comma) +
  #     labs(
  #       title = paste0("Prydz Bay mizer - ", var_label, " Time Series (1950-2010)"),
  #       subtitle = "Solid line = median, shaded area = 25th-75th percentile",
  #       x = "Year",
  #       y = paste0(var_label, " (tonnes)")
  #     ) +
  #     theme_classic() +
  #     theme(
  #       plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 10),
  #       axis.title = element_text(size = 10),
  #       axis.text = element_text(size = 8),
  #       axis.text.x = element_text(angle = 45, hjust = 1),
  #       strip.background = element_rect(fill = "#e8f4f8", color = "grey70"),
  #       strip.text = element_text(size = 10, face = "bold")
  #     )
  #   
  #   # If yield/catches, add note about fishing starting ~1930
  #   if (var_label == "Yield") {
  #     p <- p + geom_vline(xintercept = 1930, linetype = "dashed", 
  #                        color = "grey50", alpha = 0.5)
  #   }
  #   
  #   girafe(ggobj = p, height_svg = 6) |>
  #     girafe_options(
  #       opts_selection(type = "none", only_shiny = TRUE),
  #       opts_tooltip(opacity = 0.8)
  #     )
  # })
  # 
  # # Render regional summary statistics table
  # output$regional_summary_table <- renderTable({
  #   data <- regional_ts_data()
  #   
  #   if (is.null(data) || nrow(data) == 0) {
  #     return(data.frame(Message = "No data available"))
  #   }
  #   
  #   # Calculate summary statistics across all years for each species
  #   summary <- data |>
  #     group_by(Species) |>
  #     summarise(
  #       `Mean (tonnes)` = round(mean(median_t, na.rm = TRUE), 2),
  #       `Median (tonnes)` = round(median(median_t, na.rm = TRUE), 2),
  #       `Min (tonnes)` = round(min(min_t, na.rm = TRUE), 2),
  #       `Max (tonnes)` = round(max(max_t, na.rm = TRUE), 2),
  #       `Q25 (tonnes)` = round(mean(q25_t, na.rm = TRUE), 2),
  #       `Q75 (tonnes)` = round(mean(q75_t, na.rm = TRUE), 2),
  #       `Years with data` = sum(!is.na(median_t)),
  #       .groups = "drop"
  #     )
  #   
  #   return(summary)
  # }, striped = TRUE, hover = TRUE, bordered = TRUE)
  # 
  # # Download regional time series data
  # output$download_regional_ts <- downloadHandler(
  #   filename = function() {
  #     var_label <- ifelse(input$regional_variable == "Yield (Catches)", "yield", "biomass")
  #     paste0("prydz_bay_mizer_", var_label, "_", Sys.Date(), ".csv")
  #   },
  #   content = function(file) {
  #     data <- regional_ts_data()
  #     
  #     if (!is.null(data)) {
  #       write_csv(data, file)
  #     }
  #   }
  # )
  # 
  # # Render global model evaluation map
  # output$plot_global_model_map <- renderGirafe({
  #   data_sf <- global_model_data()
  #   
  #   if (is.null(data_sf) || nrow(data_sf) == 0) {
  #     return(NULL)
  #   }
  #   
  #   # Determine variable name and column
  #   var_name <- input$global_model_variable
  #   var_col <- ifelse(var_name == "Biomass", "biomass", "catches")
  #   var_label <- ifelse(var_name == "Biomass", "Biomass", "Catches")
  #   var_unit <- "tonnes/km²"
  #   
  #   # Extract data and rasterize
  #   plot_data <- data_sf |>
  #     st_drop_geometry() |>
  #     select(lon, lat, all_of(var_col), tooltip)
  #   
  #   # For catches, set zeros to NA and cap values above 100
  #   if (var_name == "Catches") {
  #     plot_data[[var_col]] <- ifelse(plot_data[[var_col]] == 0, NA, plot_data[[var_col]])
  #     plot_data[[var_col]] <- ifelse(plot_data[[var_col]] > 100, 100, plot_data[[var_col]])
  #   }
  #   
  #   # Create raster template
  #   tmplt <- rast(ext(-180, 180, -78, -45), resolution = 1, 
  #                crs = "+init=epsg:4326")
  #   
  #   # Rasterize the variable
  #   data_rast <- rasterize(
  #     plot_data[, c("lon", "lat")], 
  #     tmplt, 
  #     values = plot_data[[var_col]], 
  #     fun = mean
  #   )
  #   
  #   # Convert to stars then sf
  #   data_stars <- st_as_stars(data_rast)
  #   data_rast_sf <- st_as_sf(data_stars, merge = FALSE)
  #   
  #   # Rename column  to 'value'
  #   names(data_rast_sf)[1] <- "value"
  #   
  #   # Add tooltip
  #   data_rast_sf$tooltip <- paste0(
  #     var_label, ": ", round(data_rast_sf$value, 2), " ", var_unit
  #   )
  #   
  #   # Create plot with different scales for catches vs biomass
  #   if (var_name == "Catches") {
  #     # For catches, use log-spaced bins with more detail at lower values
  #     # Define breaks on log scale with finer resolution at low end
  #     catch_breaks <- c(0.01, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100)
  #     
  #     p <- ggplot() +
  #       geom_sf_interactive(
  #         data = data_rast_sf,
  #         aes(fill = value, tooltip = tooltip),
  #         color = NA
  #       ) +
  #       scale_fill_viridis_b(
  #         option = "viridis",
  #         name = paste0(var_label, "\n(", var_unit, ")"),
  #         na.value = "transparent",
  #         breaks = catch_breaks,
  #         limits = c(0.01, 100),
  #         trans = "log10",
  #         oob = scales::squish,
  #         labels = scales::comma
  #       )
  #   } else {
  #     # For biomass, use continuous log scale
  #     p <- ggplot() +
  #       geom_sf_interactive(
  #         data = data_rast_sf,
  #         aes(fill = value, tooltip = tooltip),
  #         color = NA
  #       ) +
  #       scale_fill_viridis_c(
  #         option = "viridis",
  #         name = paste0(var_label, "\n(", var_unit, ")"),
  #         na.value = "transparent",
  #         trans = "log10",
  #         limits = c(0.1, 1000),
  #         oob = scales::squish,
  #         labels = scales::comma
  #       )
  #   }
  #   
  #   p <- p +
  #     geom_sf(data = CCAMLR_Areas, fill = NA, colour = "grey60", lwd = 0.8) +
  #     geom_sf(data = EEZs, fill = NA, colour = "orange", lwd = 0.6) +
  #     geom_sf(data = ANT, fill = "grey99", colour = "grey60") +
  #     geom_sf(data = countries50, fill = "grey60", colour = "grey60") +
  #     coord_sf(
  #       crs = "+proj=ortho +lat_0=-90 +lon_0=0",
  #       ylim = c(-4000000, 4000000),
  #       xlim = c(-4000000, 4100000)
  #     ) +
  #     labs(
  #       title = paste0("DBPM ", var_label, " - ", input$global_model_region, " (1961-2010 mean)")
  #     ) +
  #     theme_bw() +
  #     theme(
  #       panel.border = element_rect(colour = NA),
  #       plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  #       legend.position = "bottom",
  #       axis.title = element_blank()
  #     ) +
  #     guides(
  #       fill = guide_colorbar(
  #         title.position = "top",
  #         title.hjust = 0.5,
  #         barwidth = 20,
  #         barheight = 2
  #       )
  #     )
  #   
  #   girafe(code = print(p), width_svg = 8, height_svg = 8) |>
  #     girafe_options(
  #       opts_zoom(max = 5),
  #       opts_toolbar(hidden = c("zoom_rect")),
  #       opts_hover(css = "stroke: gray1; stroke-width: 2px"),
  #       opts_tooltip(opacity = 0.8),
  #       opts_selection(type = "none", only_shiny = TRUE)
  #     )
  # })
}

# Run the application --------------------------------------------------------
shinyApp(ui = ui, server = server)
