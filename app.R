# FishMIP Southern Projections Explorer
# Shiny app for visualizing fish biomass projections in Antarctic waters
# Author: Adapted from FAO report Shiny app structure

# Load required libraries ----------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycustomloader)
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

# Mizer outputs -----------------------------------------------------------
# Regional model outputs for Prydz Bay
prydz_bay_data <- read_csv_arrow(
  "data/mizer_biomass_yield_timeseries_summary_per_year_tonnes.csv")


# Ensemble outputs --------------------------------------------------------
# Spatial data
maps_data <- read_csv_arrow(file.path(
  "/rd/gem/public/fishmip/aceas_legacy", 
  "ensemble_perc_change_fish_bio_all-ssp_mid-end-century_all-reg.csv"))

# Load summary statistics table
summary_stats <- read_csv_arrow(file.path(
  "/rd/gem/public/fishmip/aceas_legacy", 
  "ensemble_perc_change_summ-stats_all-ssp_mid-end-century_all-reg.csv"))

# Time series data
ts_data <- read_csv_arrow(
  file.path("/rd/gem/public/fishmip/aceas_legacy/ensemble_outputs",
            "ensemble_perc_change_fish_bio_ts_all-ssp_all-reg_1950-2100.csv"))


# Global MEMs -------------------------------------------------------------
global_mem_dir <- "/rd/gem/public/fishmip/aceas_legacy/global_mems"

# Timeseries
ts_global_mem <- read_parquet(file.path(
  global_mem_dir, 
  "gfdl-mom6-cobalt2_obsclim_histsoc_all-regs_yearly_perc_bio_change.parquet"))

catch_all <- read_parquet(file.path("/rd/gem/public/fishmip/aceas_legacy",
                                    "observed_catches_all_sources.parquet"))

# Maps
maps_global_mem <- read_sf(file.path(
  global_mem_dir,
  "gfdl-mom6-cobalt2_obsclim_histsoc_all-regs_mean_change.shp"))

# Load CCAMLR spatial data
CCAMLR_Areas <- load_ASDs()
EEZs <- read_sf("data/map_layers/ccamlr_eez_macq_epsg6932.shp")
ant_ice <- read_sf("data/map_layers/antarctic_ice.shp")
sh <- read_sf("data/map_layers/south_hem_countries.shp")

# Get spatial region lists (now includes subregions)
fao_list <- maps_data |>
  distinct(fao) |>
  drop_na() |> 
  pull(fao)

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

eez_list <- maps_data |> 
  distinct(eez) |> 
  drop_na() |> 
  arrange(eez) |> 
  pull()

map_files <- list.files("/rd/gem/public/fishmip/aceas_legacy/projection_maps", 
                        full.names = T)

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
  
  ## Formatting page ---------------------------------------------------------
  title = NULL,
  theme = bs_theme(bootswatch = "lux", font_scale = 1.1),
  # Header formatting
  header = tags$head(
    tags$style(HTML(".header-panel 
                    {background-color: #2c3e50; padding: 20px; 
                    margin-bottom: 0px; display: flex; align-items: center; 
                    justify-content: space-between;
                    }.header-left {flex: 1;
                    }.logo-container {display: flex; align-items: center;
                    gap: 20px; margin-bottom: 15px;
                    }.title-main {color: white; font-size: 1.8em;
                    font-weight: bold; margin: 0; line-height: 1.2;
                    }.title-sub {color: white; font-size: 1.4em;
                    font-weight: normal; margin: 5px 0 0 0;
                    }.header-right {display: flex; align-items: center;
                    margin-left: 30px;
                    }.navbar {margin-bottom: 20px;
                    }"
    )),
    
    # Header with logos and titles
    div(class = "header-panel",
        div(class = "header-left",
            div(class = "logo-container",
                img(src = "FishMIP_white_no-bg_logo.png", height = 70)
            ),
            h1(class = "title-main", "Southern Ocean Projections Explorer"),
            h2(class = "title-sub", 
               "Results from the Southern Ocean Marine Ecosystem Model Ensemble 
               (SOMEME)")
        ),
        div(class = "header-right",
            img(src = "ACEAS_logo_white.png", height = 120, width = 120)
        )
    )
  ),
  
  ## Navbar Tab 1: Marine biomass projections --------------------------------
  tabPanel("Future projections",
           titlePanel("Projected marine biomass change"),
           br(),
           "Explore mean changes in marine biomass estimated by the FishMIP ",
           "ensemble (including 10 marine ecosystem models) relative to the ",
           "reference period (2005-2014) for Antarctic waters.",
           br(), br(),
           
           sidebarLayout(
             ### Navbar Tab 1: Side bar ---------------------------------------
             sidebarPanel(
               h4(strong("Select region and scenario:")),
               radioButtons("region_type",
                            "Choose region type (Maps and time series)",
                            choiceNames = 
                              c("CCAMLR Statistical Areas", 
                                "CCAMLR Subdivisions", 
                                "CCAMLR Marine Protected Areas", 
                                "Exclusive Economic Zones", 
                                "Southern Ocean (CCAMLR convention area)"),
                            choiceValues = c("fao", "subregion", "mpa", "eez",
                                             "so"),
                            selected = "fao"),
               
               selectInput("selected_region",
                           "Choose area of interest (Maps and time series)",
                           choices = NULL),
               
               radioButtons("map_scenario",
                            "Choose emissions scenario  (Maps only)",
                            choiceNames = c("SSP1-2.6 (low emissions)", 
                                            "SSP5-8.5 (high emissions)"),
                            choiceValues = c("ssp126", "ssp585"),
                            selected = "ssp126"),
               
               radioButtons("map_decade",
                            "Choose projection period  (Maps only)",
                            choiceNames = c("2041-2050 (medium term)", 
                                            "2091-2100 (long term)"),
                            choiceValues = c("2041-2050", "2091-2100"),
                            selected = "2041-2050"),
               
               br(),
               
               p("Click 'Download' to get the data used to create plots."),
               downloadButton("download_map", "Download")
             ),# Navbar tab 1 sidebar ends
             
             
             ### Navbar Tab 1: Main panel -------------------------------------
             mainPanel(
               tabsetPanel(
                 ##### Navbar Tab 1: Map panel --------------------------------
                 tabPanel("Map",
                          br(),
                          p("Mean change in marine biomass for the selected ",
                            "scenario and time frame."),
                          withLoader(
                            girafeOutput("plot_map", height = "600px")
                            ) # withLoader ends
                 ),
                 ##### Navbar Tab 1: Time series panel ------------------------
                 tabPanel("Time series",
                          br(),
                          p("Mean percentage change in marine biomass (1950-",
                            "2100) relative to the historical reference period",
                            " (2005-2014) derived from the FishMIP model ",
                            "ensemble under two emissions scenarios: SSP1-2.6 ",
                            "and SSP5-8.5. Shaded areas show the standard ",
                            "deviation across the 10 marine ecosystem models ",
                            "included in the ensemble."),
                          withLoader(
                            girafeOutput("plot_ts", height = "550px", 
                                         width = "700px")
                            ) # withLoader ends
                 )
               )
             )# Navbar tab 1 main panel ends
           )# Navbar tab 1 side layout ends
  ),# Navbar tab 1 tab panel ends
  
  
  ## Navbar Tab 2: Model Evaluation ------------------------------------------
  navbarMenu("Model Evaluation",
    ### Navbar Tab 2 Menu 1: Global MEMs -------------------------------------
             tabPanel("Global Models",
                      titlePanel("Global Model Evaluation"),
                      br(),
      #### Navbar Tab 2 Menu 1: Side bar -------------------------------------
                      sidebarLayout(
                        sidebarPanel(
                          h4(strong("Select model, region, and variable:")),
                          
                          selectInput("global_model",
                                      "Choose FishMIP global marine ecosystem
                                      model",
                                      choices = c("BOATS", "DBEM", "DBPM", 
                                                  "EcoOcean", "FEISTY"),
                                      #, "Ensemble"),
                                      selected = "BOATS"),
                          
                          radioButtons("global_region_type",
                                       "Choose region type (Maps and time 
                                       series)",
                                       choiceNames =
                                         c("CCAMLR Statistical Areas",
                                           "CCAMLR Subdivisions",
                                           "Exclusive Economic Zones",
                                           "Southern Ocean (CCAMLR convention 
                                           area)"),
                            choiceValues = c("fao", "subreg", "eez",
                                             "so"),
                            selected = "fao"),

                          selectInput("global_selected_region",
                                      "Choose area of interest (Maps and time 
                                      series)",
                                      choices = NULL),

                          selectInput("global_model_variable",
                                      "Choose variable (Maps and time series)",
                                      choices = c("Catches" = "tc", 
                                                  "Biomass" = "tcb"),
                                      selected = "Catches"),
                          
                          # Observations only available for catches
                          conditionalPanel(
                            condition = "input.global_model_variable == 'tc'",
                            selectizeInput("global_obs_source",
                                           "Observed catches dataset (Time 
                                           series only):",
                                           choices = NULL)),

                          selectizeInput("global_model_res",
                                         "Choose model horizontal resolution 
                                         (Maps and time series)",
                                         choices = c("0.25° (default)" = 
                                                       "025deg",
                                                     "1°" = "1deg"),
                                         selected = "0.25° (default)"),

                          br(),
                          p("Click 'Download' to get data used to create plots",
                            "in this tab."),
                          downloadButton("download_global", "Download")
                          ), # Navbar tab 2 sidebar panel ends
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Map",
                                     br(),
                                     p("Mean marine biomass over the ",
                                       "historical period (1961-2010) for the",
                                       " selected scenario and time frame."),
                                     withLoader(
                                       girafeOutput("plot_global_model_map", 
                                                    height = "600px")
                                       )
                                     ), # Navbar tab 2 tabPanel (maps) ends 
                            
                            tabPanel("Time series",
                                     br(),
                                     p("The black line in the time series ",
                                       "plot shown below represents the ",
                                       "area-weighted annual mean between ",
                                       "1961 and 2010 (historical period) ",
                                       "for the variable, model, resolution,",
                                       " and area of interest selected on ", 
                                       "the right panel."),
                                     br(),
                                     withLoader(
                                       girafeOutput("global_plot_ts", 
                                                    height = "550px", 
                                                    width = "750px")
                                     )
                                     ) # Navbar tab 2 menu 1 tabPanel 
                                       #(time series) ends
                            ) # Navbar tab 2 menu 1 tabset panel (maps) ends 
                        ) # Navbar tab 2 menu 1 main panel (maps) ends
                      ) # Navbar tab 2 menu 1 sidebar layout ends
      ),# Navbar tab 2 menu 1 'Global model' ends
    
    ### Navbar Tab 2 Menu 2: Regional MEMs -----------------------------------
    tabPanel("Regional Models",
             titlePanel("Regional Model Evaluation"),
             br(),
             #### Navbar Tab 2 Menu 2: Side bar ------------------------------
             sidebarLayout(
               sidebarPanel(
                 h4(strong("Select model, region, and variable:")),
                 
                 selectInput("regional_selected_region",
                             "Choose area of interest",
                             choices = "Prydz Bay"),
                 
                 selectInput("regional_model",
                             "Choose FishMIP regional marine ecosystem model",
                             choices = c("Mizer"),
                             selected = "Mizer"),
                 ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Time series",
                            br(),
                            # withLoader(
                            #   girafeOutput("regional_plot_ts", 
                            #                height = "550px", width = "750px")
                            # )
                   ),
                   tabPanel("About",
                            br(),
                            # withLoader(
                            #   textOutput("about_text"),
                            #   verbatimTextOutput("about_code")
                            # )
                   )
                 )
               )
               
             ) # Navbar tab 2 menu 2 main panel (maps) ends

             
    )# Navbar tab 2 menu 2 'Regional model' ends
  ),# Navbar tab 2 main menu ends
  

  ## Navbar Tab 3: About -----------------------------------------------------
  tabPanel("About",
           fluidRow(
             column(12,
                    h2("About This Tool"),
                    p("This interactive tool allows users to visualise changes",
                      " in projected fish biomass under different emissions",
                      " scenarios in the waters surrounding the Antarctic ",
                      "continent. Projections are based on the FishMIP ",
                      "(Fisheries and Marine Ecosystem Model Intercomparison ",
                      "Project) ensemble of 10 global marine ecosystem models."),
                    br(),
                    
                    h2("Who is FishMIP?"),
                    p("The Fisheries and Marine Ecosystem Model Intercomparison 
                      Project (FishMIP) is an network of more than 100 marine 
                      ecosystem modellers and researchers from around the world.
                       Our goal is to bring together our collective 
                      understanding to help better project the long-term impacts
                       of climate change on fisheries and marine ecosystems, and
                       to use our findings to help inform policy. You can find 
                      more information about FishMIP on our ",
                      tags$a(href="https://fishmip.org/", "website.")),
                    br(),
                    
                    h2("Data Sources"),
                    tags$ul(
                      tags$li("Global marine biomass projections: FishMIP 
                              ensemble (10 global marine ecosystem models)"),
                      tags$li("Climate scenarios: SSP1-2.6 (low emissions) and
                               SSP5-8.5 (high emissions)"),
                      tags$li("Reference period: 2005-2014 mean"),
                      tags$li("Boundaries of CCAMLR statistical areas, subareas,
                              marine protected areas come from the ",
                              tags$a(href="https://cran.r-project.org/web/packages/CCAMLRGIS/index.html",
                                     "CCAMLRGIS package for R")),
                      tags$li("Boundaries of Exclusive Economic Zones (EEZs)
                              were obtained from version 12 of the ",
                              tags$a(href="https://www.marineregions.org/downloads.php",
                                     "Maritime Boundaries Geodatabase"))),
                    br(),
                    
                    h2("How should I cite data from this site?"),
                    p("You can download the data used to create the plots ",
                      "shown in this interactive tool using the 'Download' ",
                      "button included under each tab. All data in this tool ",
                      "is available under an open licence (",
                      tags$a(href="https://creativecommons.org/licenses/by-sa/4.0/",
                             "CC-BY-SA-4.0"), 
                      "). Under this licence, you are allowed to use, share, ",
                      "and modify our data, but one of the conditions of use ",
                      "is attribution (i.e., provide appropriate credit to ",
                      "data creators). We recommend the following citations:"),
                    tags$ul(
                      tags$li(tags$span("FishMIP Southern Ocean Projections 
                                        Explorer - An interactive tool for 
                                        Antarctic fish biomass projections: ",
                                        style = "font-weight:bold;"), 
                              "Fierro-Arcos, D., Murphy, K., & 
                              Blanchard, J. L. (2026). Southern Ocean 
                              Projections Explorer: Results from the Southern 
                              Ocean Marine Ecosystem Model Ensemble (SOMEME) 
                              (Version 1.0.0) [Computer software]. 
                              https://doi.org/TBA"),
                      tags$li("FishMIP collaborators and contributing 
                              modeling groups")),
                    br(),
                    
                    h2("Contact & Feedback"),
                    p("For questions or feedback about this tool, please ",
                      "contact the ", 
                      tags$a(href="mailto:fishmip.coordinators@gmail.com",
                             "FishMIP team."), 
                      "Alternatively, we encourage users to report potential ",
                      "errors or omissions through our ",
                      tags$a(href="https://github.com/Fish-MIP/ACEAS_legacy_app/issues",
                             "GitHub issues page.")),
                    br(),
                    
                    h2("Acknowledgments"),
                    p("This tool was developed to support understanding of ",
                      "climate change impacts on Antarctic marine ecosystems.",
                      " We acknowledge the FishMIP community and all ",
                      "contributing modelling groups."),
                    p("This research was supported by the Australian Research ",
                      "Council Special Research Initiative, Australian Centre ",
                      "for Excellence in Antarctic Science. This work ", 
                      "contributes to the Australian Antarctic Program ", 
                      "Partnership funded under the Australian ", 
                      "Government’s Antarctic Science Collaboration Initiative",
                        "programme. This work contributes to delivering the ",
                      "Australian Antarctic Science Decadal Strategy."),
                    br(),
                    
                    h3("Summary Statistics"),
                    p("Download comprehensive summary statistics for all ",
                      "regions, scenarios, and time periods."),
                    
                    downloadButton("download_summary", 
                                   "Download Summary Statistics"),
                    br()
      ) # Navbar tab 3 column ends
    ) # Navbar tab 3 fluidRow ends
  ) # Navbar tab 3 tabPanel ends
)# navbarPage ends (start of app)

# Server Logic ---------------------------------------------------------------
server <- function(input, output, session) {
  
  ## Tab 1: Fish biomass projected maps --------------------------------------
  # Update region choices based on region type
  observe({
    if(input$region_type == "fao"){
      choices <- fao_list
    }else if(input$region_type == "subregion"){
      # Use spatially-matched subregions from the data
      choices <- subregion_list
    }else if(input$region_type == "mpa"){
      # Use MPA list
      choices <- mpa_list
    }else if(input$region_type == "eez"){
      # Get unique EEZ names (exclude CCAMLR regions)
      choices <- eez_list
    }else{
      choices <- "Southern Ocean"
    }
    updateSelectInput(session, "selected_region", choices = choices)
  })
  
  ### Tab 1: Reactive data for maps ------------------------------------------
  map_data <- reactive({
    req(input$selected_region, input$map_scenario, input$map_decade)
    
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
    
    # Create title for maps with projected change
    map_title <- paste0(input$selected_region, " - ", 
                        ifelse(input$map_scenario == "ssp126", "SSP1-2.6", 
                               "SSP5-8.5"), " - ", input$map_decade)
    
    return(list(proj_rast = proj_rast,
                proj_cv = proj_cv,
                title = map_title))
  })
  
  #### Tab 1: Rendering projection maps ---------------------------------------
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
      geom_sf(data = ant_ice, fill = "grey99", colour = "grey60")+
      geom_sf(data = CCAMLR_Areas, fill = NA, colour = "red", lwd = 0.5)+
      geom_sf(data = EEZs, fill = NA, colour = "#7570b3", lwd = 0.5)+
      geom_sf(data = sh, fill = "grey60", colour = "grey60")+
      lims(y = c(-4000000, 4000000), x = c(-4000000, 4100000))+
      labs(title = map_data()$title,
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
  
  #### Tab 1: Downloading data for maps ---------------------------------------
  output$download_map <- downloadHandler(
    filename = function() {
      # region_clean <- gsub("[, ]", "_", input$selected_region)
      region_clean <- str_replace(str_to_lower(input$selected_region), " ", "-")
      paste0("fishmip_ensemble_", region_clean, "_", input$map_scenario, "_",
             input$map_decade, ".csv")
    },
    content = function(file) {
      req(input$selected_region, input$map_scenario, input$map_decade)
      
      data_to_download <- maps_data |>
        filter(scenario == input$map_scenario,
               decade == input$map_decade)
      
      if(input$region_type != "so"){
        data_to_download <- data_to_download |> 
          filter(!!sym(input$region_type) == input$selected_region)
        grouping <- c("fao", "mpa", "subregion", "eez")
        grouping <- grouping[!c("fao", "mpa", "subregion", "eez") %in%
                               input$region_type]
        data_to_download <- data_to_download |>
          select(!grouping)
      }else{
        data_to_download <- data_to_download |> 
          drop_na(fao) |> 
          select(!fao:eez)
      }
      write_csv(data_to_download, file)
    }
  )
  
  ### Tab 1: Reactive data for time series plots -----------------------------
  # Reactive data for time series
  ts_plot_data <- reactive({
    req(input$selected_region)
    
    ts_data |>
      filter(!!sym(input$region_type) == input$selected_region)
  })
  
  #### Tab 1: Rendering time series plots -------------------------------------
  output$plot_ts <- renderGirafe({
    req(ts_plot_data())
    
    p <- ggplot(ts_plot_data(), aes(x = year, y = mean_change, 
                                    colour = scenario, group = scenario))+
      geom_point_interactive(aes(tooltip = tooltip), size = 0.1, 
                             hover_nearest = TRUE)+
      geom_line(linewidth = 0.5)+
      geom_hline(yintercept = 0, color = "grey80", linewidth = 0.65,
                 linetype = 2)+
      geom_vline(xintercept = 2015, color = "grey80", linewidth = 0.65)+
      geom_ribbon(aes(ymin = mean_change-sd_change,
                      ymax = mean_change+sd_change, fill = scenario),
                  alpha = 0.3, color = NA)+
      scale_color_manual(values = c("historical" = "black", 
                                    "ssp126" = "#33bbee", "ssp585" = "#ee3377"),
                         name = "Scenarios", 
                         labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
      scale_fill_manual(values = c("historical" = "black", "ssp126" = "#33bbee",
                                   "ssp585" = "#ee3377"),
                        name = "Scenarios",
                        labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
      scale_x_continuous(breaks = seq(1950, 2100, 10))+
      labs(title = paste("Fish biomass change:", input$selected_region),
           y = "Change in exploitable fish biomass (%)")+
      guides(color = guide_legend(nrow = 1, title.position = "left"))+
      theme_classic()+
      theme(legend.position = "top", legend.justification = "center",
            legend.text = element_text(size = 11), 
            legend.title = element_text(size = 11),
            plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 11),
            axis.text.x = element_text(angle = 45, vjust = 0.765, hjust = 0.65,
                                       size = 10), 
            axis.text.y = element_text(size = 10))
    
    girafe(ggobj = p, height_svg = 5) |>
      girafe_options(opts_selection(type = "none", only_shiny = TRUE))
  })
  
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
  
  
  ## Tab 2 Menu 1: Global MEM evaluation ---------------------------------
  # Update region choices based on region type
  observe({
    if(input$global_region_type == "fao"){
      choices <- fao_list
    }else if(input$global_region_type == "subreg"){
      # Use spatially-matched subregions from the data
      choices <- subregion_list
    }else if(input$global_region_type == "eez"){
      # Get unique EEZ names (exclude CCAMLR regions)
      choices <- eez_list
    }else{
      choices <- "Southern Ocean"
    }
    
    updateSelectInput(session, "global_selected_region", choices = choices)
  })
  
  observeEvent(input$global_model,{
    if(input$global_model == "DBEM"){
      choices <- c("1°" = "1deg")
    }else{
      choices <- c("0.25° (default)" = "025deg", "1°" = "1deg")
    }
    updateSelectizeInput(session, "global_model_res", choices = choices)
  })
  
  observeEvent(input$global_region_type,{
    catch_sources <- catch_all |>
      drop_na(!!sym(input$global_region_type)) |>
      distinct(source) |>
      pull()
    updateSelectizeInput(session, "global_obs_source", choices = catch_sources)
  })
  
  ### Tab 2 Menu 1: Reactive data for maps and time series -------------------
  global_model_data <- reactive({
    req(input$global_model, input$global_selected_region, 
        input$global_model_variable)
    
    # DBPM data available for both biomass and catches
    ts <- ts_global_mem |> 
      filter(mem_name == str_to_lower(input$global_model) &
               !!sym(input$global_region_type) == input$global_selected_region &
               resolution == input$global_model_res & 
               variable == input$global_model_variable)
    
    obs <- catch_all |> 
      filter(!!sym(input$global_region_type) == input$global_selected_region &
               resolution == input$global_model_res &
               source == input$global_obs_source)
    
    maps <- maps_global_mem |>
      filter(mem_name == str_to_lower(input$global_model) &
               !!sym(input$global_region_type) == input$global_selected_region &
               resolution == input$global_model_res &
               variable == input$global_model_variable)
    
    return(list(ts = ts,
                obs = obs,
                maps = maps))
  })
  
  #### Tab 2 Menu 1: Rendering maps of mean biomass/catch estimates ----------
  output$plot_global_model_map <- renderGirafe({
    maps_data <- global_model_data()$maps
    
    cb_lab <- str_c(unique(maps_data$long_var), "\n (", 
                    unique(maps_data$unit), ")")
    
    p <- ggplot(maps_data)+
      geom_sf(aes(color = mean_val))+
      scale_color_viridis_c(name = cb_lab)+
      geom_sf(data = ant_ice, fill = "grey99", colour = "grey60")+
      geom_sf(data = CCAMLR_Areas, fill = NA, colour = "red", lwd = 0.5)+
      geom_sf(data = EEZs, fill = NA, colour = "#7570b3", lwd = 0.5)+
      geom_sf(data = sh, fill = "grey60", colour = "grey60")+
      lims(y = c(-4000000, 4000000), x = c(-4000000, 4100000))+
      theme_bw()+
      theme(panel.border = element_rect(colour = NA),
            plot.title = element_blank(), legend.position = "bottom",
            legend.title = element_text(size = 10))+
      guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                    barwidth = 20, barheight = 1.5))
    
    girafe(ggobj = p, height_svg = 6) |>
      girafe_options(opts_selection(type = "none", only_shiny = TRUE),
                     opts_tooltip(opacity = 0.8))
  })
  
  #### Tab 2 Menu 1: Rendering time series plots of biomass/catch estimates ---
  output$global_plot_ts <- renderGirafe({
    ts_data <- global_model_data()$ts
    ylab <- str_c(unique(ts_data$long_var), " (", unique(ts_data$unit), ")")
  
    if(input$global_model_variable == "tc"){
      obs_data <- global_model_data()$obs

      # Create plot
      p <- ggplot()+
        geom_line(data = ts_data, aes(year, mean_val, color = "model"))+
        geom_line(data = obs_data, aes(year, catch_g_m2, color = "obs"),
                  linetype = "dashed")+
        scale_color_manual(values = c("model" = "#ee3377", "obs" = "#33bbee"),
                           labels = c(input$global_model, input$global_obs_source),
                           name = "Fishing catches source")+
        theme_bw()+
        labs(y = str_wrap(ylab, width = 50))+
        theme(legend.position = "bottom", legend.justification = "center",
              legend.text = element_text(size = 11),
              legend.title = element_text(size = 11, hjust = 0.5, face = "bold"),
              legend.title.position = "top", axis.title.x = element_blank(),
              axis.title.y = element_text(size = 11),
              axis.text.x = element_text(vjust = 0.75, hjust = 0.65, size = 10),
              axis.text.y = element_text(size = 10))
      }else{
        # Create plot
        p <- ggplot()+
          geom_line(data = ts_data, aes(year, mean_val), color = "#ee3377")+
          theme_bw()+
          labs(y = str_wrap(ylab, width = 50))+
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(size = 11),
                axis.text.x = element_text(vjust = 0.75, hjust = 0.65, size = 10),
                axis.text.y = element_text(size = 10))
      }
 
    girafe(ggobj = p, height_svg = 5) |>
      girafe_options(opts_selection(type = "none", only_shiny = TRUE),
                     opts_tooltip(opacity = 0.8))
  })
  
  ## Tab 2 Menu 2: Regional MEM evaluation -------------------------------
  
  
}


# Run the application --------------------------------------------------------
shinyApp(ui = ui, server = server)
