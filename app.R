## Required R Packages----------------------------------------------------------

library(GpGp)
library(shiny)
library(tidyverse)
library(sp)
library(sf)
library(leaflet)
library(plotly)
library(leafpop)
library(shinycssloaders)
library(shinybusy)
library(bslib)
library(DT)
library(kableExtra)
library(gstat)
library(viridis)

ui <- navbarPage(
  theme = bs_theme(preset = "minty"), # CHOOSE THEME 
  "AQ-Shiny: A Platform for Air Quality Modeling and Prediction", #APP NAME
  id = "nav",
  
  tabPanel("Instructions", # BEGIN INSTRUCTION TAB
           id = "inst_tab",
           
           helpText( # BEGIN helptext ----------------------------------------------------------------------------------------------------------
                     
             p("Welcome to AQ-Shiny, a web-based application intended for the exploration, comparison, and prediction of PM2.5 using spatial and spatio-temporal models with built-in uncertainty quantification.", style = "font-size: 22px; color: black;"),
             
             p(icon("magnifying-glass"), strong("Key Features:"), style = "font-size: 20px; color: black;"), #BEGIN FEATURES SECTION ------------
             tags$ul(
               tags$li("End-to-end workflow: data upload → modeling → prediction → visualization", style = "font-size: 18px; color: black;"),
               tags$li("Flexible Gaussian process modeling with multiple covariance functions", style = "font-size: 18px; color: black;"),
               tags$li("Comparison of models (IDW vs GP) using RMSE, MAE, and Correlation", style = "font-size: 18px; color: black;"),
               tags$li("Spatiotemporal modeling (separable, product–sum)", style = "font-size: 18px; color: black;"),
               #tags$li("Uncertainty-aware prediction maps", style = "font-size: 18px; color: black;"),
               #tags$li("Exportable results and reproducible reports", style = "font-size: 18px; color: black;")
               tags$li("Exportable results", style = "font-size: 18px; color: black;")
               ), #END FEATURES SECTION ----------------------------------------------------------------------------------------------------------
             
             br(),
             
             p(icon("rocket"), strong("Workflow:"), style = "font-size: 20px; color: black;"), #BEGIN WORKFLOW SECTION ---------------------------
             tags$ul(
               tags$li("Upload Data: Load your dataset (CSV format with location, time, and PM2.5 values)", style = "font-size: 18px; color: black;"),
               tags$li("Visualize Data: Explore spatial and temporal patterns interactively", style = "font-size: 18px; color: black;"),
               tags$li("Select Model: Choose between IDW or Gaussian Process models and compare covariance structures", style = "font-size: 18px; color: black;"),
               tags$li("Generate Predictions: Create spatial or spatiotemporal prediction maps with uncertainty", style = "font-size: 18px; color: black;"),
               tags$li("Compare Models: Evaluate performance using RMSE, MAE, Correlation", style = "font-size: 18px; color: black;")
             ), #END WORKFLOW SECTION ------------------------------------------------------------------------------------------------------------
             
             br(),
             
             p(icon("lightbulb"), strong("What makes AQ-Shiny Unique?"), style = "font-size: 20px; color: black;"), #BEGIN UNIQUE SECTION ---------
             tags$ul(
               tags$li("Compare covariance functions interactively", style = "font-size: 18px; color: black;"),
               tags$li("Integrates spatial and spatio-temporal modeling", style = "font-size: 18px; color: black;"),
               tags$li("Designed for statisticians and public health users", style = "font-size: 18px; color: black;")
             ), #END UNIQUE SECTION ----------------------------------------------------------------------------------------------------------------
             
             br(),
             
             p(icon("anchor-lock"), strong("Notes on Data Privacy"), style = "font-size: 20px; color: black;"), #BEGIN DATA PRIVACY SECTION --------
             tags$ul(
               tags$li("Any uploaded data will only be stored during usage of AQShiny. Once you exit from the application data and and models ran will not be saved anywhere.", style = "font-size: 18px; color: black;"),
               tags$li("This also means that if you have to reload the webpage during usage, all progress will be lost.", style = "font-size: 18px; color: black;")
               
             ), #END DATA PRIVACY SECTION ----------------------------------------------------------------------------------------------------------
             
             br(),
             
             p(icon("forward"), "Click on", strong("Data Upload"), "to begin", style = "font-size: 20px; color: black;") #WHERE TO NEXT LINE -------

           ) #END helpText -------------------------------------------------------------------------------------------------------------------------
        
      ), #END INSTRUCTION TAB
  
  ##############################################################################
  
  tabPanel("Data Upload", # BEGIN DATA UPLOAD TAB
           id = "upload",
           
           sidebarLayout(
             
             #BEGIN Side Panel --------------------------------------------------------------------------------------------------------------------
             sidebarPanel(
               fileInput("upload", tags$span("Step 1: Upload Your Data Set (accepted: .csv)", style = "font-size:20px;"), accept = c(".csv")), #UPLOAD DATA
               
               helpText( # BEGIN helptext ----------------------------------------------------------------------------------------------------------
                         
                 p("Note: File must contain columns with names:", style = "font-size: 18px; color: black"), # BEGIN EXPLANATION OF DATASET COLUMNS --  
                        tags$ul(
                          tags$li("ID: ID for location", style = "font-size: 16px; color: black"),
                          tags$li("Longitude", style = "font-size: 16px; color: black"),
                          tags$li("Latitude", style = "font-size: 16px; color: black"),
                          tags$li("logPM2.5", style = "font-size: 16px; color: black"),
                          tags$li("t1: time (if doing a Spatio-Temporal Analysis)", style = "font-size: 16px; color: black")
                        ), # END EXPLANATION OF DATASET COLUMNS ------------------------------------------------------------------------------------  
                        
                        br()
                        
                     ), #END helptext --------------------------------------------------------------------------------------------------------------
               
               conditionalPanel( #BEGIN CONDITIONAL PANEL FOR CHECK IF ST DATASET ------------------------------------------------------------------
                 condition = "output.col_exists == true",
                 
                 helpText(
                   p("You have a", strong("Spatial Temporal"), "Dataset. If you would like to make this a Spatial Analysis, you will be able to select a specific time in the Spatial Modeling Tab", style = "font-size: 20px; color: black"),
                   br(),
                   p("Step 2: Verify your dataset uploaded correctly before moving on. You have the option to view the raw dataset or a table of summary statistics. View Options:", style = "font-size: 20px; color: black"),
                   selectInput("summ_view_st", NULL, c("Raw Data", "Summary Statistics")),
                   br(),
                   p("Step 3: Move to", strong("Visualization Tab"), "to create an interactive visualization of the raw data. Move to the ", strong("Modeling Tab"), "if you would like to begin modeling your data.", style = "font-size: 20px; color: black"),
                   
                 )
               ), #END CONDITIONAL PANEL FOR CHECK IF ST DATASET ------------------------------------------------------------------------------------
               
               conditionalPanel( #BEGIN CONDITIONAL PANEL FOR CHECK IF SPATIAL DATASET --------------------------------------------------------------
                 condition = "output.col_exists == false",
                 
                 helpText(
                   p("You have a", strong("Spatial"), "Dataset. Only feautres for spatial data will be made available.", style = "font-size: 20px; color: black")),
                   br(),
                   p("Step 2: Verify your dataset uploaded correctly before moving on. You have the option to view the raw dataset or a table of summary statistics. View Options:", style = "font-size: 20px; color: black"),
                   selectInput("summ_view_sp", NULL, c("Raw Data", "Summary Statistics")),
                   br(),
                   p("Step 3: Move to", strong("Visualization Tab"), "to create an interactive visualization of the raw data. Move to the ", strong("Modeling Tab"), "if you would like to begin modeling your data.", style = "font-size: 20px; color: black"),
                 

               ) #END CONDITIONAL PANEL FOR CHECK IF SPATIAL DATASET ---------------------------------------------------------------------------------
               
             ), #END Side Panel ----------------------------------------------------------------------------------------------------------------------
             
             
             # BEGIN Main Panel ----------------------------------------------------------------------------------------------------------------------
             mainPanel(
               
               conditionalPanel( #BEGIN CONDITIONAL TABLE TO DISPLAY RAW SPATIAL DATASET -------------------------------------------------------------
                 condition = "input.summ_view_sp == 'Raw Data' & output.col_exists == false",
                 dataTableOutput("data_raw_spatial") 
               ), #END CONDITIONAL TABLE TO DISPLAY RAW SPATIAL DATASET ------------------------------------------------------------------------------
               
               
               conditionalPanel( #BEGIN CONDITIONAL TABLE TO DISPLAY SUMMARY OF SPATIAL DATASET ------------------------------------------------------
                 condition = "input.summ_view_sp == 'Summary Statistics' & output.col_exists == false",
                 div(style = "display: flex; justify-content: center;", tableOutput("data_sum_spatial")) #CENTER TABLE OUTPUT
               ), #END CONDITIONAL TABLE TO DISPLAY SUMMARY OF SPATIAL DATASET -----------------------------------------------------------------------
               
               
               conditionalPanel( #BEGIN CONDITIONAL TABLE TO DISPLAY RAW ST DATASET ------------------------------------------------------------------
                 condition = "input.summ_view_st == 'Raw Data' & output.col_exists == true",
                 dataTableOutput("data_raw_st")
               ), #END CONDITIONAL TABLE TO DISPLAY RAW ST DATASET -----------------------------------------------------------------------------------
               
               
               conditionalPanel( #BEGIN CONDITIONAL TABLE TO DISPLAY SUMMARY OF ST DATASET -----------------------------------------------------------
                 condition = "input.summ_view_st == 'Summary Statistics' & output.col_exists == true",
                 div(style = "display: flex; justify-content: center;", dataTableOutput("data_sum_st"))
               ) #END CONDITIONAL TABLE TO DISPLAY SUMMARY OF ST DATASET ----------------------------------------------------------------------------
               
             ) #END MAIN PANEL ----------------------------------------------------------------------------------------------------------------------
           ) #End SIDEBAR LAYOUT --------------------------------------------------------------------------------------------------------------------
        ), #END DATA CLEANING TAB 
  
  
  ##############################################################################
  
  tabPanel("Visualization",
           sidebarLayout(

             #BEGIN Side Panel --------------------------------------------------------------------------------------------------------------------
             sidebarPanel(
                  uiOutput("message"), #MESSAGE IF HAVE NOT UPLOADED A FILE YET (ERROR HANDLING)

               conditionalPanel(condition = "output.col_exists == false", #BEGIN CONDITIONAL PANEL FOR SPATIAL VISUALIZATION ----------------------

                  helpText(p(strong("Step 1: "), "This tab creates an interactive map of the raw data. You will have the ability to download your interactive map. Press the button below to create your map:", style = "font-size: 20px; color: black;"),
                          br(),
                          ), #END HELPTEXT INSTRUCTIONS FOR SPATIAL VISUALIZATION
                 
                  div(style = "text-align: center;", actionButton("spatial_map_start", "Create Interactive Map")), #CENTER BUTTON TO CREATE SPATIAL MAP 
                 
                  br(),
                 
                      conditionalPanel(condition = "input.spatial_map_start > 0", # BEGIN SUB-COND FOR AFTER PRESS BUTTON TO CREATE SPATIAL MAP ---
                                    helpText(p("Map Details:", style = "font-size: 20px; color: black"),  #START HELP TEXT FOR PLOT DESCRIPTION
                            
                                      tags$ul(
                                              tags$li("Each point is related to a location of a sensor", style = "font-size: 16px; color: black"),
                                              tags$li("Points colors are related to their logPM2.5 values", style = "font-size: 16px; color: black"),
                                              tags$li("You can hover over a point to obtain the specific logPM2.5 value for that location.", style = "font-size: 16px; color: black"),
                                              tags$li("You can zoom in and zoom out of the map using the controls in the upper right hand corner.", style = "font-size: 16px; color: black")
                                            ),
                                       br(),
                            
                                       p(strong("Step 2: "), "To see an outline of the code used to create this map, use the toggle below. Note: This is not the exact code used within the application to create the plot. This gives a general outline of the code used if you would like to reproduce the plot within a local R session.", style = "font-size: 20px; color: black")
                                    ), #END HELP TEXT FOR PLOT DESCRIPTION
                   
                                   input_switch("show_sp_map_code", "Show Code?") #CREATE SHOW CODE TOGGLE FOR SPATIAL VIZ
                   
                    ) # END SUB-COND FOR AFTER PRESS BUTTON TO CREATE SPATIAL MAP -----------------------------------------------------------------

               ), #END CONDITIONAL PANEL FOR SPATIAL VISUALIZATION --------------------------------------------------------------------------------


               conditionalPanel(condition = "output.col_exists == true", #BEGIN CONDITIONAL PANEL FOR ST VISUALIZATION -----------------------------
                 
                  helpText(p(strong("Step 1: "), "This tab creates an interactive map of the raw data. You will have the ability to download your interactive map. For Spatio-Temporal Data, two types of interactive maps exist, with descriptions below. Once you choose your type of interactive map, it will begin to render.", style = "font-size: 18px; color: black"),
                           p(strong("Plot Descriptions: "), style = "font-size: 16px; color: black"),
                          tags$ul(
                            tags$li("Space Over Time: Animation of sensor locations where the color of the point changes over time based on new measurements.", style = "font-size: 16px; color: black;"),
                            tags$li("Time Over Space: Plot of sensor locations where hovering over a sensor location provides a time series of the change in LogPM2.5 at that location. This may take awhile to render.", style = "font-size: 16px; color: black;")
                          ), #END LIST OF PLOT DESCRIPTIONS
                           br(),
                          ), #END HELP TEXT FOR PLOT DESCRIPTIONS

                  selectInput("plot", "Select Plot Type", choices = c("","Space over Time", "Time in Space"), selected = ""), # SELECT ST PLOT TYPE
                 
                  conditionalPanel(condition = "input.plot == 'Space over Time'", #BEGIN SUB COND PANEL FOR SPACE IN TIME VIZ -----------------------
                                   
                      helpText(br(),
                              p(strong("Step 2: "), "To see an outline of the code used to create this map, use the toggle below. Note: This is not the exact code used within the application to create the plot. This gives a general outline of the code used if you would like to reproduce the plot within a local R session.", style = "font-size: 18px; color: black"),
                      ), #END HELPTEXT ABOUT TOGGLE
                    
                     input_switch("show_st_map_code1", "Show Code?") #CREATE SHOW CODE TOGGLE FOR SPACE OVER TIME VIZ
                   
                    ), #END SUB COND PANEL FOR SPACE IN TIME VIZ -------------------------------------------------------------------------------------
                 
                 conditionalPanel(condition = "input.plot == 'Time in Space'", #BEGIN SUB COND PANEL FOR TIME IN SPACE VIZ ---------------------------
                                  
                     helpText(br(),
                             p(strong("Step 2: "), "To see an outline of the code used to create this map, use the toggle below. Note: This is not the exact code used within the application to create the plot. This gives a general outline of the code used if you would like to reproduce the plot within a local R session.", style = "font-size: 18px; color: black"),
                     ), #END HELPTEXT ABOUT TOGGLE
                   
                     input_switch("show_st_map_code2", "Show Code?") #CREATE SHOW CODE TOGGLE FOR TIME IN SPACE VIZ
                   
                    ) #END SUB COND PANEL FOR TIME IN SPACE VIZ -------------------------------------------------------------------------------------------

               ), # END CONDITIONAL PANEL FOR ST VISUALIZATION --------------------------------------------------------------------------------------------

             ), #END SIDE PANEL ---------------------------------------------------------------------------------------------------------------------------

             
             # BEGIN Main Panel ----------------------------------------------------------------------------------------------------------------------------
             mainPanel(

               conditionalPanel(condition = "input.spatial_map_start > 0 & output.col_exists == false", #BEGIN CONDITIONAL PANEL TO DISPLAY SPATIAL VIZ ----

                  conditionalPanel("input.show_sp_map_code == false", #BEGIN SUB COND PANEL WHEN SHOW CODE TOGGLE IS FALSE ---------------------------------
                                  
                                  plotlyOutput("spatial_map", height = "800px"), #PLOTLY MAP OUTPUT

                                  div( style = "text-align: center;", downloadButton('download_html_spatial','Download Interactive Plot')) #DOWNLOAD SPATIAL MAP BUTTON
                  ), #END SUB COND PANEL WHEN SHOW CODE TOGGLE IS FALSE --------------------------------------------------------------------------------------
                 
                  conditionalPanel(condition = "input.show_sp_map_code == true", #BEGIN SUB COND PANEL WHEN SHOW CODE TOGGLE IS TRUE -------------------------
                   
                                  verbatimTextOutput("code_block_spatial_map") #CODE DISPLAY
                   
                  ) #END SUB COND PANEL WHEN SHOW CODE TOGGLE IS TRUE -----------------------------------------------------------------------------------------
               ), #END CONDITIONAL PANEL TO DISPLAY SPATIAL VIZ -----------------------------------------------------------------------------------------------

               conditionalPanel(condition = "input.plot == 'Space over Time' & output.col_exists == true", #BEGIN CONDITIONAL PANEL FOR SPACE IN TIME VIZ -----
                  
                  conditionalPanel(condition = "input.show_st_map_code1 == false", #BEGIN SUB COND PANEL WHEN SHOW CODE TOGGLE IS FALSE -----------------------
                    
                                   plotlyOutput("map", height = "800px"), #PLOTLY MAP OUTPUT (KNOWN ISSUE: CONVERSION OF TIMES)
                                   
                                   div(style = "text-align: center;", downloadButton('download_html','Download Interactive Plot')), #DOWNLOAD SPACE IN TIME MAP BUTTON
                   ), #END SUB COND PANEL WHEN SHOW CODE TOGGLE IS FALSE --------------------------------------------------------------------------------------
                 
                  conditionalPanel(condition = "input.show_st_map_code1 == true", #BEGIN SUB COND PANEL WHEN SHOW CODE TOGGLE IS TRUE -------------------------
                   
                                  verbatimTextOutput("code_block_st_map1") #CODE DISPLAY
                                  
                  ) #END SUB COND PANEL WHEN SHOW CODE TOGGLE IS TRUE -----------------------------------------------------------------------------------------
               ), #END CONDITIONAL PANEL TO DISPLAY SPACE IN TIME VIZ -----------------------------------------------------------------------------------------

              conditionalPanel(condition = "input.plot == 'Time in Space' & output.col_exists == true", #BEGIN CONDITIONAL PANEL FOR TIME IN SPACE VIZ --------
                 
                  conditionalPanel("input.show_st_map_code2 == false", #BEGIN SUB COND PANEL WHEN SHOW CODE TOGGLE IS FALSE -----------------------------------
                                   
                        tags$style(type = "text/css", "#map {height: calc(85vh) !important;}"), #BEGIN: MAKE POPOUTS STAY IN WINDOW
                        tags$head(
                         tags$style(HTML("
                               /* Control the overall popup width */
                               .leaflet-popup-content {
                               width: 400px !important;
                               }
              
                               /* Control the SVG size within the popup */
                               .leaflet-popup-content svg {
                               width: 100% !important;
                               height: auto !important; /* Maintain aspect ratio */
                               }  "))
                        ), #END: MAKE POPOUTS STAY IN WINDOW
                        
                         add_busy_spinner(spin = "cube-grid"), #BUSY SPINNER
                        
                         leafletOutput("map2", height = "800px"), #LEAFTLET MAP OUTPUT
                         p(""),
          
                         downloadButton('download_html2','Download Interactive Plot') #DOWNLOAD MAP BUTTON
               ), #END SUB COND PANEL WHEN SHOW CODE TOGGLE IS FALSE --------------------------------------------------------------------------------------

               conditionalPanel( condition = "input.show_st_map_code2 == true", #BEGIN SUB COND PANEL WHEN SHOW CODE TOGGLE IS TRUE -----------------------
                                 
                 verbatimTextOutput("code_block_st_map2")
               ) #END SUB COND PANEL WHEN SHOW CODE TOGGLE IS TRUE ----------------------------------------------------------------------------------------

             ) #END CONDITIONAL PANEL TO DISPLAY TIME IN SPACE VIZ ----------------------------------------------------------------------------------------

          ) # END MAIN PANEL ------------------------------------------------------------------------------------------------------------------------------
        ) #End SIDEBAR LAYOUT -----------------------------------------------------------------------------------------------------------------------------
      ), #END VISUALIZATION TAB


  ##############################################################################

  navbarMenu("Modeling",

        #IDW SUB TAB -------------------------------------------------------------------------------------------------------------------------------------

        tabPanel("Inverse Distance Weighting (IDW)",
            sidebarLayout(

              #BEGIN Side Panel --------------------------------------------------------------------------------------------------------------------------
              sidebarPanel(
                uiOutput("messageIDW"), #MESSAGE IF HAVE NOT UPLOADED A FILE YET (ERROR HANDLING)

                conditionalPanel("output.col_exists == true", #BEGIN CONDITIONAL PANEL TO TURN ST DATASET INTO A SPATIAL ONLY ANALYSIS --------------------

                                 helpText(
                                   "You have a", strong("Spatial Temporal"), "Dataset. Currently Spatio-Temporal IDW is unsupported. If you would like to make this a Spatial Analysis, select a time below",
                                   br(),
                                   br()), # HELP TEXT FOR INSTRUCTIONS

                                 uiOutput("dynamic_dropdown2") #DYANMIC DROP DOWN: OPTIONS ALL UNIQUE TIME POINTS IN ST DATA
                ), #END CONDITIONAL PANEL TO TURN ST DATASET INTO A SPATIAL ONLY ANALYSIS -----------------------------------------------------------------

                conditionalPanel("output.col_exists == false || (input.time_options2 != '' && output.col_exists == true)", #BEGIN COND PANEL: SP IDW ------

                    helpText(p(strong("About:"), "Inverse Distance Weighting predits at new locations using a spatial weighted average. The mathematical formulation is in the right-hand panel.", style = "font-size: 18px; color: black;"),
                             p(strong("Grid:"), "Currently the code creates a grid for you to predict, but you can upload your own locations in the Prediction Tab. Grid creation uses a convex hull, meaning it creates the smallest possible convex shape that complete encloses the provided locations. The default grid cell size is 5 km, but you can check the box for more options to update this.", style = "font-size: 18px; color: black;"),
                             br()
                             ), #HELP TEXT FOR BREIF OVERVIEW OF IDW AND GRID CREATION
                    
                    checkboxInput("model_option_idw_s", "Check this Box for More Model Options", FALSE), #CHECK BOX FOR MORE IDW OPTIONS
                    
                    br(),
                
                      conditionalPanel(condition = "input.model_option_idw_s == true", #BEGIN SUB COND PANEL WHEN CHECK BOX FOR MORE OPTIONS --------------
                           
                        helpText(p("1). Update the decay rate, p in the box below. This determines how heavily distance influences the weight, w, of surrounding points.", style = "font-size:18px; color: black;"),
                                 tags$ul(
                                   tags$li("0 < p < 1: Unsual. Gives further away points more influence.", style = "font-size: 16px; color: black;"),
                                   tags$li("p = 1: Weights decrease linearly with distance.", style = "font-size: 16px; color: black;"),
                                   tags$li("p > 1: As p increases, higher weights are assigned to immediate neighbors.", style = "font-size: 16px; color: black;")
                                 ) 
                              ),
                        
                          numericInput("idw_power", NULL, value = 2, min = 0),
                          
                          helpText(p("2). Update the grid size by selecting an option below. A smaller number creates a finer grid, whereas a larger number creates a coarser grid. Note: Finer grid sizes will take longer to render.", style = "font-size: 18px; color: black;"),
                            ), #HELP TEXT EXPLAINING GRID SIZE
                         
                          selectInput("grid_size", NULL, c("1 km" = 1000, "5 km" = 5000, "10 km" = 10000), selected = 5000), #CHOOSE GRID SIZE
                           
                        ), #END SUB COND PANEL WHEN CHECK BOX FOR MORE OPTIONS ----------------------------------------------------------------------------
                
                      actionButton("run_idw", "Run Inverse Distance Weighting"), #BUTTON TO RUN IDW 
                    
                    conditionalPanel(condition = "input.run_idw > 0", # BEGIN SUB-COND FOR AFTER PRESS BUTTON TO CREATE SPATIAL MAP ---
                                    br(),
                                    br(),
                                    p(strong("Code: "), "To see an outline of the code used for IDW, use the toggle below. Note: This is not the exact code used within the application. This gives a general outline of the code used if you would like to reproduce the results within a local R session.", style = "font-size: 18px; color: black"),
                                    
                                     input_switch("show_idw_code1", "Show Code?") #CREATE SHOW CODE TOGGLE FOR SPATIAL VIZ
                                     
                    )

                ) #END COND PANEL: SP IDW -----------------------------------------------------------------------------------------------------------------

              ), #END SIDE PANEL -------------------------------------------------------------------------------------------------------------------------
              
            # BEGIN Main Panel ----------------------------------------------------------------------------------------------------------------------------
            mainPanel(
               uiOutput("idw_info"), #DISPAY MATH FORMULATION OF IDW
              
               conditionalPanel(condition = "input.show_idw_code1 == false",   
                 uiOutput("idw_mod_result") #DISPLAY PLOTLY VISUALIZATION OF IDW OUTPUT
               ),
               
               conditionalPanel(condition = "input.show_idw_code1 == true", 
                 uiOutput("idw_code_result")  #DISPLAY CODE FOR  IDW OUTPUT            
               ),
               
               uiOutput("idw_mod_button") #DISPLAY DOWNLOAD BUTTON FOR IDW OUTPUT 
                          
            ) #END Main Panel -----------------------------------------------------------------------------------------------------------------------------
          )  #END SIDEBAR LAYOUT --------------------------------------------------------------------------------------------------------------------------
        ),  #END IDW TAB ----------------------------------------------------------------------------------------------------------------------------------
              
        #SPATIAL GP SUB TAB -------------------------------------------------------------------------------------------------------------------------------

        tabPanel("Spatial Modeling",
            sidebarLayout(
              
              #BEGIN Side Panel --------------------------------------------------------------------------------------------------------------------------
              sidebarPanel( 
                uiOutput("message3"), #MESSAGE IF NO DATA UPLOADED 
                
                conditionalPanel("output.col_exists == true", #BEGIN CONDITIONAL PANEL TO MAKE ST INTO A SPATIAL ANALYSIS ---------------------------------
                   helpText(
                       "You have a", strong("Spatial Temporal"), "Dataset. If you would like to make this a Spatial Analysis, select a time below",
                        br(),
                        br()),

                   uiOutput("dynamic_dropdown")
                   
                ), #END CONDITIONAL PANEL TO MAKE ST INTO A SPATIAL ANALYSIS -------------------------------------------------------------------------------

                conditionalPanel( #BEGIN CONDITIONAL PANEL TO CHOOSE COVARIANCE FUNCTION -------------------------------------------------------------------
                  condition = "output.col_exists == false || (input.time_options != '' && output.col_exists == true)",
                  
                  helpText(
                    p(strong("Start:"), "To develop your Spatial Gaussian Process Model, first specify the covariance function you would like to estimate. Currently two options are available. When you select a covariance function, it's mathematical formulation will appear. Default values are used to begin fitting proceures. To specify starting values, click the checkbox below. Once happy with your selection, click on ", strong("Run Model"), "to begin fitting procedures.", style = "font-size: 18px; color: black"),
                    #br()
                    ),

                   selectInput("cov_function_space", "Select Covariance Function:",
                                       c("",
                                         "Exponential Isotropic" = "exponential_isotropic",
                                         "Spatial Matern" = "matern_isotropic")), # SELECT COVARIANCE FUNCTION
                  
                   ), #END CONDITIONAL PANEL TO CHOOSE COVARIANCE FUNCTION ----------------------------------------------------------------------------------

                 conditionalPanel(condition = "input.cov_function_space == 'exponential_isotropic'", #BEGIN COND PANEL FOR SPACE EXPONENTIAL ----------------
                                 #helpText(
                                 #        p("The function uses default values to begin fitting procedures. If you would like to specify your own starting values, click on the checkbox below.", style = "font-size: 16px; color: black"),
                                 #        ), #HELP TEXT ABOUT DEFAULT STARTING VALUES

                                  checkboxInput("model_option_space_exponential", "Check for More Model Options", FALSE), #CHECKBOX FOR MODEL OPTIONS
                                 
                                 helpText(br()),

                                  conditionalPanel("input.model_option_space_exponential == true", #BEGIN SUB COND PANEL TO DISPLAY MORE MODEL OPTIONS -------
                                                  numericInput("max_iter_space_exponential", "Maximum Number of Iterations: Maximum number of times algorithm iterates to find parameter values:", value = 100, min = 40), #CHOOSE NUMBER OF ITERATIONS

                                                   helpText(br(),
                                                            p("Specify start values for covariance function parameters. If left blank, model will select default starting values:", style = "font-size: 16px; color: black"),
                                                             #br()
                                                             ), #HELPTEXT ABOUT SPECIFYING STARTING VALUES

                                                    tagList(withMathJax(),tags$label(HTML("Start value for process variance (\\(\\sigma^2\\)):")),
                                                            numericInput("start_var_s_e", NULL, value = NULL, min = 0)),
                                                    tagList(withMathJax(),tags$label(HTML("Start value for spatial range parmeter (\\(\\rho_s\\)):")),
                                                            numericInput("start_spatial_s_e", NULL, value = NULL, min = 0)),
                                                    tagList(withMathJax(),tags$label(HTML("Start value for nugget ratio (\\(\\tau^2\\)):")),
                                                            numericInput("start_nugget_s_e", NULL, value = NULL, min = 0))
                                                  
                                                   ), #END SUB COND PANEL TO DISPLAY MORE MODEL OPTIONS -----------------------------------------------------

                                            actionButton("run_model_space_exponential", "Run Model"), #BUTTON TO RUN MODEL
                                          
                                            conditionalPanel(condition = "input.run_model_space_exponential > 0", #BEGIN SUB COND PANEL FOR CODE TOGGLE ----
                                                helpText(br(),
                                                         br(),
                                                         p(strong("Code: "), "To see an outline of the code used for the Gaussian Process Model, use the toggle below. Note: This is not the exact code used within the application. This gives a general outline of the code used if you would like to reproduce the results within a local R session.", style = "font-size: 18px; color: black"),
                                                ), #HELP TEXT TO DESCRIBE TOGGLE
                                                
                                                input_switch("show_sp_model_code1", "Show Code?") #CODE TOGGLE
                                                
                                                ) #END SUB COND PANEL FOR CODE TOGGLE ------------------------------------------------------------------------------
                           ), #END COND PANEL FOR SPACE EXPONENTIAL ------------------------------------------------------------------------------------------------

                      conditionalPanel(condition = "input.cov_function_space == 'matern_isotropic'", #BEGIN COND PANEL FOR SPACE MATERN ----------------------------
                                            
                                       helpText(p(strong("Note:"), "This may take longer to run than the Exponential Covariance Function due to estimate of additional covariance parameter"),
                                                #br(),
                                                #p("The function uses default values for fitting procedures. If you would like to specify your own starting values, click on the checkbox below.", style = "font-size: 16px; color: black"),
                                            ), #HELP TEXT ABOUT DEFAULT STARTING VALUES

                                        checkboxInput("model_option_space_matern", "Check this Box for More Model Options", FALSE), #CHECKBOX FOR MODEL OPTIONS

                                        conditionalPanel("input.model_option_space_matern == true", #BEGIN SUB COND PANEL TO DISPLAY MORE MODEL OPTIONS -----------

                                                         numericInput("max_iter_space_matern", "Maximum Number of Iterations: Maximum number of times algorithm iterates to find parameter values:", value = 100, min = 40), #CHOOSE NUMBER OF ITERATIONS

                                                         helpText(br(),
                                                                  p("Specify start values for covariance function parameters. If left blank, model will select default starting values:", style = "font-size: 16px; color: black"),                                                                  br()
                                                             ), #HELPTEXT ABOUT SPECIFYING STARTING VALUES
                                                         
                                                          tagList(withMathJax(),tags$label(HTML("Start value for process variance (\\(\\sigma^2\\)):")),
                                                             numericInput("start_var_m_e", NULL, value = NULL, min = 0)),
                                                          tagList(withMathJax(),tags$label(HTML("Start value for sptial range parmeter (\\(\\rho_s\\)):")),
                                                             numericInput("start_spatial_m_e", NULL, value = NULL, min = 0)),
                                                          tagList(withMathJax(),tags$label(HTML("Start value for smoothness parmeter (\\(\\nu\\)):")),
                                                             numericInput("start_smooth_m_e", NULL, value = NULL, min = 0)),
                                                          tagList(withMathJax(),tags$label(HTML("Start value for nugget ratio (\\(\\tau^2\\)):")),
                                                             numericInput("start_nugget_m_e", NULL, value = NULL, min = 0))
                                            ), #END SUB COND PANEL TO DISPLAY MORE MODEL OPTIONS ----------------------------------------------------------------

                                            actionButton("run_model_space_matern", "Run the Model"), #BUTTON TO RUN MODEL
                                            
                                            conditionalPanel(condition = "input.run_model_space_matern > 0",   #BEGIN SUB COND PANEL FOR CODE TOGGLE -------------
                                                             helpText(br(),
                                                                      br(),
                                                                      p(strong("Code: "), "To see an outline of the code used for the Gaussian Process Model, use the toggle below. Note: This is not the exact code used within the application. This gives a general outline of the code used if you would like to reproduce the results within a local R session.", style = "font-size: 18px; color: black"),
                                                                      ), #HELP TEXT TO DESCRIBE TOGGLE
                                                             
                                                             input_switch("show_sp_model_code2", "Show Code?") #CODE TOGGLE
                                            ) #END SUB COND PANEL FOR CODE TOGGLE ---------------------------------------------------------------------------------
                           ) #END COND PANEL FOR MATERN EXPONENTIAL ------------------------------------------------------------------------------------------------
              ), #END Side Panel -----------------------------------------------------------------------------------------------------------------------------------

              #BEGIN Main Panel ------------------------------------------------------------------------------------------------------------------------------------
              mainPanel(
                uiOutput("space_exp_param_info"), #DISPLAY MATHEMATICAL FORMULATION OF SPATIAL EXPONENTIAL COVARIANCE FUNCTION

                uiOutput("space_matern_param_info"), #DISPLAY MATHEMATICAL FORMULATION OF SPATIAL MATERN COVARIANCE FUNCTION

                conditionalPanel( #BEGIN CONDITIONAL PANEL FOR EXPONETIAL FUNCTION AFTER RUN MODEL ------------------------------------------------------------------
                  condition = "input.cov_function_space == 'exponential_isotropic' && input.run_model_space_exponential > 0",
                  
                  conditionalPanel(condition = "input.show_sp_model_code1 == false", #BEGIN SUB COND PANEL WHEN CODE TOGGLE OFF -------------------------------------
                          helpText(br()),
                          shinycssloaders::withSpinner(uiOutput("result_space_exponential")), # RESULTS IN TABLE (SPINNER WHILE MODEL RUNS)
                  ), #END SUB COND PANEL WHEN CODE TOGGLE OFF -------------------------------------------------------------------------------------------------------
                  
                  conditionalPanel( condition = "input.show_sp_model_code1 == true", #BEGIN SUB COND PANEL WHEN CODE TOGGLE ON --------------------------------------
                    verbatimTextOutput("code_block_sp_model1")
                  ) #END SUB COND PANEL WHEN CODE TOGGLE ON ---------------------------------------------------------------------------------------------------------
                ),  #END CONDITIONAL PANEL FOR EXPONETIAL FUNCTION AFTER RUN MODEL ----------------------------------------------------------------------------------

                conditionalPanel( #BEGIN CONDITIONAL PANEL FOR MATERN FUNCTION AFTER RUN MODEL ----------------------------------------------------------------------
                  condition = "input.cov_function_space == 'matern_isotropic' && input.run_model_space_matern > 0",
                  
                  conditionalPanel(condition = "input.show_sp_model_code2 == false", #BEGIN SUB COND PANEL WHEN CODE TOGGLE OFF -------------------------------------
                          helpText(br()),
                          shinycssloaders::withSpinner(uiOutput("result_space_matern")) # RESULTS IN TABLE (SPINNER WHILE MODEL RUNS)
                    ), #END SUB COND PANEL WHEN CODE TOGGLE OFF -------------------------------------------------------------------------------------------------------
                  
                  conditionalPanel(condition = "input.show_sp_model_code2 == true", #BEGIN SUB COND PANEL WHEN CODE TOGGLE ON --------------------------------------
                    verbatimTextOutput("code_block_sp_model2")
                  ) #END SUB COND PANEL WHEN CODE TOGGLE ON --------------------------------------------------------------------------------------------------------- 
                ) #END CONDITIONAL PANEL FOR MATERN FUNCTION AFTER RUN MODEL ----------------------------------------------------------------------------------------
                
           ) # END Main Panel
         ) # END SIDEBAR Layout
        ), # END Spatial Modeling Tab


        #SPATIO-TEMPORAL GP SUB TAB ---------------------------------------------------------------------------------------------------------------------------------

        tabPanel("Spatial-Temporal Modeling",
          sidebarLayout(
            
          #BEGIN Side Panel -----------------------------------------------------------------------------------------------------------------------------------------
           sidebarPanel(
             uiOutput("message2"), #MESSAGE IF NO DATA UPLOADED 

             conditionalPanel(condition = "output.col_exists == true", #BEGIN CONDITIONAL PANEL TO SELECT COVARIANCE FUNCTION ---------------------------------------
                              
                helpText(p(strong("Start:"), "To develop your Spatio-Temporal Gaussian Process Model, first specify the covariance function you would like to estimate. Currently two options are available. When you select a covariance function, it's mathematical formulation will appear. Default values are used to begin fitting proceures. To specify starting values, click the checkbox below. Once happy with your selection, click on ", strong("Run Model"), "to begin fitting procedures.", style = "font-size: 18px; color: black"),
                                br()),  # HELP TEXT ABOUT CHOOSING COVARIANCE FUNCTION          

                selectInput("cov_function", "Select Covariance Function:",
                       c("",
                         "Exponential Space-Time" = "exponential_spacetime",
                         "Matern Space-Time" = "matern_spacetime")), #SELECT COVARIANCE FUNCTION
                ), #END CONDITIONAL PANEL TO SELECT COVARIANCE FUNCTION --------------------------------------------------------------------------------------------

                conditionalPanel(condition = "input.cov_function == 'exponential_spacetime'", #BEGIN CONDITIONAL PANEL FOR EXPONENTIAL ST --------------------------
                                     
                        #helpText(p("The function uses default values to begin fitting procedures. If you would like to specify your own starting values, click on the checkbox below.", style = "font-size: 16px; color: black;"),
                        #         ), #HELPTEXT INSTRUCTIONS
                        
                        checkboxInput("model_option", "Check this Box for More Model Options", FALSE), #CHECKBOX FOR MORE MODEL OPTIONS
                        
                        helpText(br()), 
                         
                        conditionalPanel("input.model_option == true", #BEGIN SUB CONDITIONAL PANEL FOR MODEL OPTIONS CHECKBOX -------------------------------------
                            numericInput("max_iter", "Maximum Number of Iterations: Maximum number of times algorithm iterates to find parameter values:", value = 100, min = 40), #MAXIMUM NUMBER OF ITERATIONS

                            helpText(br(),
                                     p("Specify start values for covariance function parameters. If left blank, model will select default starting values:", style = "font-size: 16px; color: black"),                                  br()), #HELPTEXT ABOUT STARTING VALUE

                            tagList(withMathJax(),tags$label(HTML("Start value for process variance (\\(\\sigma^2\\)):")),
                                 numericInput("start_var", NULL, value = NULL, min = 0)),
                            tagList(withMathJax(),tags$label(HTML("Start value for sptial range parmeter (\\(\\rho_s\\)):")),
                                 numericInput("start_spatial", NULL, value = NULL, min = 0)),
                            tagList(withMathJax(),tags$label(HTML("Start value for temporal range parmeter (\\(\\rho_t\\)):")),
                                 numericInput("start_time", NULL, value = NULL, min = 0)),
                            tagList(withMathJax(),tags$label(HTML("Start value for nugget ratio (\\(\\tau^2\\)):")),
                                 numericInput("start_nugget", NULL, value = NULL, min = 0))
                            
                          ), #END SUB CONDITIONAL PANEL FOR MODEL OPTIONS CHECKBOX --------------------------------------------------------------------------------
                         
                         actionButton("run_model", "Run Model"), #BUTTON TO RUN MODEL
                         
                         conditionalPanel(condition = "input.run_model > 0",  #BEGIN SUB COND PANEL FOR CODE TOGGLE -----------------------------------------------
                                          helpText(br(),
                                                   br(),
                                                   p(strong("Code: "), "To see an outline of the code used for the Gaussian Process Model, use the toggle below. Note: This is not the exact code used within the application. This gives a general outline of the code used if you would like to reproduce the results within a local R session.", style = "font-size: 18px; color: black"),
                                                   ), #HELPTEXT ABOUT TOGGLE
                                          
                                          input_switch("show_st_model_code1", "Show Code?") #SHOW CODE TOGGLE
                                          
                         ) #END SUB COND PANEL FOR CODE TOGGLE -----------------------------------------------------------------------------------------------------    
                       ), #END CONDITIONAL PANEL FOR EXPONENTIAL ST ------------------------------------------------------------------------------------------------
             
             conditionalPanel(condition = "input.cov_function == 'matern_spacetime'", #BEGIN CONDITIONAL PANEL FOR MATERN ST ---------------------------------------
                              
                              helpText(p("The function uses default values to begin fitting procedures. If you would like to specify your own starting values, click on the checkbox below.", style = "font-size: 16px; color: black;"),
                              ), #HELPTEXT INSTRUCTIONS
                              
                              checkboxInput("model_option_st_matern", "Check this Box for More Model Options", FALSE), #CHECKBOX FOR MORE MODEL OPTIONS
                              
                              conditionalPanel("input.model_option_st_matern == true", #BEGIN SUB CONDITIONAL PANEL FOR MODEL OPTIONS CHECKBOX ---------------------------------
                                               numericInput("max_iter_st_matern", "Maximum Number of Iterations: Maximum number of times algorithm iterates to find parameter values:", value = 100, min = 40), #MAXIMUM NUMBER OF ITERATIONS
                                               
                                               helpText(br(),
                                                        p("Specify start values for covariance function parameters. If left blank, model will select default starting values:", style = "font-size: 16px; color: black"),
                                                        br()), #HELPTEXT ABOUT STARTING VALUE
                                               
                                               tagList(withMathJax(),tags$label(HTML("Start value for process variance (\\(\\sigma^2\\)):")),
                                                       numericInput("start_var_st_matern", NULL, value = NULL, min = 0)),
                                               tagList(withMathJax(),tags$label(HTML("Start value for sptial range parmeter (\\(\\rho_s\\)):")),
                                                       numericInput("start_spatial_st_matern", NULL, value = NULL, min = 0)),
                                               tagList(withMathJax(),tags$label(HTML("Start value for temporal range parmeter (\\(\\rho_t\\)):")),
                                                       numericInput("start_time_st_matern", NULL, value = NULL, min = 0)),
                                               tagList(withMathJax(),tags$label(HTML("Start value for smoothness parmeter (\\(\\nu\\)):")),
                                                       numericInput("start_smooth_st_matern", NULL, value = NULL, min = 0)),
                                               tagList(withMathJax(),tags$label(HTML("Start value for nugget ratio (\\(\\tau^2\\)):")),
                                                       numericInput("start_nugget_st_matern", NULL, value = NULL, min = 0))
                                               
                              ), #END SUB CONDITIONAL PANEL FOR MODEL OPTIONS CHECKBOX --------------------------------------------------------------------------------
                              
                              actionButton("run_model_st_matern", "Run Model"), #BUTTON TO RUN MODEL
                              
                              conditionalPanel(condition = "input.run_model_st_matern > 0",  #BEGIN SUB COND PANEL FOR CODE TOGGLE -----------------------------------------------
                                               helpText(br(),
                                                        br(),
                                                        p(strong("Code: "), "To see an outline of the code used for the Gaussian Process Model, use the toggle below. Note: This is not the exact code used within the application. This gives a general outline of the code used if you would like to reproduce the results within a local R session.", style = "font-size: 18px; color: black"),
                                                        ), #HELPTEXT ABOUT TOGGLE
                                               
                                               input_switch("show_st_model_code2", "Show Code?") #SHOW CODE TOGGLE
                                               
                              ) #END SUB COND PANEL FOR CODE TOGGLE ----------------------------------------------------------------------------------------------------    
             ) #END CONDITIONAL PANEL FOR MATERN ST -------------------------------------------------------------------------------------------------------------------
             
           ), #END SIDE PANEL -------------------------------------------------------------------------------------------------------------------------------------

           #BEGIN MAIN PANEL --------------------------------------------------------------------------------------------------------------------------------------
           mainPanel(
             
              conditionalPanel(condition = "output.col_exists == false", #BEGIN CONDTIONAL PANEL FOR WARNING IF DATA IS SPATIAL ONLY ------------------------------
               HTML("You Do Not have A Spatio-Temporal Dataset. Please Select Spatial Modeling Tab!")
             ), #BEGIN CONDTIONAL PANEL FOR WARNING IF DATA IS SPATIAL ONLY ---------------------------------------------------------------------------------------

             uiOutput("st_exp_param_info"), #MATHEMATICAL FORMULATION OF ST EXPONENTIAL FUNCTION

             conditionalPanel(condition = "input.cov_function == 'exponential_spacetime' && input.run_model > 0", #BEGIN COND PANEL FOR RUNNING EXPONENTIAL ST -----

                conditionalPanel(condition = "input.show_st_model_code1 == false", #BEGIN SUB COND PANEL IF CODE TOGGLE IS FALSE -----------------------------------
                  shinycssloaders::withSpinner(uiOutput("result")) # RESULTS IN TABLE (SPINNER WHILE MODEL RUNS)
                 ), #END SUB COND PANEL IF CODE TOGGLE IS FALSE ----------------------------------------------------------------------------------------------------
             
                conditionalPanel(condition = "input.show_st_model_code1 == true", #BEGIN SUB COND PANEL IF CODE TOGGLE IS TRUE -------------------------------------
                  verbatimTextOutput("code_block_st_model1")
                 ), #END SUB COND PANEL IF CODE TOGGLE IS TRUE -----------------------------------------------------------------------------------------------------
             
             ), #END COND PANEL FOR RUNNING EXPONENTIAL ST -------------------------------------------------------------------------------------------------------

             uiOutput("st_matern_param_info"), #MATHEMATICAL FORMULATION OF ST EXPONENTIAL FUNCTION
             
             conditionalPanel(condition = "input.cov_function == 'matern_spacetime' && input.run_model_st_matern > 0", #BEGIN COND PANEL FOR RUNNING MATERN ST -----
                              
                        conditionalPanel(condition = "input.show_st_model_code2 == false", #BEGIN SUB COND PANEL IF CODE TOGGLE IS FALSE --------------------------
                                  shinycssloaders::withSpinner(uiOutput("result_st_matern")) # RESULTS IN TABLE (SPINNER WHILE MODEL RUNS)
                              ), #END SUB COND PANEL IF CODE TOGGLE IS FALSE ----------------------------------------------------------------------------------------------------
                              
                        conditionalPanel(condition = "input.show_st_model_code2 == true", #BEGIN SUB COND PANEL IF CODE TOGGLE IS TRUE -------------------------------------
                                               verbatimTextOutput("code_block_st_model2")
                              ), #END SUB COND PANEL IF CODE TOGGLE IS TRUE -----------------------------------------------------------------------------------------------------
                              
             ) #END COND PANEL FOR RUNNING MATERN ST ----------------------------------------------------------------------------------------------------------

           ) #END MAIN PANEL --------------------------------------------------------------------------------------------------------------------------------
         ) #END SIDEBAR LAYOUT ------------------------------------------------------------------------------------------------------------------------------
        ) #END Spatial-Temporal Modeling Tab ----------------------------------------------------------------------------------------------------------------

    ), #END MODEL TAB

  ##############################################################################

  tabPanel("Predictions",
          sidebarLayout(
           
            # BEGIN SIDE PANEL ------------------------------------------------------------------------------------------------------------------------------
            sidebarPanel(
              
             uiOutput("choice_container"),  

             conditionalPanel(condition = "output.current_selection == 'IDW' || output.current_selection == 'Exponential Isotropic' || output.current_selection == 'Matern Isotropic'", #BEGIN COND PANEL FOR UPLOAD INSTRUCTION FOR SPATIAL DATASETS ---------

                     fileInput("upload_spatial_pred", "Upload Predicted Locations file (accepted: .csv)", accept = c(".csv")), #UPLOAD PREDICTON DATASET
                     
                     helpText(p("File must contain columns names:", style = "font-size: 16px; color: black"),  #START HELP TEXT FOR PLOT DESCRIPTION
                              
                              tags$ul(
                                tags$li("site_id: ID for location prediction", style = "font-size: 14px; color: black"),
                                tags$li("Longitude", style = "font-size: 14px; color: black"),
                                tags$li("Latitude", style = "font-size: 14px; color: black")
                              )
                     )

              ), #END COND PANEL FOR UPLOAD INSTRUCTION FOR SPATIAL DATASETS ----------------------------------------------------------------------
             
               conditionalPanel(condition = "output.current_selection == 'IDW'", # BEGIN CONDITIONAL PANEL FOR IDW PREDICTION BUTTON --------------
                       helpText(p(strong("Step 2:"), "Once you have uploaded your dataset for prediction, click the button below. A map of your predictions will appear on the right and you will have the ability to download your predicted dataset. You can repeat this process for any model that you have ran by selecting a different checkbox above.", style = "font-size: 16px; color: black"),
                                br(),
                                ),
                        actionButton("run_pred_idw", "Calculate Prediction")
               ), # END CONDITIONAL PANEL FOR IDW PREDICTION BUTTON -------------------------------------------------------------------------------

               conditionalPanel(condition = "output.current_selection == 'Exponential Isotropic' || output.current_selection == 'Matern Isotropic'", # BEGIN CONDITIONAL PANEL FOR SPATIAL GP PREDICTION BUTTON --------------
                       helpText(p(strong("Step 2:"), "Once you have uploaded your dataset for prediction, click the button below. A map of your predictions will appear on the right and you will have the ability to download your predicted dataset. You can repeat this process for any model that you have ran by selecting a different checkbox above.", style = "font-size: 16px; color: black"),
                                 br(),
                                ),
                       actionButton("run_pred_space_exp", "Calculate Prediction")
               ), # END CONDITIONAL PANEL FOR SPATIAL GP PREDICTION BUTTON --------------
             
               conditionalPanel(condition = "output.current_selection == 'Exponential Space-Time' || output.current_selection == 'Matern Space-Time'", #BEGIN COND PANEL FOR UPLOAD INSTRUCTION FOR ST DATASETS ---------

                  fileInput("upload_st_pred", "Upload Predicted Locations file", accept = c(".csv")), #UPLOAD PREDICTON DATASET

                  helpText(p("File must contain columns names:", style = "font-size: 16px; color: black"),  #START HELP TEXT FOR PLOT DESCRIPTION
                           
                           tags$ul(
                             tags$li("site_id: ID for location prediction", style = "font-size: 14px; color: black"),
                             tags$li("Longitude", style = "font-size: 14px; color: black"),
                             tags$li("Latitude", style = "font-size: 14px; color: black"),
                             tags$li("t: time", style = "font-size: 14px; color: black")
                           )
                  ),
                  
                  helpText(p(strong("Step 2:"), "Once you have uploaded your dataset for prediction, click the button below. A map of your predictions will appear on the right and you will have the ability to download your predicted dataset. You can repeat this process for any model that you have ran by selecting a different checkbox above.", style = "font-size: 16px; color: black"),
                            ),

                           actionButton("run_pred_st", "Calculate Prediction")

             ) #END COND PANEL FOR UPLOAD INSTRUCTION FOR ST DATASETS --------------------------------------------------------------------------------------------------------------------------
           
           ), #END SIDE panel ------------------------------------------------------------------------------------------------------------------------------------------------------------------
           
           #BEGIN MAIN PANEL -------------------------------------------------------------------------------------------------------------------------------------------------------------------
           mainPanel(

            uiOutput("message4"),
           
            conditionalPanel(condition = "output.current_selection == 'Exponential Isotropic' || output.current_selection == 'Matern Isotropic'", #BEGIN COND PANEL SPATIAL GP OUTPUT ---------
                            shinycssloaders::withSpinner(uiOutput("spat_exp_pred_result")),
                            br(),
                            br(),
                            uiOutput("space_pred_button")
             ), #END COND PANEL SPATIAL GP OUTPUT ---------------------------------------------------------------------------------------------------------------------------------------------
           
           conditionalPanel(condition = "output.current_selection == 'IDW'", #BEGIN COND PANEL IDW OUTPUT -------------------------------------------------------------------------------------
                            shinycssloaders::withSpinner(uiOutput("spat_idw_pred_result")),
                            br(),
                            br(),
                            uiOutput("idw_pred_button")
           ), #END COND PANEL IDW OUTPUT -----------------------------------------------------------------------------------------------------------------------------------------------------
           
           conditionalPanel(condition = "output.current_selection == 'Exponential Space-Time' || output.current_selection == 'Matern Space-Time'", #BEGIN COND PANEL FOR ST GP OUTPUT --------
                            shinycssloaders::withSpinner(uiOutput("st_pred_result")),
                            br(),
                            br(),
                            uiOutput("st_pred_button")
           ) #END COND PANEL FOR ST GP OUTPUT ------------------------------------------------------------------------------------------------------------------------------------------------

          ) #END MAIN PANEL ------------------------------------------------------------------------------------------------------------------------------------------------------------------
        ) #END SIDEBAR LAYOUT ----------------------------------------------------------------------------------------------------------------------------------------------------------------
      ), #END PREDICTIONS TAB


  tabPanel("Model Comparison",
           sidebarLayout(
             
             #BEGIN Side Panel --------------------------------------------------------------------------------------------------------------------
             sidebarPanel(
               
               uiOutput("choice_container2"), #Choices of models available to be ran
               
               helpText(p(strong("Step 2"), "Check the Model Comparison Metrics you would like to calculate for your chosen model:", style = "font-size: 18px; color: black;"),
               ), #HELPTEXT INSTRUCTIONS
               
               tags$head(
                 tags$style(HTML(".small-checkbox label {font-size: 14px;}"))
               ), #Make Checkbox Text smaller
               
               div(class = "small-checkbox", checkboxInput("rmse", "Calculate the Root Mean Square Error", FALSE)), #RMSE checkbox
               div(class = "small-checkbox", checkboxInput("mae", "Calculate the Mean Absolute Error", FALSE)), #MAE checkbox
               div(class = "small-checkbox", checkboxInput("corr", "Calculate the Correlation Between Actual & Predicted Values", FALSE)), #Correlation Checkbox
               
              uiOutput("show_comp_button") #Make button available once data set uploaded.

               
             ), #END Side Panel --------------------------------------------------------------------------------------------------------------------
             
             #BEGIN Main Panel ---------------------------------------------------------------------------------------------------------------------
             mainPanel(
               
               uiOutput("messageComp"), #Message if not dataset uploaded
               
               conditionalPanel(condition = "input.run_model_comp_but == 0 && input.rmse == true",
                uiOutput("rmse_info")
               ), #CONDITONAL PANEL FOR RMSE INFO -------------------------------------------------------------------------------------------------
               
               conditionalPanel(condition = "input.run_model_comp_but == 0 && input.mae == true",
                                uiOutput("mae_info")
               ), #CONDITONAL PANEL FOR MAE INFO -------------------------------------------------------------------------------------------------
               
               conditionalPanel(condition = "input.run_model_comp_but == 0 && input.corr == true",
                                uiOutput("corr_info")
               ), #CONDITONAL PANEL FOR CORR INFO -------------------------------------------------------------------------------------------------
               
               conditionalPanel(condition = "input.run_model_comp_but > 0 && input.rmse == true",
                                tableOutput("rmse_table") 
               ), #CONDITONAL PANEL FOR RMSE TABLE -------------------------------------------------------------------------------------------------
               
               conditionalPanel(condition = "input.run_model_comp_but > 0 && input.mae == true",
                                tableOutput("mae_table") 
               ), #CONDITONAL PANEL FOR MAE TABLE -------------------------------------------------------------------------------------------------
               
               conditionalPanel(condition = "input.run_model_comp_but > 0 && input.corr == true",
                                tableOutput("corr_table") 
               ) #CONDITONAL PANEL FOR CORR TABLE -------------------------------------------------------------------------------------------------
               
             )#END Main Panel ----------------------------------------------------------------------------------------------------------------------
             
           ) #END SIDEBAR LAYOUT


  ), ##END MODEL COMPARISON TAB
  
  navbarMenu("Resources",
             
             tabPanel("Version Information",
                    p(strong("Version Information Page"), style = "font-size: 18px; color: black; text-align:center;"),
                    br(),
                    p(strong("Description:"), "This page will be update everytime a new version of AQ-Shiny is available. It will list any new updates made to AQ-Shiny.", style = "font-size: 14px; color: black; center;"),
                    br(),
                    p(strong("Current Version:"), "1.0.0", style = "font-size: 14px; color: black; center;")
                    
             ),
             
             tabPanel("R Packages",
                      
                      p(strong("R Packages List"), style = "font-size: 18px; color: black; text-align:center;"),
                      br(),
                      p(strong("Description:"), "This page includes a list of all R Packages used throughout this application to visualize and model the data, allow with the version used and citation.", style = "font-size: 14px; color: black; center;"),
                      br(),
                      p(strong("R Version:"), "4.5.2", style = "font-size: 14px; color: black; center;"),
                      br(),
                      p(strong("Data Cleaning:"), style = "font-size: 14px; color: black; center;"),
                      tags$ul(
                        tags$li("tidyverse (Version: 2.0.0)", style = "font-size: 12px; color: black;"),
                            tags$ul(tags$li("Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL,
                                          Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019).
                                          “Welcome to the tidyverse.” Journal of Open Source Software, 4(43), 1686. doi:10.21105/joss.01686"), style = "font-size: 12px; color: black;"),
                        tags$li("sf (Version: 1.0-24)", style = "font-size: 12px; color: black;"),
                              tags$ul(
                                tags$li("Pebesma, E., & Bivand, R. (2023). Spatial Data Science: With Applications in R. Chapman and Hall/CRC.https://doi.org/10.1201/9780429459016"),
                                tags$li("Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal 10(1), 439-446, https://doi.org/10.32614/RJ-2018-009"), style = "font-size: 12px; color: black;"),
                        tags$li("sp (Version: 2.2-1)", style = "font-size: 12px; color: black;"),
                               tags$ul(
                                  tags$li("Pebesma E, Bivand R (2005). “Classes and methods for spatial data in R.” R News, 5(2), 9-13. https://CRAN.R-project.org/doc/Rnews/"),
                                  tags$li("Bivand R, Pebesma E, Gomez-Rubio V (2013). Applied spatial data analysis with R, Second edition. Springer, NY. https://asdar-book.org/"), style = "font-size: 12px; color: black;")
                      ),
                      p(strong("Visualization:"), style = "font-size: 14px; color: black; center;"),
                      tags$ul(
                        tags$li("plotly (Version: 4.12.0)", style = "font-size: 12px; color: black;"),
                            tags$ul(tags$li("C. Sievert. Interactive Web-Based Data Visualization with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020"), style = "font-size: 12px; color: black;"),
                        tags$li("leaflet (Version: 2.2.3)", style = "font-size: 12px; color: black;"),
                            tags$ul(tags$li("Cheng J, Schloerke B, Karambelkar B, Xie Y, Aden-Buie G (2025). leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library. doi:10.32614/CRAN.package.leaflet"), style = "font-size: 12px; color: black;"),
                        tags$li("viridis (Version: 0.6.5)", style = "font-size: 12px; color: black;"),
                            tags$ul(tags$li("Simon Garnier, Noam Ross, Robert Rudis, Antônio P. Camargo, Marco Sciaini, and Cédric Scherer (2024). viridis(Lite) - Colorblind-Friendly Color Maps for R. viridis package version 0.6.5"), style = "font-size: 12px; color: black;"),
                        tags$li("leafpop (Version: 0.1.0)", style = "font-size: 12px; color: black;"),
                            tags$ul(tags$li("Appelhans T, Detsch F (2021). leafpop: Include Tables, Images and Graphs in Leaflet Pop-Ups. doi:10.32614/CRAN.package.leafpop, R package version 0.1.0"), style = "font-size: 12px; color: black;")
                      ),
                      p(strong("Modeling:"), style = "font-size: 14px; color: black; center;"),
                      tags$ul(
                        tags$li("GpGp (Version: 1.0.0)", style = "font-size: 12px; color: black;"),
                        tags$ul(tags$li("Guinness, J., and Katzfuss, M. (2025). GpGp: Fast Gaussian Process Computation Using Vecchia's Approximation"), style = "font-size: 12px; color: black;")
                      ),
                      tags$ul(
                        tags$li("gstat (Version: 2.1-6)", style = "font-size: 12px; color: black;"),
                        tags$ul(
                          tags$li("Pebesma, E.J., 2004. Multivariable geostatistics in S: the gstat package. Computers & Geosciences, 30:683-691"), 
                          tags$li("Benedikt Gräler, Edzer Pebesma and Gerard Heuvelink, 2016. Spatio-Temporal Interpolation using gstat. The R Journal 8(1), 204-218"), style = "font-size: 12px; color: black;")
                      )
                      
                      
             ) #End tab panel
             
  ) #END NAVBAR MENU
  
) #END UI
  
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  

# DATA INPUT TAB   ------------------------------------------------------------------------------------------------------------------------
  
  # DATASET DISPLAY OPTIONS -----------------------------------------------------------------------------------------------------------------
                             
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")) #Verify the data set is a csv file
  })  # Upload Dataset -------------------------------------------------------------------------------------------------------------------
  
  output$data_raw_spatial <- renderDataTable({ 
   data <- data()
   
   data %>%
     select("ID", "Latitude", "Longitude", "logPM2.5")
   
  }, rownames = FALSE) # Create Raw Data Table for Spatial Data --------------------------------------------------------------------------
  
  output$data_sum_spatial <- renderTable({
    data <- data()
    
    data %>%
      summarise("Number of Locations" = n(),
                "Average of LogPM2.5" = round(mean(logPM2.5, na.rm = TRUE), 4),
                "Standard deviation of LogPM2.5" = round(sd(logPM2.5, na.rm = TRUE), 4))
    
  }, rownames = FALSE, align = 'c')   # Create Summary Table for Spatial Data -------------------------------------------------------------

    output$data_raw_st <- renderDataTable({ 
    data <- data()
    
    data %>%
      select("ID", "Latitude", "Longitude", "t1", "logPM2.5") %>% 
      mutate(across(c(t1, logPM2.5), round, digits = 4))
    
  }, rownames = FALSE) # Create Raw Data Table for ST Data --------------------------------------------------------------------------------

  output$data_sum_st<- renderDataTable({
    data <- data()
    
    data %>%
      group_by(ID) %>%
      summarise("Number of Measurements" = n(),
                "Average Latitude" = round(mean(Latitude),4),
                "Average Longitude" = round(mean(Longitude),4),
                "Average of LogPM2.5" = round(mean(logPM2.5, na.rm = TRUE), 4),
                "Standard deviation of LogPM2.5" = round(sd(logPM2.5, na.rm = TRUE), 4))
    
  }, rownames = FALSE)  # Create Summary Table for ST Data ---------------------------------------------------------------------------------
  
  output$col_exists <- reactive({
    data <- data()
    req(data) # Ensure data is loaded
    "t1" %in% names(data)
  }) #CHECK FOR TIME COLUMN ---------------------------------------------------------------------------------------------------------------
  
  outputOptions(output, "col_exists", suspendWhenHidden = FALSE)

  # Visualization TAB ---------------------------------------------------------------------------------------------------------------------

  output$message <- renderUI({
    if (is.null(input$upload)) {
      h4("Please upload a file to proceed.")
    }
  }) # Remove options if no file uploaded yet ----------------------------------------------------------------------------------------------

  ## START: Spatial Map --------------------------------------------------------------------------------------------------------------------

  spatial_plot_data <- eventReactive(input$spatial_map_start, {
    dat <- data()

    plot_ly(
      data = dat,
      lon = ~Longitude,
      lat = ~Latitude,
      type = 'scattermapbox',
      mode = 'markers',
      color = ~logPM2.5, # Color by a categorical variable, e.g., status
      colors = viridis::cividis(n = 100), #color blind friendly palette
      marker = list(size = 10, opacity = 0.8),
      showlegend = FALSE,
      text = ~paste("logPM2.5: ", round(logPM2.5, 4)),
      hoverinfo = "text"
    ) %>%
      layout(
        mapbox = list(
          style = "open-street-map", #zoom into california
          zoom = 5,
          center = list(
            lon = mean(dat$Longitude),
            lat = mean(dat$Latitude)
          )
        )
      )
  }) # Reactive expression to hold the plot. ---------------------------------------------------------------------------------------------
  
  output$spatial_map <- renderPlotly({
    spatial_plot_data()
  }) # Render Spatial Map with Plotly ----------------------------------------------------------------------------------------------------

  output$download_html_spatial <- downloadHandler( #create file download
    filename = function() {
      paste0("map_", Sys.Date(), ".html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(
        widget = spatial_plot_data(),
        file = file,
        selfcontained = TRUE
      )
    }
  ) # Create file download for Spatial Plot -------------------------------------------------------------------------------------------------
  
  output$code_block_spatial_map <- renderPrint({
    cat("# This is a raw R code block snippet\n")
    cat("library(tidyverse)\n")
    cat("library(viridis)\n")
    cat("library(plotly)\n")

    cat("\n plot_ly(data = data, #name of dataset
    lon = ~longitude_column_name, 
    lat = ~latitude_column_name,
    type = 'scattermapbox', #plot geographic points over an interactive map
    mode = 'markers', #dispaly individual data points
    color = ~response_column_name, #color points by response variable
    colors = viridis::cividis(n = 100), #use color blind friendly palette
    marker = list(size = 10, opacity = 0.8), #aesthetics for point markers
    showlegend = FALSE, 
    text = ~paste(`logPM2.5: `, round(response_column_name, 4)), #information to display when over over a point rounded to 4 decimals
    hoverinfo = `text`) %>% #information to display when over over a point
    layout(
      mapbox = list(
        style = `open-street-map`, #type of background map to use
        zoom = 5, #control initial zooming
        center = list( #where to focus map
          lon = mean(data$Longitude), 
          lat = mean(data$Latitude)
        )
      )
    )") # Info to display when code toggle is true ---------------------------------------------------------------------------------------
  })
  
  ## END: Spatial Map --------------------------------------------------------------------------------------------------------------------

  ## START: Space in Time Map ------------------------------------------------------------------------------------------------------------

    meuse_plot <- reactive({
    dat <- data()

    new_data <- pivot_longer(dat,cols = c(4,5, 8:13, 15), #This step not necessary, but keeps things more flexible for future functionality
                             names_to = "variable",
                             values_to = "value")

    filter_meuse <- new_data %>% filter(variable == "logPM2.5")

    plot_ly(
      data = filter_meuse,
      lon = ~Longitude,
      lat = ~Latitude,
      type = 'scattermapbox',
      mode = 'markers',
      color = ~value, # Color by a categorical variable, e.g., status
      colors = viridis::cividis(n = 100),
      frame = ~t1, # Animate across time points
      marker = list(size = 10, opacity = 0.8),
      showlegend = FALSE,
      text = ~paste("logPM2.5: ", round(value, 4)),
      hoverinfo = "text"
    ) %>%
      layout(
        mapbox = list(
          style = "open-street-map", #zoom into california
          zoom = 5,
          center = list(
            lon = mean(filter_meuse$Longitude),
            lat = mean(filter_meuse$Latitude)
          )
        ))

  }) # Reactive expression to hold the plot. ---------------------------------------------------------------------------------------------

  output$map <- renderPlotly({
    meuse_plot()
  }) # Render  Map with Plotly ----------------------------------------------------------------------------------------------------------
  
  output$code_block_st_map1 <- renderPrint({
    cat("# This is a raw R code block snippet\n")
    cat("library(tidyverse)\n")
    cat("library(viridis)\n")
    cat("library(plotly)\n")
    
    cat("\n plot_ly(data = data, #name of dataset
    lon = ~longitude_column_name, 
    lat = ~latitude_column_name,
    type = 'scattermapbox', #plot geographic points over an interactive map
    mode = 'markers', #dispaly individual data points
    color = ~response_column_name, #color points by response variable
    colors = viridis::cividis(n = 100), #use color blind friendly palette
    frame = ~time_column_name, # Animate across time points
    marker = list(size = 10, opacity = 0.8), #aesthetics for point markers
    showlegend = FALSE, 
    text = ~paste(`logPM2.5: `, round(response_column_name, 4)), #information to display when over over a point rounded to 4 decimals
    hoverinfo = `text`) %>% #information to display when over over a point
    layout(
      mapbox = list(
        style = `open-street-map`, #type of background map to use
        zoom = 5, #control initial zooming
        center = list( #where to focus map
          lon = mean(data$Longitude), 
          lat = mean(data$Latitude)
        )
      )
    )") 
  }) # Info to display when code toggle is true ---------------------------------------------------------------------------------------

  output$download_html <- downloadHandler( #create file download
    filename = function() {
      paste0("map_", Sys.Date(), ".html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(
        widget = meuse_plot(),
        file = file,
        selfcontained = TRUE
      )
    }
  ) # Create file download for Space in Time Plot --------------------------------------------------------------------------------------
  
  ## END: Space in Time Map ------------------------------------------------------------------------------------------------------------

  ## BEGIN: Time in Space Map ----------------------------------------------------------------------------------------------------------
  
  popup_shown2 <- reactiveVal(FALSE)
  observeEvent(input$plot, { 
    if (input$plot == "Time in Space" && !popup_shown2()) {
      showModal(modalDialog(
        title = "Important Notice",
        "This plot can take a while to render as it is generating individual time series plots for each location. Please be patient",
        easyClose = FALSE,
        footer = modalButton("OK")
      ))
      popup_shown2(TRUE)  # Update the reactive value so it only runs once
    }
  }) #Create Pop-Up to warn people this plot may take awhile to render -----------------------------------------------------------------

  makePopupPlot <- function (clickedArea, df) {
    plotData <- df[c("ID", "t1","logPM2.5", "Latitude", "Longitude")]
    plotDataSubset <- subset(plotData, plotData['ID'] == clickedArea)

    popupPlot <- ggplot(data = plotDataSubset,  aes(x = t1, y = logPM2.5)) +
      geom_point() +
      geom_line(group = 1) +
      xlab("Time") +
      ggtitle(paste0("Time Series of logPM2.5 in ", clickedArea)) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      theme(plot.margin = unit(c(0,0.5,0,0), "cm"), plot.title = element_text(size = 10))

    return (popupPlot)
  } #Define function to create pop ups when hover over points -------------------------------------------------------------------------

  time_map <- reactive({
    data <- data()

    id_only <- data %>%
      group_by(ID) %>%
      summarise(lat = mean(Latitude),
                long = mean(Longitude))

    p <- as.list(NULL)
    p <- lapply(1:length(unique(data$ID)), function(i) {
      p[[i]] <- makePopupPlot(unique(data$ID)[i], data)
    })

    leaflet(id_only) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        popup = popupGraph(p, type = "svg", width = 200, height = 200) # Embed the plots
      )
  }) #Run created function and create leaflet map to show time series when hover over a point -----------------------------------------

  output$map2 <- renderLeaflet({
    time_map()
  }) # Render leaflet map --------------------------------------------------------------------------------------------------------------

  output$download_html2 <- downloadHandler(
    filename = function() {
      paste("my_leaflet_map_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Save the map widget to a temporary file
      temp_html_file <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(time_map(), file = temp_html_file, selfcontained = TRUE)

      file.copy(temp_html_file, file) # Copy the temporary file to the final download destination
    }
  ) #Create file download for time in space plot ---------------------------------------------------------------------------------------
  
  output$code_block_st_map2 <- renderPrint({
    cat("# This is a raw R code block snippet\n")
    cat("library(tidyverse)\n")
    cat("library(leaflet)\n")

    cat("makePopupPlot <- function(clickedArea, df) { ##create an individual time series plot for one sensor location
    plotData <- df[c('ID', 't1', 'logPM2.5', 'Latitude', 'Longitude')]
    plotDataSubset <- subset(plotData, plotData['ID'] == clickedArea)

    popupPlot <- ggplot(data = plotDataSubset,  aes(x = t1, y = logPM2.5)) + 
      geom_point() +
      geom_line(group = 1) +
      xlab('Time') +
      ggtitle(paste0('Time Series of logPM2.5 in ', clickedArea)) +
      theme(legend.position = 'none',
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      theme(plot.margin = unit(c(0,0.5,0,0), 'cm'), plot.title = element_text(size = 10))

    return (popupPlot)
  } \n")

    cat("\n id_only <- data %>%
      group_by(ID) %>%
      summarise(lat = mean(Latitude),
                long = mean(Longitude))

    p <- as.list(NULL)
    p <- lapply(1:length(unique(data$ID)), function(i) { #apply created function above to all sensor locations
      p[[i]] <- makePopupPlot(unique(data$ID)[i], data) 
    })

    leaflet(id_only) %>% #create map where hover over a point displays that sensors pop up graph
      addTiles() %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        popup = popupGraph(p, type = `svg`, width = 200, height = 200) # Embed the plots
      )")
  }) # Info to display when code toggle is true ---------------------------------------------------------------------------------------

  # Model TAB -------------------------------------------------------------------------------------------------------------------------

  ### Block out functionality for models when dataset not uploaded yet ---------------------------------------------------------------
  output$message2 <- renderUI({
    if (is.null(input$upload)) {
      h4("Please upload a file to proceed.")
    }
  }) #ST Modeling Tab

  output$message3 <- renderUI({
    if (is.null(input$upload)) {
      h4("Please upload a file to proceed.")
    }
  }) #Spatial Modeling Tab

  output$messageIDW <- renderUI({
    if (is.null(input$upload)) {
      h4("Please upload a file to proceed.")
    }
  }) ##IDW Tab
  
  
  output$messageComp <- renderUI({
    if (is.null(input$upload_st_pred) & is.null(input$upload_spatial_pred)) {
      h4("Please upload a file in the Prediction Tab to proceed.")
    }
  }) #ST Modeling Tab

  ## IDW ---------------------------------------------------------------------------------------------------------------------------

  output$idw_info <- renderUI({
    if (!is.null(input$upload)) {
      withMathJax(
        helpText(p(strong("Definition:"), style = "font-size: 18px; color: black;"),
                 tags$p(HTML("Calculating a weighted average using weight inverse proportion to distance from the interpolated location."), style = "font-size: 16px; color: black;"),
                 tags$p(HTML("$$ \\hat{Z}(s_0) = \\frac{\\sum_{i=1}^{n} w_i \\, z(s_i)}{\\sum_{i=1}^{n} w_i} $$"), style = "font-size: 16px; color: black;"),
                 tags$p(HTML("where"), style = "font-size: 16px; color: black;"),
                 tags$p(HTML("$$  s_0 = \\text{grid location}, \\quad  s_i = \\text{observed locations} $$"), style = "font-size: 16px; color: black;"),
                 tags$p(HTML("$$ w_i = \\lVert s_0 - s_i \\rVert^{-p}, \\quad  n = \\text{number of sample locations} $$"), style = "font-size: 16px; color: black;"),
                 tags$p(HTML("Default: p = 2"), style = "font-size: 16px; color: black;")
        ) #End HelpText
      ) #end mathjax
    } 
  }) # Mathematical Formulation of IDW ----------------------------------------------------------------------------------------------

  output$dynamic_dropdown2 <- renderUI({
    df <- req(data())
    req("t1" %in% names(df))

    choices2 <- as.character(unique(df$t1))
    selectInput("time_options2", "Select a time", choices = c(" " = "", setNames(choices2, choices2)), selected = "")
  }) # Drop down of time points if want to turn ST into a spatial analysis -----------------------------------------------------------

  calculation_idw_spatial <- eventReactive(input$run_idw, {
    req(data())
    data <- data()

    if("t1" %in% names(data)){
      data_new <- filter(data, t1 == as.numeric(input$time_options2))
    } else{
      data_new <- data
    }
    
    pts <- st_as_sf(
      data_new,
      coords = c("Longitude", "Latitude"),
      crs = 4326
    ) # Convert to sf points
    
    pts_proj <- st_transform(pts, 3857)
    
    boundary <- pts_proj |>
      st_union() |>
      st_concave_hull(ratio = 0.4) #create boundary (convex hull around points)
    
    grid <- st_make_grid(
      boundary,
      cellsize = as.numeric(input$grid_size),   # meters
      what = "centers"
    ) # Create regular grid INSIDE boundary
    
    grid <- grid[boundary]  # Keep only points inside polygon
    
    grid_sf <- st_sf(geometry = grid) # Convert to sf
    
    idw_result <- idw(formula = logPM2.5 ~ 1,
                  locations = pts_proj, newdata = grid_sf,
                  idp = input$idw_power
                  ) # Run IDW
    
    idw_sf <- st_as_sf(idw_result)  # Convert back to sf if needed
  }) # Create Grid and Run IDW ----------------------------------------------------------------------------------------------------------
  
  output$idw_model_plot <- renderPlotly({
    data <- calculation_idw_spatial()

    idw_ll <- st_transform(data, 4326) # Convert to lon/lat
    
    idw_df <- idw_ll %>%
      cbind(st_coordinates(idw_ll)) %>%
      as.data.frame() # Extract coordinates
    
    plot_ly(
      data = idw_df,
      lon = ~X,
      lat = ~Y,
      color = ~var1.pred,
      colors = "viridis",
      type = "scattermapbox",
      mode = "markers",
      marker = list(size = 6),
      text = ~paste("logPM2.5:", round(var1.pred, 4)),
      hoverinfo = "text"
    ) %>%
      layout(
        mapbox = list(
          style = "open-street-map",
          zoom = 4.5,
          center = list(
            lon = mean(idw_df$X),
            lat = mean(idw_df$Y)
          )
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0)
      )

  }) # Make spatial plot of predictions with Plotly ---------------------------------------------------------------------------------
  
  output$code_block_idw1 <- renderPrint({
    cat("# This is a raw R code block snippet\n")
    cat("library(tidyverse)\n")
    cat("library(gstat)\n")
    cat("library(sf)\n")
    
    cat("\n Create Grid: \n")
    
    cat("\n    pts <- st_as_sf(
      data_new,
      coords = c('Longitude', 'Latitude'),
      crs = 4326
    ) # Convert to sf points
    
    pts_proj <- st_transform(pts, 3857) # Reproject to a projected CRS before IDW (meters)
    
    boundary <- pts_proj |>
      st_union() |>
      st_concave_hull(ratio = 0.4) #create boundary (convex hull around points)
    
    grid <- st_make_grid(
      boundary,
      cellsize = as.numeric(input$grid_size),   # meters
      what = 'centers'
    ) # Create regular grid INSIDE boundary
    
    grid <- grid[boundary]  # Keep only points inside polygon
    
    grid_sf <- st_sf(geometry = grid) # Convert to sf
        \n ")
    
    cat("\n Run IDW: \n")
    
    cat("\n idw(formula = logPM2.5 ~ 1,
                  locations = pts_proj, newdata = grid_sf,
                  idp = input$idw_power
                  )")
  }) # What to display when code toggle is true ---------------------------------------------------------------------------------
  
  output$download_idw_data <- downloadHandler(
    filename = function() {
      paste("predictions-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      pred <- data.frame(calculation_idw_spatial())
      write.csv(pred, file)
    }
  )# Create download for specified. grid predictions ------------------------------------------------------------------------------
  
  output$idw_mod_button <- renderUI({
    conditionalPanel(
      condition = "input.run_idw > 0 && input.show_idw_code1 == false",
      br(),
      br(),
      downloadButton("download_idw_data", "Download IDW Prediction Dataset")
    )
  }) # Create download button for specified. grid predictions ---------------------------------------------------------------------

  output$idw_mod_result<- renderUI({
    plotlyOutput("idw_model_plot")
  }) #Plotly Output when code toggle false
  
  output$idw_code_result<- renderUI({
    verbatimTextOutput("code_block_idw1")
  }) #Text Output when code toggle true
  
  ## Exponential Space --------------------------------------------------------------------------------------------------------------

  output$space_exp_param_info <- renderUI({
    if (input$cov_function_space == "exponential_isotropic") {

      withMathJax(
        helpText(strong("Mathematical Formulation:", style = "color: black; font-size: 18px;"),
                 tags$p(HTML("Parameter vector: \\((\\sigma^2, \\rho_s, \\tau^2)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p("where:", style = "color: black; font-size: 14px;"),
                 tags$ul(
                   tags$li(HTML("\\(\\sigma^2\\) = process variance"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\rho_s\\) = spatial range parameter"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\tau^2\\) = nugget ratio"), style = "color: black; font-size: 14px;")
                 ),

                 tags$p(HTML("Let the space-time locations be: \\(x = (s_1, \\ldots, s_d)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("Define the diagonal scaling matrix: \\(D = \\mathrm{diag}(\\rho_s, \\ldots, \\rho_s)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("The covariance function is parameterized as: \\(M(x,y) = \\sigma^2 \\exp\\!\\left(- \\left\\| D^{-1}(x - y) \\right\\|\\right)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("Equivalently: \\(\\left\\| D^{-1}(x - y) \\right\\| = \\sqrt{ \\frac{\\| s_x - s_y \\|^2}{\\rho_s^2} }\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("The nugget added to the diagonal of the covariance matrix is: \\((\\sigma^2 \\tau^2)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(tags$strong("Note:"), "The nugget is \\(\\sigma^2 \\tau^2\\) not \\(\\tau^2\\)", style = "color: black; font-size: 14px;")
                 ) #End HelpText
        ) #end mathjax
    }
  }) # Mathematical Formulation for Spatial Exponential Covariance Function------------------------------------------------------------

  output$dynamic_dropdown <- renderUI({
    req(data())
    data <- data()

    choices <- as.character(unique(data$t1))
    selectInput("time_options", "Select a time", choices = c("Select..." = "", choices), selected = "")
  }) # Drop down of time points if want to turn ST into a spatial analysis ------------------------------------------------------------

  calculation_space_exponential <- eventReactive(input$run_model_space_exponential, {
    req(data())
    data <- data()

    if("t1" %in% names(data)){
      data_new <- filter(data, t1 == as.numeric(input$time_options))
    } else{
      data_new <- data
    } # If ST dataset, filter for selected time for spatial analysis

    add_busy_spinner(spin = "cube-grid")

    locs <- as.matrix(data_new[, c("Longitude", "Latitude")])
    response  <- data_new$logPM2.5

    X <- matrix(1, nrow(locs), 1) # Intercept-only mean model

    if(is.na(input$start_var_s_e) & is.na(input$start_spatial_s_e) & is.na(input$start_nugget_s_e)){
      params <- NULL} else {
        params <- c(input$start_var_s_e, input$start_spatial_s_e, input$start_nugget_s_e)
      } #What to do if have start values or not

    result <- tryCatch({
      fit_exp <- fit_model(
        y = response,
        locs = locs,
        X = X,
        covfun_name = "exponential_isotropic",
        m_seq = c(15, 30), max_iter = input$max_iter_space_exponential,
        start_parms=params, convtol = 1e-05, reorder = TRUE)
    }, error = function(e) {
      # Return NULL or a custom message on failure
      return("Model Needs Better Starting Values")
    }) #Try Catch is not working, but model runs (need a warning message handler)
  }) # Run spatial model with exponential_isotropic covariance function-----------------------------------------------------------
  
  
  output$code_block_sp_model1<- renderPrint({
    
    cat(" library(GpGp) \n")
    
 cat("\n locs <- as.matrix(dataset[, c('Longitude', 'Latitude')]) #create matrix of longitude/latitude
 response  <- dataset$reponse_column_name #pull out response variable
 X <- matrix(1, nrow(locs), 1) # Intercept-only mean model

 fit_model(y = response,
           locs = locs,
           X = X,
           covfun_name = 'exponential_isotropic',
           m_seq = c(15, 30), #sequence of values for number of neighbors
           max_iter = maximum_number_of_iterations,
           start_parms=c(variance, range, nugget), #optional specified starting values
           convtol = 1e-05, #convergence criteria
           reorder = TRUE)")
  }) # What to display when code toggle is true ---------------------------------------------------------------------------------

  output$result_space_exponential <- renderUI({
    covparms1 <- calculation_space_exponential()["covparms"]
    covparms <- round(covparms1[1:3], 4)

    withMathJax(
      helpText(p(strong("Results: Table of Estimated Covariance Parameters"), style = "color: black; font-size: 18px")),
      tags$table(
        style = "
          border-collapse: separate;
          border-spacing: 8px;
          text-align: center;
          margin-top: 10px;
          color: black;
      ",
        tags$thead(
          tags$tr(
            tags$th("\\(\\sigma^2\\)"),
            tags$th("\\(\\rho_s\\)"),
            tags$th("\\(\\tau^2\\)"),
            tags$th("Converge?")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(covparms[1]),
            tags$td(covparms[2]),
            tags$td(covparms[3]),
            tags$td(as.character(calculation_space_exponential()$conv))
          )
        )
      ),
      helpText("If `Converge?` = FALSE, this means the numerical optimizer failed to find a stable, optimal solution given the provided maximum number of iterations. Either increase the maximum number of iterations or provide starting values for your covariance parameters.")
    )
  }) #Create Table for Estimate Covariance Parameters ----------------------------------------------------------------------------

  ## Matern Space ----------------------------------------------------------------------------------------------------------------

  output$space_matern_param_info <- renderUI({
    if (input$cov_function_space == "matern_isotropic") {

      withMathJax(
        helpText(strong("Mathematical Formulation:", style = "color: black; font-size: 18px;"),
                 tags$p(HTML("Parameter vector: \\((\\sigma^2, \\rho_s, \\nu, \\tau^2)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p("where:", style = "color: black; font-size: 14px;"),
                 tags$ul(
                   tags$li(HTML("\\(\\sigma^2\\) = process variance"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\rho_s\\) = spatial range parameter"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\nu\\) = smoothness parameter"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\tau^2\\) = nugget ratio"), style = "color: black; font-size: 14px;")
                 ),

                 tags$p(HTML("Let the space-time locations be: \\(x = (s_1, s_2)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("Define the diagonal scaling matrix: \\(D = \\mathrm{diag}(\\rho_s, \\rho_s)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("Define the scaled space distance: \\(r = \\left\\| D^{-1}(x - y) \\right\\| = \\sqrt{ \\frac{(s_{1x} - s_{1y})^2 + (s_{2x} - s_{2y})^2}{\\rho_s^2} }\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("The Matern covariance function is parameterized as: \\(M(x,y) = \\sigma^2 \\frac{1}{2^{\\nu-1}\\Gamma(\\nu)} \\, r^{\\nu} K_{\\nu}(r)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("where:"), style = "color: black; font-size: 14px;"),

                 tags$ul(
                   tags$li(HTML("\\(K_{\\nu}(\\cdot)\\) = is the modified Bessel function of the second kind")), style = "color: black; font-size: 14px;"),

                 tags$p(HTML("The nugget added to the diagonal of the covariance matrix is: \\((\\sigma^2 \\tau^2)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(tags$strong("Note:"), "The nugget is \\(\\sigma^2 \\tau^2\\) not \\(\\tau^2\\)", style = "color: black; font-size: 14px;")
        ) #end help text
      ) #end mathjax
    }
  }) # Mathematical Formulation for Spatial Matern Covariance Function------------------------------------------------------------

  calculation_space_matern <- eventReactive(input$run_model_space_matern, {
    req(data())
    data <- data()

    if("t1" %in% names(data)){
      data_new <- filter(data, t1 == input$time_options)
    } else{
      data_new <- data
    }

    add_busy_spinner(spin = "cube-grid")

    if(is.na(input$start_var_m_e) & is.na(input$start_spatial_m_e) & is.na(input$start_smooth_m_e) &is.na(input$start_nugget_m_e)){
      params <- NULL} else {
        params <- c(input$start_var_m_e, input$start_spatial_m_e, input$start_smooth_m_e, input$start_nugget_m_e)
      }

    locs <- as.matrix(data_new[, c("Longitude", "Latitude")])
    response  <- data_new$logPM2.5

    # Intercept-only mean model
    X <- matrix(1, nrow(locs), 1)

    fit_mat <- fit_model(
      y = response,
      locs = locs,
      X = X,
      covfun_name = "matern_isotropic",
      m_seq = c(15, 30), max_iter = input$max_iter_space_matern,
      start_parms=params, convtol = 1e-05,reorder = TRUE)

  }) # Run spatial model with matern_isotropic covariance function-----------------------------------------------------------
  
  output$code_block_sp_model2<- renderPrint({
    cat(" library(GpGp) \n")
    
    cat("\n locs <- as.matrix(dataset[, c('Longitude', 'Latitude')]) #create matrix of longitude/latitude
 response  <- dataset$reponse_column_name #pull out response variable
 X <- matrix(1, nrow(locs), 1) # Intercept-only mean model

 fit_model(y = response,
           locs = locs,
           X = X,
           covfun_name = 'matern_isotropic',
           m_seq = c(15, 30), #sequence of values for number of neighbors
           max_iter = maximum_number_of_iterations,
           start_parms=c(variance, range, smooth, nugget), #optional specified starting values
           convtol = 1e-05, #convergence criteria
           reorder = TRUE)")
  }) # What to display when code toggle is true ---------------------------------------------------------------------------------


  output$result_space_matern <- renderUI({
    covparms1 <- calculation_space_matern["covparms"]
    covparms <- round(covparms1[1:4], 4)
    covparms <- round(calculation_space_matern()$covparms[1:4], 4)

    withMathJax(
      helpText(p(strong("Results: Table of Estimated Covariance Parameters"), style = "color: black; font-size: 18px")),
      tags$table(
        style = "
          border-collapse: separate;
          border-spacing: 8px;
          text-align: center;
          margin-top: 10px;
          color: black;
      ",

        tags$thead(
          tags$tr(
            tags$th("\\(\\sigma^2\\)"),
            tags$th("\\(\\rho_s\\)"),
            tags$th("\\(\\nu\\)"),
            tags$th("\\(\\tau^2\\)"),
            tags$th("Converge?")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(covparms[1]),
            tags$td(covparms[2]),
            tags$td(covparms[3]),
            tags$td(covparms[4]),
            tags$td(as.character(calculation_space_matern()$conv))
          )
        )
      ),
      helpText("If `Converge?` = FALSE, this means the numerical optimizer failed to find a stable, optimal solution given the provided maximum number of iterations. Either increase the maximum number of iterations or provide starting values for your covariance parameters.")
    ) #end mathjax
  }) #Create Table for Estimate Covariance Parameters ----------------------------------------------------------------------------

  ## Exponential Space Time ------------------------------------------------------------------------------------------------------

  output$st_exp_param_info <- renderUI({
    if (input$cov_function == "exponential_spacetime") {

      withMathJax(
        helpText(strong("Mathematical Formulation:", style = "color: black; font-size: 18px;"),
                 tags$p(HTML("Parameter vector: \\((\\sigma^2, \\rho_s, \\rho_t, \\tau^2)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p("where:", style = "color: black; font-size: 14px;"),
                 tags$ul(
                   tags$li(HTML("\\(\\sigma^2\\) = process variance"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\rho_s\\) = spatial range parameter"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\rho_t\\) = temporal range parameter"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\tau^2\\) = nugget ratio"), style = "color: black; font-size: 14px;")
                 ),

                 tags$p(HTML("Let the space-time locations be: \\(x = (s_1, \\ldots, s_d, t)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("where the first <em>d<em> components are spatial coordinates and the last component is time."), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("Define the diagonal scaling matrix: \\(D = \\mathrm{diag}(\\rho_s, \\ldots, \\rho_s, \\rho_t)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("The covariance function is parameterized as: \\(M(x,y) = \\sigma^2 \\exp\\!\\left(- \\left\\| D^{-1}(x - y) \\right\\|\\right)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("Equivalently: \\(\\left\\| D^{-1}(x - y) \\right\\| = \\sqrt{ \\frac{\\| s_x - s_y \\|^2}{\\rho_s^2} +  \\frac{\\| t_x - t_y \\|^2}{\\rho_t^2}}\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("The nugget added to the diagonal of the covariance matrix is: \\((\\sigma^2 \\tau^2)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(tags$strong("Note:"), "The nugget is \\(\\sigma^2 \\tau^2\\) not \\(\\tau^2\\)", style = "color: black; font-size: 14px;")
        ) #end helptext
      ) #end mathjax
    }
  }) # Mathematical Formulation for ST Exponential Covariance Function--------------------------------------------------------------

  calculation <- eventReactive(input$run_model, {
    req(data())
    data <- data()

    add_busy_spinner(spin = "cube-grid")
    loc <- data[,c("Longitude","Latitude","t1")]
    locs <- as.matrix(loc)
    response <- as.matrix(data[,c("logPM2.5")])
    X <- as.matrix( rep(1,nrow(locs)))

    if(is.na(input$start_var) & is.na(input$start_spatial) & is.na(input$start_time) & is.na(input$start_nugget)){
      params <- NULL} else {
        params <- c(input$start_var, input$start_spatial, input$start_time, input$start_nugget)
        }

    result <- tryCatch({
      # Potentially failing model code
      fit <- fit_model(response, locs, X, "exponential_spacetime", max_iter = input$max_iter, start_parms= params, convtol = 1e-05, reorder = TRUE)
    }, error = function(e) {
      # Return NULL or a custom message on failure
      return("Model Needs Starting Values")
    })
  }) # Run ST model with Exponential covariance function---------------------------------------------------------------------

  output$code_block_st_model1<- renderPrint({
    cat(" library(GpGp) \n")
    
    cat(" loc <- data[,c('Longitude','Latitude','t1')]
 locs <- as.matrix(loc)
 response <- as.matrix(data[,c('logPM2.5')])
 X <- as.matrix( rep(1,nrow(locs)))

 fit_model(y = response,
           locs = locs,
           X = X,
           covfun_name = 'exponential_spacetime',
           max_iter = maximum_number_of_iterations,
           start_parms=c(variance, spatial range, temporal range, nugget), #optional specified starting values
           convtol = 1e-05, #convergence criteria
           reorder = TRUE)")
  }) # What to display when code toggle is true ---------------------------------------------------------------------------------

  output$result <- renderUI({
    covparms <- round(calculation()$covparms[1:4], 4)

    withMathJax(
      helpText(p(strong("Results: Table of Estimated Covariance Parameters"), style = "color: black; font-size: 18px")),
      tags$table(
        style = "
          border-collapse: separate;
          border-spacing: 8px;
          text-align: center;
          margin-top: 10px;
          color: black;
      ",
        tags$thead(
          tags$tr(
            tags$th("\\(\\sigma^2\\)"),
            tags$th("\\(\\rho_s\\)"),
            tags$th("\\(\\rho_t\\)"),
            tags$th("\\(\\tau^2\\)"),
            tags$th("Converge?")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(covparms[1]),
            tags$td(covparms[2]),
            tags$td(covparms[3]),
            tags$td(covparms[4]),
            tags$td(as.character(calculation()$conv))
          )
        )
      ),
      helpText("If `Converge?` = FALSE, this means the numerical optimizer failed to find a stable, optimal solution given the provided maximum number of iterations. Either increase the maximum number of iterations or provide starting values for your covariance parameters.")
    ) #End mathjax
  }) #Create Table for Estimate Covariance Parameters ----------------------------------------------------------------------------
  

  ## Matern Space Time -----------------------------------------------------------------------------------------------------------
  
  popup_shown3 <- reactiveVal(FALSE)
  observeEvent(input$run_model_st_matern, { 
    if (!popup_shown3()) {
      showModal(modalDialog(
        title = "Important Notice",
        "Due to estimation of additional covariance parameter, this model can take awhile to run. Please be patient.",
        easyClose = FALSE,
        footer = modalButton("OK")
      ))
      popup_shown3(TRUE)  # Update the reactive value so it only runs once
    }
  })
  
  output$st_matern_param_info <- renderUI({
    if (input$cov_function == "matern_spacetime") {
      
      withMathJax(
        helpText(strong("Mathematical Formulation:", style = "color: black; font-size: 18px;"),
                 tags$p(HTML("Parameter vector: \\((\\sigma^2, \\rho_s, \\rho_t, \\tau^2)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p("where:", style = "color: black; font-size: 14px;"),
                 tags$ul(
                   tags$li(HTML("\\(\\sigma^2\\) = process variance"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\rho_s\\) = spatial range parameter"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\rho_t\\) = temporal range parameter"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\nu\\) = smoothness parameter"), style = "color: black; font-size: 14px;"),
                   tags$li(HTML("\\(\\tau^2\\) = nugget ratio"), style = "color: black; font-size: 14px;")
                 ),
                 
                 tags$p(HTML("Let the space-time locations be: \\(x = (s_1, s_2, t)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("Define the diagonal scaling matrix: \\(D = \\mathrm{diag}(\\rho_s, \\rho_s, \\rho_t)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("Define the scaled space-time distance: \\( r = \\left\\| D^{-1}(x - y) \\right\\| = \\sqrt{ \\frac{(s_{1x} - s_{1y})^2 + (s_{2x} - s_{2y})^2}{\\rho_s^2} + \\frac{(t_x - t_y)^2}{\\rho_t^2} } \\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("The Matern covariance function is parameterized as: \\(M(x,y) = \\sigma^2 \\frac{1}{2^{\\nu-1}\\Gamma(\\nu)} \\, r^{\\nu} K_{\\nu}(r)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(HTML("where:"), style = "color: black; font-size: 14px;"),
                 
                 tags$ul(
                   tags$li(HTML("\\(K_{\\nu}(\\cdot)\\) = is the modified Bessel function of the second kind")), style = "color: black; font-size: 14px;"),
                 
                 tags$p(HTML("The nugget added to the diagonal of the covariance matrix is: \\((\\sigma^2 \\tau^2)\\)"), style = "color: black; font-size: 14px;"),
                 tags$p(tags$strong("Note:"), "The nugget is \\(\\sigma^2 \\tau^2\\) not \\(\\tau^2\\)", style = "color: black; font-size: 14px;")
        ) #end helptext
      ) #end mathjax
    }
  }) # Mathematical Formulation for ST Matern Covariance Function--------------------------------------------------------------------

  calculation_st_matern <- eventReactive(input$run_model_st_matern, {
    req(data())
    data <- data()
    
    add_busy_spinner(spin = "cube-grid")
    loc <- data[,c("Longitude","Latitude","t1")]
    locs <- as.matrix(loc)
    response <- as.matrix(data[,c("logPM2.5")])
    X <- as.matrix( rep(1,nrow(locs)))
    
    if(is.na(input$start_var_st_matern) & is.na(input$start_spatial_st_matern) & is.na(input$start_time_st_matern)& is.na(input$start_smooth_st_matern) & is.na(input$start_nugget_st_matern)){
      params <- NULL} else {
        params <- c(input$start_var_st_matern, input$start_spatial_st_matern, input$start_time_st_matern, input$start_smooth_st_matern, input$start_nugget_st_matern)
      }
    
    result <- tryCatch({
      # Potentially failing model code
      fit <- fit_model(response, locs, X, "matern_spacetime", max_iter = input$max_iter_st_matern, start_parms= params, convtol = 1e-05, reorder = TRUE)
    }, error = function(e) {
      # Return NULL or a custom message on failure
      return("Model Needs Starting Values")
    })
  }) # Run ST model with Matern covariance function-----------------------------------------------------------------------------------
  
  output$code_block_st_model2<- renderPrint({
    cat(" library(GpGp) \n")
    
    cat(" loc <- data[,c('Longitude','Latitude','t1')]
 locs <- as.matrix(loc)
 response <- as.matrix(data[,c('logPM2.5')])
 X <- as.matrix( rep(1,nrow(locs)))

 fit_model(y = response,
           locs = locs,
           X = X,
           covfun_name = 'matern_spacetime',
           max_iter = maximum_number_of_iterations,
           start_parms=c(variance, spatial range, temporal range, smooth, nugget), #optional specified starting values
           convtol = 1e-05, #convergence criteria
           reorder = TRUE)")
  }) # What to display when code toggle is true ---------------------------------------------------------------------------------
  
  output$result_st_matern <- renderUI({
    covparms <- round(calculation_st_matern()$covparms[1:5], 4)
    
    withMathJax(
      helpText(p(strong("Results: Table of Estimated Covariance Parameters"), style = "color: black; font-size: 18px")),
      tags$table(
        style = "
          border-collapse: separate;
          border-spacing: 8px;
          text-align: center;
          margin-top: 10px;
          color: black;
      ",
        tags$thead(
          tags$tr(
            tags$th("\\(\\sigma^2\\)"),
            tags$th("\\(\\rho_s\\)"),
            tags$th("\\(\\rho_t\\)"),
            tags$th("\\(\\nu\\)"),
            tags$th("\\(\\tau^2\\)"),
            tags$th("Converge?")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(covparms[1]),
            tags$td(covparms[2]),
            tags$td(covparms[3]),
            tags$td(covparms[4]),
            tags$td(covparms[5]),
            tags$td(as.character(calculation_st_matern()$conv))
          )
        )
      ),
      helpText("If `Converge?` = FALSE, this means the numerical optimizer failed to find a stable, optimal solution given the provided maximum number of iterations. Either increase the maximum number of iterations or provide starting values for your covariance parameters.")
    ) #End mathjax
  }) #Create Table for Estimate Covariance Parameters ----------------------------------------------------------------------------
  
  
  ####################### Prediction TAB   #####################################

  output$message4 <- renderUI({
    if (is.null(input$upload)) {
      h4("Please upload a file to proceed.")
    }
  }) #MESSAGE TO DISPLAY IF NO DATASET UPLAODED -------------------------------------------
  
  
  #BEGIN: Create Dynamically added checkboxes as models are ran ---------------------------
  rv <- reactiveValues(
    controls = list(),
    count = 0,
    ids = character()
  )
  
  button_map <- list(
      run_idw = "IDW",
      run_model_space_exponential = "Exponential Isotropic",
      run_model_space_matern = "Matern Isotropic",
      run_model = "Exponential Space-Time",
      run_model_st_matern = "Matern Space-Time"
    )
  

    add_choice_group <- function(choices, session) {
      rv$count <- rv$count + 1
      id <- paste0("choice_", rv$count)
      rv$ids <- c(rv$ids, id)
      
      new_control <- checkboxGroupInput(
        inputId = id,
        label = NULL,
        choices = choices
      )
      
      rv$controls <- c(rv$controls, list(new_control))
      
      observeEvent(input[[id]], {
        val <- input[[id]]
        if (length(val) > 1) {
          updateCheckboxGroupInput(
            session,
            id,
            selected = tail(val, 1)
          )
        }
        if (length(val) > 0) {
          for (other in setdiff(rv$ids, id)) {
            updateCheckboxGroupInput(
              session,
              other,
              selected = character(0)
            )
          }
        }
      }, ignoreInit = TRUE)
    }
      
      # ONE generic handler for ALL buttons
      lapply(names(button_map), function(btn) {
        observeEvent(input[[btn]], {
          add_choice_group(button_map[[btn]], session)
        })
      })
      
      #END: Create Dynamically added checkboxes as models are ran ---------------------------
      
      output$choice_container <- renderUI({
        tagList(
          helpText(
            p(strong("Step 1:"), "Of your previously run models from the ",
            strong("Modeling"),
            " tab, which would you like to use to predict? Once you select an option, you will be able to upload your model prediction data set.", style = "color: black; font-size: 18px;"
          ), 
          br()),
          tagList(rv$controls)
        )
      }) #Display options of models that were ran --------------------------------------------
    
      current_selection <- reactive({
        vals <- unlist(lapply(rv$ids, function(id) input[[id]]))
        vals <- vals[nzchar(vals)]
        if (length(vals) == 0) return(NULL)
        vals[1]
      }) ## Only allow for one option to be displayed at a time. Reactive to use in server ---- 
  
      output$current_selection <- renderText({
        current_selection()
      }) ## Allow to use options in UI --------------------------------------------------------
      
      outputOptions(output, "current_selection", suspendWhenHidden = FALSE)
  
  
  # Spatial Predictions -----------------------------------------------------------------------

  data_pred_spatial <- reactive({
    req(input$upload_spatial_pred)

    ext <- tools::file_ext(input$upload_spatial_pred$name)
    switch(ext,
           csv = vroom::vroom(input$upload_spatial_pred$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")) #Verify the data set is a csv file

  }) #upload spatial only prediction dataset
      
      
  ### IDW Prediction  ### ----------------------------------------------------------------------    
      
  pred_idw <- eventReactive(input$run_pred_idw, {  
    pred.locs <- data_pred_spatial()
    data <- data()
    
    m <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 28992)
    m2 <- st_as_sf(pred.locs, coords = c("Longitude", "Latitude"), crs = 28992)
    
    idw_result <- idw(
      formula = logPM2.5 ~ 1,
      locations = m,
      newdata = m2,
      idp = input$idw_power
    )
    
  }) #Make IDW Predictions using uploaded dataset ----------------------------------------------
      
      output$idw_predictions_plot <- renderPlotly({
        data <- pred_idw()
        
        idw_df <- data %>%
          cbind(st_coordinates(data)) %>%
          as.data.frame() # Extract coordinates
        
        plot_ly(
          data = idw_df,
          lon = ~X,
          lat = ~Y,
          color = ~var1.pred,
          colors = "viridis",
          type = "scattermapbox",
          mode = "markers",
          marker = list(size = 6),
          text = ~paste("logPM2.5:", round(var1.pred, 4)),
          hoverinfo = "text"
        ) %>%
          layout(
            mapbox = list(
              style = "open-street-map",
              zoom = 4.5,
              center = list(
                lon = mean(idw_df$X),
                lat = mean(idw_df$Y)
              )
            ),
            margin = list(l = 0, r = 0, t = 0, b = 0)
          )
      }) #Make spatial plot of predictions ----------------------------------------------------------------------------------------------------------------    
   
      output$download_idw_pred <- downloadHandler(
        filename = function() {
          paste("predictions-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          pred <- data.frame(data_pred_spatial()[,c("site_id", "Longitude","Latitude")], predicted = pred_idw()$var1.pred)
          write.csv(pred, file)
        }
      ) #Download Spatial GP Predictions -----------------------------------------------------------------------------------------------------------------
      
      output$spat_idw_pred_result <- renderUI({
        plotlyOutput("idw_predictions_plot")
      }) # Display Plot of Spatial GP Predictions --------------------------------------------------------------------------------------------------------
      
      output$idw_pred_button <- renderUI({
        conditionalPanel(
          condition = "input.run_pred_idw > 0",
          downloadButton("download_idw_pred", "Download IDW Prediction Dataset")
        )
      })     
      
  ### Spatial GP Prediction  ### ---------------------------------------------------------------
  
  model_pred_spatial <- eventReactive(input$run_pred_space_exp, {
    if (current_selection() == "Exponential Isotropic") {
      
    pred.locs <- data_pred_spatial()

    longlat_pred<-pred.locs[,c("Longitude","Latitude")]
    locs_pred<-as.matrix(cbind(longlat_pred))
    X_pred <- as.matrix(rep(1,nrow(locs_pred)))

    prediction<-predictions(calculation_space_exponential(),locs_pred, X_pred, covparms = calculation_space_exponential()$covparms, covfun_name = calculation_space_exponential()$covfun_name, y_obs = calculation_space_exponential()$y,
                            locs_obs = calculation_space_exponential()$locs, X_obs= calculation_space_exponential()$X, beta=calculation_space_exponential()$betahat, m = 60, reorder = TRUE)

    data <- data.frame(Longitude = pred.locs$Longitude, Latitude = pred.locs$Latitude, predicted = prediction)
    
    }

  }) #Make Spatial GP Predictions using uploaded dataset -----------------------------------------
      
  model_pred_spatial2 <- eventReactive(input$run_pred_space_exp, {
    
    if (current_selection() == "Matern Isotropic") {
      
    pred.locs <- data_pred_spatial()

    longlat_pred<-pred.locs[,c("Longitude","Latitude")]
    locs_pred<-as.matrix(cbind(longlat_pred))
    X_pred <- as.matrix(rep(1,nrow(locs_pred)))

    prediction<-predictions(calculation_space_matern(),locs_pred, X_pred, covparms = calculation_space_matern()$covparms, covfun_name = calculation_space_matern()$covfun_name, y_obs = calculation_space_matern()$y,
                              locs_obs = calculation_space_matern()$locs, X_obs= calculation_space_matern()$X, beta=calculation_space_matern()$betahat, m = 60, reorder = TRUE)
    
    data <- data.frame(Longitude = pred.locs$Longitude, Latitude = pred.locs$Latitude, predicted = prediction)
    
    }

  })    

  output$exp_space_predictions_plot <- renderPlotly({
    
    if (current_selection() == "Exponential Isotropic") {
    data <- model_pred_spatial()
    } else if (current_selection() == "Matern Isotropic") {
      data <- model_pred_spatial2()
    }

    plot_ly(
      data = data,
      lon = ~Longitude,
      lat = ~Latitude,
      type = 'scattermapbox',
      mode = 'markers',
      color = ~predicted, # Color by a categorical variable, e.g., status
      marker = list(size = 10, opacity = 0.8),
      showlegend = FALSE,
      text = ~paste("Predicted logPM2.5: ", round(predicted, 4)),
      hoverinfo = "text"
    ) %>%
      layout(
        mapbox = list(
          style = "open-street-map", #zoom into California
          zoom = 4,
          center = list(
            lon = mean(data$Longitude),
            lat = mean(data$Latitude)
          )
        )
      )
  }) #Make spatial plot of predictions ----------------------------------------------------------------------------------------------------------------

  output$download_space_exp_data <- downloadHandler(
    filename = function() {
      paste("predictions-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if (current_selection() == "Exponential Isotropic") {
        pred <- data.frame(data_pred_spatial()[,c("site_id", "Longitude","Latitude")], predicted = model_pred_spatial()[,"predicted"])
      } else if (current_selection() == "Matern Isotropic") {
        pred <- data.frame(data_pred_spatial()[,c("site_id", "Longitude","Latitude")], predicted = model_pred_spatial2()[,"predicted"])
      }
      write.csv(pred, file)
    }
  ) #Download Spatial GP Predictions -----------------------------------------------------------------------------------------------------------------

  output$spat_exp_pred_result <- renderUI({
    plotlyOutput("exp_space_predictions_plot")
  }) # Display Plot of Spatial GP Predictions --------------------------------------------------------------------------------------------------------

  output$space_pred_button <- renderUI({
    conditionalPanel(
      condition = "input.run_pred_space_exp > 0",
      downloadButton("download_space_exp_data", "Download Spatial Prediction Dataset")
    )
  }) # Download Button of Spatial GP Predictions -----------------------------------------------------------------------------------------------------

  ## Spatial-Temporal Exponential Prediction ---------------------------------------------------------------------------------------------------------
      
  data_pred_st <- reactive({
    req(input$upload_st_pred)

    ext <- tools::file_ext(input$upload_st_pred$name)
    switch(ext,
           csv = vroom::vroom(input$upload_st_pred$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")) #Verify the data set is a csv file

  }) #upload spatial-temporal prediction dataset ------------------------------------------------------------------------------------------------------


  model_pred_st <- eventReactive(input$run_pred_st, {
    if (current_selection() == "Exponential Space-Time") {
    pred.locs <- data_pred_st()[,-1]

    t_pred<-pred.locs[,"t"]
    longlat_pred<-pred.locs[,c("Longitude","Latitude")]
    locs_pred<-as.matrix(cbind(longlat_pred,t_pred))
    X_pred <- as.matrix(rep(1,nrow(locs_pred)))
    
    prediction<-predictions(calculation(),locs_pred, X_pred, covparms = calculation()$covparms, covfun_name = calculation()$covfun_name, y_obs = calculation()$y,
                            locs_obs = calculation()$locs, X_obs= calculation()$X, beta=calculation()$betahat, m = 60, reorder = TRUE)
    
    data <- data.frame(Longitude = pred.locs$Longitude, Latitude = pred.locs$Latitude, predicted = prediction)
    }
  }) #Make predictions using uploaded dataset for EXP ST -------------------------------------------------------------------------------------------
  
  model_pred_st2 <- eventReactive(input$run_pred_st, {
    if (current_selection() == "Matern Space-Time") {
    pred.locs <- data_pred_st()[,-1]
    
    t_pred<-pred.locs[,"t"]
    longlat_pred<-pred.locs[,c("Longitude","Latitude")]
    locs_pred<-as.matrix(cbind(longlat_pred,t_pred))
    X_pred <- as.matrix(rep(1,nrow(locs_pred)))
    
      prediction<-predictions(calculation_st_matern(),locs_pred, X_pred, covparms = calculation_st_matern()$covparms, covfun_name = calculation_st_matern()$covfun_name, y_obs = calculation_st_matern()$y,
                              locs_obs = calculation_st_matern()$locs, X_obs= calculation_st_matern()$X, beta=calculation_st_matern()$betahat, m = 60, reorder = TRUE)
    
    data <- data.frame(Longitude = pred.locs$Longitude, Latitude = pred.locs$Latitude, predicted = prediction)
  }
  }) #Make predictions using uploaded dataset for MAT ST -------------------------------------------------------------------------------------------

  output$predictions_plot <- renderPlotly({
    
    if (current_selection() == "Exponential Space-Time") {
      dat <- data.frame(data_pred_st()[,c("site_id", "Longitude","Latitude", "t")],  predicted = model_pred_st()[,"predicted"])
    } else if (current_selection() == "Matern Space-Time") {
      dat <- data.frame(data_pred_st()[,c("site_id", "Longitude","Latitude", "t")],  predicted = model_pred_st2()[,"predicted"])
    }

    plot_ly(
    data = dat,
    lon = ~Longitude,
    lat = ~Latitude,
    type = 'scattermapbox',
    mode = 'markers',
    color = ~predicted, # Color by a categorical variable, e.g., status
    frame = ~t, # Animate across time points
    marker = list(size = 10, opacity = 0.8),
    showlegend = FALSE,
    text = ~paste("logPM2.5: ", round(predicted, 4)),
    hoverinfo = "text"
  ) %>%
    layout(
      mapbox = list(
        style = "open-street-map", #zoom into california
        zoom = 4,
        center = list(
          lon = mean(dat$Longitude),
          lat = mean(dat$Latitude)
        )
      )
    )
  }) # Make plot of predictions for ST Predictions  -------------------------------------------------------------------------------------------

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("predictions-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if (current_selection() == "Exponential Space-Time") {
      pred <- data.frame(data_pred_st()[,c("site_id", "Longitude","Latitude", "t")], predicted = model_pred_st()[,"predicted"])
      } else if (current_selection() == "Matern Space-Time"){
        pred <- data.frame(data_pred_st()[,c("site_id", "Longitude","Latitude", "t")], predicted = model_pred_st2()[,"predicted"])
      }
      write.csv(pred, file, row.names = FALSE)
    } 
  ) # Make download file for ST Predictions  --------------------------------------------------------------------------------------------------

  output$st_pred_result <- renderUI({
    plotlyOutput("predictions_plot")
  }) #UI SIDE for render Plot -----------------------------------------------------------------------------------------------------------------

  output$st_pred_button <- renderUI({
    conditionalPanel(
      condition = "input.run_pred_st > 0",
      downloadButton("downloadData", "Download Prediction Dataset")
    ) 
  }) #UI SIDE for rendering download button  --------------------------------------------------------------------------------------------------
  
  ## Model Comparison Tab ---------------------------------------------------------------------------------------------------------------------
  
  output$choice_container2 <- renderUI({
    tagList(
      helpText(
        p(strong("Step 1:"), "Currently this tab will only calculate Model Comparison Metrics for models in which you have calculated a prediction on in the", strong("Prediction"),
          "tab. You will be able to run the Model Comparison Metrics for as many models as you want. Select a model and click the", strong("Calculate Predictions"), "button and repeat for each additional model. New rows will be added to the outputted tables.", style = "color: black; font-size: 18px"
        ), 
        br()),
      
      tagList(rv$controls),
      
      helpText(
        p(strong("Note:"), "To be able to calculate Model Comparison Metrics, your dataset uploaded in the", strong("Prediction"), "tab must have a column called logPM2.5. Metrics will only be able to be calculated for Models which you have predicted on.", style = "color: black; font-size: 16px"
        ), 
        br()),
    )
  }) #Dyanmic Container for adding model each time one is ran ----------------------------------------------------------------------------------
  
  output$show_comp_button <- renderUI({
    req(input$run_pred_st > 0 || input$run_pred_idw  > 0 || input$run_pred_space_exp > 0)
    actionButton("run_model_comp_but", "Calculate Model Comparison Metrics")
  }) # Dynamically button to run model comparison as long as one of prediction datas ets exist --------------------------------------------------
  
  output$rmse_info <- renderUI({
      withMathJax(
        helpText(strong("Mathematical Formulation:", style = "color: black;"),
                 tags$p(HTML("$$\\text{RMSE} = \\sqrt{\\frac{\\sum_{i=1}^{n}(y_i - \\hat{y}_i)^2}{n}}$$"), style = "color: black;")
        ) #End HelpText
      ) #end mathjax
  }) ## INFO ON RMSE ----------------------------------------------------------------------------------------------------------------------------
  
  output$mae_info <- renderUI({
    withMathJax(
      helpText(strong("Mathematical Formulation:", style = "color: black;"),
               tags$p(HTML("$$\\text{MAE} = \\frac{1}{n} \\sum^{n}_{i=1}|y_i - \\hat{y}_i|$$"), style = "color: black;")
      ) #End HelpText
    ) #end mathjax
  }) ## INFO ON MAE ----------------------------------------------------------------------------------------------------------------------------
  
  output$corr_info <- renderUI({
    withMathJax(
      helpText(strong("Mathematical Formulation:", style = "color: black;"),
               tags$p(HTML("$$\\text{Correlation} = cor(Y, \\hat{Y})$$"), style = "color: black;")
      ) #End HelpText
    ) #end mathjax
  }) ## INFO ON CORR ----------------------------------------------------------------------------------------------------------------------------

  current_selection2 <- reactive({
    vals <- unlist(lapply(rv$ids, function(id) input[[id]]))
    vals <- vals[nzchar(vals)]
    if (length(vals) == 0) return(NULL)
    vals[1]
  }) ## Only allow for one option to be chosen at a time. Reactive to use in server -----------------------------------------------------------

  
  ## BEGIN: Create Dyanamic Table where add rows based on models ran --------------------------------------------------------------------------
  results <- reactiveValues(
    history = data.frame(
      Model = character(),
      RMSE = numeric(),
      Timestamp = as.POSIXct(character()),
      stringsAsFactors = FALSE
    )
  )
  
  results2 <- reactiveValues(
    history = data.frame(
      Model = character(),
      MAE = numeric(),
      Timestamp = as.POSIXct(character()),
      stringsAsFactors = FALSE
    )
  )
  
  results3 <- reactiveValues(
    history = data.frame(
      Model = character(),
      Correlation = numeric(),
      Timestamp = as.POSIXct(character()),
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$run_model_comp_but, {
    req(current_selection2())
    sel <- current_selection2()
    
    pred <- switch(
      sel,
      "IDW" = data.frame(data_pred_spatial()[, c("site_id", "Longitude", "Latitude", "logPM2.5")], predicted = pred_idw()$var1.pred),
      "Exponential Isotropic" = data.frame(data_pred_spatial()[, c("site_id", "Longitude", "Latitude", "logPM2.5")], predicted = model_pred_spatial()[, "predicted"]),
      "Matern Isotropic" = data.frame(data_pred_spatial()[, c("site_id", "Longitude", "Latitude", "logPM2.5")], predicted = model_pred_spatial2()[, "predicted"]),
      "Exponential Space-Time" = data.frame(data_pred_st()[, c("site_id", "Longitude", "Latitude", "t", "logPM2.5")], predicted = model_pred_st()[, "predicted"]),
      "Matern Space-Time" = data.frame(data_pred_st()[, c("site_id", "Longitude", "Latitude", "t", "logPM2.5")], predicted = model_pred_st2()[, "predicted"])
    )
    
    rmse <- Metrics::rmse(pred$logPM2.5, pred$predicted)
    
    results$history <- rbind(
      results$history,
      data.frame(
        Model = sel,
        RMSE = round(rmse, 7)
      )
    )
  })
  
  observeEvent(input$run_model_comp_but, {
    req(current_selection2())
    sel <- current_selection2()
    
    pred <- switch(
      sel,
      "IDW" = data.frame(data_pred_spatial()[, c("site_id", "Longitude", "Latitude", "logPM2.5")], predicted = pred_idw()$var1.pred),
      "Exponential Isotropic" = data.frame(data_pred_spatial()[, c("site_id", "Longitude", "Latitude", "logPM2.5")], predicted = model_pred_spatial()[, "predicted"]),
      "Matern Isotropic" = data.frame(data_pred_spatial()[, c("site_id", "Longitude", "Latitude", "logPM2.5")], predicted = model_pred_spatial2()[, "predicted"]),
      "Exponential Space-Time" = data.frame(data_pred_st()[, c("site_id", "Longitude", "Latitude", "t", "logPM2.5")], predicted = model_pred_st()[, "predicted"]),
      "Matern Space-Time" = data.frame(data_pred_st()[, c("site_id", "Longitude", "Latitude", "t", "logPM2.5")], predicted = model_pred_st2()[, "predicted"])
    )
    
    mae <- Metrics::mae(pred$logPM2.5, pred$predicted)
    
    results2$history <- rbind(
      results2$history,
      data.frame(
        Model = sel,
        MAE = round(mae, 7)
      )
    )
  })
  
  observeEvent(input$run_model_comp_but, {
    req(current_selection2())
    sel <- current_selection2()
    
    pred <- switch(
      sel,
      "IDW" = data.frame(data_pred_spatial()[, c("site_id", "Longitude", "Latitude", "logPM2.5")], predicted = pred_idw()$var1.pred),
      "Exponential Isotropic" = data.frame(data_pred_spatial()[, c("site_id", "Longitude", "Latitude", "logPM2.5")], predicted = model_pred_spatial()[, "predicted"]),
      "Matern Isotropic" = data.frame(data_pred_spatial()[, c("site_id", "Longitude", "Latitude", "logPM2.5")], predicted = model_pred_spatial2()[, "predicted"]),
      "Exponential Space-Time" = data.frame(data_pred_st()[, c("site_id", "Longitude", "Latitude", "t", "logPM2.5")], predicted = model_pred_st()[, "predicted"]),
      "Matern Space-Time" = data.frame(data_pred_st()[, c("site_id", "Longitude", "Latitude", "t", "logPM2.5")], predicted = model_pred_st2()[, "predicted"])
    )
    
    cor <- cor(pred$logPM2.5, pred$predicted)
    
    results3$history <- rbind(
      results3$history,
      data.frame(
        Model = sel,
        Correlation = round(cor, 7)
      )
    )
  })
  
  output$rmse_table <- renderTable({
    df <- results$history
    df$RMSE <- format(round(df$RMSE, 4), nsmall = 4)
    df
  })
  
  output$mae_table <- renderTable({
    df <- results2$history
    df$MAE <- format(round(df$MAE, 4), nsmall = 4)
    df
  })
  
  output$corr_table <- renderTable({
    df <- results3$history
    df$Correlation <- format(round(df$Correlation, 4), nsmall = 4)
    df
  })
  
  ## END: Create Dyanamic Table where add rows based on models ran --------------------------------------------------------------------------

} #END SERVER

shinyApp(ui = ui, server = server) #RUN THE APPLICATION
