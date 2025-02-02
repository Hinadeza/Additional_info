## Code for r shiny app
# Made by: Hina Gluza
# Contact: hinagluza@icloud.com

rm(list=ls())

# Load all the packages

source("R/packages.R")
source("R/functions.R")

# UI page

ui <- page_fluid(theme = shinytheme("cerulean"),
                 
                 navbarPage("Synthos Shiny",
                            tabPanel("Introduction", tags$style ("li a {font-size: 20px;font-weight: bold;}"),
                                     fluidRow(
                                       column(4, 
                                              card(height = 350, class = "card border-dark mb-3",
                                                   card_header(class="card-header bg-transparent border-dark",
                                                               h3(strong("What is synthetic data?", 
                                                                         br(), "Why use it?")), align = "center"),
                                                   card_body(h4("Synthetic data are generated to resemble the real dataset. It has same statistical proprieties and structure.", 
                                                                            br(), "Synthetic data are used to avoid privacy and confidentiality concerns of real-data.", tags$sup("1"), 
                                                                            " In marine ecology research, his approach enables to address the lack of long-term observations, sparsity of monitoring data, uneven sampling ",
                                                                            br(),
                                                                            "In this application, we propose a new approach to create synthetic data using coral-reef as testing case study. 
                                                              The next tabs will bring you through the four steps of data generation with functionalities to validate and download your data table.", 
                                                                style = "color:black;"))),
                                                  br(),
                                                  tags$a(href = "https://github.com/open-AIMS/synthos", "Synthos package here", class = "btn btn-primary", align ="center"),
                                                  br(),br(),
                                                  card(height = 225, class = "card border-info mb-3",
                                                       card_header(class="card-header bg-transparent border-info",
                                                                   h5(style = "margin-top: 10px;", strong("References"), align="center")),
                                                  card_body(h6(HTML(paste0(tags$sup("1"), "Raghunathan, T. E. (2021). Synthetic Data.", tags$i("Annual Review of Statistics and Its Application"),", 8(Volume 8, 2021), 129‑140. https://doi.org/10.1146/annurev-statistics-040720-031848" ))),
                                                  h6(HTML(paste0(tags$sup("2"), "Norse, E. A. et Crowder, L. B. (dir.). (2005). Marine conservation biology: the science of maintaining the sea’s biodiversity.", tags$i( "Island Press.")))),
                                                  h6(HTML(paste0(tags$sup("3"), "Underwood,A.J. (1989). The analysis of stress in natural populations.", tags$i("Biological Journal of the Linnean Society"), ",37(1‑2), 51‑78. https://doi.org/10.1111/j.1095-8312.1989.tb02005.x"))),
                                                  h6(HTML(paste0(tags$sup("4"), "Lindgren, F., H. Rue, and J. Lindström. (2011). An Explicit Link Between Gaussian Fields and Gaussian Markov Random Fields: The Stochastic Partial Differential Equation Approach (with Discussion) ", tags$i("J. R. Statist. Soc." )))
                                                  )))),
                                       
                                       column(8, 
                                              card(height = 660, class = "card border-dark mb-3",
                                                    card_header(class="card-header bg-transparent border-dark",
                                                                 h3(strong("How to generate synthetic data?")), align="center"),
                                              card_body(h4(strong("1.	Spatial and temporal domain creation")), br(),
                                                  h4("We start by defining limits of our spatial domain where we will create coral reefs. 
                                                               To be realistic, the entire domain will not be saturated with coral reefs: only a small fraction will contain them. 
                                                               Coral reefs will be represented by irregular hollow polygons and heterogeneously distributed across the spatial domain.", 
                                                     br(),
                                                     "The spatial domain is then discretized using Stochastic Partial Differential Equations (SPDE) framework from the R INLA package.
                                                               The SPDE mesh breaks the domain into a network of connected triangles to represent complex spatial processes including modelling disturbances and species abundance across space.",
                                                     br(),
                                                     tags$i("Parameters associated with the spatial variability can be specified by the user."),
                                                     br(),
                                                     "The spatial domain is modeled across multiple time steps, seamlessly incorporating the temporal dimension into the synthetic data. 
                                                               In our case study, we generate a distinct spatial domain for each year, capturing dynamic changes over time.",
                                                     style = "color: black;"),
                                                  br(),
                                                  h4(strong("2.Baseline construction of coral cover")), br(),
                                                  h4("Our testing case study focuses on the spatio-temporal variation of three marine benthic communities living on the seafloor.",
                                                     br(),
                                                     "First class is hard corals– also known as “reef-builders” –  a key functional group in coral reef. It is widely used in studies on coral reef responses to disturbances. This class of coral has the particularity to produce a rigid skeleton made of calcium carbonate.",
                                                     br(),
                                                     "Second class is soft corals, they resemble to plants. Unlike hard corals, they do not produce a rigid skeleton.",
                                                     br(),
                                                     "Finally, we have macroalgae, known as seaweeds, which are multicellular and macroscopic autotrophs.This class is often considered in competition with corals.",tags$sup("4"),
                                                     br(),
                                                     "The generation of hard coral, soft coral and algae cover in space and time uses baseline values represented by the spatial pattern for the year prior to sampling defined at time t0. 
                                                      The baseline layer is defined by a mathematical model projected on the SPDE mesh. In our case study, the baseline layer represents a gradient of cover values from west to east.",
                                                     br(),
                                                     tags$i("The user can specify values of annual growth for hard and soft corals during the generation of cover values at time t+1."),
                                                     style = "color: black;"),
                                                  br(),
                                                  h4(strong("3. Generation of disturbances")), br(),
                                                  h4("Coral reefs are highly sensitive marine ecosystems, vulnerable to a range of disturbances.", tags$sup("2,3"),
                                                     "Among these, cyclones and mass coral bleaching stand out as the most devastating, often causing a significant and rapid decline in benthic community coverage.",
                                                     br(),
                                                     "As part of the synthetic data generation, three disturbances can occur in the domain:",
                                                     br(),"-	Cyclones are defined as the probability of cyclone occurrence per year. 
                                                      It is based on a mathematical model representing the path of a cyclone with higher intensity and more spatially correlated values closer to the path",
                                                     br(),"-	Coral bleaching is represented by the Degree Heating Weeks values (DHW). 
                                                      A mathematical model is used to represent the spatio-temporal footprint of DHWs.",
                                                     br(),"-	The category Other represent more fine-scale disturbances that can occur on coral reefs. 
                                                      It is represented by a random noise with low spatial correlation.", 
                                                     br(),
                                                     "Disturbances are then projected on the INLA mesh to obtain values bounded between 0 and 1 across the spatial domain for each annual survey.", 
                                                     br(),
                                                     tags$i("The user can define the relative influence of each disturbance across the entire simulation"),
                                                     style = "color: black;"),
                                                  br(),
                                                  h4(strong("4. Generation of sampling designs")), br(),
                                                  h4("Creating a sampling design involves selecting specific locations within the spatial domain and recording associated values for species abundance and disturbances. 
                                                   In case study, we adopt a spatial hierarchy—coral reefs, sites within reefs, and transects within sites—mirroring the design of the long-term coral reef monitoring program by the Australian Institute of Marine Science.",
                                                     br(),
                                                     tags$i("The user defines the number of reefs, sits and transects."),
                                                     br(),
                                                     "Finally, the characteristics of the data are defined based on the collection methodology.
                                                   We estimate cover abundance using a point count method. 
                                                   This approach mirrors data produced by artificial intelligence base marine monitoring",
                                                     br(),
                                                     tags$i("The user defines the number of images per transect to be used and the number of classification points within an image on which hard coral, soft coral or algae is labelled."),
                                                     "The sum of each class at the transect-level is then used a the variable of interest named « COUNT ».",
                                                     style = "color: black;")),
                                                  br())))),
                            
                            tabPanel("Create your data", 
                                     
                                     page_fillable(
                                       layout_columns(
                                         card(h3(strong("Follow the steps in order, instructions are developped for each")), class = "primary"),
                                         navset_card_underline(
                                           
                                           nav_panel(h3("1. Spatial domain",style = "font-size:16px; color:black;"),
                                                     fluidRow(box(width =4, title = h4(strong("To create your spatial domain :"),style = "color: black;"), 
                                                                  h5("1. Select the polygon icon on the left",
                                                                     br(),
                                                                     "2. Draw a shape around area of interest", 
                                                                     br(),
                                                                     "3. Select “finish” beside the polygon icon", 
                                                                     br() ,
                                                                     "4. See your spatial domain on the right" ,
                                                                     style = "color: black;")),
                                                              box(width=4, h4(strong("If you want to delete your polygon :"), style = "color: black;"),
                                                                  h5("1.Select the trash icon on the left",
                                                                     br(),
                                                                     "2.Click on the polygon(s) you want to delete",
                                                                     br(),
                                                                     "3. Select “Done”", style = "color: black;")),
                                                              box(width = 4, h5("When you are satisfied of your domain click on the button bellow",style = "color:black;",
                                                                                br()),
                                                                  textOutput("area"), actionButton("Next", "Next", size = "xs", block = TRUE)),
                                                              column(6,editModUI("draw")),
                                                              column(6,plotOutput("spatial_domain")) 
                                                     )),
                                           
                                           
                                           nav_panel(h3("2. Temporal domain",style = "font-size:16px; color:black;"),
                                                     layout_columns(
                                                       h4(strong("Select the number of year for your simulation"),style = "color: black;"),
                                                       card(
                                                         card_header(class ="bg-primary",
                                                                     (h4(strong("Years"), style = "color:white; font-size:14px; text-align:center;"))),
                                                         
                                                         sliderInput("years",
                                                                     "Number of years:",
                                                                     min = 1,
                                                                     max = 50,
                                                                     value = 15)),
                                                       
                                                       card(
                                                         card_header(h4(strong("Locations sampling"), style = "color:white; font-size:14px; text-align:center;"), class = "bg-primary"),
                                                         radioButtons("loc","location sampling",
                                                                      c("fixed"= "fixed",
                                                                        "random" = "random"))), col_widths = c(12,-1, 3, -2, 3))
                                           ),
                                           
                                           
                                           nav_panel(h3("3. Coral baseline",style = "font-size:16px; color:black;"),
                                                     h4(strong("Spatial patterns at t0"),style = "color:black;" ),
                                                     h5(strong("The baseline is fixed for now."),
                                                        br()," Three benthic classes are considered here : Hard coral, Soft coral and Macroalgae.",
                                                        br(),"We assume that macroalgae simply fills the space not occupied by corals. Consequently, there is not baseline generated for this class.
                                                    It will be calculated from the hard and soft coral spatio-temporal projections later.",
                                                        style = "color:black;"), 
                                                     card(layout_column_wrap(width = 1/2,
                                                                             plotOutput("baseline_HCC"),
                                                                             plotOutput("baseline_SC")))),
                                           
                                           
                                           nav_panel(h3("4. Disturbances effect",style = "font-size:16px; color:black;"),
                                                     h4(strong("Define disturbances influence on the coral cover"),style = "color: black;"),
                                                     h5("First, simulate disturbances on your spatial domain across time by clicking on the button bellow",style = "color: black;"),
                                                     actionButton("simulation", "simulation", size = "xs", align = "center"),
                                                     h5("Then you can select the relative influences of each disturbance.", 
                                                        br(),
                                                        tags$i("You have to think of all the disturbances effects together rather than separately 
                                             and say for example that cyclones will have more impact on coral cover than coral bleaching"), 
                                                        style = "color:black;"),
                                                     layout_columns(
                                                       card(
                                                         card_header(class = "bg-primary",
                                                                     (h4(strong(" Disturbances across time in the spatial domain:"),style = "color:white; font-size:16px; text-align:center;"))),
                                                         plotOutput("effects")),
                                                       
                                                       card(
                                                         card_header(class = "bg-primary",
                                                                     (h4(strong("Cyclones"),style = "color:white; font-size:16px; text-align:center;"))),
                                                         
                                                         sliderInput("cyc_eff",
                                                                     "Relative influence:",
                                                                     min = 0,
                                                                     max = 1,
                                                                     value = 0.5)),
                                                       
                                                       card(
                                                         card_header(class = "bg-primary",
                                                                     (h4(strong("Degree heating weeks"),style = "color:white; font-size:16px; text-align:center;"))),
                                                         sliderInput("dhw_eff",
                                                                     "Relative influence:",
                                                                     min = 0,
                                                                     max = 1,
                                                                     value = 0.5)),
                                                       
                                                       card(
                                                         card_header(class = "bg-primary",
                                                                     (h4(strong("Other effects"),style = "color:white; font-size:16px; text-align:center;"))),
                                                         sliderInput("ot_eff",
                                                                     "Relative influence:",
                                                                     min = 0,
                                                                     max = 1,
                                                                     value = 0.5)
                                                       ),col_widths = c(12, -1, 3,-1, 3, -1, 3, -2))
                                                     
                                           ),
                                           nav_panel(h3("5. Population dynamics",style = "font-size:16px; color:black;"),
                                                     layout_columns(h4(strong("Select the annual growth of hard and soft corals", style = "color:black;")),
                                                                    card(
                                                                      card_header(class = "bg-primary",
                                                                                  (h4(strong("Hard Coral"),style = "color:white; font-size:14px; text-align:center;"))),
                                                                      sliderInput("growthHCC",
                                                                                  "Annual growth",
                                                                                  min = 0,
                                                                                  max = 1,
                                                                                  value = 0.3)
                                                                    ),
                                                                    card(
                                                                      card_header(class = "bg-primary",
                                                                                  (h4(strong("Soft Coral"),style = "color:white; font-size:14px; text-align:center;"))),
                                                                      sliderInput("growthSC",
                                                                                  "Annual growth of Hard coral",
                                                                                  min = 0,
                                                                                  max = 1,
                                                                                  value = 0.3)), col_widths = c(12,-2,3,-2,3,-2))),
                                           
                                           nav_panel(h3("5bis. simulation plots",style = "font-size:16px; color:black;"),
                                                     box(width =12, h4("After selecting the spatial and temporal domain, disturbance effects and population dynamics, 
                                                            you can simulate the evolution of benthic communities over time and space by clicking on the button below.",
                                                                       br(), 
                                                                       "When the simulation is complete, 3 graphs will be displayed showing the evolution of the 3 groups : Hard coral, soft coral and macroalgae",
                                                                       br(),
                                                                       style ="color:black;"),
                                                         br(),
                                                         waiter::use_waiter(),
                                                         actionButton("simulate","simulate",width = "400px")),
                                                     
                                                     layout_columns(
                                                       card(
                                                         card_header(class = "bg-primary",
                                                                     (h4(strong("Hard coral cover evolution"),style = "color:white; font-size:14px; text-align:center;"))),
                                                         plotOutput("hcc"), full_screen = TRUE), 
                                                       card(card_header(class = "bg-primary",
                                                                        (h4(strong("Soft coral cover evolution"),style = "color:white; font-size:14px; text-align:center;"))),
                                                            plotOutput("SC"), full_screen = TRUE), 
                                                       card(card_header(class = "bg-primary",
                                                                        (h4(strong("Macroalgae cover evolution"),style = "color:white; font-size:14px; text-align:center;"))),
                                                            plotOutput("MA"), full_screen = TRUE),
                                                       col_widths = c(12,12,12))),
                                           
                                           nav_panel(h3("6. Sampling design",style = "font-size:16px; color:black;"),
                                                     
                                                     layout_columns( card(
                                                       sliderInput("nLocs","number of reefs surveyed",min=1, max=50, value=25),
                                                       sliderInput("nSites",
                                                                   "Number of sites:",
                                                                   min = 1,
                                                                   max = 40,
                                                                   value = 2),
                                                       
                                                       sliderInput("Number_of_transects_per_site",
                                                                   "Number of transects per site",
                                                                   min = 0,
                                                                   max = 50,
                                                                   value = 5),
                                                       
                                                       sliderInput("Number_of_frames_per_transect",
                                                                   "Number of frames per transect",
                                                                   min = 0,
                                                                   max = 200,
                                                                   value = 100),
                                                       
                                                       numericInput("Depths","Depth(s) observed",
                                                                    1, min = 1, max = 3
                                                       ),
                                                       
                                                       sliderInput("Points_per_frame",
                                                                   "Number of points per frame",
                                                                   min = 0,
                                                                   max = 50,
                                                                   value = 5)),
                                                       
                                                       card(h4("After selecting your sampling designs, click on the button below to create your data :",style = "color:black;" ), actionButton("create","create", width = "400px"), 
                                                            br(),
                                                            h4("You will see the plot, dataset, summary and validation on the page 'your data'",style = "color:black;")), 
                                                       col_widths = c(4,-1, 6)
                                                     ))),col_widths = c(12,12)
                                       ))),
                            
                            tabPanel("Your data",
                                     
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    
                                                    h4(strong("Download synthetic dataset")),
                                                    br(),
                                                    h5(strong("1. Download the synthetic data using the button below:")), 
                                                    br(),
                                                    textInput("name","insert the name of your folder:"),
                                                    downloadButton("downloadSynth", "Download synthetic dataset")), 
                                       
                                       mainPanel( width = 9,
                                         navset_card_underline(
                                           # Panel with plot ----
                                           nav_panel(h3("Plots",style = "font-size:12px; color:black;"), style = "height:800px;",card(full_screen = TRUE, plotOutput("plot", width ="100%", height = "700px"))),
                                           
                                           # Panel with Dataset and summary ----
                                           nav_panel(h3("Dataset", style = "font-size:12px; color:black;"), card(full_screen = TRUE, DT::DTOutput("data", width ="100%", height ="700px"))),
                                           
                                           nav_panel(h3("Summary", style = "font-size:12px; color:black;"), style ="height:1000px;",  card(full_screen = TRUE, DT::DTOutput("sum")), 
                                                     card(max_height = 250, class = "card border-dark mb-3", card_header(h4(strong("Total observation"))), 
                                                          card_body(textOutput("total"))), 
                                                     card(max_height = 250, layout_column_wrap(
                                                       width = 1/2,
                                                       DT::DTOutput("data_reef"), 
                                                     DT::DTOutput("data_group"))),
                                                     card(uiOutput(outputId  = "report_bis"))),
                                           
                                           nav_panel(h3("Data validation", style = "font-size:12px; color:black;"), style = "height:800px", card(full_screen = TRUE, uiOutput(outputId = "validation")))
                                         )
                                       )))
                 ))

# Server function

server <- function(input, output, session) {
  
  ## ---- Spatial domain creation
  edits <- callModule(editMod,
                      "draw",
                      mapview(map.types = ("Esri.WorldImagery"))@map %>% setView(lat = 46, lng =8, zoom =1)
  )
  
  seed <- 123
  
  observeEvent(edits()$finished,{
    spatial.domain <- st_geometry(req(edits()$finished))
    
    ## --- save for later
    assign("spatial.domain", spatial.domain, envir = .GlobalEnv)
    
    area <- ({as.numeric(st_area(st_geometry(req(spatial.domain))))})
    
    ##--- correct buffer
    
    if (1e+10 < area) {(k1 = 0.05) & (k2 = 0.01) & (k3 = -0.01)
    } else if (1e+10 > area & area >= 1e+9) {(k1 = 0.01) & (k2 = 0.001) & (k3 = -0.001)
    } else if (1e+9 > area & area > 1e+8) { (k1= 0.002) & (k2 = 0.0001) & (k3 = -0.0001)
    } else {(k1 = 0.0002) & (k2 = 0.00002) & (k3 = -0.0001)}
    
    
    ## ---- SpatialPoints
    set.seed(seed)
    grid <- spatial.domain %>%
      st_set_crs(NA) %>%
      st_sample(size = 10000, type = 'regular') %>%
      st_set_crs(4236)
    
    ## Compile the spatial data frame - note it is important for RFsimulate
    ## that the data be sorted by Longitude then Latitude
    spatial.grid.pts.df <- grid %>%
      st_coordinates() %>%
      as.data.frame() %>%
      dplyr::rename(Longitude = X, Latitude = Y) %>%
      arrange(Longitude, Latitude)
    assign("spatial.grid.pts.df", spatial.grid.pts.df, envir = .GlobalEnv)
    
    ## Generate reefs ------------------------------------------------------
    ## Create a random field................................................
    ## ---- reefsRF
    RFoptions(seed = 1) # change the seed if we want different reef in the same spatial domain
    threshold = 1.75
    model <- RMexp(var = 1, scale = 0.1)
    sim <- RFsimulate(model,
                      x = as.vector(scale(spatial.grid.pts.df$Longitude, 
                                          scale = FALSE)),
                      y = as.vector(scale(spatial.grid.pts.df$Latitude, 
                                          scale = FALSE)))
    ## combine with spatial data
    reefs <- spatial.grid.pts.df  %>%
      mutate(Y = as.vector(sim))
    
    ## ----end
    
    ## ---- reefsRFpolygons
    reefs.sf <- reefs %>% 
      st_as_sf(coords = c('Longitude','Latitude')) %>%
      filter(Y > threshold) %>%
      st_buffer(k1, endCapStyle = 'SQUARE') %>%
      st_cast('POLYGON') %>%
      st_union() %>%
      st_set_crs(4326)
    
    ## ---- Save for later
    assign("reefs.sf", reefs.sf, envir = .GlobalEnv)
    
    ## ----end
    
    ## ---- reefsRFpolygonsHollow
    sf_use_s2(FALSE)  # negative buffers dont work if this is true
    reefs.sf <- reefs.sf %>%
      st_buffer(k2) %>%
      st_difference(reefs.sf %>% st_buffer(k3))
    sf_use_s2(TRUE)
    
    
    reefs.poly.sf <- reefs.sf %>% 
      st_cast('POLYGON') %>% 
      st_as_sf() %>%
      mutate(Reef = paste0('Reef', 1:n()))
    
    ## ---- Save for later
    assign("reefs.poly.sf", reefs.poly.sf, envir = .GlobalEnv)
    
    ## ----- plot and area
    output$spatial_domain <- renderPlot({
      ggplot() +
        geom_sf(data = spatial.domain, fill = NA) + 
        geom_sf(data = reefs.poly.sf) +
        theme_bw() +
        theme(axis.title = element_text(size=12),
              legend.position = c(0.95,0.95),
              legend.justification = c(1,1)) + 
        xlab("longitude") + ylab("latitude")
    })
    
    output$area <- renderText(
      paste(
        "Domain area = ",label_number()(area), "m2"))
    
  })
  
  ## ---- Generating baseline while people choose the first parameters.
  # To remove if changes can be applied on baseline from users
  
  observeEvent(input$Next,{
    
    ## ----- Creating the mesh    
    withProgress( message = "Generating baseline coral cover",{
      
      incProgress(amount = 0.1)
      
      variance <- 1                       
      alpha <- 2
      kappa <- 1
      mesh.pars <- c(1, 0.5, 0.1, 1, 0.5)*sqrt(alpha-ncol(spatial.grid.pts.df )/2)/kappa 
      s = inla.mesh.segment(spatial.grid.pts.df [chull(spatial.grid.pts.df ), ])
      mesh = inla.mesh.2d(
        spatial.grid.pts.df [chull(spatial.grid.pts.df ), ], 
        max.edge = mesh.pars[1:2], 
        cutoff = mesh.pars[3], 
        offset = mesh.pars[4:5],
        boundary = s)
      
      ## ---- SyntheticData_Spatial.spde2
      spde <- inla.spde2.matern(mesh, alpha = alpha)
      ## ----end
      
      ## ---- SyntheticData_Spatial.precision.matrix
      theta <- c(-0.5*log(4*pi*variance*kappa^2), log(kappa))
      Q <- inla.spde2.precision(spde, theta = theta)
      ## ----end
      
      ## Calculate a lattice projection to and from the mesh
      ## ---- SyntheticData_Spatial.A
      A <- inla.spde.make.A(mesh = mesh, 
                            loc = as.matrix(spatial.grid.pts.df ))
      ## ----end
      
      ## ---- Save for later
      assign("mesh", mesh, envir = .GlobalEnv)
      ## ----end
      
      
      ## Synthetic Baselines ----------------------------------------------------
      ## ---- SyntheticData_BaselineSpatial.HCC
      baseline.sample.hcc <- mesh$loc[,1:2] %>%
        as.data.frame() %>%
        dplyr::select(Longitude = V1, Latitude = V2) %>%
        mutate(clong = as.vector(scale(Longitude, scale = FALSE)),
               clat = as.vector(scale(Latitude, scale = FALSE)),
               Y = clong + sin(clat) + #rnorm(1,0,1) +
                 1.5*clong + clat) %>%
        mutate(Y = scales::rescale(Y, to = c(-2, 0.8)))
      
      baseline.effects.hcc <- baseline.sample.hcc %>%
        dplyr::select(Y) %>%
        as.matrix
      baseline.pts.sample.hcc <- inla.mesh.project(mesh,
                                                   loc=as.matrix(spatial.grid.pts.df [,1:2]),
                                                   baseline.effects.hcc)
      baseline.pts.effects.hcc = baseline.pts.sample.hcc %>% 
        cbind() %>% 
        as.matrix() %>% 
        as.data.frame() %>%
        cbind(spatial.grid.pts.df ) %>% 
        pivot_longer(cols = c(-Longitude,-Latitude),
                     names_to = c('Year'),
                     names_pattern = 'sample:(.*)',
                     values_to = 'Value') %>%
        mutate(Year = as.numeric(Year))
      
      ## ---- Save for later
      assign("baseline.pts.effects.hcc", baseline.pts.effects.hcc, envir = .GlobalEnv)
      assign("baseline.pts.sample.hcc", baseline.pts.sample.hcc, envir = .GlobalEnv)
      assign("baseline.sample.hcc", baseline.sample.hcc, envir = .GlobalEnv)
      
      ## ---- Plot
      hcc_baseline <- ggplot(baseline.pts.effects.hcc, aes(y = Latitude, x = Longitude)) +
        geom_tile(aes(fill = 100 * plogis(Value))) +
        scale_fill_gradientn(name = "Cover (%)", colors = terrain.colors(10)) +
        coord_sf(crs = 4236) +
        theme_bw(base_size = 14) +
        theme(
          axis.title = element_blank(),
          legend.position = c(0.95, 0.95),
          legend.justification = c(1, 1)
        ) +
        ggtitle("Hard coral baseline")
      
      output$baseline_HCC <- renderPlot(hcc_baseline)
      
      ## ----end
      
      ## ---- SyntheticData_BaselineSpatial.SC
      baseline.sample.sc <- mesh$loc[,1:2] %>%
        as.data.frame() %>%
        dplyr::select(Longitude = V1, Latitude = V2) %>%
        mutate(clong = as.vector(scale(Longitude, scale = FALSE)),
               clat = as.vector(scale(Latitude, scale = FALSE)),
               Y = clong + sin(clat) + #rnorm(1,0,1) +
                 1.5*clong + -1.5*clat) %>%
        mutate(Y = scales::rescale(Y, to = c(-4, -2)))
      
      baseline.effects.sc <- baseline.sample.sc %>%
        dplyr::select(Y) %>%
        as.matrix
      baseline.pts.sample.sc <- inla.mesh.project(mesh,
                                                  loc=as.matrix(spatial.grid.pts.df [,1:2]),
                                                  baseline.effects.sc)
      baseline.pts.effects.sc = baseline.pts.sample.sc %>% 
        cbind() %>% 
        as.matrix() %>% 
        as.data.frame() %>%
        cbind(spatial.grid.pts.df ) %>% 
        pivot_longer(cols = c(-Longitude,-Latitude),
                     names_to = c('Year'),
                     names_pattern = 'sample:(.*)',
                     values_to = 'Value') %>%
        mutate(Year = as.numeric(Year))
      
      ## ---- Save for later
      assign("baseline.pts.effects.sc", baseline.pts.effects.sc, envir = .GlobalEnv)
      assign("baseline.pts.sample.sc", baseline.pts.sample.sc, envir = .GlobalEnv)
      assign("baseline.sample.sc", baseline.sample.sc, envir = .GlobalEnv)
      
      ## ---- Plot
      sc_baseline <-  ggplot(baseline.pts.effects.sc, aes(y = Latitude, x = Longitude)) +
        geom_tile(aes(fill = 100 * plogis(Value))) +
        scale_fill_gradientn(name = "Cover (%)", colors = terrain.colors(10)) +
        coord_sf(crs = 4236) +
        theme_bw(base_size = 14) +
        theme(
          axis.title = element_blank(),
          legend.position = c(0.95, 0.95),
          legend.justification = c(1, 1)
        ) +
        ggtitle("Soft coral baseline")
      
      
    })                             
    
    output$baseline_SC <- renderPlot(sc_baseline)
  })
  ## ----end
  
  ## ----- Disturbances simulation
  # To remove and change if people can choose more precisely 
  # the disturbances such as cyclones occurancy, DHW periodicity...
  observeEvent(input$simulation, {
    
    ## ---- TemporalPoints
    # Fixed monitoring - years = sequence between year_beg and year_end 
    
    years <- 1:(input$years)
    
    # Random monitoring - years = number of years replicated for each nLocs, need to choose a range 
    n_min <- 2 # always a mininmum of two replicates 
    n_max <- (input$years)/2 # max replicates   
    
    rep <- n_min:n_max
    
    ## ----end
    
    variance <- 1
    kappa <- 1
    alpha <- 2
    
    ## ---- SyntheticData_Spatial.spde2
    spde <- inla.spde2.matern(mesh, alpha = alpha)
    ## ----end
    
    ## ---- SyntheticData_Spatial.precision.matrix
    theta <- c(-0.5*log(4*pi*variance*kappa^2), log(kappa))
    Q <- inla.spde2.precision(spde, theta = theta)
    ## ----end
    
    ## Calculate a lattice projection to and from the mesh
    ## ---- SyntheticData_Spatial.A
    A <- inla.spde.make.A(mesh = mesh, 
                          loc = as.matrix(spatial.grid.pts.df ))
    ## ----end
    
    ## Synthetic DHW --------------------------------------------------------
    ## ---- SyntheticData_CovariatesDHW.temporal.trend
    set.seed(seed)
    dhw.temporal <- data.frame(Year = years) %>%
      mutate(cYear = Year-1, 
             Y = 0.2*cYear + sin(cYear),
             Y = Y*rbeta(length(years), Y, 1),
             Y = scales::rescale(Y-min(Y), to = c(0,5)))
    
    ## ----end
    
    ## ---- SyntheticData_CovariatesDHW.effect
    set.seed(seed)
    dhw.sample <- inla.qsample(length(years), 
                               Q,
                               seed = seed, 
                               constr = spde$f$extraconstr)
    
    rho <- rep(0.7,length(years))
    rho <- rbeta(length(years), 0.2, 1)
    x <- dhw.sample
    for (j in 2:length(years)) {
      x[, j] <- rho[j] * x[, j - 1] + sqrt(1 - rho[j]^2) * dhw.sample[, j]
    }
    x <- sweep(x, 2, dhw.temporal$Y, FUN = '+')
    dhw.effects <- scales::rescale(x, to = c(0,1))
    dhw.pts.sample <- inla.mesh.project(mesh,
                                        loc = as.matrix(spatial.grid.pts.df [,1:2]),
                                        dhw.effects)
    
    dhw.pts.effects.df = dhw.pts.sample %>% 
      as.matrix() %>% 
      as.data.frame() %>%
      cbind(spatial.grid.pts.df) %>% 
      pivot_longer(cols = c(-Longitude,-Latitude),
                   names_to = c('Year'),
                   names_pattern = 'sample:(.*)',
                   values_to = 'Value') %>%
      mutate(Year = as.numeric(Year))
    
    dhw <- ggplot(dhw.pts.effects.df, aes(y = Latitude, x = Longitude)) +
      geom_tile(aes(fill = Value)) +
      facet_wrap(~Year, ncol = 4) +
      scale_fill_gradientn(colors = rev(heat.colors(10))) +
      coord_sf(crs = 4236) +
      theme_bw(base_size = 12) +
      theme(axis.title = element_blank()) +
      ggtitle("Degree Heating weeks")
    
    ##---- Save for later
    assign("dhw.effects", dhw.effects, envir = .GlobalEnv)
    
    ## Synthetic CYC --------------------------------------------------------
    ## ---- CovariatesCYC.temporal.trend
    set.seed(seed)
    cyc <- vector('list', length(years))
    
    for (yr in years) {
      #cat(paste("Year:", yr, '\n'))
      cyc.occur <- rbinom(1, 1, prob = min(0.05*yr^2, 0.6)) # Line to change if we want users to can choose how many cyclones there are
      #cat(paste("Cyclone Occurance:", cyc.occur, '\n'))
      cyc.intensity <- rbeta(1, 2, 1) %>% round(2)
      #cat(paste("Cyclone intensity:", cyc.intensity, '\n'))
      lat_offset <- runif(1,0,5)
      cyc.spatial <- mesh$loc[,1:2] %>%
        as.data.frame() %>%
        dplyr::select(Longitude = V1, Latitude = V2) %>%
        mutate(clong = as.vector(scale(Longitude, scale = FALSE)),
               clat = as.vector(scale(Latitude, scale = FALSE)),
               Y = lat_offset + runif(1,-1,1)*clong + runif(1,-1,1)*clat + sin(clat),
               Y = abs(Y),
               Y = ifelse(Y > cyc.intensity, cyc.intensity, Y),
               Y = cyc.intensity-Y,
               Value = Y*cyc.occur
        )
      cyc[[yr]] <- cyc.spatial %>% mutate(Year = yr)
    }
    cyc = do.call('rbind', cyc)
    cyc.effects.df <- cyc %>%
      mutate(Value = scales::rescale(Value, to = c(0, 1)))
    
    ## ---- SyntheticData_CovariatesCYC.effects
    cyc.effects <- cyc.effects.df %>%
      dplyr::select(-clong, -clat, -Y) %>%
      pivot_wider(id_cols = c(Longitude, Latitude), 
                  names_prefix = 'sample:',
                  names_from = Year, 
                  values_from = Value) %>%
      dplyr::select(-Longitude, -Latitude) %>%
      as.matrix
    
    cyc.pts.sample <- inla.mesh.project(mesh,
                                        loc = as.matrix(spatial.grid.pts.df [,1:2]),
                                        cyc.effects)
    
    cyc.pts.effects <- cyc.pts.sample %>% 
      as.matrix() %>% 
      as.data.frame() %>%
      cbind(spatial.grid.pts.df) %>% 
      pivot_longer(cols = c(-Longitude,-Latitude),
                   names_to = c('Year'),
                   names_pattern = 'sample:(.*)',
                   values_to = 'Value') %>%
      mutate(Year = as.numeric(Year))
    
    c <- ggplot(cyc.pts.effects, aes(y = Latitude, x= Longitude)) +
      geom_tile(aes(fill = Value)) +
      facet_wrap(~Year, ncol = 4) +
      scale_fill_gradientn(name = "Value", colors = terrain.colors(10)) +
      coord_sf(crs = 4236) +
      theme_bw(base_size = 12) +
      theme(axis.title = element_blank()) +
      ggtitle("Cyclones")
    
    ## ---- Save for later
    assign("cyc.effects", cyc.effects, envir = .GlobalEnv)
    ## ----end
    
    ## Synthetic Other --------------------------------------------------------
    ## ---- SyntheticData_CovariatesOther.effects
    set.seed(seed+1)
    other.sample <- inla.qsample(length(years), 
                                 Q,
                                 seed = seed+1, 
                                 constr = spde$f$extraconstr)
    
    rho <- rep(0.7, length(years))
    rho <- rbeta(length(years), 0.2, 1)
    x <- other.sample
    for (j in 2:length(years)) {
      x[, j] <- rho[j] * x[, j - 1] + sqrt(1 - rho[j]^2) * other.sample[, j]
    }
    other.effects <- scales::rescale(x, to = c(0,1))
    other.pts.sample <- inla.mesh.project(mesh,
                                          loc = as.matrix(spatial.grid.pts.df [,1:2]),
                                          other.effects)
    
    other.pts.effects = other.pts.sample %>% 
      as.matrix() %>% 
      as.data.frame() %>%
      cbind(spatial.grid.pts.df ) %>% 
      pivot_longer(cols = c(-Longitude, -Latitude),
                   names_to = c('Year'),
                   names_pattern = 'sample:(.*)',
                   values_to = 'Value') %>%
      mutate(Year = as.numeric(Year))
    
    g_spat_ot <- ggplot(other.pts.effects, aes(y = Latitude, x = Longitude)) +
      geom_tile(aes(fill = Value)) +
      facet_wrap(~Year, ncol = 4) +
      scale_fill_gradientn(colors = terrain.colors(10)) + 
      coord_sf(crs = 4236) +
      theme_bw(base_size = 12) +
      theme(axis.title = element_blank()) +
      ggtitle("Other disturbances")
    
    ## ---- Save for later
    assign("other.effects", other.effects, envir = .GlobalEnv)
    ## ----end
    
    output$effects <- renderPlot(dhw + c + g_spat_ot)
  }) 
  
  
  ## ---- Synthetic coral cover data generation
  observeEvent(input$simulate, {
    
    waiter::Waiter$new(id = "hcc")$show()
    waiter::Waiter$new(id = "SC")$show()
    waiter::Waiter$new(id = "MA")$show()
    
    ## ---- TemporalPoints
    # Fixed monitoring - years = sequence between year_beg and year_end 
    
    years <- 1:(input$years)
    
    # Random monitoring - years = number of years replicated for each nLocs, need to choose a range 
    n_min <- 2 # always a mininmum of two replicates 
    n_max <- (input$years)/2 # max replicates   
    
    rep <- n_min:n_max
    
    ## ---- Disturbance effects 
    
    # Bleaching 
    dhw_eff <- (input$dhw_eff)
    # Cyclone
    cyc_eff <- (input$cyc_eff)
    # Other
    ot_eff <- (input$ot_eff)
    
    
    ## Compile all disturbances------------------------------------------------
    disturb.effects <-
      (dhw_eff*dhw.effects) + (cyc_eff*cyc.effects) + (ot_eff*other.effects) %>%
      as.data.frame()
    all.effects.df <- mesh$loc[,1:2] %>%
      as.data.frame() %>%
      dplyr::rename(Longitude = V1, Latitude = V2) %>%
      cbind(disturb.effects) %>%
      pivot_longer(cols = c(-Longitude, -Latitude),
                   names_to = 'Year',
                   names_pattern = 'sample:(.*)',
                   values_to = 'Y') %>%
      mutate(Year = factor(Year, levels=sort(unique(as.numeric(as.character(Year)))))) %>%
      group_by(Longitude, Latitude) %>%
      mutate(
        Growth.HCC = (input$growthHCC),                ## Add growth onto this
        Growth.SC = (input$growthSC),
        Y.HCC = cumsum(-Y + Growth.HCC), ## cumsum on link scale will accumulate effects
        Y.SC = cumsum(-Y + Growth.SC)
      )
    ## ----end
    
    
    
    ## Synthetic data ---------------------------------------------------------
    ## Hard coral
    
    all.effects.hcc <- all.effects.df %>%
      full_join(baseline.sample.hcc %>% dplyr::select(Longitude, Latitude, BASE.HCC=Y)) %>%
      group_by(Longitude, Latitude) %>%
      mutate(HCC = BASE.HCC + Y.HCC) %>%
      ungroup() %>%
      dplyr::select(Longitude, Latitude, Year,HCC) %>%
      tidyr::pivot_wider(
        names_prefix = 'sample:',
        names_from = Year, 
        values_from = HCC) %>%
      dplyr::select(-Longitude, -Latitude) %>%
      as.matrix
    
    
    all.pts.sample.hcc <- inla.mesh.project(mesh,
                                            loc = as.matrix(spatial.grid.pts.df [,1:2]),
                                            all.effects.hcc)
    all.pts.effects.hcc = all.pts.sample.hcc %>% 
      as.matrix() %>% 
      as.data.frame() %>%
      cbind(spatial.grid.pts.df ) %>% 
      pivot_longer(cols = c(-Longitude, -Latitude),
                   names_to = c('Year'),
                   names_pattern = 'sample:(.*)',
                   values_to = 'Value') %>%
      mutate(Year = as.numeric(Year),
             Value = Value)
    
    ## ----end
    
    ## Soft coral
    ## ---- SyntheticData_CompileSyntheticData.SC
    ## Do all this on the link scale so that can use cumsum
    all.effects.sc <- all.effects.df %>%
      full_join(baseline.sample.sc %>% dplyr::select(Longitude, Latitude, BASE.SC=Y)) %>%
      group_by(Longitude, Latitude) %>%
      mutate(SC = BASE.SC + Y.SC) %>%
      ungroup() %>%
      dplyr::select(Longitude, Latitude, Year,SC) %>%
      pivot_wider(id_cols = c(Longitude, Latitude), 
                  names_prefix = 'sample:',
                  names_from = Year, 
                  values_from = SC) %>%
      dplyr::select(-Longitude, -Latitude) %>%
      as.matrix()
    
    all.pts.sample.sc <- inla.mesh.project(mesh,
                                           loc = as.matrix(spatial.grid.pts.df [,1:2]),
                                           all.effects.sc)
    all.pts.effects.sc = all.pts.sample.sc %>% 
      as.matrix() %>% 
      as.data.frame() %>%
      cbind(spatial.grid.pts.df ) %>% 
      pivot_longer(cols = c(-Longitude, -Latitude),
                   names_to = c('Year'),
                   names_pattern = 'sample:(.*)',
                   values_to = 'Value') %>%
      mutate(Year = as.numeric(Year),
             Value = Value)
    
    
    ## ----end
    
    ## Macroalgae
    ## ---- SyntheticData_CompileSyntheticData.MA
    ## Do all this on the link scale so that can use cumsum
    all.pts.effects.ma <- all.pts.effects.hcc %>%
      dplyr::rename(HCC=Value) %>% 
      bind_cols(all.pts.effects.sc %>%
                  dplyr::select(SC=Value)) %>%
      mutate(Total.Avail = 0.8 - plogis(HCC) + plogis(SC),
             MA = Total.Avail,
             Value = qlogis(MA)) %>%
      dplyr::select(-HCC, -SC, -Total.Avail, -MA)
    
    
    ## ----end
    
    ## Broad scale reef patterns ==================================================
    ## INLA ----------------------------------------------------------
    ## - rasterize the reefs frame
    ## - convert to points (centroids of raster cells)
    ## - filter to the values of 1
    ## - extract coordinates
    ## - convert to data frame
    
    ## ---- SyntheticData_PointsInReefs
    data.reefs.sf <- reefs.sf %>%
      st_as_stars(dx = 0.01) %>%  # rasterize
      st_as_sf(as_points = TRUE) %>%
      filter(values == 1L)
    
    data.reefs.df = data.reefs.sf %>%
      st_coordinates() %>%
      as.data.frame() %>%
      dplyr::rename(Longitude = X, Latitude = Y)
    ## ----end
    
    ## ---- SyntheticData_ProjectOntoReefs.HCC
    data.reefs.sample.hcc <- inla.mesh.project(mesh,
                                               loc = as.matrix(data.reefs.df[,1:2]),
                                               all.effects.hcc)
    data.reefs.hcc = data.reefs.sample.hcc %>% 
      as.matrix() %>% 
      as.data.frame() %>%
      cbind(data.reefs.df) %>% 
      pivot_longer(cols = c(-Longitude,-Latitude),
                   names_to = c('Year'),
                   names_pattern = 'sample:(.*)',
                   values_to = 'Value') %>%
      mutate(Year = as.numeric(Year),
             Value = Value)
    
    data.reefs.pts.hcc.sf = data.reefs.hcc %>%
      st_as_sf(coords = c('Longitude', 'Latitude')) %>%
      st_set_crs(st_crs(data.reefs.sf))
    sf_use_s2(FALSE)
    data.reefs.pts.hcc.sf <- data.reefs.pts.hcc.sf %>%
      st_intersection(reefs.poly.sf)
    
    ## ---- Save for later
    assign("data.reefs.pts.hcc.sf", data.reefs.pts.hcc.sf, envir = .GlobalEnv)
    
    ## ---- Plot
    output$hcc <- renderPlot(data.reefs.pts.hcc.sf %>% 
                               mutate(Y = plogis(Value)) |>
                               ggplot() +
                               geom_sf(aes(color = Y)) +
                               geom_sf(data =spatial.domain, fill =NA)+
                               facet_wrap(~Year, nrow = 2) +
                               scale_color_gradientn(name = "Cover(%)", colors = terrain.colors(10)) +
                               coord_sf(
                                 crs = 4236,
                               ) +
                               theme_bw(base_size = 12) +
                               theme(axis.title = element_blank())
    )
    
    sf_use_s2(TRUE)
    ## ----end
    
    ## ---- SyntheticData_ProjectOntoReefs.SC
    data.reefs.sample.sc <- inla.mesh.project(mesh,
                                              loc = as.matrix(data.reefs.df[,1:2]),
                                              all.effects.sc)
    
    data.reefs.sc = data.reefs.sample.sc %>% 
      as.matrix() %>% 
      as.data.frame() %>%
      cbind(data.reefs.df) %>% 
      pivot_longer(cols = c(-Longitude,-Latitude),
                   names_to = c('Year'),
                   names_pattern = 'sample:(.*)',
                   values_to = 'Value') %>%
      mutate(Year = as.numeric(Year),
             Value = Value)
    
    data.reefs.pts.sc.sf = data.reefs.sc %>%
      st_as_sf(coords = c('Longitude', 'Latitude')) %>%
      st_set_crs(st_crs(data.reefs.sf))
    sf_use_s2(FALSE)
    data.reefs.pts.sc.sf <- data.reefs.pts.sc.sf %>%
      st_intersection(reefs.poly.sf)
    
    ## ---- Save for later
    assign("data.reefs.pts.sc.sf", data.reefs.pts.sc.sf, envir = .GlobalEnv)
    
    ## ---- Plot
    output$SC <- renderPlot(data.reefs.pts.sc.sf %>%
                              mutate(Y = plogis(Value)) |>
                              ggplot() +
                              geom_sf(aes(color = Y)) +
                              geom_sf(data =spatial.domain, fill =NA)+
                              facet_wrap(~Year, nrow = 2) +
                              scale_color_gradientn(name = "Cover(%)", colors = terrain.colors(10)) + 
                              coord_sf(crs = 4236 
                              ) +
                              theme_bw(base_size = 12) +
                              theme(axis.title = element_blank())
    )
    sf_use_s2(TRUE)
    
    ## ----end
    
    ## ---- SyntheticData_ProjectOntoReefs.MA
    data.reefs.ma <- data.reefs.hcc %>%
      rename(HCC = Value) %>%
      full_join(data.reefs.sc %>% rename(SC = Value)) %>%
      mutate(Total.Avail = 0.8 - plogis(HCC) + plogis(SC),
             MA = Total.Avail,
             Value = qlogis(MA)) %>%
      dplyr::select(-HCC, -SC, -Total.Avail, -MA)
    
    data.reefs.pts.ma.sf = data.reefs.ma %>%
      st_as_sf(coords = c('Longitude', 'Latitude')) %>%
      st_set_crs(st_crs(data.reefs.sf))
    sf_use_s2(FALSE)
    data.reefs.pts.ma.sf <- data.reefs.pts.ma.sf %>%
      st_intersection(reefs.poly.sf)
    
    ## ---- Save for later
    assign("data.reefs.pts.ma.sf", data.reefs.pts.ma.sf, envir = .GlobalEnv)
    
    ## ---- Plot
    output$MA <- renderPlot(data.reefs.pts.ma.sf%>% mutate(Y = plogis(Value)) |>
                              ggplot() +
                              geom_sf(aes(color = Y)) +
                              geom_sf(data =spatial.domain, fill =NA)+
                              facet_wrap(~Year, nrow = 2) +
                              scale_color_gradientn(name = "Cover(%)", colors = terrain.colors(10)) +
                              coord_sf(
                                crs = 4236
                              ) +
                              theme_bw(base_size = 12) +
                              theme(axis.title = element_blank()))
    sf_use_s2(TRUE)
    ## ----end
  })
  
  ## ---- Sampling designs
  observeEvent(input$create, {
    
    withProgress(message = "Your data are being generated", {
      
      ## ---- TemporalPoints
      # Fixed monitoring - years = sequence between year_beg and year_end 
      
      years <- 1:(input$years)
      
      # Random monitoring - years = number of years replicated for each nLocs, need to choose a range 
      n_min <- 2 # always a mininmum of two replicates 
      n_max <- (input$years)/2 # max replicates   
      
      rep <- n_min:n_max
      
      ## Note, the following are on the link scale
      
      # Hard corals
      hcc_site_sigma <- 0.7       # variability in Sites within Locations
      hcc_transect_sigma <- 0.4   # variability in Transects within Sites
      hcc_sigma <- 0.8             # random noise
      
      # Soft corals
      sc_site_sigma <- 0.7         # variability in Sites within Locations
      sc_transect_sigma <- 0.4     # variability in Transects within Sites
      sc_sigma <- 0.8             # random noise
      
      # Macroalgae
      ma_site_sigma <- 0.7        # variability in Sites within Locations
      ma_transect_sigma <- 0.4     # variability in Transects within Sites
      ma_sigma <- 0.8            # random noise
      
      nLocs =(input$nLocs)
      nSites = (input$nSites)     # Number of 'sites' within 'reefs'
      surveys <- "fixed" # or "random"
      
      ## Sampling designs (large scale components) ==================================
      
      ## ---- ProjectOntoReefs
      
      ## ---- SyntheticData_ProjectOntoReefs
      data.reefs.pts.sf <- data.reefs.pts.hcc.sf %>%
        rename(HCC = Value) %>%
        bind_cols(data.reefs.pts.sc.sf %>%
                    dplyr::select(SC = Value) %>%
                    st_drop_geometry()
        ) %>% 
        bind_cols(data.reefs.pts.ma.sf %>%
                    dplyr::select(MA = Value) %>%
                    st_drop_geometry()
        ) 
      
      ## Finer sampling design components - use it for replications ===============================
      # Parameters to choose 
      Number_of_transects_per_site = (input$Number_of_transects_per_site)
      Depths = (input$Depths)
      Number_of_frames_per_transect = (input$Number_of_frames_per_transect)
      Points_per_frame = (input$Points_per_frame)
      #Calcul for validation
      tot_pts = Points_per_frame*Number_of_frames_per_transect
      tot_data = (input$years) * Depths * nSites * Number_of_transects_per_site * 3
      
      ## INLA ------------------------------------------------------------
      
      data.fixed_locs.sf <- data.reefs.pts.sf %>%
        dplyr::select(Reef, geometry) %>%
        distinct(.keep_all=TRUE) %>% 
        group_by(Reef) %>%
        slice_sample(n=nSites) %>%
        mutate(Site = paste0('S',1:n())) %>%
        ungroup %>% 
        st_join(data.reefs.pts.sf %>% 
                  dplyr::select(-Reef))
      
      ## ----end
      
      ## Finer sampling design components ===============================
      
      
      ## Fixed locations ....................................................
      ## ---- SyntheticData_fixedLocsObs
      set.seed(seed)
      data.fixed_locs.obs <- data.fixed_locs.sf %>%
        bind_cols(data.fixed_locs.sf %>%
                    st_coordinates() %>%
                    as.data.frame %>%
                    dplyr::rename(Longitude = X, Latitude = Y)) %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        group_by(Longitude, Latitude, Reef) %>%
        crossing(Transect = paste0('T',1:Number_of_transects_per_site)) %>%
        group_by(Site, .add = TRUE) %>%
        mutate(
          SiteEffects.HCC = rnorm(1, 0, hcc_site_sigma),
          SiteEffects.SC = rnorm(1, 0, sc_site_sigma),
          SiteEffects.MA = rnorm(1, 0, ma_site_sigma)
        ) %>%
        group_by(Transect, .add = TRUE) %>%
        mutate(
          TransectEffects.HCC = rnorm(1, 0, hcc_transect_sigma),
          TransectEffects.SC = rnorm(1, 0, sc_transect_sigma),
          TransectEffects.MA = rnorm(1, 0, ma_transect_sigma)
        ) %>%
        ungroup() %>%
        mutate(
          HCC1 = HCC + SiteEffects.HCC + TransectEffects.HCC + rnorm(n(), 0, hcc_sigma),
          HCC2 = 100*plogis(HCC1),
          SC1 = SC + SiteEffects.SC + TransectEffects.SC + rnorm(n(), 0, sc_sigma),
          SC2 = 100*plogis(SC1),
          MA1 = MA + SiteEffects.MA + TransectEffects.MA + rnorm(n(), 0, ma_sigma),
          MA2 = 100*plogis(MA1)
        ) %>%
        arrange(Reef, Site, Transect, Year) %>%
        dplyr::select(Reef, Longitude, Latitude, Site, Transect, Year, HCC = HCC2, SC = SC2, MA = MA2) %>%
        mutate(Year = 2021 - max(years) + Year,
               Date = as.POSIXct(paste0(Year, '-01-01 14:00:00')))
      ## ----end
      ## ---- SyntheticData_fixedLocsObsDepths
      ## The following are on a fold scale.
      ## Hence a value of 0.8, indicates that 
      Depth_effect.multiplier <- 2
      
      #Extract information about the depth 
      d_info <- depth_info(Depths)
      
      data.fixed_locs.obs <- data.fixed_locs.obs %>%
        tidyr::crossing(Depth= d_info) %>% 
        pivot_longer(cols = c('HCC', 'SC', 'MA'), names_to = 'Group', values_to = 'Value') %>%
        group_by(Reef, Site, Transect, Year, Date) %>%
        mutate(Value = Value + rev(sort(Depth_effect.multiplier * scale(rnorm(Depths), center = TRUE, scale = FALSE)))) %>%
        ungroup()
      data.fixed_locs.obs %>%
        head()
      
      ## ----end
      
      ## ---- SyntheticData_fixedLocsObsFortifyData
      ## Need to split the percentage cover into point and frames
      data.fixed_locs.obs <- data.fixed_locs.obs %>%
        group_by(Reef,Site,Transect,Year,Depth,Date) %>%
        mutate(Points = round(Number_of_frames_per_transect * Points_per_frame * (Value/sum(Value)),0),
               Points = ifelse(Points<0, 0, Points)) %>%
        tidyr::uncount(Points) %>%
        sample_n(n(), replace=FALSE) %>%
        mutate(POINT_NO = rep_len(1:Points_per_frame, length = n()),
               FRAME = rep(1:Number_of_frames_per_transect, each=Points_per_frame, length = n())) %>%
        ungroup() 
      
      reef_data.synthetic_fixed <- data.fixed_locs.obs %>% 
        mutate(P_CODE = paste0("SYNTHETIC-", surveys),
               ID = 1:n(),
               REEF_NAME = Reef,
               LATITUDE = Latitude,
               LONGITUDE = Longitude,
               SITE_NO = Site,
               TRANSECT_NO = Transect,
               SITE_DEPTH = Depth,
               REEF_ZONE = '-',
               REPORT_YEAR = Year,
               SURVEY_DATE = Date,
               FRAME = paste0(P_CODE, '/', REEF_NAME, '/', REEF_ZONE, '/', SITE_NO, '/', SITE_DEPTH, '/', TRANSECT_NO, '/', REPORT_YEAR, '/', FRAME),
               POINT_NO = POINT_NO,
               GROUP_DESC = Group,
               BENTHIC_CATEGORY = paste0(Group,'_alt')
        ) %>%
        dplyr::select(P_CODE, ID, REEF_NAME,
                      LATITUDE, LONGITUDE, SITE_NO, TRANSECT_NO, SITE_DEPTH,
                      REEF_ZONE, REPORT_YEAR, SURVEY_DATE, FRAME, POINT_NO,
                      GROUP_DESC, BENTHIC_CATEGORY)
      
      
      # Prepare format for the model - create response variable TRUE_COUNT
      reef_data.synthetic_fixed_ready_all <- reef_data.synthetic_fixed  %>%
        mutate(
          P_CODE = factor(P_CODE),
          ID = factor(ID),
          fYEAR = factor(REPORT_YEAR),
          SITE_DEPTH = ifelse(is.na(SITE_DEPTH),'_', SITE_DEPTH),  # replace missing depths with a non NA value
          fDEPTH = factor(SITE_DEPTH),
          across(c(SITE_NO, TRANSECT_NO, fYEAR, fDEPTH), function(x) factor(as.character(x))),
          DATE = as.Date(SURVEY_DATE, format = '%Y-%m-%d %h:%m:%s'),
          fGROUP = factor(GROUP_DESC)) %>%
        group_by(P_CODE, REEF_NAME, SITE_NO, TRANSECT_NO,
                 LATITUDE, LONGITUDE,
                 REPORT_YEAR, DATE, fYEAR, fDEPTH, REEF_ZONE,
                 fGROUP, GROUP_DESC) %>%
        summarise(TRUE_COUNT = n()) %>%
        ungroup(fGROUP, GROUP_DESC) %>%
        mutate(TOTAL=sum(TRUE_COUNT)) %>%
        ungroup %>%
        filter(!is.na(REPORT_YEAR)) %>% droplevels() 
      
      # Select nReef to be treated as observed locations and NAs to the others 
      ## Selecting n Reefs
      Reefs.observed <- reef_data.synthetic_fixed_ready_all  %>%
        dplyr::select(REEF_NAME) %>%
        distinct() %>%
        slice_sample(n = nLocs) %>%
        pull(REEF_NAME)
      
      reef_data.synthetic_fixed_ready <- reef_data.synthetic_fixed_ready_all %>%
        mutate(COUNT = case_when(.$REEF_NAME %in% Reefs.observed ~ TRUE_COUNT, TRUE ~ NA_real_))  %>%
        dplyr::select(P_CODE:LONGITUDE,fYEAR,fDEPTH, fGROUP, TRUE_COUNT,TOTAL, COUNT)
      
      map(reef_data.synthetic_fixed_ready, ~sum(is.na(.)))
      
      ## ----end
      
      ## --- Data visualization  
      
      p_vis_data_fixed <- ggplot(reef_data.synthetic_fixed_ready %>% filter(!is.na(COUNT)) %>% filter(fGROUP == "HCC")) + 
        geom_line(aes(x = fYEAR, y = (COUNT/TOTAL)*100, group = interaction(as.factor(TRANSECT_NO), as.factor(SITE_NO), REEF_NAME),
                      col = as.factor(SITE_NO)), 
                  show.legend = FALSE) + 
        facet_wrap(~REEF_NAME, ncol=4) + theme_bw() +
        labs(x = "Year", y = "Coral cover") +
        ylab("Coral cover") + xlab("Year")+theme_bw(base_size = 24)+
        theme(axis.text.x = element_text(size=10, angle = 90, hjust = 1),legend.position = "right",
              axis.text.y = element_text(size=10),axis.title.y=element_text(size=11),
              axis.title.x=element_text(size=11),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_rect(fill = 'white'),
              strip.text = element_text(size = 10, margin = margin())) + 
        ggtitle("Fixed design")
      
      
      output$plot <- renderPlot(p_vis_data_fixed)
      
      output$data <- DT::renderDataTable(reef_data.synthetic_fixed_ready%>% filter(.$REEF_NAME %in% Reefs.observed),options = list(pageLength =20,height = "700px"))
      
      ## Summary
      output$sum <- DT::renderDataTable(reef_data.synthetic_fixed_ready%>% filter(.$REEF_NAME %in% Reefs.observed) %>% group_by(REEF_NAME,SITE_NO,fYEAR)%>%
                                          dplyr::summarise(max = label_number(accuracy = .2, suffix = "%")((max(COUNT)/tot_pts)*100),
                                                           min = label_number(accuracy = .2, suffix = "%")((min(COUNT)/tot_pts)*100),
                                                           mean = label_number(accuracy = .2, suffix = "%")((mean(COUNT)/tot_pts)*100)))
      
      data_reef <- as.data.frame(by(reef_data.synthetic_fixed_ready%>% filter(.$REEF_NAME %in% Reefs.observed), (reef_data.synthetic_fixed_ready%>% filter(.$REEF_NAME %in% Reefs.observed))$REEF_NAME,nrow))
      
      output$data_reef <- DT::renderDataTable(data_reef %>% rename("Number of data" = "x"))
      
      output$total <- renderText(sum(data_reef$x))
      
      output$data_group <- DT::renderDataTable(by(reef_data.synthetic_fixed_ready%>% filter(.$REEF_NAME %in% Reefs.observed), (reef_data.synthetic_fixed_ready%>% filter(.$REEF_NAME %in% Reefs.observed))$fGROUP,nrow) %>% as.data.frame() %>% rename("Number of data" = "x"))
      
      report_bis <- data_validation_report() 
      data.validator::validate(data_reef, description = "Data number") %>%
       validate_cols(in_set(tot_data), x, description = "missing row") %>%
        add_results(report_bis)
     
      
      output$report_bis <- renderUI({
        render_semantic_report_ui(get_results(report = report_bis), df_error_head_n = nrow(data_reef))
      })
      
      ##---- Download
      output$downloadSynth <- downloadHandler(filename = function() {paste (input$name,'csv', sep =".")},
                                              content = function(file){write_csv(reef_data.synthetic_fixed_ready, file)
                                              })
      
      ## ---- Data validation 
      # -- Data preparation 
      
      data_hcc <- reef_data.synthetic_fixed_ready %>% 
        filter(fGROUP == "HCC") %>% 
        filter(.$REEF_NAME %in% Reefs.observed) %>% 
        mutate(percentage = (COUNT/tot_pts)*100)
      
      data_hcc <- data_hcc %>% group_by(TRANSECT_NO) %>%
        mutate(vcount = as.numeric(c(NA, as.numeric(diff(percentage))))) %>%
        ungroup()
      
      # -- Data validation 
      
      report <- data_validation_report() 
      data.validator::validate(data_hcc, description = "Data validation") %>%
        validate_cols(in_set(tot_pts), TOTAL,  description = "Exact correct number of points in frames per site ") %>%
        validate_cols(between(tot_pts - (tot_pts/100), tot_pts + (tot_pts/100)), TOTAL, description = "Nb of points in frames per site in an acceptable range") %>%
        validate_cols(predicate = not_na, COUNT, description = "No missing values") %>%
        validate_cols(predicate = under(10), vcount, description = "Coral growth under 10% per year")%>%
        add_results(report)
      
      
      output$validation <- renderUI({
        render_semantic_report_ui(get_results(report = report), df_error_head_n = nrow(data_hcc))
      })
    })} )
  
  
}



shinyApp(ui,server)
