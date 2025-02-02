
# Functions 

##################################
############ Create folders structure of the pipeline 
##################################

make_folders <- function(title_of_run){
  
  wd <- getwd() 
  
  # lEVEL 1 - name of run 
  pathway = paste0(wd,"/", title_of_run, "/")
  dir.create(pathway)
  ifelse(dir.exists(title_of_run)!=TRUE,print("directory already exists - is it a new simulation?"), FALSE)
  
  # LEVEL 2 - subdirectories 
  
  create_subdir = function (x) {
    for (i in x){
      if(!dir.exists(paste0(pathway, i))){dir.create(paste0(pathway, i))}
    }}
  
  
  create_subdir(c("data","figures", "model_outputs", "report"))
  
  # LEVEL 3 - subsubdirectories 
  
  # within model_outputs 
  pathway_2 = paste0(wd,"/", title_of_run, "/model_outputs/")
  
  create_subsubdir = function (x) {
    for (i in x){
      if(!dir.exists(paste0(pathway_2, i))){dir.create(paste0(pathway_2, i))}
    }}
  
  create_subsubdir(c("full_run","leave_out", "predictions"))
  
  # within report 
  pathway_3 = paste0(wd,"/", title_of_run, "/report/")

  create_subsubdir2 = function (x) {
  for (i in x){
    if(!dir.exists(paste0(pathway_3, i))){dir.create(paste0(pathway_3, i))}
  }}

  create_subsubdir2(c("extra","resources"))
  
  # copying file in new folder 
  
  file.copy(paste0(wd,"/report/resources/toc_logo.html"), paste0(pathway_3,"/resources/"))

}


##################################
############ Information about the depth(s) 
##################################
depth_info <- function(x) {
if (x > 1){
  Depth_info = seq(3, 10, length=x)
}else{
  Depth_info = 10
}
return(Depth_info)
}

##################################
############ Select nth element from a vector 
##################################

nth_element <- function(vector, starting_position, n) { 
  vector[seq(starting_position, length(vector), n)] 
  }

##################################
############ Extract covariates 
##################################
extract_cov <- function(predictive_layer, cov_name) {

intersectlist <- st_intersects(predictive_layer, cov_name %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326)))
  
datlist <- list()
for (i in 1:nrow(pred_layer)){
  intersect <-  intersectlist[[i]]
  datlist[[i]] <-  cov_name[intersect,] %>%
       group_by(Year) %>%
       summarize(mean_value = mean(Value)) %>%
       mutate(tier = i + 999) %>%
       mutate(fYEAR = Year + min(dat$fYEAR) - 1)
  }
  
dat_all <- do.call(rbind, datlist) %>%
    merge(pred_layer) %>%
    st_as_sf() %>%
    dplyr::select(tier, fYEAR, mean_value,geometry)

return(dat_all)
}

##################################
############ GAM model 
##################################

# Picks number of spatial and temporal knots from mgcv models 

# pick_knots_mgcv <- function(dat) {
# 
# # Degree of freedom = number of unique location x number of years 
#  df <- dat %>%
#     group_by(LONGITUDE, LATITUDE) %>%
#     slice(1)
#   
#   max_kspat <- nrow(df)
#   max_ktemp <- length(unique(dat$fYEAR))
#   
#   kspat <- seq(30, max_kspat, by = 20) # minium of 30 knots on the spatial dimension
#   ktemp <- seq(8, max_ktemp, by = 2)  # minimum of 10 knots of the temporal dimension
#   
#   knbre<- expand.grid(kspat,ktemp)
#   
#   mod_list <- list()
#   
#   for ( i in 1 : nrow(knbre)){
#     
#     mod0 <- mgcv::gam(COUNT/TOTAL ~ te(LONGITUDE,LATITUDE, fYEAR, # inputs over which to smooth
#                                        bs = c("tp", "cr"), # types of bases
#                                        k=c(knbre[i,1],knbre[i,2]), # knot count in each dimension
#                                        d=c(2,1)), # (s,t) basis dimension
#                       data = dat,
#                       control =  gam.control(scalePenalty = FALSE),
#                       method = "GCV.Cp", family = binomial("logit"),
#                       weights = TOTAL)
#     
#     mod_list[[i]] <- cbind(as.numeric(summary(mod0)$r.sq), as.numeric(summary(mod0)$s.table[[1]]), as.numeric(summary(mod0)$sp.criterion),
#                            as.numeric(AIC(mod0)), knbre[i,1],knbre[i,2])
#   }
#   
#   table_result <- do.call(rbind, mod_list) 
#   colnames(table_result) <- c("Rsquared", "edf", "GCV", "AIC", "kspat", "ktemp")
#   
#   ## Criteria 
#   # edf cannot be greater than degree of freedom 
#   ## lowest GCV
#   ## highest r2
#   ## lowest AIC
#   
#   table_result <- table_result %>%
#     data.frame() %>%
#     arrange(desc(Rsquared), GCV, desc(AIC))
#   
#   return(table_result)
# }

##################################
############ INLA model 
##################################

inla_prep <- function(dat, hexpred){
  
   ## ---- meshINLADataGrid
full.grid <- hexpred %>%
              st_centroid() %>%
              mutate(Longitude = st_coordinates(.)[,1],
                     Latitude = st_coordinates(.)[,2]) %>%
              st_drop_geometry()
  
full.coords <- full.grid %>%
              dplyr::select(Longitude,Latitude) %>%
              distinct()

max.edge =diff(range(full.coords[,1]))/15
bound.outer = diff(range(full.coords[,1]))/15
pol =   st_bbox(hexpred) %>%  st_as_sfc() 

mesh = inla.mesh.2d(loc.domain = st_coordinates(pol)[,1:2],
                     n=4,
                     max.edge = c(1,2)*max.edge,
                     offset=c(max.edge, bound.outer),
                     cutoff = max.edge/5)

 class(mesh) <- "inla.mesh"

spde <- inla.spde2.matern(mesh, alpha = 2) #, prior.range = c(.02, .01), prior.sigma = c(3, 0.01)
  i.spatial <- inla.spde.make.index('spatial.field',
                                    n.spde = spde$n.spde,
                                    n.group = length(unique(full.grid$fYEAR)))
# Data grid
hexpred_unique <- hexpred %>%
                group_by(tier) %>%
                filter(row_number() == 1) %>%
                dplyr::select(tier) 

data.grid_sf <- dat %>%
               filter(tier_cat == "tier_train") %>%
               left_join(hexpred_unique) %>% 
              mutate(
                P_CODE = factor(P_CODE),
                SITE_NO = factor(interaction(Reef, SITE_NO)),
                TRANSECT_NO = factor(interaction(SITE_NO, TRANSECT_NO)),
                Reef = factor(interaction(tier, Reef))) %>%
              dplyr::select(P_CODE, tier, fYEAR, fDEPTH, Reef, SITE_NO, TRANSECT_NO, DHW, CYC, OT, COUNT, TOTAL, geometry) %>%
              distinct() %>%
              st_as_sf()

# Filter NAs in covariate because of issues with edging (to be fixed later) 

data.grid_sf <- data.grid_sf %>% 
filter(is.na(st_dimension(.)) == FALSE )

coords <- data.grid_sf %>%
             st_centroid() %>%
             st_coordinates() %>%
              `[`(,c('X','Y'))

data.grid_sf$fYEAR <- as.factor(data.grid_sf$fYEAR)
  
data.grid <- data.grid_sf %>%
              mutate(fYEAR = factor(fYEAR, levels = rev(sort(levels(fYEAR))))) %>%
              st_drop_geometry() %>%
              data.frame()

 A.est <- inla.spde.make.A(mesh = mesh,
                            loc = coords,
                            group = as.numeric(data.grid$fYEAR))
  
  covariate <- data.grid %>%
    dplyr::select(fYEAR, Reef, DHW, CYC, OT, SITE_NO, TRANSECT_NO) %>%
    st_drop_geometry()
  
  covariate$Reef <- as.numeric(as.factor(covariate$Reef))
  covariate$DHW <- scale(covariate$DHW, scale = FALSE)
  covariate$CYC <- scale(covariate$CYC, scale = FALSE)
  covariate$OT <- scale(covariate$OT, scale = FALSE)

# Testing objects

data.grid_p_sf <- dat %>% 
              group_by(tier, fYEAR) %>%
              mutate(TRUE_COUNT_tier = mean(TRUE_COUNT),
              TOTAL_tier = mean(TOTAL)) %>%
              filter(row_number() == 1) %>%
              dplyr::select(tier, fYEAR, TRUE_COUNT_tier, TOTAL_tier, tier_cat, Reef, DHW, CYC, OT) %>%
              filter(tier_cat == "tier_test") %>%
              left_join(hexpred_unique) %>%
              ungroup() %>%
              st_as_sf()

# Filter NAs in covariate because of issues with edging (to be fixed later) 

data.grid_p_sf <- data.grid_p_sf %>% 
filter(is.na(st_dimension(.)) == FALSE )

coords_p <- data.grid_p_sf %>%
             st_centroid() %>%
             st_coordinates() %>%
              `[`(,c('X','Y'))

data.grid_p_sf$fYEAR <- as.factor(data.grid_p_sf$fYEAR)
  
data.grid_p <- data.grid_p_sf %>%
              mutate(fYEAR = factor(fYEAR, levels = rev(sort(levels(fYEAR))))) %>%
              mutate(TOTAL_tier = 500) %>%
              st_drop_geometry() %>%
              data.frame()

A.pred <- inla.spde.make.A(mesh = mesh,
                            loc = coords_p,
                            group = as.numeric(data.grid_p$fYEAR))

covariate_p <- data.grid_p %>%
  dplyr::select(fYEAR, Reef, DHW, CYC, OT) %>%
  st_drop_geometry()

covariate_p$Reef <- as.numeric(as.factor(covariate_p$Reef))
covariate_p$DHW <- scale(covariate_p$DHW, scale = FALSE)
covariate_p$CYC <- scale(covariate_p$CYC, scale = FALSE)
covariate_p$OT <- scale(covariate_p$OT, scale = FALSE)

# Stacks 

rprior<- list(theta = list(prior = "pccor1", param = c(0,0.9)))

stack.est <- inla.stack(data=list(y = data.grid$COUNT,
                                    Total=data.grid$TOTAL),
                          A=list(A.est, 1, 1, 1, 1, 1),
                          effects=list(c(i.spatial, list(b0=1)),
                          list(fYEAR = covariate$fYEAR),
                          list(Reef = covariate$Reef),
                          list(DHW = covariate$DHW),
                          list(CYC = covariate$CYC),
                          list(OT = covariate$OT)),
                          tag = 'est')


stack.pred <- inla.stack(data=list(y = NA,
                                  Total= data.grid_p$TOTAL_tier),
                        A=list(A.pred, 1, 1, 1, 1, 1),
                        effects=list(c(i.spatial, list(b0=1)),
                                     list(fYEAR = covariate_p$fYEAR),
                                     list(Reef = covariate_p$Reef),
                                     list(DHW = covariate_p$DHW),
                                     list(CYC = covariate_p$CYC),
                                     list(OT = covariate_p$OT)),
                        tag = 'pred')

stack.full <- inla.stack(stack.est, stack.pred)
                      
  if(formula == "cov"){
    form <- y ~ -1 + b0 + fYEAR + DHW + CYC + OT +
    f(spatial.field, model = spde, group = spatial.field.group, 
    control.group = list(model = "ar1", hyper=rprior)) + 
    f(Reef, model = "iid") 
  }else{
    form <- y ~ -1 + b0 + fYEAR +
    f(spatial.field, model = spde, group = spatial.field.group, 
    control.group = list(model = "ar1", hyper=rprior)) + 
    f(Reef, model = "iid") 
  }

  obj_inla <- list("stack.est" = stack.est, "spde" = spde, "form" = form, "Total" = dat$TOTAL, "mesh" = mesh,
                   "i.spatial" = i.spatial, "formula" = formula, "stack.full" = stack.full)
  
  return(obj_inla)
}

##################################
############ FRK model 
##################################

frk_prep <- function(dat){
  
  ## Construct STIDF object from data
  dat$Year <- as.Date(paste0(as.character(dat$fYEAR),"-01-01")) 
  dat$k_Z <- dat$TOTAL                                         
  lon_idx <- which(names(dat) == "LONGITUDE")                  
  lat_idx <- which(names(dat) == "LATITUDE")
  STObj <- stConstruct(x = dat,                               
                       space = c(lon_idx, lat_idx), 
                       time = "Year",                      
                       interval = TRUE)     
  
  ## Predictive layer
  HexPred_sp <- as_Spatial(hexpred)                                   
  nHEX <- nrow(subset(HexPred_sp, fYEAR == unique(dat$fYEAR)[1]))       
  nYEAR <- length(unique(HexPred_sp@data$fYEAR))            
  
  HexPred_sp@data$n_spat <- rep(1:nHEX, each = nYEAR)   
  BAUs_spat <- subset(HexPred_sp, fYEAR == unique(dat$fYEAR)[1])        
  coordnames(BAUs_spat) <- c("LONGITUDE", "LATITUDE")
  
  nrow(BAUs_spat@data)
  
  ## Construct spatio-temporal BAUs (will not contain covariate information for now)
  ST_BAUs <- auto_BAUs(manifold = STplane(),
                       data = STObj,
                       spatial_BAUs = BAUs_spat,
                       tunit = "years")
  
  ST_BAUs <- ST_BAUs[, 1:nYEAR, 1:2]                 
  ST_BAUs$fYEAR <- as.character(ST_BAUs$t + unique(dat$fYEAR)[1]-1)    
  ST_BAUs$n_spat <- rep(1:nHEX, nYEAR)              
  
  HexPred_sp@data$fYEAR <- as.character(HexPred_sp@data$fYEAR) 
  HexPred_sp@data$tier <- as.factor(HexPred_sp@data$tier) 
  
  ST_BAUs@data <- left_join(ST_BAUs@data, HexPred_sp@data , by = c("fYEAR","n_spat")) 
  
  ST_BAUs$fs <- 1                  
  ST_BAUs@sp@proj4string  <- CRS()  
  
  head(ST_BAUs@data)
  head(HexPred_sp@data)
  
  ## Covariates must only be in BAUs, so remove covariates associated with data
  overlapping_fields <- intersect(names(STObj@data), names(ST_BAUs@data))
  STObj@data[,overlapping_fields] <- NULL
  
  ## Create basis functions
  basis <- auto_basis(STplane(),
                      STObj,
                      tunit = "years",
                      #nres = 2L, # for development
                      nres = 3L, # for final run
                      regular = TRUE)

  
  obj_frk <- list("ST_BAUs" = ST_BAUs, "STObj" = STObj, "basis" = basis)
  return(obj_frk)
  
}

##################################
############ model predictions 
##################################

predictions_INLA <- function(model.out, n.sim){
  
draws <- inla.posterior.sample(n.sim, result=model.out, seed=123) 
  
hexpred_dat <- hexpred %>%
    st_centroid() %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>%
    dplyr::select(fYEAR, Longitude, Latitude, Reef, tier, DHW, CYC, OT) %>%
    st_drop_geometry()%>% 
    mutate(across(fYEAR, as.factor)) 
  
  full.coords <- hexpred_dat %>%
    dplyr::select(Longitude,Latitude) %>%
    distinct()

  proj.grid <- inla.mesh.projector(obj_inla$mesh, loc=as.matrix(full.coords))
  cellmeans = sapply(draws, function(x) x[['latent']])
  
  if(obj_inla$formula == "cov"){
  i.mod <- sapply(c('APredictor','^Predictor','spatial.field','Reef','fYEAR[0-9]*[:][0-9]*$', 'DHW', 'CYC', 'OT', 'b0'),
                  function(x) grep(x, draws[[1]]$latent %>% rownames))
  }else{
  i.mod <- sapply(c('APredictor','^Predictor','spatial.field','Reef','fYEAR[0-9]*[:][0-9]*$','b0'),
                  function(x) grep(x, draws[[1]]$latent %>% rownames))
  }

  # retrieve the spatial.fields posteriors
  
  cellmeans.full <- cellmeans[i.mod[[3]],] %>%         
    as.data.frame %>%                                 
    mutate(fYEAR = rep(as.numeric(levels(obj_inla$stack.est$effects$data$fYEAR)),
                       each = which(obj_inla$i.spatial$spatial.field.group == 1) %>% length)) %>%
    group_by(fYEAR) %>%
    tidyr::nest() %>% 
    mutate(Spatial = map(.x = data,
                         .f = function(x)
                           as.matrix(inla.mesh.project(proj.grid, x)))) %>%
    mutate(geometry = list(hexpred_dat %>%
                             dplyr::select(tier, Longitude, Latitude) %>%
                             distinct())) 
  
  cellmeans.full$fYEAR <- as.factor(cellmeans.full$fYEAR)
  
  # retrieve the fixed effects 
  
  if(obj_inla$formula == "cov"){
  Xmat <- cbind(1, model.matrix(~ -1 + fYEAR + DHW + CYC + OT, data=hexpred_dat))
  wch <- grep(paste0(c("b0","fYEAR", "DHW", "CYC", "OT"), collapse="|"), names(i.mod))
  ii = unlist(i.mod[wch])
  }else{
  Xmat <- cbind(1, model.matrix(~ -1 + fYEAR, data=hexpred_dat)) 
  wch <- grep(paste0(c("b0","fYEAR"), collapse="|"), names(i.mod))
  ii = unlist(i.mod[wch])}

 cellmeans.full.1 <- t(cellmeans[ii,]) %*% t(Xmat)
    
  cellmeans.fixed <- hexpred_dat %>%
    dplyr::select(Longitude, Latitude, fYEAR) %>%
    cbind(V = t(cellmeans.full.1)) %>%
    as.data.frame %>%                           
    dplyr::select(starts_with("V"))%>%
    mutate(fYEAR = rep(unique(hexpred_dat$fYEAR),each=nrow(full.coords)))%>%
    group_by(fYEAR) %>%
    tidyr::nest() 

  # Add the posteriors together
  cellmeans.full.c <-
    cellmeans.full %>%
    full_join(cellmeans.fixed %>%
                rename(data1 = data)) %>%
    mutate(value = map2(.x = Spatial, .y = data1,
                        .f = function(.x, .y) as.data.frame(.x + .y))) %>%
    dplyr::select(fYEAR, geometry, value) %>%
    tidyr::unnest(cols = c(geometry, value)) %>%
    tidyr::pivot_longer(c = starts_with("V"), names_to = "Rep") %>%
    mutate(Rep = gsub('\\.','',Rep)) %>%
    ungroup()
  
  cellmeans.full.cc <- cellmeans.full.c %>%
    mutate(pred = plogis(value))
  
  # ###################################### Sum across tiers 
  
  pred_sum_sf <- cellmeans.full.cc %>%
    group_by(fYEAR,tier) %>% 
    median_hdci(pred) %>%
    inner_join(hexpred %>% group_by(tier) %>% slice(1) %>% dplyr::select(geometry,tier)) %>% 
    st_as_sf(sf_column_name = "geometry") %>%
    mutate(Unc = .upper - .lower) %>%
    mutate(tier_fYEAR = paste0(tier,fYEAR)) %>%
    dplyr::select(fYEAR, tier, pred, .lower, .upper, Unc, tier_fYEAR)

  return(pred_sum_sf)
 
}

predictions_INLA_stacked <- function(model.out, hexpred, dat, index){

hexpred_unique <- hexpred %>%
                group_by(tier) %>%
                filter(row_number() == 1) %>%
                dplyr::select(tier) 

data.grid_p_sf <- dat %>% 
              group_by(tier, fYEAR) %>%
              filter(row_number() == 1) %>%
              filter(tier_cat == "tier_test")  %>%
              left_join(hexpred_unique) %>%
              ungroup() %>%
              st_as_sf()

# Filter NAs in covariate because of issues with edging (to be fixed later) 

data.grid_p <- data.grid_p_sf %>% 
filter(is.na(st_dimension(.)) == FALSE )


data.grid_p$pred <- model.out$summary.fitted.values[index,"mean"]
data.grid_p$.lower <- model.out$summary.fitted.values[index,"0.025quant"]
data.grid_p$.upper <- model.out$summary.fitted.values[index,"0.975quant"]

pred_sum_sf <- data.grid_p %>% 
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(4326)) %>%
    mutate(Unc = .upper - .lower) %>%
    mutate(tier_fYEAR = paste0(tier,fYEAR)) %>%
    dplyr::select(fYEAR, tier, pred, .lower, .upper, Unc, tier_fYEAR) 

  return(pred_sum_sf)
  
}

predictions_FRK <- function(model.out){
  
  pred <- predict(model.out, type = c("mean"))
  
  # Extracting posterior distributions of predictive locations 
  
  post_dist_df <- as.data.frame(pred$MC$mu_samples) %>% 
    mutate(fYEAR = obj_frk$ST_BAUs@data$fYEAR) %>%
    mutate(tier = obj_frk$ST_BAUs@data$tier) %>%
    mutate(id_loc = row_number()) %>%
    tidyr::pivot_longer(!c(fYEAR,tier,id_loc),
                        names_to = "draw", 
                        values_to = "pred"
    )
  
  # Summary predictions at tier level
  hexpred$tier <- as.factor(hexpred$tier)
  
  pred_sum_sf <- post_dist_df %>% group_by(fYEAR,tier) %>% 
    median_hdci(pred)%>%
    inner_join(hexpred %>% group_by(tier) %>% slice(1) %>% dplyr::select(geometry,tier)) %>% 
    st_as_sf(sf_column_name = "geometry") %>%
    mutate(Unc = .upper - .lower) %>%
    mutate(tier_fYEAR = paste0(tier,fYEAR)) %>%
    dplyr::select(fYEAR, tier, pred, .lower, .upper, Unc, tier_fYEAR)

  return(pred_sum_sf)
}

predictions_brms <- function(model.out, n.sim){
  
  hexpred_dat <- hexpred %>%
    st_centroid() %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>%
    dplyr::select(fYEAR, Longitude, Latitude, DHW, CYC, OT, Reef, tier) %>%
    st_drop_geometry()%>% 
    mutate(TOTAL = round(mean(dat$TOTAL),0)) %>%
    rename(LONGITUDE = Longitude) %>%
    rename(LATITUDE = Latitude)
  
  
  pred <- predict(model.out, hexpred_dat, allow_new_levels = TRUE, ndraws = n.sim,
                  incl_autocor = TRUE, nug =  1e-12) %>%
    data.frame() %>%
    mutate(across(everything()), . / unique(hexpred_dat$TOTAL)) 
 
  # Summary predictions at tier level
  hexpred$tier <- as.factor(hexpred$tier)
  
  pred_sum_sf <- pred %>%
    data.frame() %>%
    cbind(hexpred) %>% 
    st_as_sf(sf_column_name = "geometry") %>%
    mutate(Unc = Q97.5 - Q2.5) %>%
    mutate(tier_fYEAR = paste0(tier,fYEAR)) %>%
    rename(pred = Estimate) %>%
    rename(.upper = Q97.5) %>%
    rename(.lower = Q2.5) %>%
    mutate(tier_fYEAR = paste0(tier,fYEAR)) %>%
    dplyr::select(fYEAR, tier, pred, .lower, .upper, Unc, tier_fYEAR)

  return(pred_sum_sf)
  
}

##################################
############ model predictions - broad spatial scale
##################################

predictions_INLA_broad <- function(model.out, n.sim){
  
draws <- inla.posterior.sample(n.sim, result=model.out, seed=123) 
  
hexpred_dat <- hexpred %>%
    st_centroid() %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>%
    dplyr::select(fYEAR, Longitude, Latitude, Reef, tier) %>%
    st_drop_geometry()%>% 
    mutate(across(fYEAR, as.factor)) 
  
  full.coords <- hexpred_dat %>%
    dplyr::select(Longitude,Latitude) %>%
    distinct()
   
  proj.grid <- inla.mesh.projector(obj_inla$mesh, loc=as.matrix(full.coords))
  cellmeans = sapply(draws, function(x) x[['latent']])
  
  i.mod <- sapply(c('APredictor','^Predictor','spatial.field','Reef','fYEAR[0-9]*[:][0-9]*$'),
                  function(x) grep(x, draws[[1]]$latent %>% rownames))
  
  # retrieve the spatial.fields posteriors
  
  cellmeans.full <- cellmeans[i.mod[[3]],] %>%         
    as.data.frame %>%                                 
    mutate(fYEAR = rep(as.numeric(levels(obj_inla$stack.est$effects$data$fYEAR)),
                       each = which(obj_inla$i.spatial$spatial.field.group == 1) %>% length)) %>%
    group_by(fYEAR) %>%
    tidyr::nest() %>% 
    mutate(Spatial = map(.x = data,
                         .f = function(x)
                           as.matrix(inla.mesh.project(proj.grid, x)))) %>%
    mutate(geometry = list(hexpred_dat %>%
                             dplyr::select(tier, Longitude, Latitude) %>%
                             distinct())) 
  
  cellmeans.full$fYEAR <- as.factor(cellmeans.full$fYEAR)
  
  # retrieve the fixed effects 
  
  Xmat <- model.matrix(reformulate(c("0 + fYEAR")), data=hexpred_dat)
  
  #wch <- c(6,7,8)
  #ii = unlist(i.mod[wch])
  
  ii = unlist(i.mod[5])
  
  cellmeans.full.1 <- t(cellmeans[ii,]) %*% t(Xmat)
  
  cellmeans.fixed <- hexpred_dat %>%
    dplyr::select(Longitude, Latitude, fYEAR) %>%
    cbind(V = t(cellmeans.full.1)) %>%
    as.data.frame %>%                           
    dplyr::select(starts_with("V"))%>%
    mutate(fYEAR = rep(unique(hexpred_dat$fYEAR),each=nrow(full.coords)))%>%
    group_by(fYEAR) %>%
    tidyr::nest() 
  
  # Add the posteriors together
  cellmeans.full.c <-
    cellmeans.full %>%
    full_join(cellmeans.fixed %>%
                rename(data1 = data)) %>%
    mutate(value = map2(.x = Spatial, .y = data1,
                        .f = function(.x, .y) as.data.frame(.x + .y))) %>%
    dplyr::select(fYEAR, geometry, value) %>%
    tidyr::unnest(cols = c(geometry, value)) %>%
    tidyr::pivot_longer(c = starts_with("V"), names_to = "Rep") %>%
    mutate(Rep = gsub('\\.','',Rep)) %>%
    ungroup()
  
  cellmeans.full.cc <- cellmeans.full.c %>%
    mutate(pred = plogis(value))
  
  # ###################################### Sum across tiers 
  
  pred_sum_sf <- cellmeans.full.cc %>%
    group_by(fYEAR) %>% 
    median_hdci(pred) 

  return(pred_sum_sf)
 
}

predictions_INLA_broad_stacked <- function(model.out, dat, index, hexpred){

hexpred_unique <- hexpred %>%
                group_by(tier) %>%
                filter(row_number() == 1) %>%
                dplyr::select(tier) 

data.grid_p <- dat %>% 
              group_by(tier, fYEAR) %>%
              filter(row_number() == 1) %>%
              filter(tier_cat == "tier_test")  %>%
              left_join(hexpred_unique) %>%
              ungroup() %>%
              st_as_sf()

data.grid_p$pred <- model.out$summary.fitted.values[index,"mean"]
data.grid_p$.lower <- model.out$summary.fitted.values[index,"0.025quant"]
data.grid_p$.upper <- model.out$summary.fitted.values[index,"0.975quant"]

# Filter NAs in covariate because of issues with edging (to be fixed later) 

data.grid_p <- data.grid_p %>% 
filter(is.na(st_dimension(.)) == FALSE )

pred_sum_sf <- data.grid_p %>%
    mutate(Unc = .upper - .lower) %>%
    mutate(tier_fYEAR = paste0(tier,fYEAR)) %>%
    dplyr::select(fYEAR, tier, pred, .lower, .upper, Unc, tier_fYEAR) %>%
    group_by(fYEAR) %>% 
    median_hdci(pred) 

  return(pred_sum_sf)
  
}



predictions_FRK_broad <- function(model.out){
  
  pred <- predict(model.out, type = c("mean"))
  
  # Extracting posterior distributions of predictive locations 
  
  post_dist_df <- as.data.frame(pred$MC$mu_samples) %>% 
    mutate(fYEAR = obj_frk$ST_BAUs@data$fYEAR) %>%
    mutate(tier = obj_frk$ST_BAUs@data$tier) %>%
    mutate(id_loc = row_number()) %>%
    tidyr::pivot_longer(!c(fYEAR,tier,id_loc),
                        names_to = "draw", 
                        values_to = "pred"
    )
  
  # Summary predictions at tier level
  hexpred$tier <- as.factor(hexpred$tier)
  
  pred_sum_sf <- post_dist_df %>% group_by(fYEAR) %>% 
    median_hdci(pred)

  return(pred_sum_sf)
}

predictions_brms_broad <- function(model.out, n.sim){
  
  hexpred_dat <- hexpred %>%
    st_centroid() %>%
    mutate(Longitude = st_coordinates(.)[,1],
           Latitude = st_coordinates(.)[,2]) %>%
    dplyr::select(fYEAR, Longitude, Latitude, DHW, CYC, OT, Reef, tier) %>%
    st_drop_geometry()%>% 
    mutate(TOTAL = round(mean(dat$TOTAL),0)) %>%
    rename(LONGITUDE = Longitude) %>%
    rename(LATITUDE = Latitude)
  
  
  pred <- predict(model.out, hexpred_dat, allow_new_levels = TRUE, ndraws = n.sim,
                  incl_autocor = TRUE) %>%
    data.frame() %>%
    mutate(across(everything()), . / unique(hexpred_dat$TOTAL)) 
 
  # Summary predictions at tier level
  hexpred$tier <- as.factor(hexpred$tier)
  
  pred_sum_sf <- pred %>%
    data.frame() %>%
    cbind(hexpred) %>% 
    st_as_sf(sf_column_name = "geometry") %>%
    mutate(Unc = Q97.5 - Q2.5) %>%
    mutate(tier_fYEAR = paste0(tier,fYEAR)) %>%
    rename(pred = Estimate) %>%
    rename(.upper = Q97.5) %>%
    rename(.lower = Q2.5) %>% group_by(fYEAR) %>% 
    median_hdci(pred)

  return(pred_sum_sf)
  
}

##################################
############ plotting 
##################################

plot_predictions <- function(dat){

pal_pred <- lacroix_palette("Pamplemousse", n = 10, type = "continuous")
  
  p_pred <- ggplot() + 
    geom_sf(data = dat, aes(fill = pred)) +
    facet_wrap(~fYEAR) +  scale_fill_gradientn(colours = pal_pred) + 
    theme_bw() + 
    xlab("longitude") +
    ylab("latitude")

return(p_pred)
}

plot_predictions_unc <- function(dat){
  
pal_unc<- wes_palette("Zissou1", 10, type = "continuous")

  p_unc <- ggplot() + 
    geom_sf(data = dat, aes(fill = Unc)) +
    facet_wrap(~fYEAR) +  scale_fill_gradientn(colours = pal_unc) + 
    theme_bw() + 
    xlab("longitude") +
    ylab("latitude")

return(p_unc)
}

plot_diff <- function(dat){
  
  p_diff <- ggplot() + 
    geom_sf(data = dat, aes(fill = Diff)) +
    geom_sf(data = dat %>% filter(tier_cat == "tier_train"), col = "black") +
    facet_wrap(~fYEAR) +  
    theme_bw() + 
    xlab("longitude") +
    ylab("latitude") + 
    scale_fill_continuous(
    type = "viridis",
    guide = "colorbar",
    na.value = "transparent",
    limits = c(-0.7, 0.7)
  )

return(p_diff)
}

plot_traj <- function(dat, dat_pred){
p_traj <- ggplot() + 
  geom_line(data = dat, aes(x = fYEAR, y = (COUNT/TOTAL)*100, group = interaction(as.factor(TRANSECT_NO), REEF_NAME)), 
            show.legend = FALSE, linewidth=.1, col="grey30") + 
  geom_ribbon(data = dat_pred, aes(x=fYEAR,ymin=.lower*100, ymax=.upper*100, group=1),alpha=.2, fill ="#0072B2FF") +
  geom_line(data = dat_pred, aes(x=fYEAR, y=pred*100, group=1),size=.4) +
  facet_wrap(~tier, ncol=3) +
  ylab("Cover") + xlab("Year")+theme_bw()+
  theme(axis.text.x = element_text(size=8, angle = 90, hjust = 1),
        axis.text.y = element_text(size=8),axis.title.y=element_text(size=11),
        axis.title.x=element_text(size=11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'white')) +
  scale_x_discrete(breaks= nth_element(unique(dat$fYEAR),1,4))

return(p_traj)
}

plot_traj_broad <- function(dat, GRMF_all){
  ggplot() +
  geom_ribbon(data = pred_sum_sf %>% data.frame(), aes(x = fYEAR, ymin=.lower, ymax=.upper, group=1), alpha=.2, fill="#83BFA9")+
  geom_line(data = pred_sum_sf %>% data.frame(), aes(x=fYEAR, y=pred,group=1), col="black", linewidth=1.1)+
  geom_point(data = pred_sum_sf %>% data.frame(), aes(x=fYEAR, y=pred), col="black", size=2.1)+
  geom_line(data = GRMF_all, aes(x=fYEAR, y=true, group=1),size=.4, col = "blue", linetype = "dashed") +
  xlab("Year") +ylab("Coral cover")+theme_bw()+
  ylim(0,1) +
  theme(axis.text.x = element_text(size=13),legend.position = "none",
        axis.text.y = element_text(size=13),axis.title.y=element_text(size=15),
        axis.title.x=element_text(size=15))+
  scale_x_discrete(breaks= nth_element(unique(dat$fYEAR),1,4))

}
##################################
############ predictive indicators  
##################################

## 95% coverage
coverage95 <- function(z, lower, upper) {
  sum((z < upper) & (z > lower)) / length(z)
}

## 95% interval score
IS95 <- function(true, lower, upper) {
  alpha = 0.05
  pred95l <- lower 
  pred95u <- upper
  ISs <- (pred95u - pred95l) + 2/alpha * (pred95l - true) * (true < pred95l) +
    2/alpha * (true - pred95u) * (true > pred95u)
  mean(ISs)
}

## Root-mean-squared prediction error
RMSPE <- function(z,pred) {
  Y <- (z - pred)^2
  sqrt(mean(Y))
}

crps <- function(obs, pred, ...)
  ## Tilmann Gneiting's crps code, assumes pred is either a vector of length
  ## 2 (mu, sig) or a matrix of mu and sig if each forcast is different
{
  if(is.null( dim(pred)) & length(pred)==2){mu <- pred[1];
  sigma <- pred[2]} else {
    mu<- as.numeric( pred[,1] ); sigma <- as.numeric( pred[,2]) }
  
  z <- (obs-mu)/sigma ## center and scale
  
  crps<- sigma * (z*(2*pnorm(z,0,1)-1) + 2*dnorm(z,0,1) - 1/sqrt(pi))
  ign <-  0.5*log(2*pi*sigma^2) + (obs - mu)^2/(2*sigma^2)
  pit <- pnorm(obs, mu,sigma )
  
  return(list(crps = crps, CRPS = mean(crps), ign = ign, IGN = mean(ign), pit = pit) )
  
}

## data.validator helper function

# - Annual growth under a%
under <- function(a) {
  function(x) {
    ifelse( x <= a | is.na(x), TRUE, FALSE)
  }
}

# - Range for correct number of points in frames per transect
between <- function(a,b) {
  function(x) {
    ifelse( a<= x & x<= b, TRUE, FALSE)
  }
}

# # MATERN SPDE MODEL IN MGCV 
# # These functions define the Matern SPDE model as a basis-penalty smoother
# # Setup the SPDE Matern smooth in mgcv
# # See ?smooth.construct in mgcv for details of the input and output 
# # Special note: the xt argument in s(..., bs = "spde", xt = ...) can be used
# # to specify a mesh, if NULL, a mesh with regular knots is constructed. 
# smooth.construct.spde.smooth.spec <- function(object, data, knots){
#   # observation locations
#   dim <- length(object$term) 
#   if (dim > 2 | dim < 1) stop("SPDE Matern can only be fit in 1D or 2D.")
#   if (dim == 1) {
#     x <- data[[object$term]]
#   } else {
#     x <- matrix(0, nr = length(data[[1]]), nc = 2) 
#     x[,1] <- data[[object$term[1]]]
#     x[,2] <- data[[object$term[2]]]  
#   }
#   # setup mesh or accept user mesh
#   if (is.null(object$xt)) {
#     if (dim == 1) {
#       t <- seq(min(x), max(x), len=object$bs.dim)
#       mesh <- inla.mesh.1d(loc=t, degree=2, boundary="free") 
#     } else {
#       stop("For 2D, mesh must be supplied as argument xt$mesh in s(...,xt = )")
#     }
#   } else {
#     if (class(object$xt$mesh) != "inla.mesh") stop("xt must be NULL or an inla.mesh object")
#     mesh <- object$xt$mesh 
#   }
#   # model matrix: projects parameters to observation locations on mesh 
#   object$X <- as.matrix(inla.spde.make.A(mesh, x))
#   # compute finite element matrices used as smoothing penalty matrices 
#   inlamats <- inla.mesh.fem(mesh)
#   object$S <- list()
#   object$S[[1]] <- as.matrix(inlamats$c1)
#   object$S[[2]] <- 2 * as.matrix(inlamats$g1)
#   object$S[[3]] <- as.matrix(inlamats$g2)
#   # L is a matrix with a column for each smoothing parameter (tau, kappa) 
#   # and a row for each smoothing matrix (c1, g1, g2). 
#   # The (i,j)^th entry of L contains the power that smoothing parameter i 
#   # is computed to before being multiplied by smoothing matrix j. 
#   # E.g. If (1, 2) has value 4, then smoothing parameter 2 (kappa) is taken
#   # to the power 4 before being multiplied by smoothing matrix 1 (c1): i.e. kappa^4*c1
#   # All of these computations for each element of L are then summed to create a single
#   # smoothing matrix. 
#   object$L <- matrix(c(2,2,2,4,2,0), ncol = 2)
#   # Rank is the basis dimension, it is repeated for each smoothing matrix 
#   object$rank <- rep(object$bs.dim,3)
#   # As kappa > 0, the null space of the Matern SPDE is empty 
#   object$null.space.dim <- 0 
#   # Save the mesh
#   object$mesh <- mesh
#   object$df <- ncol(object$X)     # maximum DoF (if unconstrained)
#   # Give object a class
#   class(object) <- "spde.smooth" 
#   return(object)
# }
# 
# # Prediction function for the `spde' smooth class
# # See ?smooth.construct in mgcv for details on input and output 
# Predict.matrix.spde.smooth <- function(object, data){
#   dim <- length(object$term) 
#   if (dim > 2 | dim < 1) stop("SPDE Matern can only be fit in 1D or 2D.")
#   if (dim == 1) {
#     x <- data[[object$term]]
#   } else {
#     x <- matrix(0, nr = length(data[[1]]), nc = 2) 
#     x[,1] <- data[[object$term[1]]]
#     x[,2] <- data[[object$term[2]]]  
#   }
#   Xp <- inla.spde.make.A(object$mesh, x)
#   return(as.matrix(Xp))
# }
