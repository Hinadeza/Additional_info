# Produce viz model performances across simulations 

rm(list=ls())

# Indicate the working directory
setwd(paste0(getwd(),"/SP_models"))

# Loading packages and functions
source("R/packages.R")
source("R/functions.R")

# Grep names of simulations 
path <- "./"
all_dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
exclude_patterns <- c("R", "scripts", "old", "Mid_disturbance", "No_disturbance")
included_dirs <- all_dirs[!sapply(all_dirs, function(dir) any(grepl(paste(exclude_patterns, collapse = "|"), dir)))] 

path_fig.list <- list()
for (i in 1:length(included_dirs)){
path_fig.list[[i]] <- paste0(path, included_dirs[i], "/model_outputs/leave_out/table_performances_tier_true.csv")
}
file_paths <- do.call(rbind, path_fig.list)

combined_table <- file_paths %>%
  map_dfr(~ read_csv(.x) %>% 
            filter(Model == "ML_original"))

# Viz model performances
scale <- 4.5 # scale is an adjustment for a 4k screen

p_perf <- ggspider(combined_table %>% dplyr::select(Title_of_run, RMSPE, `Cvg`, `IS`, `CRPS`, time), axis_name_offset = 0.15,
               background_color = "gray98", fill_opacity = 0.15, polygon = FALSE) +
  labs(col = "Sampling design") +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 5*scale, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 4*scale, hjust = 0.5),
        plot.caption = element_text(size = 3*scale),
        legend.text = element_text(size = 2*scale),
        legend.title = element_text(size = 2*scale, face = "bold"))

ggsave(p_perf, filename = "model_perf_allML.png",
       width=5.5, height=6)
