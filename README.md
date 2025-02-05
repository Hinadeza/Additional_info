# Additional_info
Additional informations of the report "Optimizing the use of spatio-temporal models to improve the monitoring of ecological change at broad spatial scales using coral reefs as a case study" by Hina Gluza

## 1. Synthos shiny app
Link to a screen recording of the use of "synthos" app to generate synthetic data: [app](https://www.dropbox.com/scl/fi/etaypdqzsceedhu9y8743/Screen_recording_synthos_app.mov?rlkey=r96tt8fbe0sogw6dwf3pd77wy&st=h89zqgv0&dl=0)

`Code_Synthos_app.R`  is the code of this app.

## 2. Machine learning model
In the folder ML_model you can find the code of the machine learning model : `scripts_ML_only` with the ressources `R`

The results : 

`model_perf_allML.png` is the figure with the performance of each model.

For each sampling design:

`trend_data_fixed` is the evolution of coral cover in reefs surveyed. The two colors red and blue are for two different sites of collecting data. 
`list_of_parameters.RData` is the parameters chosen for synthetic data generation with synthos package

`check_true_MLonly` is the result of model tunning. We can see predicted value for each observed value, the closer the values, the closer the points will be to the line.
`pred_broadML_original` show the evolution of coral cover through time. The black line is the predicted trend of the model, the blue line is the true trend from the synthetic data. The blue shape is the uncertainty.
`pred_traj_tier_dataML_original` is the evolution of coral cover per tier where there is data. The black line show the predicted value of the model. It is the mean of the bootstraps in gray lines. The blue shape is the uncertainty.
`pred_traj_tier_no_dataML_original` is the evolution of coral cover per tier where there is no data. The black line show the predicted value of the model. It is the mean of the bootstraps in gray lines. The red shape is the uncertainty.
`viz_perf_MLonly` show model performance
