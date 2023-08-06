library(reticulate)

dir <- getwd()

for (exp in seq(25, 40)){
  setwd(paste0(dir, "/experiment", exp))
  
  py_data_frames <- py_load_object(file = "data_frames.obj")
  print(paste(exp, "loaded"))
  
  for (k in seq(1, length(py_data_frames))) {
    py_data_frames[[k]]$normalized_distance <- py_data_frames[[k]]$distance/py_data_frames[[k]]$n_features
  } 
  
  py_save_object(py_data_frames, "data_frames.obj", pickle = "pickle")
  print(paste(exp, "saved"))
}
