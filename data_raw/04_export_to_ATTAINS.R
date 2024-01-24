library(odeqtmdl)

action_ids <- "OR_TMDL_20230915"
out_dir <- "C:/Users/rmichie/OneDrive - Oregon/Projects/TMDL_ATTAINS_Project/ATTAINS_uploads"

export_status <- tmdl_export_attains(out_dir = out_dir, action_ids = action_ids)
