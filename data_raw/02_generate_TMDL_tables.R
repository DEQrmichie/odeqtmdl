# This script generates or makes targeted updates to the tmdl package tables

# It should be opened within the Project Rstudio view so the working directory is set correctly.

# This does not update the LU tables

library(odeqtmdl)
library(readxl)


# update/import everything
# update_action_ids <- unique(odeqtmdl::tmdl_actions$action_id)

# update info for specific action ids
update_action_ids <- c("2039", "2038", "OR_TMDL_20191122", "OR_TMDL_20191230", "OR_TMDL_20230915")

# Read paths
paths <- readxl::read_excel(path = "data_raw/project_paths.xlsx",
                            sheet = "paths" , col_names = TRUE,
                            na = c("", "NA"),
                            col_types = c('text', 'text'))

update_template <- file.path(paths$package_path[1], "data_raw", "TMDL_db_tabular.xlsx")

odeqtmdl::tmdl_update(action_ids = update_action_ids,
                      gis_path = file.path(paths$tmdl_reaches_shp[1], "GIS"),
                      xlsx_template = update_template,
                      update_tables = TRUE,
                      update_reaches = TRUE,
                      package_path = paths$package_path[1])
