# odeqtmdl

The odeqtmdl R package includes data tables of TMDL information from non-tribal 
TMDLs in Oregon and a set of functions that assist in assessment of water quality 
data against select TMDL targets. The data tables are still in development and 
for some TMDLs may be incomplete. See each relevant TMDL document 
for the official record.

https://www.oregon.gov/deq/wq/tmdls/Pages/default.aspx

## Install

```R
library(devtools)

devtools::install_github("OR-Dept-Environmental-Quality/odeqtmdl",
                         host = "https://api.github.com",
                         dependencies = TRUE, force = TRUE, upgrade = FALSE)
```

## Data Table Usage

A full listing of all non-tribal TMDL actions in Oregon.
```R
odeqtmdl::tmdl_actions
```

Inventory of NHD reaches where non-tribal Oregon TMDLs have been developed.
Note this table is large and may take a minute to load.
```R
odeqtmdl::tmdl_reaches()
```

Inventory of assessment units and unique stream name (GNIS) assessment units 
where non-tribal Oregon TMDLs have been developed. 
```R
odeqtmdl::tmdl_au
odeqtmdl::tmdl_au_gnis
```

Summary of all unique water quality limited parameter and pollutant pair 
combinations for each TMDL action.
```R
odeqtmdl::tmdl_parameters
```

Partial inventory of TMDL targets from non-tribal Oregon TMDLs. The table includes 
TMDL target value, target unit, statistical base, season start/end, and the 
geo_id. A geo ID is a unique ID used to identify the applicable NHD 
reaches and Oregon Assessment Unit where the target applies.
```R
odeqtmdl::tmdl_targets
```

Inventory and narrative description of unique TMDL geo IDs for non-tribal 
Oregon TMDLs.
```R
odeqtmdl::tmdl_geo_ids
```

Lookup table of DEQ and EPA ATTAINS water quality parameter names and IDs.
```R
odeqtmdl::LU_pollutant
```

There are many other individual package functions. See odeqtmdl package help 
for more information.
```R
?LU_pollutnat
?LU_wqstd_code
?LU_wqstd
?consecutive_median
?monthly_mean
?monthly_median
?seasonal_mean
?seasonal_median
?target_assessment
?tmdl_export_attains
?tmdl_export_gpkg
?which_target_df
?which_target
```
