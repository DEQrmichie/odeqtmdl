# odeqtmdl

The odeqtmdl R package includes databases of TMDL information from non-tribal 
Oregon TMDLs and a set of functions that assist in assessment of water quality 
data against select TMDL targets. Currently the databases are incomplete. 
See each relevant TMDL document for the official record

https://www.oregon.gov/deq/wq/tmdls/Pages/default.aspx

## Install

```R
library(devtools)

devtools::install_github("OR-Dept-Environmental-Quality/odeqtmdl",
                         host = "https://api.github.com",
                         dependencies = TRUE, force = TRUE, upgrade = FALSE)
```

## Database Usage

A full listing of all non-tribal TMDL actions in Oregon.
```R
odeqtmdl::tmdl_actions
```

Partial inventory of TMDL targets from non-tribal Oregon TMDLs. Database includes 
TMDL target value, target unit, statistical base, season start/end, and
applicable NHD reachcode and Oregon Assessment Unit IDs where the target applies.
```R
odeqtmdl::tmdl_db
```

See odeqtmdl package help for information on individual functions
```R
?consecutive_median
?monthly_mean
?monthly_median
?seasonal_mean
?seasonal_median
?target_assessment
?tmdl_actions
?tmdl_db
?which_target_df
?which_target
```
