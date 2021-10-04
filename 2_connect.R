library(DBI)
library(RSQLite)

e4_con <- dbConnect(RSQLite::SQLite(), paste0(config$E4_db_loc,config$E4_db_name))

dbListTables(e4_con)

start <- lubridate::ymd_hms('2021-04-15 07:00:00')
lubridate::tz(start) <- "America/New_york"
stop <- start + lubridate::hours(12)
test <- tbl(e4_con, "Table_A025B3_EDA") %>% 
  filter(TimeStamp >= start & TimeStamp <= stop) %>%
  collect()


