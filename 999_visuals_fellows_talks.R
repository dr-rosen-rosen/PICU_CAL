#################################################################
############ Create data visuals for Fellows talk
#################################################################
reticulate::source_python('1_funcs.py')
tracking_df <- read_excel(paste0(config$tracking_file_loc, config$tracking_file))

measure <- 'ACC'
test <- tracking_df[which(tracking_df$shift_day == 'Shift_03'),] %>%
  group_by(shift_day) %>%
  group_map(~ get_synchronies(.x,.y, measure)) %>% # this says 'get_synchronies' but is just getting raw data
  do.call("rbind", .)
colnames(test) <- lapply(colnames(test), rename_func)

### renaming for charts

# shift 3:
test <- test %>%
  rename(
    tm_1 = id_1266,
    tm_2 = id_2118,
    tm_3 = id_2487,
    tm_4 = id_2802
  )

test <- test %>%
  pivot_longer(!TimeStamp, names_to = 'indvidual', values_to = 'value')
test$indvidual <- ordered(test$indvidual, levels = c('tm_1', 'tm_2', 'tm_3','tm_4'))

code_time <- lubridate::ymd("2020-11-19")
lubridate::tz(code_time) <- "America/New_york"
lubridate::hour(code_time) <- 8
lubridate::minute(code_time) <- 27

code_end <- lubridate::ymd("2020-11-19")
lubridate::tz(code_end) <- "America/New_york"
lubridate::hour(code_end) <- 9
lubridate::minute(code_end) <- 19

# t_breaks <- lubridate::hms(c('07:00:00', '11:00:00', '15:00:00','19:00:00'))
# my_time <- lubridate::ymd("2020-11-19")
# lubridate::tz(my_time) <-"America/New_york"
# t_breaks <- t_breaks + my_time
p_HR <- ggplot(test, aes(x=TimeStamp, y=value)) + #, colour = indvidual, group = indvidual
  geom_line() + 
  xlab("") +
  facet_grid(indvidual ~ .) +
  annotate("rect", xmin = code_time, xmax = (code_end), ymin = min(test$value,na.rm = TRUE), ymax = Inf,
           alpha = .2, fill = 'red') +
  
  ylab('Cardiac Activity \n (Avg HR)') +
  #scale_x_datetime(breaks = lubridate::hms(t_breaks)) +
  theme_light()
p_EDA <- ggplot(test, aes(x=TimeStamp, y=value)) + #, colour = indvidual, group = indvidual
  geom_line() + 
  xlab("") +
  facet_grid(indvidual ~ .) +
  annotate("rect", xmin = code_time, xmax = (code_end), ymin = min(test$value,na.rm = TRUE), ymax = Inf,
           alpha = .2, fill = 'red') +
  ylab('Electro Dermal Activity \n (MicroS)') +
  #scale_x_datetime(breaks = lubridate::hms(t_breaks)) +
  theme_light()


test$indvidual <- factor(test$indvidual, levels = rev(levels(test$indvidual)))
hm_df <- test %>%
  mutate(time_interval = lubridate::round_date(TimeStamp, "5 mins")) %>%
  group_by(indvidual, time_interval) %>%
  summarize(AvgEnergy = mean(value)) %>%
  ungroup()

rtls_hm_df <- rtls_df %>%
  mutate(Time_In = lubridate::force_tz(Time_In,"UTC")) %>%
  mutate(Time_In = lubridate::with_tz(Time_In, tzone = "America/New_York")) %>%
  mutate(time_interval = lubridate::round_date(Time_In, "5 mins")) %>%
  mutate(RTLS_ID = as.factor(RTLS_ID)) %>%
  group_by(RTLS_ID, time_interval) %>%
  summarize(Transition_count = n()) %>%
  ungroup()

trans_hm_p <- ggplot(rtls_hm_df, aes(x=time_interval)) +
  #geom_tile(aes(fill = Transition_count), colour = "white", na.rm = TRUE) +
  geom_bar(stat = "count") +
  facet_grid(RTLS_ID ~ .) +
  theme_light() #+
#scale_fill_viridis(discrete=FALSE) 


hm_p <- ggplot(hm_df, aes(x=time_interval, y = indvidual)) +
  geom_tile(aes(fill = AvgEnergy), colour = "white", na.rm = TRUE)  +
  # annotate("rect", xmin = code_time, xmax = (code_time+1800), ymin = min(test$value,na.rm = TRUE), ymax = Inf,
  #          alpha = .2, fill = 'red') +
  theme_light() +
  scale_fill_viridis(discrete=FALSE) +
  #scale_y_discrete(position = "right") + 
  ylab("Accelerometer Movement Metrics")
#theme(legend.position="none")
hm_p

p_EDA / p_HR / hm_p