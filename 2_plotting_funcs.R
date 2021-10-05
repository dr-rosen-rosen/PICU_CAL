###############################################################################################
###############################################################################################
#####################   FUNCS for Plotting PICU CAL data
#####################
###############################################################################################
###############################################################################################
library(ggplot2)
library(plotly)
library(ggthemes)

get_E4_plot <- function(measure, shift, tracking_df) {
  lab_text <- c(EDA = 'Electro Dermal Activity (MicroS)', HR = 'Cardiac Activity (Avg HR)', ACC = "Accelerometer Movement Metrics")
  df <- tracking_df[which(tracking_df$shift_day == shift),] %>%
    group_by(shift_day) %>%
    group_map(~ get_synchronies(.x,.y, measure)) %>% # this says 'get_synchronies' but is just getting raw data
    do.call("rbind", .)
  colnames(df) <- lapply(colnames(df), rename_func)
  df <- df %>%
    pivot_longer(!TimeStamp, names_to = 'indvidual', values_to = 'value')
  if (measure == 'ACC') {
    # df$individual <- factor(df$individual)
    df <- df %>%
      mutate(time_interval = lubridate::round_date(TimeStamp, "5 mins")) %>%
      group_by(indvidual, time_interval) %>%
      summarize(AvgEnergy = mean(value)) %>%
      ungroup()
    p <- ggplot(df, aes(x=time_interval, y = indvidual)) +
      geom_tile(aes(fill = AvgEnergy), colour = "white", na.rm = TRUE)  +
      theme_light() +
      scale_fill_viridis(discrete=FALSE) +
      ylab(lab_text[[measure]])
  } else {
    p <- ggplot(df, aes(x=TimeStamp, y=value)) + #, colour = indvidual, group = indvidual
      geom_line() + 
      xlab("") +
      facet_grid(indvidual ~ .) +
      #annotate("rect", xmin = code_time, xmax = (code_end), ymin = min(df$value,na.rm = TRUE), ymax = Inf,
      #         alpha = .2, fill = 'red') +
      ylab(lab_text[[measure]]) +
    #scale_x_datetime(breaks = lubridate::hms(t_breaks)) +
    theme_tufte()
  }
  return(ggplotly(p))
}

test <- get_E4_plot(measure = 'ACC', shift = 'Shift_03', tracking_df = tracking_df)
test2 <- ggplotly(test)