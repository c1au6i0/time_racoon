# Plots

# https://stackoverflow.com/questions/48000292/center-align-legend-title-and-legend-keys-in-ggplot2-for-long-legend-titles
# sleeptime ----
sleeptime_plot <- function(dat = dat, dwm =  input$dwm, date_r = input$date_r) {
      dat <-  
        dat %>% 
      filter(date_ins >= date_r[1],  date_ins <= date_r[2])
      
      sleeptime_plot <- dat %>% 
        mutate(sleep = sleep/60) %>% 
        ggplot() +
        geom_hline(yintercept = 9, lty = 4, lwd = 0.5) +
        scale_y_continuous(limits = c(0,12), breaks = 1:12, labels = 1:12) +
        scale_color_brewer(palette = "Dark2") +
        scale_size_ordinal(range = c(0,4), drop = FALSE) +
        labs(size = "sleep quality\n[0-5]") +
        theme(legend.title.align = 0.5)
      
      if (dwm == "day") {
        sleeptime_plot <- sleeptime_plot +
          geom_label(aes(min(date_ins), y = 9.5), label = "target") +
          labs(x = NULL, y = "hours", color = NULL) +
          geom_line(aes(date_ins, sleep)) +
          geom_point(aes(date_ins, sleep, color = day_ins, size = sleep_quality)) +
          scale_x_date(labels = date_format("%b %d %Y")) 
      }
      
      if  (dwm == "week") {
        sleeptime_plot <- sleeptime_plot +
          geom_label(aes(min(week_year_ins), y = 9.5), label = "target") +
          labs(x = "week starting", y = "hours", color = NULL) +
          geom_jitter(aes(week_year_ins, sleep, color = day_ins, size = sleep_quality), width = 0.2, alpha = 0.8) +
          scale_x_date(labels = date_format("%b %d %Y"))
      }
      
      if  (dwm == "month"){
        sleeptime_plot <- sleeptime_plot +
          geom_label(aes(x = min(month_year_ins), y = 9.5), label = "target") +
          labs(x = "month", y = "hours", color = NULL) +
          geom_jitter(aes(month_year_ins, sleep, color = day_ins, size = sleep_quality), width = 1, alpha = 0.8) +
          scale_x_date(date_breaks = "1 month", labels = date_format("%b %Y"))
      }
      return(sleeptime_plot)
}



dat_long %>% 
  select(ends_with("ins"), get_up, bed_time, sleep_quality) %>% 
  pivot_longer(cols= c(get_up, bed_time), names_to = "activity")  %>% 
  separate(value, c("date_ins2", "time_ins"), sep = " " ) %>%
  mutate(time_ins = as.POSIXct(strptime(time_ins, format = "%H:%M:%S"))) %>% 

  ggplot(aes(date_ins, time_ins, group = activity, shape = activity, size = sleep_quality, colour = day_ins)) +
    scale_y_datetime(date_labels = "%H:%M", date_breaks = "2 hours") +
    geom_point() +
    scale_size_ordinal(range = c(0,4), drop = FALSE) +
    scale_color_brewer(palette = "Dark2") +
    scale_shape_manual(values = c(20,3))

# https://ggplot2.tidyverse.org/reference/guide_legend.html
# https://stackoverflow.com/questions/13143894/how-do-i-position-two-legends-independently-in-ggplot
