# Plots

# sleeptime
sleeptime_plot <- function(dat = dat, dwm =  input$dwm, date_r = input$date_r) {
      
      dat <-  
        dat %>% 
      filter(date_ins >= date_r[1],  date_ins <= date_r[2])
      
      sleeptime_plot <- dat %>% 
        mutate(sleep = sleep/60) %>% 
        ggplot() +
        geom_hline(yintercept = 9, lty = 4, lwd = 0.5) +
        scale_y_continuous(limits = c(0,12), breaks = 1:12, labels = 1:12) +
        scale_color_brewer(palette = "Dark2")
      
      if (dwm == "day") {
        sleeptime_plot <- sleeptime_plot +
          geom_label(aes(min(date_ins), y = 9.5), label = "target") +
          labs(x = NULL, y = "hours", color = NULL) +
          geom_line(aes(date_ins, sleep)) +
          geom_point(size = 3, aes(date_ins, sleep, color = day_ins)) +
          scale_x_date(labels = date_format("%b %d %Y")) 
      }
      
      if  (dwm == "week") {
        sleeptime_plot <- sleeptime_plot +
          geom_label(aes(min(week_year_ins), y = 9.5), label = "target") +
          labs(x = "week starting", y = "hours", color = NULL) +
          geom_jitter(size = 3, aes(week_year_ins, sleep, color = day_ins), width = 0.2) +
          scale_x_date(labels = date_format("%b %d %Y"))
      }
      
      if  (dwm == "month"){
        sleeptime_plot <- sleeptime_plot +
          geom_label(aes(x = min(month_year_ins), y = 9.5), label = "target") +
          labs(x = "month", y = "hours", color = NULL) +
          geom_jitter(size = 3, aes(month_year_ins, sleep, color = day_ins), width = 1) +
          scale_x_date(date_breaks = "1 month", labels = date_format("%b %Y"))
      }
      return(sleeptime_plot)
}