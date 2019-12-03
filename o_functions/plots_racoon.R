# Plots



# ggiraph in next version
# https://stackoverflow.com/questions/48000292/center-align-legend-title-and-legend-keys-in-ggplot2-for-long-legend-titles
# sleeptime ----
sleeptime_plot <- function(dat, dwm =  input$dwm) {
      
      racoon_plots <- dat %>% 
        mutate(sleep = sleep/60) %>% 
        ggplot() +
        geom_hline(yintercept = 9, lty = 4, lwd = 0.5) +
        scale_y_continuous(limits = c(0,12), breaks = 1:12, labels = 1:12) +
        scale_color_brewer(palette = "Dark2") +
        scale_size_ordinal(range = c(0,4), drop = FALSE) +
        theme(legend.title.align = 0.5) +
        guides(
          size = guide_legend(title = "sleep quality\n[0-5]", title.hjust = 0.5, order = 2, nrow = 3)
          )
      
      if (dwm == "day") {
        racoon_plots <- racoon_plots +
          geom_label(aes(min(date_ins), y = 9.5), label = "target") +
          labs(x = NULL, y = "hours", color = NULL) +
          geom_line(aes(date_ins, sleep)) +
          geom_point(aes(date_ins, sleep, color = day_ins, size = sleep_quality)) +
          scale_x_date(labels = date_format("%b %d %Y")) 
      }
      
      if  (dwm == "week") {
        racoon_plots <- racoon_plots +
          geom_label(aes(min(week_year_ins), y = 9.5), label = "target") +
          labs(x = "week starting", y = "hours", color = NULL) +
          geom_jitter(aes(week_year_ins, sleep, color = day_ins, size = sleep_quality), width = 0.2, alpha = 0.8) +
          scale_x_date(labels = date_format("%b %d %Y"))
      }
      
      if  (dwm == "month"){
        racoon_plots <- racoon_plots +
          geom_label(aes(x = min(month_year_ins), y = 9.5), label = "target") +
          labs(x = "month", y = "hours", color = NULL) +
          geom_jitter(aes(month_year_ins, sleep, color = day_ins, size = sleep_quality), width = 1, alpha = 0.8) +
          scale_x_date(date_breaks = "1 month", labels = date_format("%b %Y"))
      }
      
      racoon_plots <- 
        racoon_plots +
          ggtitle("Time Sleeping")
      
      return(racoon_plots)
}


# getup ----
getup_plot <- function(dat, dwm =  input$dwm) {
  
  time_lims <- as.POSIXct(strptime(c("2019-11-29 04:00:00", "2019-11-30 02:00:00"),
                                   format = "%Y-%m-%d %H:%M:%S", tz = "EST"))
  racoon_plots <- dat %>%
    select(ends_with("ins"), get_up, bed_time, sleep_quality) %>%
    pivot_longer(cols= c(get_up, bed_time), names_to = "activity")  %>%
    separate(value, c("date_ins2", "time_ins"), sep = " " ) %>%
    mutate(time_ins = ymd_hms(paste("2019-11-29", time_ins, " "), tz = "EST")) %>% 
    mutate(date_ins2 = if_else(time_ins > ymd_hms("2019-11-29 03:00:00", tz = "EST"),
                               "2019-11-29",
                               "2019-11-30")) %>%
    separate(time_ins, c("to_rm", "time_ins"), sep = " " ) %>%
    mutate(time_ins = ymd_hms(paste(date_ins2, time_ins, " "), tz = "EST")) %>%
    
    ggplot(aes(y = time_ins, group = activity, colour = day_ins,  size = sleep_quality, shape = activity)) +
      scale_y_datetime(date_labels = "%H:%M", date_breaks = "1 hours", limits = time_lims, timezone = "EST") +
      scale_size_ordinal(range = c(0, 4), drop = FALSE) +
      scale_color_brewer(palette = "Dark2") +
      scale_shape_manual(values = c(20,3)) +
      scale_x_date(labels = date_format("%b %d %Y")) +
      guides(
        shape = guide_legend(title.hjust = 0.5, order = 1),
        size = guide_legend(title = "sleep quality\n[0-5]", title.hjust = 0.5, order = 2, nrow = 3),
        colour =  guide_legend(title = NULL)) +
      labs(x = NULL, y = "time of the day") 
  
    if (dwm == "day") {
      racoon_plots <- 
        racoon_plots +
        geom_point(aes(x = date_ins), stroke = 2) 
    }
  
    if (dwm == "week") {
      racoon_plots <- 
        racoon_plots +
        geom_jitter(aes(x = week_year_ins), width = 0.2, alpha = 0.8, stroke = 2) +
        labs(x = "week starting") 
    }
  
    if (dwm == "month") {
      racoon_plots <- 
        racoon_plots +
        geom_jitter(aes(x = month_year_ins), width = 1, alpha = 0.7, stroke = 2) +
        labs(x = "month") +
        scale_x_date(date_breaks = "1 month", labels = date_format("%b %Y"))
    }
  
  racoon_plots <- 
    racoon_plots +
    ggtitle("Get-up and Bed Time")
  
  return(racoon_plots)
  
}

# other durarion activities----

dur_act <- c( "work_time",
              "home_time",
              "commuting_time",
              "processing_time",
              "tai_chi", 
              "walking" ,  
              "swimming"
               )

time_plot <- function(dat, measure, dwm =  input$dwm){
   racoon_plots <- dat %>%
    rename(y_plot = !!measure) %>% 
    ggplot(aes(y = y_plot / 60)) +
      scale_color_brewer(palette = "Dark2") +
      scale_x_date(labels = date_format("%b %d %Y")) +
      # scale_y_time()+
      labs(y = "hours", colour = NULL, x = NULL)
    
    if (dwm == "day") {
    racoon_plots <-
      racoon_plots +
      geom_line(aes(x = date_ins)) +
      geom_point(aes(x = date_ins, color = day_ins), size = 5) 
    }
    
   if (dwm == "week") {
     racoon_plots <- 
       racoon_plots +
       geom_jitter(aes(x = week_year_ins, color = day_ins), width = 0.2, alpha = 0.7, size = 5) +
       labs(x = "week starting") 
   }

   if (dwm == "month") {
     racoon_plots <- 
       racoon_plots +
       geom_jitter(aes(x = month_year_ins, color = day_ins), width = 1, alpha = 0.7, size = 5) +
       labs(x = "month") +
       scale_x_date(date_breaks = "1 month", labels = date_format("%b %Y"))
   }
    
  title_r  <- case_when(
     measure == "work_time" ~ "Time in Workplace",
     measure == "home_time" ~ "Time Commuting",
     measure == "commuting_time" ~ "Time Working at Home",
     measure == "processing_time" ~ "Processing Time",
     measure == "tai_chi" ~ "Time doing Tai Chi", 
     measure == "walking" ~ "Time Walking",  
     measure == "swimming" ~ "Time Swimming" 
       )
  
  racoon_plots <- 
    racoon_plots +
    ggtitle(title_r)  
   
   
   return(racoon_plots)
  
}

