# Plots



# ggiraph in next version
# https://stackoverflow.com/questions/48000292/center-align-legend-title-and-legend-keys-in-ggplot2-for-long-legend-titles


# for title?
# https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/ 

recode_col <- function(x, repl) {
  intv_id <- as.character(unique(x))
  id_recode <- repl[seq_along(intv_id)]
  newnames <- stats::setNames(id_recode, intv_id)
  as.vector(newnames[x])
}




# sleeptime ----
sleeptime_plot <- function(dat, dwm =  input$dwm) {
      
      racoon_plots <- dat %>% 
        mutate(sleep = sleep/60) %>% 
        ggplot() +
        geom_hline(yintercept = 9, lty = 4, lwd = 0.5) +
        scale_y_continuous(limits = c(0,12), breaks = 1:12, labels = 1:12) +
        scale_fill_brewer(palette = "Dark2") +
        scale_size_ordinal(range = c(0, 5), drop = FALSE) +
        theme(legend.title.align = 0.5) +
        guides(
          size = guide_legend(title = "sleep quality\n[0-5]", title.hjust = 0.5, order = 2, nrow = 3)
          )
      
      if (dwm == "day") {
        racoon_plots <- racoon_plots +
          geom_label(aes(min(date_ins), y = 9.5), label = "target") +
          labs(x = NULL, y = "duration (hours)", fill = NULL) +
          geom_line(aes(date_ins, sleep, group = week_year_ins)) +
          geom_point(aes(date_ins, sleep, fill = day_ins, size = sleep_quality), shape = 21) +
          scale_x_date(labels = date_format("%b %d %Y")) 
      }
      
      if  (dwm == "week") {
        racoon_plots <- racoon_plots +
          geom_label(aes(min(week_year_ins), y = 9.5), label = "target") +
          labs(x = "week starting", y = "hours", color = NULL) +
          geom_jitter(aes(week_year_ins, sleep, fill = day_ins, size = sleep_quality), width = 0.2, alpha = 0.8, shape = 21) +
          scale_x_date(labels = date_format("%b %d %Y"))
      }
      
      if  (dwm == "month"){
        racoon_plots <- racoon_plots +
          geom_label(aes(x = min(month_year_ins), y = 9.5), label = "target") +
          labs(x = "month", y = "hours", color = NULL) +
          geom_jitter(aes(month_year_ins, sleep, fill = day_ins, size = sleep_quality), width = 1, alpha = 0.8, shape = 21) +
          scale_x_date(date_breaks = "1 month", labels = date_format("%b %Y"))
      }
      
      racoon_plots <- 
        racoon_plots +
          ggtitle("Time Sleeping") +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
      
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
    ggtitle("Get-up and Bed Time") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
  
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
    mutate(y_plot = minutes(y_plot)) %>% 
    ggplot(aes(y = y_plot)) +
      scale_fill_brewer(palette = "Dark2") +
      scale_x_date(labels = date_format("%b %d %Y")) +
      labs(y = "duration( hours, minutes)", fill = NULL, x = NULL) +
      scale_y_time(labels = time_format("%H,%M")) 

    
    if (dwm == "day") {
    racoon_plots <-
      racoon_plots +
      geom_line(aes(x = date_ins, group = week_year_ins)) +
      geom_point(aes(x = date_ins, fill = day_ins), size = 5, shape = 21) 
    }
    
   if (dwm == "week") {
     racoon_plots <- 
       racoon_plots +
       geom_jitter(aes(x = week_year_ins, fill = day_ins), width = 0.2, alpha = 0.7, size = 5, shape = 21) +
       labs(x = "week starting") 
   }

   if (dwm == "month") {
     racoon_plots <- 
       racoon_plots +
       geom_jitter(aes(x = month_year_ins, fill = day_ins), width = 1, alpha = 0.7, size = 5, shape = 21) +
       labs(x = "month") +
       scale_x_date(date_breaks = "1 month", labels = date_format("%b %Y"))
   }
    
  title_r  <- case_when(
     measure == "work_time" ~ "Time Working",
     measure == "home_time" ~ "Time at Home",
     measure == "commuting_time" ~ "Time commuting",
     measure == "processing_time" ~ "Processing Time",
     measure == "tai_chi" ~ "Time doing Tai Chi", 
     measure == "walking" ~ "Time Walking",  
     measure == "swimming" ~ "Time Swimming" 
       )
  
  racoon_plots <-
    racoon_plots +
    ggtitle(title_r) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
   
   return(racoon_plots)
  
}

# comparisons --------

compare_plot <- function(dat, dwm =  input$dwm) {
  
   dat_long <- dat %>%
    mutate(work_time_tot = work_time + processing_time,
           phisical_activity = tai_chi + swimming + walking) %>% 
    select(ends_with("ins"), 
           work_time_tot, 
           commuting_time, 
           phisical_activity,
           sleep
    ) %>% 
    pivot_longer(cols= c(work_time_tot, commuting_time, phisical_activity, sleep), names_to = "activity") %>% 
    mutate(activity = recode_col(activity, c("work", "commuting", "excercise", "sleep"))) %>% 
    mutate(activity = factor(activity, levels = c("work", "commuting", "excercise", "sleep")))
    
  if (dwm == "day") {
  racoon_plots <-  dat_long %>% 
    mutate(value = minutes(value), activity = as.factor(activity)) %>% 
    ggplot(aes(x = date_ins, y = value, shape = activity, fill = day_ins)) +
      geom_line( aes(group = week_year_ins), color = "black") +
      scale_fill_brewer(palette = "Dark2", guide = "legend") +
      scale_y_time() +
      geom_point(size = 4) +
      facet_grid(rows = vars(activity)) +
      labs(x = NULL, y = "duration (hours, minutes)", fill = NULL) +
      guides(shape = FALSE) +
      scale_shape_manual(values = c(21, 22, 23, 24)) +
      guides(fill=guide_legend(override.aes=list(shape = 21))) +
      scale_y_time(labels = time_format("%H,%M")) +
      scale_x_date(labels = date_format("%b %d %Y"))
  }

  if (dwm == "week") {

    racoon_plots <- dat_long %>%
      group_by(week_year_ins, activity) %>%
      summarize(mean_y = mean(value, na.rm = TRUE)) %>%
      ungroup() %>% 
      mutate(mean_y = minutes(ceiling(mean_y))) %>%

      ggplot(aes(x = week_year_ins, y = mean_y, shape = activity, fill = activity)) +
      scale_fill_brewer(palette = "Dark2") +
      geom_line(show.legend = FALSE) +
      geom_point(size = 5) +
      labs(x = "week starting", y = "mean duration (hours, minutes)", fill = NULL, shape = NULL) +
      scale_shape_manual(values = c(21, 22, 23, 24), guide = "legend") +
      scale_y_time(labels = time_format("%H,%M")) +
      scale_x_date(labels = date_format("%b %d %Y"))

  }
  
  if (dwm == "month") {
    
    racoon_plots <- dat_long %>%
      group_by(month_year_ins, activity) %>%
      summarize(mean_y = mean(value, na.rm = TRUE), sd_y = sd(value, na.rm = TRUE)) %>%
      ungroup() %>% 
      mutate(mean_y = minutes(ceiling(mean_y))) %>%
    
      ggplot(aes(x = month_year_ins, y = mean_y, shape = activity, fill = activity)) +
      scale_fill_brewer(palette = "Dark2") +
      geom_line(show.legend = FALSE) +
      geom_point(size = 4) +
      labs(x = "month", y = "mean duration (hours, minutes)", fill = NULL, shape = NULL) +
      scale_shape_manual(values = c(21, 22, 23, 24), guide = "legend") +
      scale_y_time(labels = time_format("%H,%M")) +
      scale_x_date(date_breaks = "1 month", labels = date_format("%b %Y")) 
  }
   
   racoon_plots <-
     racoon_plots +
     ggtitle("Activities") +
     theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))

  
  return(racoon_plots)

  
  
}

# people comtacted
people_plot <- function(dat, dwm =  input$dwm) {
  
  racoons <- dat %>% 
  select(ends_with("ins"), 
         family_others, 
         friends) %>% 
  mutate_at(c("family_others", "friends"), ~ str_split(., ",")) %>% {
    family_others <- select(., ends_with("ins"), 
           family_others) %>% 
            rename(people = family_others)
    
    friends <- select(., ends_with("ins"), 
                      friends) %>% 
            rename(people = friends)
    list(family_others = family_others, friends = friends)
    
  }
  
  dat_long  <- map_dfr(racoons, function(x){
    unnest(x, cols = "people")
  }, .id = "relationship")
  
  
  dat_long$people <- str_remove(dat_long$people, "^[\\s]")
  dat_long$people <- str_replace_all(dat_long$people, "^zia", "Zia")
  dat_long$people <- str_replace_all(dat_long$people, "^zio", "Zio")
  dat_long$people[dat_long$people == 0] <- NA
  dat_long$people[dat_long$people == "Zia Mick"] <- "Zia Michelangela"
  

  # word_cloud
  # dat_long %>% 
  #   group_by(people, relationship) %>% 
  #   summarize(freq = n()) %>% 
  #   filter(!is.na(people)) %>% 
  #   ggplot(aes(label = people, size = freq, color = relationship)) +
  #     geom_text_wordcloud(eccentricity = 1, shape = "circle", rstep = 0.01) +
  #     scale_size_area(max_size = 5) 
  
  racoon_plots <- dat_long %>%
    group_by(people, relationship) %>%
    summarize(freq = as.integer(n())) %>%
    filter(!is.na(people)) %>% 
    mutate(relationship = str_remove(relationship, "_others")) %>% 
    
  ggplot(aes(x = reorder(people, freq), y = freq, fill = relationship)) +
    geom_col(col = "black") +
    labs(x = NULL,  y = "frequency") +
    geom_text(aes(y = freq/2, label = freq), size = 5, position = position_dodge(width = 1)) +
    coord_flip() +
    ggtitle("People contacted") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
  
  return(racoon_plots)
}
  

