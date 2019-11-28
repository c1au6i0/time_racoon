# Import
# dat_long <- readxl::read_excel("personal_tt.xlsx")
clean_data <- function(dat) {
  dat_long <-
    dat %>%
    mutate_all(~ as.character(.)) %>%
    select(-starts_with("WEEK")) %>% 
    pivot_longer(
      col = 3:ncol(.),
      names_to = "date_ins",
      values_to = "value"
    ) %>%
    mutate(date_ins = mdy(date_ins)) %>%
    filter(!is.na(date_ins), !is.na(activity)) %>%
    select(-unit) %>%
    pivot_wider(names_from = activity, values_from = value) %>%
    mutate_at(
      c("get_up", "bed_time"),
      ~ as.POSIXct(as.numeric(.) * 60 * 60 * 24, origin = "1899-12-30", tz = "GMT")
    ) %>%
    mutate_at(
      vars(
        -get_up,
        -bed_time,
        -date_ins,
        -parents,
        -partner,
        -brother,
        -family_others,
        -friends
      ),
      as.numeric) %>%
    clean_names() %>%
    mutate(
      month_year_ins = floor_date(date_ins, "month"),
      week_year_ins = floor_date(date_ins, "week", week_start = 1),
      day_ins = lubridate::wday(date_ins, label = TRUE, week_start = 1)
    )
}


