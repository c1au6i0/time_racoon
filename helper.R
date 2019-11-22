library(readxl)
library(tidyr)
library(data.table)
library(tidyverse)


dat <- readxl::read_excel("personal_tt.xlsx")


dat_long <-
  dat %>%
  mutate_all(~as.character(.)) %>% 
  pivot_longer(col= 3: ncol(dat), names_to = "date_ins", values_to = "value") %>% 
  mutate(date_ins =   as.POSIXct(as.numeric(date_ins) * 60*60*24, origin="1899-12-30", tz="GMT")) %>% 
  na.omit() %>% 
  select(-unit) %>% 
  pivot_wider(names_from = activity, values_from = value) %>% 
  mutate_at(c("get_up", "bed_time"), ~ as.POSIXct(as.numeric(.) * 60*60*24, origin="1899-12-30", tz="GMT"))
              
names(dat)
