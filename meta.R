library(here)
library(magrittr)
library(tidyverse)
library(meta)

tblRota <- read.csv(here("rota.csv")) %>%
  as_tibble() %>%
  filter(Events.1>0 & is.na(df))

