#  ------------------------------------------------------------------------
#
# Title : import APPUI
#    By : PhM
#  Date : 2025-06-27
#
#  ------------------------------------------------------------------------



library(tidyverse)
library(janitor)
library(readODS)
library(labelled)
#

bn <- read_ods("datas/appui.ods", sheet = "bnom")

tt <- read_ods("datas/appui.ods", sheet = "donnees", col_types = NULL, na = c("ND", "NA")) |>
  janitor::clean_names() |>
  mutate_if(is.character, as.factor) |>
  mutate(id = as.factor(id))
var_label(tt) <- bn$nom

tt <- tt |>
  select(!starts_with("si_")) |>
  select(!starts_with("date"))
save(tt, file = "datas/appui.RData")
