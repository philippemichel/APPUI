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
pam <- function(pp) {
  pa <- str_split_fixed(pp, "/", 2)
  pas <- as.numeric(pa[, 1])
  pad <- as.numeric(pa[, 2])
  pam <- (pas + pad * 2) / 3
  return(pam)
}

bn <- read_ods("datas/appui.ods", sheet = "bnom")

tt <- read_ods("datas/appui.ods", sheet = "donnees", col_types = NULL, na = c("ND", "NA", "", " ")) |>
  janitor::clean_names() |>
  mutate_if(is.character, as.factor) |>
  mutate(id = as.factor(id)) |>
  mutate(ta = pam(ta)) |>
  mutate(age = fct_relevel(
    age,
    "< 65 ans", "entre 65 et 75 ans", "> 75 ans"
  )) |>
  mutate(
    grossesse = case_when(
      sexe == "M" ~ NA,
      .default = grossesse
    )
  ) |>
  mutate(
    fragilite = case_when(
      age == "< 65 ans" ~ NA,
      .default = fragilite
    )
  )
var_label(tt) <- bn$nom

tt <- tt |>
  dplyr::select(!starts_with("date"))
save(tt, file = "datas/appui.RData")
