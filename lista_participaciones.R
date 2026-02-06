library(readxl)
library(stringr)
library(dplyr)
library(flextable)
library(readr)

alumnos_2026 <- read_csv("posts/00-Bienvenida/Lista-de-alumnos-2026.csv")

# Arregla aleatoriamente a los alumnos. Usa el operador |>, ahora en R estandar
alumnos_2026 |> 
  select(alumno) |> 
  filter(!str_detect(str_to_lower(alumno), "briones")) |> 
  mutate(alumno = str_to_title(alumno))  |> 
  slice_sample(n = 19 , replace = F) |> 
  mutate(id = row_number())  |> 
  flextable(col_keys = c("id", "alumno")) |> 
  width(width = 2.8) |> 
  save_as_docx(path = "lista de presentación-2026.docx")



