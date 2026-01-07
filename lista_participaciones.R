library(readxl)
library(stringr)
library(dplyr)
library(flextable)

alumnos_2025 <- read_excel("posts/00-Bienvenida/Lista-de-alumnos-2025.xlsx")

# Estos dos  escripts son eqivalentes. 
# Arrela aleatoriamente a los alumnos. Usa el operador |>, ahora en R estandar
alumnos_2025 |> 
  select(Estudiante, Adscripción) |> 
  filter(str_detect(Adscripción,"baja", negate = TRUE)) |> 
  mutate(Estudiante = str_to_title(Estudiante))  |> 
  slice_sample(n = 19 , replace = F) |> 
  flextable(col_keys = "Estudiante") |> 
  width(width = 2.8) |> 
  save_as_docx(path = "lista de presentación.docx")

# Utiliza el comando %>% de la biblioteca "dplyer"
alumnos_2025 %>% 
  select(Estudiante, Adscripción) %>% 
  filter(str_detect(Adscripción,"baja", negate = TRUE)) %>% 
  mutate(Estudiante = str_to_title(Estudiante))  %>% 
  slice_sample(n = 19 , replace = F) %>% 
  flextable(col_keys = "Estudiante") %>% 
  width(width = 2.8) %>% 
  save_as_docx(path = "lista de presentación.docx")



