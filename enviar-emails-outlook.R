# Outlook 365
pacman::p_load(Microsoft365R, blastula, dplyr, stringr, DT, flextable, glue, readxl)


# Función para convertir cifra a palabras (0-10 con un decimal)
convertir_a_letras <- function(num) {
  enteros <- c("cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
  
  # verifica que el número esté en el intervalo válido
  num <- round(num, 1)
  if (num < 0 || num > 10) return("Número fuera de rango")
  
  parte_entera <- floor(num)
  parte_decimal <- round((num - parte_entera) * 10)
  
  # Dar formato sl resultado
  if (parte_decimal == 0) {
    return(enteros[parte_entera + 1])
  } else {
    return(paste(enteros[parte_entera + 1], "punto", enteros[parte_decimal + 1]))
  }
}


# Recupera los mensajes de interés
alumnos <- read.csv("posts/00-Bienvenida/Lista-de-alumnos-2026.csv")
notas <-  read_xlsx("_encuestas/Calificaciones-2026.xlsx")
notas_email <- notas |> 
  inner_join(alumnos, by = "alumno") |> 
  select(alumno, `calif-final-ajustada`, correo) |> 
  mutate(nota = round(`calif-final-ajustada`, 1))

outlb <- get_business_outlook()

for (i in 1:length(notas_email$alumno))
{
  
  bl_body <- paste0("<div style='font-size: 12pt; font-family: Arial, sans-serif;'>\n\n", 
  "### ¡Hola ", notas_email$alumno[i], "!\n\n",
  "Te comparto los resultados de la evaluación de tu desempeño en el **módulo 3**: *Causalidad*.
  
  Consideré la entrega que recibí de tu parte para cada ejercicio y lectura que 
  asigné durante el curso. También valoré, en forma de compensación aditiva, 
  tus intervenciones en las encuestas *Vevox* que hicimos en clase. El resultado 
  final en tu caso fue: **",
  format(notas_email$nota[i], nsmall = 1), 
  " (", convertir_a_letras(notas_email$nota[i]), ")**\n\n", 
  "En la eventualidad de que consideres que omití alguna entrega tuya, por favor házmelo saber de 
  inmediato, para revisar el caso.
  
  Saludos,
  
  Miguel Equihua")
  
  bl_em <- compose_email(
    body = md(bl_body),
    footer = md("Mensaje preparado en **R** con *Microsoft365R*")
  )
  
  em <- outlb$create_email(
    bl_em, 
    subject = "[M3-2026] Calificación", 
    to = notas_email$correo[i]
  )
  
  em$send()
 
}

