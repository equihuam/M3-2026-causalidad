# Outlook 365


pacman::p_load(Microsoft365R, dplyr, stringr)

#outl <- get_personal_outlook()

outlb <- get_business_outlook()

inbox_ref <- outlb$get_inbox()


#inbox_ref$get_list_pager(lst = )

correos <- inbox_ref$list_emails(search = "m3-2026", pagesize = 200)

tabla_correo <- tibble(fecha = character(), 
                       alumno = character(), 
                       archivo = character())


for (correo in correos)
{
  remitente <-  correo$properties$sender$emailAddress$address
  print(remitente)
  asunto <- correo$properties$subject
  if (str_detect(str_to_lower(asunto), "m3-2026|m3 2026|m3_2026"))
  {
    d  <- correo$list_attachments()
    anexo <- d[[1]]$properties$name
    fecha  <- as.POSIXlt( as.Date(correo$properties$receivedDateTime))
    year <- fecha$year + 1900
    mes <- fecha$mon + 1
    if (str_detect(str_to_lower(anexo), "lectura"))
    {
      d[[1]]$download(dest = paste0("ejercicios/control-de-entregas/lecturas/",anexo), 
                    overwrite = TRUE)
    } else {
      d[[1]]$download(dest = paste0("ejercicios/control-de-entregas/tareas/",anexo), 
                      overwrite = TRUE)
      
    }
    
    fecha_str <- correo$properties$receivedDateTime
    tabla_correo <- bind_rows(tabla_correo, c(fecha = fecha_str,
                                              alumno = remitente,
                                              archivo = anexo))
  }
}

