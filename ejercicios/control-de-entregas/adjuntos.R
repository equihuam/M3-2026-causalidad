for (correo in correos)
{
  remitente <-  correo$properties$sender$emailAddress$address
  asunto <- correo$properties$subject
  
  if (str_detect(str_to_lower(asunto), "m3-2026|m3 2026|m3_2026|mm-2026"))
  {
    d  <- correo$list_attachments()
    if (length(d) > 1) 
    {
      print(glue("Múltiples adjuntos {length(d)} ------"))
      print(remitente)
      print(asunto)
      for (i in (1:length(d)))
      {
        anexo <- d[[i]]$properties$name
        print(glue("     {anexo}"))
             
      }
    }
  }
}
