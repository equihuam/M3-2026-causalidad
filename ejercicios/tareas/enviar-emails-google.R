pacman::p_load(gmailr, emayili, keyring, stringr, dplyr, tidyr)


# Preparar y enviar correos con "emayili"
email <- envelope() %>%
  render(
    "### Prueba
    
    Check out [`{emayili}`](https://cran.r-project.org/package=emayili).
    
    ") %>% 
  from("equihuam@gmail.com") %>%
  to("equihuam@gmail.com") %>%
  subject("This is a MD text message!")

print(email, details = TRUE)

smtp <- server(
  host = "smtp.gmail.com",
  port = 587,
  username = "equihuam@gmail.com",
  password = key_get("smtp-gmail", "miguel")
)

smtp(email, verbose = TRUE)
 

