library(stringr)
quartos <- list.files(c("posts", "presentaciones", "ejercicios"), recursive = T, pattern = "qmd$", full.names = T)

for (q in quartos) 
{
  a <- readLines(q)
  for (i in (1:length(a)))
  {
    if (str_detect(a[i], "draft: false"))
    {
      a[i] <- str_replace(a[i], "false", "true")
    }
  }
  writeLines(text = a, con = q)
}

