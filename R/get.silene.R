get.silene <- function(x, 
                       path.silene = "C:/Users/munozfra/Documents/Botanique/Localisations/CSV",
                       dates = NULL,
                       locations = NULL)
{
  if(!file.exists(path.silene))
    stop("No file with Silene data")
  
  f.silene <- list.files(path.silene, pattern = ".csv")
}