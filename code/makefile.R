# The following code chunk will render the document
rmarkdown::render(input = 'code/main.Rmd', encoding = 'UTF-8', output_dir = "docs")

# The following code chunk will update the exiting document on Google Drive
id <- "1l09a5x98YTJmME1DMojzlkXqkAZjCiLy1rKzFCe5sgU"
# googledrive::drive_update(file = googledrive::as_id(id), media = "docs/main.html")
