# Make paper and push up to google doc
rmarkdown::render("manuscript.Rmd", output_format = "bookdown::word_document2")

# upload to google drive for feedback
googledrive::drive_put("manuscript.docx", 'moramanga_vax')
