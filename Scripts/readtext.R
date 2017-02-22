readtext <- function(file)
{
	text_file <- suppressWarnings(readLines(file, encoding="UTF-8mb4"))
	text_file <- tolower(text_file)
	text_file
}