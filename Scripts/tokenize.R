tokenize <- function (target)
{

#List of libraries to be used
	package_list <- c("tm")

#Library check and load
	for( i in length(package_list))
	{
		if (!require(package_list[i],character.only = TRUE))
    			{
		      	install.packages(package_list[i],dep=TRUE)
			        if(!require(package_list[i],character.only = TRUE)) stop("Package not found")
			}
	}

#Tokenizer
	i <- 1
	tokens <- c()
	filt_tokens <- list()

	while(i <= length(target))
	{
		tokens[i] <- list(MC_tokenizer(target[i]))

		j <- 1
		element <- 1
		filt_tokens[[i]] <- array()

		while (j <= length(tokens[[i]]))
		{
			if (nchar(tokens[[i]][j]) > 0)
				{
					filt_tokens[[i]][element] <- tokens[[i]][j]
					element <- element+1
				}

			j <- j+1
		}
		i <- i+1
	}
	filt_tokens <- lapply(filt_tokens,tolower)
	filt_tokens
}