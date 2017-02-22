filter_tokens <- function(tokens, language)
{
#Word filter
	i <- 1
	filt_tokens <- list()

	while(i <= length(tokens))
	{
		j <- 1
		element <- 1
		filt_tokens[[i]] <- array()

		while(j <= length(tokens[[i]]))
		{
			k <- 1
			to_filt <- FALSE

			while(k <= length(language))
			{
				if (is.na(tokens[[i]][1])==FALSE && (tokens[[i]][j] == language[k] || nchar(tokens[[i]][j]) < 2))
				{
					to_filt <- TRUE
				}
				
				k <- k+1
			}
			
			if(to_filt == FALSE)
			{
				filt_tokens[[i]][element] <- tokens[[i]][j]
				element <- element+1
			}

			j <- j+1
		}

		i <- i+1

	}

	filt_tokens

}