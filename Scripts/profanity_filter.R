profanity_filter <- function(tokens, profanity_list)
{

#Filters tokens according to profanity_list variable
	prof_filt_tokens <- tokens
	i <- 1
	while (i <= length(prof_filt_tokens))
		{
			j <- 1
			while (j <= length(prof_filt_tokens[[i]]))
				{
					k <- 1
					while (k <= length(profanity_list))
						{
							if (prof_filt_tokens[[i]][j]== profanity_list[k])
							{
								prof_filt_tokens[[i]][j] <- paste(replicate(nchar(prof_filt_tokens[[i]][j]), "*"), collapse = "")
							}
							k <- k+1
						}
					j <- j+1
				}
			i <- i+1
		}
	prof_filt_tokens
}