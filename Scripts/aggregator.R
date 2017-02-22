aggregator <- function(tokens)
{
#Count the number of times each word appears in text

	list_tokens <- data.frame()
	i <- 1

	while (i <= length(tokens))
	{
		if(is.na(tokens[[i]][1]) == FALSE)
		{
			list_tokens <- rbind(as.data.frame(table(tokens[[i]])),list_tokens)
		}
		i <- i+1
	}

	if(nrow(list_tokens) > 0)
	{
		list_tokens <- aggregate(.~Var1,list_tokens,sum)
		list_tokens[order(-list_tokens$Freq),]
	}
	else{print("0")}

}