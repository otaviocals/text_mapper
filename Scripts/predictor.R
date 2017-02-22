predictor<-function(token,text,lines=0,filter=FALSE,language="english")
{
#Loading libraries
	package_list <- c("tm")
	for( i in length(package_list))
	{
		if (!require(package_list[i],character.only = TRUE))
    			{
		      	install.packages(package_list[i],dep=TRUE)
			        if(!require(package_list[i],character.only = TRUE)) stop("Package not found")
			}
	}

#Loading scripts
	source("../Scripts/readtext.R")
	source("../Scripts/tokenize.R")
	source("../Scripts/filter_tokens.R")
	source("../Scripts/aggregator.R")
	source("../Scripts/ngramer.R")

#Grabbing Lines

	split_token <- strsplit(token," ")[[1]]
	k <- 1
	n<- 0
	new_token <- character()
	for (k in 1:length(split_token))
	{
		if(nchar(split_token[k])>0)
		{
			n <- n+1
			if(length(new_token)>0)
			{
				new_token <- paste(new_token,split_token[k])
			} else{
				new_token <- split_token[k]
			}
		}
	}
	token <- new_token
	token <- tolower(token)

	if (lines==0){
		loaded_text <- readtext(text)
	} else {
		loaded_text <- readtext(text)[1:lines]
	}

	grep_lines <- loaded_text[grepl(token,loaded_text)]

#Tokenization

	token_grep <- tokenize(grep_lines)
	if (filter==TRUE)
	{
		token_grep <- filter_tokens(token_grep,stopwords(language))
	}
	if (n>1)
	{
		token_grep <- ngramer(token_grep,n)
	}

#Matching

	i<-1
	j<-1
	word_list <- list()

	if(length(token_grep)>0)
	{
		for(i in 1:length(token_grep))
		{
			for(j in 1:length(token_grep[[i]]))
			{
				if(token_grep[[i]][j]==token)
				{
					split_strings <- strsplit(token_grep[[i]][j+1]," ")
					word_add <- split_strings[[1]][n]
					if(!is.na(word_add))
					{
						word_list <- c(word_list,word_add)
					}
				}
			}
		}
		prediction <- aggregator(word_list)
	}else{
		prediction <- "no data"
	}
	prediction
}