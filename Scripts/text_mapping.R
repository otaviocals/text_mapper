text_mapping<- function(word,text,lines=0,depth=1,synthesize=TRUE,filter=FALSE,language="english")
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
	source("Scripts/readtext.R")
	source("Scripts/tokenize.R")
	source("Scripts/filter_tokens.R")
	source("Scripts/aggregator.R")
	source("Scripts/ngramer.R")

#Grabbing Lines
	word <- tolower(word)

	if (lines==0){
		loaded_text <- readtext(text)
	} else {
		loaded_text <- readtext(text)[1:lines]
	}

	grep_lines <- loaded_text[grepl(word,loaded_text)]

#Getting First Row
	n<-1
	token_grep <- tokenize(grep_lines)
	token_grep <- filter_tokens(token_grep,word)
	if (filter==TRUE)
	{
		token_grep <- filter_tokens(token_grep,stopwords(language))
	}
	if (n>1)
	{
		token_grep <- ngramer(token_grep,n)
	}
	rank_first <- head(aggregator(token_grep),5)
	points<- rank_first[2]*10
	rank_first <- cbind(rank_first,points)
	colnames(rank_first) <- c(word,"Freq","Points")
	result <- rank_first

#Getting Following Rows
	if(depth>1)
	{	
		words_depth <- list()
		words_depth[[1]] <- word

		word_list <- array()
		new_word_list <- array()
		word_list <- c(word)

		total_rank <- list()
		total_rank[[1]] <- list()
		total_rank[[1]][1] <- array()
		total_rank[[1]][[1]] <- rank_first

		for (i in 2:depth)
		{
			total_rank[[i]] <- list()
			j<-1
			previous_words <- list()


			for (j in 1:length(total_rank[[i-1]]))
			{
				suppressWarnings(if(!is.na(total_rank[[i-1]][[j]]))
				{
					previous_words <- c(previous_words,as.character(total_rank[[i-1]][[j]][,1]))
					new_word_list <- c(word_list,as.character(total_rank[[i-1]][[j]][,1]))
					words_depth[[i]] <- previous_words
				} else {
					previous_words <- c(previous_words,c(NA,NA,NA,NA,NA))
					new_word_list <- c(word_list,c(NA,NA,NA,NA,NA))
					words_depth[[i]] <- previous_words
				})
			}


			j<-1


#Synthesizer
			if (synthesize==TRUE)
			{	

				k <- 1
				list_word_to_credit <- list()
					
				for (k in 1:length(words_depth[[i]]))
				{
					remove <- FALSE
					remove <- as.logical(length(grep(words_depth[[i]][[k]],word_list)))

					if(remove)
					{
						q<-1
						w<-1
						stop_flag<-FALSE
						
						for(q in 1:(i-1))
						{
							for(w in 1:length(words_depth[[q]]))
							{

								suppressWarnings(if(!is.na(words_depth[[i]][[k]]) && words_depth[[i]][[k]] != word && !is.na(total_rank[[q]][[w]]))
								{
		
									to_point <- grep(paste0("^",words_depth[[i]][[k]],"$"),word_list)

									if(length(to_point)>0)
									{
										list_word_to_credit <- c(list_word_to_credit, word_list[to_point[1]])

										#total_rank[[q]][[w]][to_point,3]<- total_rank[[q]][[w]][to_point,3] + 100/i

										stop_flag<-TRUE
									}
								})
								
								if(stop_flag) break
							}
								
							if(stop_flag) break
						}

						words_depth[[i]][[k]] <- NA

					} else{

						word_list<-c(word_list,words_depth[[i]][[k]])

					}


				}
				
			}

			j<-1

#Saving Current Rows
			for (j in 1:length(words_depth[[i]]))
			{
				total_rank[[i]][j] <- array()
				curr_word <- words_depth[[i]][j]

				if(!is.na(curr_word))
				{
					curr_grep_lines<- loaded_text[grepl(curr_word,loaded_text)]
					curr_token_grep <- tokenize(curr_grep_lines)
					curr_token_grep <- filter_tokens(curr_token_grep,curr_word)

					if(filter==TRUE)
					{
						curr_token_grep <- filter_tokens(curr_token_grep,stopwords(language))
					}

					if (n>1)
					{
						curr_token_grep <- ngramer(curr_token_grep,n)
					}

					curr_rank <- head(aggregator(curr_token_grep),5)
					curr_points<- curr_rank[2]*(10/i)
					curr_rank <- cbind(curr_rank,curr_points)
					colnames(curr_rank) <- c(curr_word,"Freq","Points")
					total_rank[[i]][[j]]<-curr_rank

					k<-1
					for(k in 1:5)
					{

						to_credit <- grep(paste0("^",total_rank[[i]][[j]][k,1],"$"),word_list)

						if(length(to_credit)>0)
						{
							word_to_credit <- word_list[to_credit[1]]

							q<-1
							w<-1
							e<-1
							stop_flag<-FALSE
							for(q in 1:depth)
							{
								for(w in 1:length(total_rank[[q]]))
								{
									for(e in 1:5)
									{
										if(!is.na(total_rank[[q]][[w]]) && !is.na(total_rank[[i]][[j]][k,3]) && as.character(total_rank[[q]][[w]][e,1]) == word_to_credit)
										{

												total_rank[[q]][[w]][e,3]<- total_rank[[q]][[w]][e,3] + total_rank[[i]][[j]][k,3]
												total_rank[[i]][[j]][k,3] <- NA
												word_list <- c(word_list,total_rank[[i]][[j]][k,1])
												stop_flag<-TRUE
										}
										if(stop_flag) break
									}
									if(stop_flag) break
								}
								if(stop_flag) break
							}
							
						}
						
						
					}
					

				} else{
					total_rank[[i]][[j]]<-as.data.frame(matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),nrow=5,ncol=3,byrow = TRUE))
					colnames(total_rank[[i]][[j]]) <- c("NA","Freq","Points")				
				}
			}
			
		}

		i<-1
		j<-1

		if (synthesize==TRUE)
		{

			word_list <- character()
			
			for(i in 1:depth)
			{
				for(j in 1:length(total_rank[[i]]))
				{	
					suppressWarnings(if(!is.na(total_rank[[i]][[j]]))
					{
						k<- 1
						current_words_list_depth <- as.character(total_rank[[i]][[j]][,1])
						for(k in 1:length(current_words_list_depth))
						{
							to_credit <- grep(paste0("^",current_words_list_depth[k],"$"),word_list)

							if(length(to_credit)>0)
							{
								word_to_credit <- word_list[to_credit[1]]
	
								q<-1
								w<-1
								e<-1
								stop_flag<-FALSE
								for(q in 1:depth)
								{
									for(w in 1:length(total_rank[[q]]))
									{
										for(e in 1:5)
										{
											if(!is.na(total_rank[[q]][[w]])&& !is.na(total_rank[[i]][[j]][k,3]) && as.character(total_rank[[q]][[w]][e,1]) == word_to_credit && (q!=i || w!=j || e!=k))
											{

												total_rank[[q]][[w]][e,3]<- total_rank[[q]][[w]][e,3] + total_rank[[i]][[j]][k,3]
												total_rank[[i]][[j]][k,3] <- NA
												stop_flag<-TRUE

											}
											if(stop_flag) break
										}
										if(stop_flag) break
									}
									if(stop_flag) break
								}


								#total_rank[[depth]][[j]][k,3] <- NA
							}
							else 
							{
								word_list <- c(word_list,as.character(current_words_list_depth[k]))
							}
						}
					} 
					else
					{
						total_rank[[depth]][[j]]<- as.data.frame(matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),nrow=5,ncol=3,byrow = TRUE))
						colnames(total_rank[[depth]][[j]]) <- c("NA","Freq","Points")				
					}
					)
				}
			}
		}

		result <- total_rank
	}

	result
}