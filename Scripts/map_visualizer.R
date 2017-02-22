map_visualizer <- function(map, type="pie", min=0.01)
{
#Loading libraries
	package_list <- c("googleVis")
	for( i in length(package_list))
	{
		if (!require(package_list[i],character.only = TRUE))
    			{
		      	install.packages(package_list[i],dep=TRUE)
			        if(!require(package_list[i],character.only = TRUE)) stop("Package not found")
			}
	}

#Pie Chart Visualizer
	if(type=="pie")
	{

		pie_data <- data.frame(Word=character(),Points=numeric())

		i<-1
		j<-1
		k<-1
		other_total <- 0
		to_drop <- integer()

		for (i in 1:length(map))
		{

			for (j in 1:length(map[[i]]))
			{

				for (k in 1:length(map[[i]][[j]][,1]))
				{
					if(!is.na(map[[i]][[j]][k,3]))
					{
						curr_data <- data.frame(as.character(map[[i]][[j]][k,1]),map[[i]][[j]][k,3])
						pie_data <- rbind(pie_data,curr_data)
					}
				}

			}

		}

		sum_points <- sum(pie_data[,2])
		pie_data[,2] <- pie_data[,2]/sum_points
		pie_data <- pie_data[order(-pie_data[,2]),]

		for (i in 1:length(pie_data[,1]))
		{
			if(pie_data[i,2] < min)
			{
				other_total <- other_total + pie_data[i,2]
				to_drop <- c(to_drop,i)
			}
		}


		pie_data <- pie_data[-to_drop,]
		colnames(pie_data) <- c("Word","Percents")
		pie_data <- rbind(pie_data,data.frame(Word="Other",Percents=other_total))

		pie_map <- gvisPieChart(pie_data)
		result <- plot(pie_map)
	}

#Tree Map Visualizer
	else if(type=="tree")
	{
		tree_data <- data.frame(Word=character(),Points=numeric(),Parent=character())

		i<-1
		j<-1
		k<-1
		other_total <- 0
		to_drop <- integer()

		for (i in 1:length(map))
		{

			for (j in 1:length(map[[i]]))
			{

				for (k in 1:length(map[[i]][[j]][,1]))
				{
					if(!is.na(map[[i]][[j]][k,3]))
					{
						curr_data <- data.frame(as.character(map[[i]][[j]][k,1]),map[[i]][[j]][k,3],colnames(map[[i]][[j]])[1])
						tree_data <- rbind(tree_data,curr_data)
					}
				}

			}

		}

		sum_points <- sum(tree_data[,2])
		tree_data[,2] <- tree_data[,2]/sum_points
		tree_data <- tree_data[order(tree_data[,3]),]
		colnames(tree_data) <- c("Word","Percents","Parent")

		child_apply <- tapply(tree_data$Percents,tree_data$Parent,sum)
		child_data <- data.frame(Child_Sum=child_apply, Parent=rownames(child_apply))
		tree_data <- merge(tree_data,child_data,by="Parent")
		tree_data <- rbind(data.frame(Word=colnames(map[[1]][[1]])[1],Percents=1.0,Parent=NA,Child_Sum=1.0), tree_data)

		tree_map <- gvisTreeMap(tree_data,idvar="Word",parentvar="Parent",sizevar="Percents", colorvar = "Child_Sum",
					 options=list(width=600, height=500,
                                 fontSize=16,
                                 minColor='#EDF8FB',
                                 midColor='#66C2A4',
                                 maxColor='#006D2C',
                                 headerHeight=20,
                                 fontColor='black',
                                 showScale=TRUE))

		result <- plot(tree_map)
	}

#Org Map Visualizer
	else if(type=="org")
	{
		org_data <- data.frame(Word=character(),Points=numeric(),Parent=character())

		i<-1
		j<-1
		k<-1
		other_total <- 0
		to_drop <- integer()

		for (i in 1:length(map))
		{

			for (j in 1:length(map[[i]]))
			{

				for (k in 1:length(map[[i]][[j]][,1]))
				{
					if(!is.na(map[[i]][[j]][k,3]))
					{
						curr_data <- data.frame(as.character(map[[i]][[j]][k,1]),map[[i]][[j]][k,3],colnames(map[[i]][[j]])[1])
						org_data <- rbind(org_data,curr_data)
					}
				}

			}

		}

		sum_points <- sum(org_data[,2])
		org_data[,2] <- org_data[,2]/sum_points
		org_data <- org_data[order(org_data[,3]),]
		colnames(org_data) <- c("Word","Percents","Parent")
		org_data <- rbind(data.frame(Word=colnames(map[[1]][[1]])[1],Percents=1.0,Parent=NA), org_data)

		org_map <- gvisOrgChart(org_data,idvar="Word",parentvar="Parent",tipvar="Percents", options=list(size='large', allowCollapse=TRUE))

		result <- plot(org_map)
	}

#Error
	else
	{
		result <- "Error: Not a valid visualization type."
	}

result
}