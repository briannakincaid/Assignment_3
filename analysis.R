#Analysis

A2010 <- read.csv( "BP Apprehensions 2010.csv" , header = TRUE, stringsAsFactors = FALSE)
A2017 <- read.csv("PB Apprehensions 2017.csv", header = TRUE, stringsAsFactors = FALSE)
A2000.2017 <- read.csv("PB monthly summaries.csv", header = TRUE, stringsAsFactors = FALSE)


#### organiza data with Rownames, Column Totals, and Row Totals

## Use strings in Col 1 as row names
rownames(A2010) <- A2010[,1]

## Drop column 1
A2010 <-  subset(A2010, select= -c(Sector))

## rbind ColSums to dataframe
A2010 <- rbind(A2010, colSums(A2010))

## rbind assigns a rowname -- drop this name
-length(rownames(A2010))  

## rename the row with column totals "Total"
rownames(A2010) <- c(rownames(A2010)[-length(rownames(A2010))], "Total")

## cbind rowSums to dataframd
A2010 <- cbind(A2010,rowSums(A2010))

## rename last column "Totals
colnames(A2010) <- c(colnames(A2010)[-length(colnames(A2010))], "Total")



####################################################################################

## Use strings in Col 1 as row names
rownames(A2017) <- A2017[,1]

## Drop column 1
A2017 <-  subset(A2017, select= -c(Sector))

## rbind ColSums to dataframe
A2017 <- rbind(A2017, colSums(A2017))

## rbind assigns a rowname -- drop this name
-length(rownames(A2017))  

## rename the row with column totals "Total"
rownames(A2017) <- c(rownames(A2017)[-length(rownames(A2017))], "Total")

## cbind rowSums to dataframd
A2017 <- cbind(A2017,rowSums(A2017))

## rename last column "Totals
colnames(A2017) <- c(colnames(A2017)[-length(colnames(A2017))], "Total")


#COMPARE 2010 AND 2017 BY SECTOR

par(mfcol=c(1,2),oma=c(0,0,2,0))

barplot(A2010[1:9,13], 
        names.arg = rownames(A2010)[1:9], 
        las=2,
        axisnames=TRUE,
        main="2010",
        border="blue",
        col="yellow",
        ylim=c(0,200000))

barplot(A2017[1:9,13], 
        names.arg = rownames(A2017)[1:9], 
        las=2,
        axisnames=TRUE,
        main="2017",
        border="blue",
        col="yellow",
        ylim=c(0,200000))

title("Border Patrol Apprehensions by Sector", outer=TRUE)

#COMPARE 2010 AND 2017 BY MONTH

par(mfcol=c(1,2))
barplot(A2010$Total, 
        names.arg = colnames(A2010)[1:12], 
        las=2,
        axisnames=TRUE,
        main="2010 Border Patrol Apprehensions by Month",
        border="blue",
        col="yellow")






#use sample statistics tests to compare sector with most apprehensions for 2010
#with sector with most apprehensions in 2017


#compare 3 month periods with the most apprehensions in 2010 and 2017


#make time series chart from monthly summary