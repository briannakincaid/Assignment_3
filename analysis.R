#########BASE ANALYSIS#########

###Data Import and Cleaning###

##Import Data##
A2010 <- read.csv( "BP Apprehensions 2010.csv" , header = TRUE, stringsAsFactors = FALSE)
A2017 <- read.csv("PB Apprehensions 2017.csv", header = TRUE, stringsAsFactors = FALSE)
  
##Clean Data##

#2010 Data#
  
rownames(A2000.2017) <- A2000.2017[,1]
A2000.2017 <-  subset(A2000.2017, select= -c(year))

A2000.2017 <- A2000.2017[18:1,]
rownames(A2000.2017) <- c()
A2000.2017 <- unname(A2000.2017)

rownames(A2010) <- A2010[,1]
A2010 <-  subset(A2010, select= -c(Sector))
A2010 <- rbind(A2010, colSums(A2010))
rownames(A2010) <- c(rownames(A2010)[-length(rownames(A2010))], "Total")
A2010 <- cbind(A2010,rowSums(A2010))
colnames(A2010) <- c(colnames(A2010)[-length(colnames(A2010))], "Total")

#2017 Data#
rownames(A2017) <- A2017[,1]
A2017 <-  subset(A2017, select= -c(Sector))
A2017 <- rbind(A2017, colSums(A2017))
rownames(A2017) <- c(rownames(A2017)[-length(rownames(A2017))], "Total")
A2017 <- cbind(A2017,rowSums(A2017))
colnames(A2017) <- c(colnames(A2017)[-length(colnames(A2017))], "Total")

    
###2010 and 2017 Compared###
  
##By Sector##
  
  year2010s <- t(as.data.frame(matrix(A2010[1:9,13])))
  colnames(year2010s) <- rownames(A2010[1:9,])
  colnames(year2010s)[6] <- "Rio Grande"
 
  year2017s <- t(as.data.frame(matrix(A2017[1:9,13])))
  colnames(year2017s) <- rownames(A2017[1:9,])
  colnames(year2017s)[6] <- "Rio Grande"
   
  year2010_17s <- rbind(year2010s, year2017s)
  row.names(year2010_17s) <- c("2010", "2017")

  barplot(as.matrix(year2010_17s), beside = TRUE, col = c("red", "blue"), bty="n",las=2)
  legend("topleft", c("2010","2017"), pch=15,  col=c("red","blue"),  bty="n")
  title("Border Patrol Apprehensions by Sector")
  
  
##By Month##

year2010m <- t(as.data.frame(matrix(unname(t(A2010)[1:12,10]))))
colnames(year2010m) <- colnames(A2010[1:12])

year2017m <- t(as.data.frame(matrix(unname(t(A2017)[1:12,10]))))
colnames(year2017m) <- colnames(A2017[1:12])
   
year2010_17m <- rbind(year2010m, year2017m)
row.names(year2010_17m) <- c("2010", "2017")

barplot(as.matrix(year2010_17m), beside = TRUE, col = c("red", "blue"), bty="n",las=2)
legend("topleft", c("2010","2017"), pch=15,  col=c("red","blue"),  bty="n")
title("Border Patrol Apprehensions by Month")
  
##t-test##
  

#A comparison between the sector with most apprehensions for 2010 and the sector with most apprehensions in 2017#
  
Sector_Totals_2010 <- A2010[1:9,13]
names(Sector_Totals_2010) <- rownames(A2010[1:9,])
Sector_Totals_2017 <- A2017[1:9,13]
names(Sector_Totals_2017) <- rownames(A2017[1:9,])

MA_2017_index <- which(Sector_Totals_2017 == max(Sector_Totals_2017))
MA_2010_index <- which(Sector_Totals_2010 == max(Sector_Totals_2010))

MA_2017 <- A2017[MA_2017_index,1:12]
MA_2010 <- A2010[MA_2010_index,1:12]

t.test(MA_2010,MA_2017)
t.test(MA_2010,A2017[MA_2010_index,1:12])
t.test(MA_2017,A2010[MA_2017_index,1:12])


#A comparison between the 3 month periods with the most apprehensions in 2010 and 2017#
  
col <- c("Oct-Dec", "Jan-Mar", "Apr-Jun","Jul-Sep")
Monthly_Totals_2010 <- (t(A2010)[1:12,10])
  
A2010_3 <- rbind(sum(Monthly_Totals_2010[1:3]),sum(Monthly_Totals_2010[4:6]),sum(Monthly_Totals_2010[7:9]),sum(Monthly_Totals_2010[10:12]))
Monthly_Totals_2017 <- (t(A2017)[1:12,10])
  
A2017_3 <- rbind(sum(Monthly_Totals_2017[1:3]),sum(Monthly_Totals_2017[4:6]),sum(Monthly_Totals_2017[7:9]),sum(Monthly_Totals_2017[10:12]))

t.test(Monthly_Totals_2010[4:6],Monthly_Totals_2017[1:3])

###Overall Trends###
  
##Data Cleaning##
  
  
A2000.2017 <- read.csv("PB monthly summaries.csv", header = TRUE, stringsAsFactors = FALSE)
rownames(A2000.2017) <- A2000.2017[,1]

A2000.2017 <-  subset(A2000.2017, select= -c(year))
A2000.2017 <- A2000.2017[18:1,]
rownames(A2000.2017) <- c()
A2000.2017 <- unname(A2000.2017)

##Time Series##
  
ts2 <- as.vector(t(A2000.2017))
time_series <- ts(ts2, start = c(2000,10), frequency=12)
ts.plot(time_series, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)))
  
meanbyyear <- rowMeans(A2000.2017)
years <- c(2000:2017)
lines(years,meanbyyear,col="red")
  
title("Border Patrol Apprehensions Year")
legend("topright", c("Average Apprehensions"), pch=15,  col="red",  bty="n")
  