library(shiny)
library(DBI)
# library(odbc)
library(ggplot2)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
#con <- dbConnect(odbc::odbc(), "GSA_SQL", timeout = 10, UID = "AngelM", 
#                 PWD = "AM$gsa4545")




## Inventory#####################################################
#FullInventory <- dbGetQuery(con, "select * from [dbo].[Inventory_Close] where partnumber IN  (\'PT00061769-E\',\'PT00061769-F\',\'PT00061566-E\',\'PT00053946-F\',\'C100061769-E\',\'C200061769-F\',\'C100061566-E\',\'C200053946-F\', \'C300000560-J\',\'C400000560-J\',\'C200000565-J\',\'C300000565-J\') AND wh IN (\'GSA - MORGAN HILL - MAIN\',\'GSA - MAIN\') order by id desc")
FullInventory <- read.csv("FullInventory")

# Input Criteria Below:
#################################################################################
#test <- dbGetQuery(con,"SELECT * FROM (
#	SELECT        c.PartNumber, (SUM(CASE WHEN datepart(hour, c.[date]) BETWEEN 0 AND 4 THEN 1 ELSE 0 END) + SUM(CASE WHEN datepart(hour, c.[date]) BETWEEN 16 AND 
#							 23 THEN 1 ELSE 0 END)) AS Expr1, SUM(CASE WHEN datepart(hour, c.[date]) = 7 THEN 1 ELSE 0 END) AS Expr2, SUM(CASE WHEN datepart(hour, c.[date])
	#						  = 8 THEN 1 ELSE 0 END) AS Expr3, SUM(CASE WHEN datepart(hour, c.[date]) = 9 THEN 1 ELSE 0 END) AS Expr4, SUM(CASE WHEN datepart(hour, c.[date]) = 10 THEN 1 ELSE 0 END) AS Expr5, SUM(CASE WHEN datepart(hour, 
#							 c.[date]) = 11 THEN 1 ELSE 0 END) AS Expr6, SUM(CASE WHEN datepart(hour, c.[date]) = 12 THEN 1 ELSE 0 END) AS Expr7, SUM(CASE WHEN datepart(hour, c.[date]) = 13 THEN 1 ELSE 0 END) AS Expr8, 
#							 SUM(CASE WHEN datepart(hour, c.[date]) = 14 THEN 1 ELSE 0 END) AS Expr9, SUM(CASE WHEN datepart(hour, c.[date]) = 15 THEN 1 ELSE 0 END) AS Expr10, COUNT(c.Serie) AS total, CONVERT(date, c.Date) AS Date, dbo.SLP.Area
#	FROM            dbo.Cable AS c INNER JOIN
#							 dbo.SLP ON dbo.SLP.SLP = c.SLP
#	GROUP BY c.PartNumber, CONVERT(date, c.Date), dbo.SLP.Area
#) Numbers_view
#WHERE PartNumber IN (\'C100061769-E\',\'C200061769-F\',\'C100061566-E\',\'C200053946-F\', \'C400000560-J\', \'C300000565-J\')
#AND Date BETWEEN \'10-01-2021\' AND GETDATE()")

test <- read.csv("HourlyProduction")
#################################################################################

# Edits made to dataframes

test <- test %>% 
  group_by(Date,PartNumber) %>%
  summarize(total = sum(total))
test <- test[,c("PartNumber","total","Date")]

#Rivian DF edit: We load it, filter by the rivian PO, Change the date format, then subset it to the columns that we need.
#EDIFiles <- file.info(list.files("U:/GSA_Files/GSA_EDI", full.names = T))

##EDIFiles <- read.csv("EDIFiles")

#rivian_demand <- read_xlsx("U:/GSA_Files/GSA_EDI/one_edi_06-Feb-2022.xlsx")
#rivian_demand <- read_xlsx(row.names(EDIFiles)[which.max(EDIFiles$mtime)])  
rivian_demand <- read_xlsx("one_edi_16-Dec-2022.xlsx")
#This will be to pull the most recently modified EDI file from the folder (;
colnames(rivian_demand)[which(names(rivian_demand) == "Part Number")] <- "PartNumber"
colnames(rivian_demand)[which(names(rivian_demand) == "Ship Date")] <- "ShipDate"
colnames(rivian_demand)[which(names(rivian_demand) == "Open Quantity")] <- "OpenQuantity"
colnames(rivian_demand)[which(names(rivian_demand) == "PO Number")] <- "PONumber"
rivian_demand <- rivian_demand %>% filter(PONumber == 4500015860)
#rivian_demand <- rivian_demand[rivian_demand$PartNumber == "C100061769-00-E",]
rivian_demand$ShipDate <- as.Date(rivian_demand$ShipDate,"%m/%d/%Y")
#rivian_demand$ShipDate <- as.Date(as.numeric(rivian_demand$ShipDate),origin = "1899-12-30") (original if the date comes out as days from origin again)
rivian_demand <- subset(rivian_demand, select = c("PartNumber", "ShipDate", "OpenQuantity"))


#In <- dbGetQuery(con, "select * from (select d.PartNumber,r.Truck,sum(d.quantity) as quantity,r.area,CONVERT(date,r.Date) as date,
#CASE WHEN LEN(TRIM(a.PartNumber)) > 0 THEN 1 ELSE 0 END as QCR from Received r inner join ReceivedDetail d on d.Truck = r.Truck 
#left join (select PartNumber,truck from quality_inspection_truck group by PartNumber,truck) a on a.truck = r.Truck and a.PartNumber = d.PartNumber WHERE r.[Date] 
#BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'PT00061769-E\' group by d.partnumber,r.Truck,Area,CONVERT(date,r.Date),a.PartNumber) A")

In <- read.csv("In")

# Out <- dbGetQuery(con, "select * from (select d.PartNumber,p.packing,sum(d.quantity) as quantity,p.area,CONVERT(date,p.Date) as date,PLNO 
# 			   from packing p 
# 	              inner join packingDetail d on p.packing = d.packing 
# 			   WHERE p.[Date] BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'C100061769-E\' 
# 			   group by d.partnumber,p.packing,Area,CONVERT(date,p.Date),PLNO) A")

Out <- read.csv("Out")

#In/Out DF edit: In preparation to merge these two dataframes together, I change the PartNumber and quantity 
#column name to add an "In" and "Out" for the entries that have the same date and end up merging. 
colnames(In)[2] <- "PartNumberIn"
colnames(In)[4] <- "QuantityIn"
colnames(In)[6] <- "Date"
colnames(Out)[2] <- "PartNumberOut"
colnames(Out)[4] <- "QuantityOut"
colnames(Out)[6] <- "Date"
In <- In[,c("PartNumberIn","QuantityIn","Date")]
Out <- Out[,c("PartNumberOut","QuantityOut","Date")]

#group together all of the days with multiple entries
In <- In %>% group_by(Date,PartNumberIn) %>% summarize(QuantityIn = sum(QuantityIn))
Out <- Out %>% group_by(Date,PartNumberOut) %>% summarize(QuantityOut = sum(QuantityOut))

#Merging In and Out DFs: merge by date, but keep all values that don't have a match.
InOut <- merge(In,Out,by = "Date",all.x = TRUE,all.y = TRUE)
#Fill in the NA values. We cycle through each value in the PN columns and if it is NA then we fill it with the PN.
for(i in 1:nrow(InOut)){
  if(is.na(InOut[i,2]) == TRUE){
    InOut[i,2] = "PT00061769-E"
  }
}

for(i in 1:nrow(InOut)){
  if(is.na(InOut[i,4]) == TRUE){
    InOut[i,4] = "C100061769-E"
  }
}

#Next, we fill the rest of the NA values with 0 since those are all that remain 
InOut[is.na(InOut)] <- 0
#Next, we add a column containing the differential between In/Out and a column UpDown solely for color coding
InOut$Differential <- InOut$QuantityIn - InOut$QuantityOut
InOut$UpDown <- rep("",nrow(InOut))
for(i in 1:nrow(InOut)){
  if(InOut[i,6] < 0){
    InOut[i,7] = "Down"
  }
  else{
    InOut[i,7] = "Up"
  }
}
#Finally, set a factor for the graph legend order
InOut$UpDown <- factor(InOut$UpDown,c("Up","Down"))

#New Column: Running total differential to track total InvsOut instead of by day:
InOut$RunningDifferential <- c(InOut$Differential[1],rep(0,nrow(InOut)-1))
for(i in 2:nrow(InOut)){
  InOut$RunningDifferential[i] <- InOut$RunningDifferential[i-1] + InOut$Differential[i]}


OutvsProd <- merge(Out,subset(test, PartNumber == "C100061769-E"), all.x = TRUE, all.y = TRUE)
for(i in 1:nrow(OutvsProd)){
  if(is.na(OutvsProd[i,3]) == TRUE){
    OutvsProd[i,3] = 0
  }
}
for(i in 1:nrow(OutvsProd)){
  if(is.na(OutvsProd[i,5]) == TRUE){
    OutvsProd[i,5] = 0
  }
}
colnames(OutvsProd)[which(names(OutvsProd) == "QuantityOut")] <- "Shipped"
colnames(OutvsProd)[which(names(OutvsProd) == "total")] <- "Produced"

OutvsProdmelted <- OutvsProd %>% 
  pivot_longer(OutvsProd,cols = c(Shipped,Produced), names_to = "OutAndTotal")


# In1 <- dbGetQuery(con, "select * from (select d.PartNumber,r.Truck,sum(d.quantity) as quantity,r.area,CONVERT(date,r.Date) as date,
# CASE WHEN LEN(TRIM(a.PartNumber)) > 0 THEN 1 ELSE 0 END as QCR from Received r inner join ReceivedDetail d on d.Truck = r.Truck 
# left join (select PartNumber,truck from quality_inspection_truck group by PartNumber,truck) a on a.truck = r.Truck and a.PartNumber = d.PartNumber WHERE r.[Date] 
#BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'PT00061769-F\' group by d.partnumber,r.Truck,Area,CONVERT(date,r.Date),a.PartNumber) A")

In1 <- read.csv("In1")

# Out1 <- dbGetQuery(con, "select * from (select d.PartNumber,p.packing,sum(d.quantity) as quantity,p.area,CONVERT(date,p.Date) as date,PLNO 
# 			   from packing p 
# 	              inner join packingDetail d on p.packing = d.packing 
# 			   WHERE p.[Date] BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'C200061769-F\' 
# 			   group by d.partnumber,p.packing,Area,CONVERT(date,p.Date),PLNO) A")

Out1 <- read.csv("Out1")

#In/Out DF edit: In preparation to merge these two dataframes together, I change the PartNumber and quantity column name to add an "In" and "Out" for the entries that have the same date and end up merging. 
colnames(In1)[2] <- "PartNumberIn"
colnames(In1)[4] <- "QuantityIn"
colnames(In1)[6] <- "Date"
colnames(Out1)[2] <- "PartNumberOut"
colnames(Out1)[4] <- "QuantityOut"
colnames(Out1)[6] <- "Date"
In1 <- In1[,c("PartNumberIn","QuantityIn","Date")]
Out1 <- Out1[,c("PartNumberOut","QuantityOut","Date")]

#group together all of the days with multiple entries
In1 <- In1 %>% group_by(Date,PartNumberIn) %>% summarize(QuantityIn = sum(QuantityIn))
Out1 <- Out1 %>% group_by(Date,PartNumberOut) %>% summarize(QuantityOut = sum(QuantityOut))

#Merging In and Out DFs: merge by date, but keep all values that don't have a match.
InOut1 <- merge(In1,Out1,by = "Date",all.x = TRUE,all.y = TRUE)
#Fill in the NA values. We cycle through each value in the PN columns and if it is NA then we fill it with the PN.
for(i in 1:nrow(InOut1)){
  if(is.na(InOut1[i,2]) == TRUE){
    InOut1[i,2] = "PT00061769-F"
  }
}

for(i in 1:nrow(InOut1)){
  if(is.na(InOut1[i,4]) == TRUE){
    InOut1[i,4] = "C200061769-F"
  }
}
#Next, we fill the rest of the NA values with 0 since those are all that remain 
InOut1[is.na(InOut1)] <- 0
#Next, we add a column containing the differential between In/Out and a column UpDown solely for color coding
InOut1$Differential <- InOut1$QuantityIn - InOut1$QuantityOut
InOut1$UpDown <- rep("",nrow(InOut1))
for(i in 1:nrow(InOut1)){
  if(InOut1[i,6] < 0){
    InOut1[i,7] = "Down"
  }
  else{
    InOut1[i,7] = "Up"
  }
}

#Finally, set a factor for the graph legend order
InOut1$UpDown <- factor(InOut1$UpDown,c("Up","Down"))

#New Column: Running total differential to track total InvsOut instead of by day:
InOut1$RunningDifferential <- c(InOut1$Differential[1],rep(0,nrow(InOut1)-1))
for(i in 2:nrow(InOut1)){
  InOut1$RunningDifferential[i] <- InOut1$RunningDifferential[i-1] + InOut1$Differential[i]}

OutvsProd1 <- merge(Out1,subset(test, PartNumber == "C200061769-F"), all.x = TRUE, all.y = TRUE)
for(i in 1:nrow(OutvsProd1)){
  if(is.na(OutvsProd1[i,3]) == TRUE){
    OutvsProd1[i,3] = 0
  }
}
for(i in 1:nrow(OutvsProd1)){
  if(is.na(OutvsProd1[i,5]) == TRUE){
    OutvsProd1[i,5] = 0
  }
}
colnames(OutvsProd1)[which(names(OutvsProd1) == "QuantityOut")] <- "Shipped"
colnames(OutvsProd1)[which(names(OutvsProd1) == "total")] <- "Produced"

#This DF was too long so I split it into two and apend them
OutvsProdmelted11 <- OutvsProd1[1:72,] %>% 
  pivot_longer(OutvsProd1[1:72,],cols = c(Shipped,Produced), names_to = "OutAndTotal")

OutvsProdmelted111 <- OutvsProd1[73:144,] %>% 
  pivot_longer(OutvsProd1[73:144,],cols = c(Shipped,Produced), names_to = "OutAndTotal")

OutvsProdmelted1 <- bind_rows(OutvsProdmelted11, OutvsProdmelted111) %>% 
  arrange(Date)

# Shipping In
########################################################################################
# In2 <- dbGetQuery(con, "select * from (select d.PartNumber,r.Truck,sum(d.quantity) as quantity,r.area,CONVERT(date,r.Date) as date,
# CASE WHEN LEN(TRIM(a.PartNumber)) > 0 THEN 1 ELSE 0 END as QCR from Received r inner join ReceivedDetail d on d.Truck = r.Truck 
# left join (select PartNumber,truck from quality_inspection_truck group by PartNumber,truck) a on a.truck = r.Truck and a.PartNumber = d.PartNumber WHERE r.[Date] 
# BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'PT00053946-F\' group by d.partnumber,r.Truck,Area,CONVERT(date,r.Date),a.PartNumber) A")

In2 <- read.csv("In2")

###########################################################################################


# Shipping Out
########################################################################################
# Out2 <- dbGetQuery(con, "select * from (select d.PartNumber,p.packing,sum(d.quantity) as quantity,p.area,CONVERT(date,p.Date) as date,PLNO 
# 			   from packing p 
# 	              inner join packingDetail d on p.packing = d.packing 
# 			   WHERE p.[Date] BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'C200053946-F\' 
# 			   group by d.partnumber,p.packing,Area,CONVERT(date,p.Date),PLNO) A")

Out2 <- read.csv("Out2")

#########################################################################################

#In/Out DF edit: In preparation to merge these two dataframes together, I change the PartNumber and quantity column name to add an "In" and "Out" for the entries that have the same date and end up merging. 
colnames(In2)[2] <- "PartNumberIn"
colnames(In2)[4] <- "QuantityIn"
colnames(In2)[6] <- "Date"
colnames(Out2)[2] <- "PartNumberOut"
colnames(Out2)[4] <- "QuantityOut"
colnames(Out2)[6] <- "Date"
In2 <- In2[,c("PartNumberIn","QuantityIn","Date")]
Out2 <- Out2[,c("PartNumberOut","QuantityOut","Date")]

#group together all of the days with multiple entries
In2 <- In2 %>% group_by(Date,PartNumberIn) %>% summarize(QuantityIn = sum(QuantityIn))
Out2 <- Out2 %>% group_by(Date,PartNumberOut) %>% summarize(QuantityOut = sum(QuantityOut))

#Merging In and Out DFs: merge by date, but keep all values that don't have a match.
InOut2 <- merge(In2,Out2,by = "Date",all.x = TRUE,all.y = TRUE)
#Fill in the NA values. We cycle through each value in the PN columns and if it is NA then we fill it with the PN.
for(i in 1:nrow(InOut2)){
  if(is.na(InOut2[i,2]) == TRUE){
    InOut2[i,2] = "PT00053946-F"
  }
}

for(i in 1:nrow(InOut2)){
  if(is.na(InOut2[i,4]) == TRUE){
    InOut2[i,4] = "C200053946-F"
  }
}
#Next, we fill the rest of the NA values with 0 since those are all that remain 
InOut2[is.na(InOut2)] <- 0
#Next, we add a column containing the differential between In/Out and a column UpDown solely for color coding
InOut2$Differential <- InOut2$QuantityIn - InOut2$QuantityOut
InOut2$UpDown <- rep("",nrow(InOut2))
for(i in 1:nrow(InOut2)){
  if(InOut2[i,6] < 0){
    InOut2[i,7] = "Down"
  }
  else{
    InOut2[i,7] = "Up"
  }
}
#Finally, set a factor for the graph legend order
InOut2$UpDown <- factor(InOut2$UpDown,c("Up","Down"))

#New Column: Running total differential to track total InvsOut instead of by day:
InOut2$RunningDifferential <- c(InOut2$Differential[1],rep(0,nrow(InOut2)-1))
for(i in 2:nrow(InOut2)){
  InOut2$RunningDifferential[i] <- InOut2$RunningDifferential[i-1] + InOut2$Differential[i]}

OutvsProd2 <- merge(Out2,subset(test, PartNumber == "C200053946-F"), all.x = TRUE, all.y = TRUE)
for(i in 1:nrow(OutvsProd2)){
  if(is.na(OutvsProd2[i,3]) == TRUE){
    OutvsProd2[i,3] = 0
  }
}
for(i in 1:nrow(OutvsProd2)){
  if(is.na(OutvsProd2[i,5]) == TRUE){
    OutvsProd2[i,5] = 0
  }
}
colnames(OutvsProd2)[which(names(OutvsProd2) == "QuantityOut")] <- "Shipped"
colnames(OutvsProd2)[which(names(OutvsProd2) == "total")] <- "Produced"

OutvsProdmelted2 <- OutvsProd2 %>% 
  pivot_longer(OutvsProd2,cols = c(Shipped,Produced), names_to = "OutAndTotal")




# Shipping In
########################################################################################
# In3 <- dbGetQuery(con, "select * from (select d.PartNumber,r.Truck,sum(d.quantity) as quantity,r.area,CONVERT(date,r.Date) as date,
# CASE WHEN LEN(TRIM(a.PartNumber)) > 0 THEN 1 ELSE 0 END as QCR from Received r inner join ReceivedDetail d on d.Truck = r.Truck 
# left join (select PartNumber,truck from quality_inspection_truck group by PartNumber,truck) a on a.truck = r.Truck and a.PartNumber = d.PartNumber WHERE r.[Date] 
# BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'PT00061566-E\' group by d.partnumber,r.Truck,Area,CONVERT(date,r.Date),a.PartNumber) A")

In3 <- read.csv("In3")
###########################################################################################


# Shipping Out
########################################################################################
# Out3 <- dbGetQuery(con, "select * from (select d.PartNumber,p.packing,sum(d.quantity) as quantity,p.area,CONVERT(date,p.Date) as date,PLNO 
# 			   from packing p 
# 	              inner join packingDetail d on p.packing = d.packing 
# 			   WHERE p.[Date] BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'C100061566-E\' 
# 			   group by d.partnumber,p.packing,Area,CONVERT(date,p.Date),PLNO) A")

Out3 <- read.csv("Out3")
#########################################################################################

# Edits made to dataframes

#Rivian DF edit: We load it, filter by the rivian PO, Change the date format, then subset it to the columns that we need.
#rivian_demand3 <- read_xlsx("U:/GSA_Files/GSA_EDI/one_edi_06-Feb-2022.xlsx")
#rivian_demand3 <-rivian_demand3 %>% filter(POnumber == 4500015860, PartNumber == "C100061566-E")
#rivian_demand3$ShipDate <- as.Date(as.numeric(rivian_demand3$ShipDate),origin = "1899-12-30")
#rivian_demand3 <- subset(rivian_demand3, select = c("PartNumber", "ShipDate", "OpenQuantity"))

#In/Out DF edit: In preparation to merge these two dataframes together, I change the PartNumber and quantity column name to add an "In" and "Out" for the entries that have the same date and end up merging. 
colnames(In3)[2] <- "PartNumberIn"
colnames(In3)[4] <- "QuantityIn"
colnames(In3)[6] <- "Date"
colnames(Out3)[2] <- "PartNumberOut"
colnames(Out3)[4] <- "QuantityOut"
colnames(Out3)[6] <- "Date"
In3 <- In3[,c("PartNumberIn","QuantityIn","Date")]
Out3 <- Out3[,c("PartNumberOut","QuantityOut","Date")]

#group together all of the days with multiple entries
In3 <- In3 %>% group_by(Date,PartNumberIn) %>% summarize(QuantityIn = sum(QuantityIn))
Out3 <- Out3 %>% group_by(Date,PartNumberOut) %>% summarize(QuantityOut = sum(QuantityOut))

#Merging In and Out DFs: merge by date, but keep all values that don't have a match.
InOut3 <- merge(In3,Out3,by = "Date",all.x = TRUE,all.y = TRUE)
#Fill in the NA values. We cycle through each value in the PN columns and if it is NA then we fill it with the PN.
for(i in 1:nrow(InOut3)){
  if(is.na(InOut3[i,2]) == TRUE){
    InOut3[i,2] = "PT00061566-E"
  }
}

for(i in 1:nrow(InOut3)){
  if(is.na(InOut3[i,4]) == TRUE){
    InOut3[i,4] = "C100061566-E"
  }
}
#Next, we fill the rest of the NA values with 0 since those are all that remain 
InOut3[is.na(InOut3)] <- 0
#Next, we add a column containing the differential between In/Out and a column UpDown solely for color coding
InOut3$Differential <- InOut3$QuantityIn - InOut3$QuantityOut
InOut3$UpDown <- rep("",nrow(InOut3))
for(i in 1:nrow(InOut3)){
  if(InOut3[i,6] < 0){
    InOut3[i,7] = "Down"
  }
  else{
    InOut3[i,7] = "Up"
  }
}
#Finally, set a factor for the graph legend order
InOut3$UpDown <- factor(InOut3$UpDown,c("Up","Down"))

#New Column: Running total differential to track total InvsOut instead of by day:
InOut3$RunningDifferential <- c(InOut3$Differential[1],rep(0,nrow(InOut3)-1))
for(i in 2:nrow(InOut3)){
  InOut3$RunningDifferential[i] <- InOut3$RunningDifferential[i-1] + InOut3$Differential[i]}

OutvsProd3 <- merge(Out3,subset(test, PartNumber == "C100061566-E"), all.x = TRUE, all.y = TRUE)
for(i in 1:nrow(OutvsProd3)){
  if(is.na(OutvsProd3[i,3]) == TRUE){
    OutvsProd3[i,3] = 0
  }
}
for(i in 1:nrow(OutvsProd3)){
  if(is.na(OutvsProd3[i,5]) == TRUE){
    OutvsProd3[i,5] = 0
  }
}
colnames(OutvsProd3)[which(names(OutvsProd3) == "QuantityOut")] <- "Shipped"
colnames(OutvsProd3)[which(names(OutvsProd3) == "total")] <- "Produced"

OutvsProdmelted3 <- OutvsProd3 %>% 
  pivot_longer(OutvsProd3,cols = c(Shipped,Produced), names_to = "OutAndTotal")


# Shipping In
########################################################################################
# In5 <- dbGetQuery(con, "select * from (select d.PartNumber,r.Truck,sum(d.quantity) as quantity,r.area,CONVERT(date,r.Date) as date,
# CASE WHEN LEN(TRIM(a.PartNumber)) > 0 THEN 1 ELSE 0 END as QCR from Received r inner join ReceivedDetail d on d.Truck = r.Truck 
# left join (select PartNumber,truck from quality_inspection_truck group by PartNumber,truck) a on a.truck = r.Truck and a.PartNumber = d.PartNumber WHERE r.[Date] 
# BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'C300000560-J\' group by d.partnumber,r.Truck,Area,CONVERT(date,r.Date),a.PartNumber) A")

In5 <- read.csv("In5")
###########################################################################################


# Shipping Out
########################################################################################
# Out5 <- dbGetQuery(con, "select * from (select d.PartNumber,p.packing,sum(d.quantity) as quantity,p.area,CONVERT(date,p.Date) as date,PLNO 
# 			   from packing p 
# 	              inner join packingDetail d on p.packing = d.packing 
# 			   WHERE p.[Date] BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'C400000560-J\' 
# 			   group by d.partnumber,p.packing,Area,CONVERT(date,p.Date),PLNO) A")

Out5 <- read.csv("Out5")
#########################################################################################

#In/Out DF edit: In preparation to merge these two dataframes together, I change the PartNumber and quantity column name to add an "In" and "Out" for the entries that have the same date and end up merging. 
colnames(In5)[2] <- "PartNumberIn"
colnames(In5)[4] <- "QuantityIn"
colnames(In5)[6] <- "Date"
colnames(Out5)[2] <- "PartNumberOut"
colnames(Out5)[4] <- "QuantityOut"
colnames(Out5)[6] <- "Date"
In5 <- In5[,c("PartNumberIn","QuantityIn","Date")]
Out5 <- Out5[,c("PartNumberOut","QuantityOut","Date")]

#group together all of the days with multiple entries
In5 <- In5 %>% group_by(Date,PartNumberIn) %>% summarize(QuantityIn = sum(QuantityIn))
Out5 <- Out5 %>% group_by(Date,PartNumberOut) %>% summarize(QuantityOut = sum(QuantityOut))

#Merging In and Out DFs: merge by date, but keep all values that don't have a match.
InOut5 <- merge(In5,Out5,by = "Date",all.x = TRUE,all.y = TRUE)
#Fill in the NA values. We cycle through each value in the PN columns and if it is NA then we fill it with the PN.
for(i in 1:nrow(InOut5)){
  if(is.na(InOut5[i,2]) == TRUE){
    InOut5[i,2] = "C300000560-J"
  }
}

for(i in 1:nrow(InOut5)){
  if(is.na(InOut5[i,4]) == TRUE){
    InOut5[i,4] = "C400000560-J"
  }
}
#Next, we fill the rest of the NA values with 0 since those are all that remain 
InOut5[is.na(InOut5)] <- 0
#Next, we add a column containing the differential between In/Out and a column UpDown solely for color coding
InOut5$Differential <- InOut5$QuantityIn - InOut5$QuantityOut
InOut5$UpDown <- rep("",nrow(InOut5))
for(i in 1:nrow(InOut5)){
  if(InOut5[i,6] < 0){
    InOut5[i,7] = "Down"
  }
  else{
    InOut5[i,7] = "Up"
  }
}
#Finally, set a factor for the graph legend order
InOut5$UpDown <- factor(InOut5$UpDown,c("Up","Down"))

#New Column: Running total differential to track total InvsOut instead of by day:
InOut5$RunningDifferential <- c(InOut5$Differential[1],rep(0,nrow(InOut5)-1))
for(i in 2:nrow(InOut5)){
  InOut5$RunningDifferential[i] <- InOut5$RunningDifferential[i-1] + InOut5$Differential[i]}

OutvsProd5 <- merge(Out5,subset(test, PartNumber == "C400000560-J"), all.x = TRUE, all.y = TRUE)
for(i in 1:nrow(OutvsProd5)){
  if(is.na(OutvsProd5[i,3]) == TRUE){
    OutvsProd5[i,3] = 0
  }
}
for(i in 1:nrow(OutvsProd5)){
  if(is.na(OutvsProd5[i,5]) == TRUE){
    OutvsProd5[i,5] = 0
  }
}
colnames(OutvsProd5)[which(names(OutvsProd5) == "QuantityOut")] <- "Shipped"
colnames(OutvsProd5)[which(names(OutvsProd5) == "total")] <- "Produced"

OutvsProdmelted5 <- OutvsProd5 %>% 
  pivot_longer(OutvsProd5,cols = c(Shipped,Produced), names_to = "OutAndTotal")

# Shipping In
########################################################################################
# In6 <- dbGetQuery(con, "select * from (select d.PartNumber,r.Truck,sum(d.quantity) as quantity,r.area,CONVERT(date,r.Date) as date,
# CASE WHEN LEN(TRIM(a.PartNumber)) > 0 THEN 1 ELSE 0 END as QCR from Received r inner join ReceivedDetail d on d.Truck = r.Truck 
# left join (select PartNumber,truck from quality_inspection_truck group by PartNumber,truck) a on a.truck = r.Truck and a.PartNumber = d.PartNumber WHERE r.[Date] 
# BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'C200000565-J\' group by d.partnumber,r.Truck,Area,CONVERT(date,r.Date),a.PartNumber) A")

In6 <- read.csv("In6")
###########################################################################################


# Shipping Out
########################################################################################
# Out6 <- dbGetQuery(con, "select * from (select d.PartNumber,p.packing,sum(d.quantity) as quantity,p.area,CONVERT(date,p.Date) as date,PLNO 
# 			   from packing p 
# 	              inner join packingDetail d on p.packing = d.packing 
# 			   WHERE p.[Date] BETWEEN \'10-01-2021\' and GETDATE() and d.state = 1 AND d.PartNumber = \'C300000565-J\' 
# 			   group by d.partnumber,p.packing,Area,CONVERT(date,p.Date),PLNO) A")

Out6 <- read.csv("Out6")
#########################################################################################

#In/Out DF edit: In preparation to merge these two dataframes together, I change the PartNumber and quantity column name to add an "In" and "Out" for the entries that have the same date and end up merging. 
colnames(In6)[2] <- "PartNumberIn"
colnames(In6)[4] <- "QuantityIn"
colnames(In6)[6] <- "Date"
colnames(Out6)[2] <- "PartNumberOut"
colnames(Out6)[4] <- "QuantityOut"
colnames(Out6)[6] <- "Date"
In6 <- In6[,c("PartNumberIn","QuantityIn","Date")]
Out6 <- Out6[,c("PartNumberOut","QuantityOut","Date")]

#group together all of the days with multiple entries
In6 <- In6 %>% group_by(Date,PartNumberIn) %>% summarize(QuantityIn = sum(QuantityIn))
Out6 <- Out6 %>% group_by(Date,PartNumberOut) %>% summarize(QuantityOut = sum(QuantityOut))

#Merging In and Out DFs: merge by date, but keep all values that don't have a match.
InOut6 <- merge(In6,Out6,by = "Date",all.x = TRUE,all.y = TRUE)
#Fill in the NA values. We cycle through each value in the PN columns and if it is NA then we fill it with the PN.
for(i in 1:nrow(InOut6)){
  if(is.na(InOut6[i,2]) == TRUE){
    InOut6[i,2] = "C200000565-J"
  }
}

for(i in 1:nrow(InOut6)){
  if(is.na(InOut6[i,4]) == TRUE){
    InOut6[i,4] = "C300000565-J"
  }
}
#Next, we fill the rest of the NA values with 0 since those are all that remain 
InOut6[is.na(InOut6)] <- 0
#Next, we add a column containing the differential between In/Out and a column UpDown solely for color coding
InOut6$Differential <- InOut6$QuantityIn - InOut6$QuantityOut
InOut6$UpDown <- rep("",nrow(InOut6))
for(i in 1:nrow(InOut6)){
  if(InOut6[i,6] < 0){
    InOut6[i,7] = "Down"
  }
  else{
    InOut6[i,7] = "Up"
  }
}
#Finally, set a factor for the graph legend order
InOut6$UpDown <- factor(InOut6$UpDown,c("Up","Down"))

#New Column: Running total differential to track total InvsOut instead of by day:
InOut6$RunningDifferential <- c(InOut6$Differential[1],rep(0,nrow(InOut6)-1))
for(i in 2:nrow(InOut6)){
  InOut6$RunningDifferential[i] <- InOut6$RunningDifferential[i-1] + InOut6$Differential[i]}

OutvsProd6 <- merge(Out6,subset(test, PartNumber == "C300000565-J"), all.x = TRUE, all.y = TRUE)
for(i in 1:nrow(OutvsProd6)){
  if(is.na(OutvsProd6[i,3]) == TRUE){
    OutvsProd6[i,3] = 0
  }
}
for(i in 1:nrow(OutvsProd6)){
  if(is.na(OutvsProd6[i,5]) == TRUE){
    OutvsProd6[i,5] = 0
  }
}
colnames(OutvsProd6)[which(names(OutvsProd6) == "QuantityOut")] <- "Shipped"
colnames(OutvsProd6)[which(names(OutvsProd6) == "total")] <- "Produced"

OutvsProdmelted6 <- OutvsProd6 %>% 
  pivot_longer(OutvsProd6,cols = c(Shipped,Produced), names_to = "OutAndTotal")








ui <- fluidPage(tags$img(height = 75,width = 150, src = "GSA.png"), 
                tags$h1("GSA Dashboard"), 
                selectInput("PartNumber", label = "Part Number", 
                            choices = c("PN 1" = "InOut3","PN 2" = "InOut", "PN 3" = "InOut1","PN 4" = "InOut2","PN 5" = "InOut6", "PN 6" = "InOut5"), selected = "C100061769-E"), 
                tags$h3("Daily Production Log"), 
                tags$p("This graph tracks our total production by day. The x-axis represents the date while the y-axis represents total pieces produced for that given day." ),
                fluidRow(column(2,dateRangeInput(
                  "ProdDates",
                  label = "Select Date Range",
                  start = Sys.Date() - 7,
                  end = Sys.Date(),
                  min = NULL,
                  max = NULL,
                  format = "yyyy-mm-dd",
                  startview = "month",
                  weekstart = 0,
                  language = "en",
                  separator = " to ",
                  width = NULL,
                  autoclose = TRUE
                ))
                , column(9,plotOutput("ProdvsDemand")), column(1, tableOutput("TotalProdTable"))), tags$h3("Production vs Outbound By Day"), tags$p("This bar graph plots the total number of units that we produce and total number of units that we ship by day. The horizontal black line is our target line, it's critical that we are consistently producing and shipping over 50 pieces every day to fulfill demand."), 
                fluidRow(column(2,dateRangeInput(
                  "OutDates",
                  label = "Select Date Range",
                  start = Sys.Date() - 7,
                  end = Sys.Date(),
                  min = NULL,
                  max = NULL,
                  format = "yyyy-mm-dd",
                  startview = "month",
                  weekstart = 0,
                  language = "en",
                  separator = " to ",
                  width = NULL,
                  autoclose = TRUE
                ))
                , column(8,plotOutput("C1InOut"))), 
                tags$h3("All-time Inventory Log"), tags$p("This graph keeps track of the total differential between pieces recieved and pieces shipped. It's similar to the previous graph, but this one counts the all-time totals. The goal here is to get this blue line as close to 0 as posssible, as that would mean that we've re-worked and shipped out every singe piece that we've recieved."),
                fluidRow(column(12,plotOutput("RunningDifferential"))))



server <- function(input, output){
  
  OutvsProdReac <- reactive({
    #Return the inventory of the selected partnumber
    if(input$PartNumber == "InOut"){
      OutvsProdReac <- OutvsProdmelted
    }
    else if(input$PartNumber == "InOut1"){
      OutvsProdReac <- OutvsProdmelted1
    }
    else if(input$PartNumber == "InOut2"){
      OutvsProdReac <- OutvsProdmelted2
    }
    else if(input$PartNumber == "InOut3"){
      OutvsProdReac <- OutvsProdmelted3
    }
    else if(input$PartNumber == "InOut5"){
      OutvsProdReac <- OutvsProdmelted5
    }
    else if(input$PartNumber == "InOut6"){
      OutvsProdReac <- OutvsProdmelted6
    }
  })
  
  Inventory <- reactive({
    #Return the inventory of the selected partnumber
    if(input$PartNumber == "InOut"){
      Inventory <- subset(FullInventory, PartNumber %in% c("PT00061769-E","C100061769-E"), select = c(PartNumber,OnHand,wh))
    }
    else if(input$PartNumber == "InOut1"){
      Inventory <- subset(FullInventory, PartNumber %in% c("PT00061769-F","C200061769-F"), select = c(PartNumber,OnHand,wh))
    }
    else if(input$PartNumber == "InOut2"){
      Inventory <- subset(FullInventory, PartNumber %in% c("PT00053946-F","C200053946-F"), select = c(PartNumber,OnHand,wh))
    }
    else if(input$PartNumber == "InOut3"){
      Inventory <- subset(FullInventory, PartNumber %in% c("PT00061566-E","C100061566-E"), select = c(PartNumber,OnHand,wh))
    }
    else if(input$PartNumber == "InOut5"){
      Inventory <- subset(FullInventory, PartNumber %in% c("C300000560-J","C400000560-J"), select = c(PartNumber,OnHand,wh))
    }
    else if(input$PartNumber == "InOut6"){
      Inventory <- subset(FullInventory, PartNumber %in% c("C200000565-J","C300000565-J"), select = c(PartNumber,OnHand,wh))
    }
  }) 
  
  Production <- reactive({
    #Return the inventory of the selected partnumber
    if(input$PartNumber == "InOut"){
      Production <- subset(test, PartNumber == "C100061769-E")
    }
    else if(input$PartNumber == "InOut1"){
      Production <- subset(test, PartNumber == "C200061769-F")
    }
    else if(input$PartNumber == "InOut2"){
      Production <- subset(test, PartNumber == "C200053946-F")
    }
    else if(input$PartNumber == "InOut3"){
      Production <- subset(test, PartNumber == "C100061566-E")
    }
    else if(input$PartNumber == "InOut5"){
      Production <- subset(test, PartNumber == "C400000560-J")
    }
    else if(input$PartNumber == "InOut6"){
      Production <- subset(test, PartNumber == "C300000565-J")
    }
  }) 
  
  TitleProdvsDemand <- reactive({
    #Return the appropriate data source depending on
    #the chosen Part Number button
    if (input$PartNumber == "InOut") {
      TitleProdvsDemand <- "PN 2 Total Production By Day"
    } else if (input$PartNumber == "InOut1") {
      TitleProdvsDemand <- "PN 3 Total Production By Day"
    }
    else if (input$PartNumber == "InOut2"){
      TitleProdvsDemand <- "PN 4 Total Production By Day"
    }
    else if (input$PartNumber == "InOut3"){
      TitleProdvsDemand <- "PN 1 Total Production By Day"
    }
    else if (input$PartNumber == "InOut5"){
      TitleProdvsDemand <- "PN 6 Total Production By Day"
    }
    else if (input$PartNumber == "InOut6"){
      TitleProdvsDemand <- "PN 5 Total Production By Day"
    }
  })
  
  TitleInOut <- reactive({
    #Return the appropriate data source depending on
    #the chosen Part Number button
    if (input$PartNumber == "InOut") {
      TitleInOut <- "PN 2 Produced vs Shipped (By Day)"
    } else if (input$PartNumber == "InOut1") {
      TitleInOut <- "PN 3 Produced vs Shipped (By Day)"
    }
    else if (input$PartNumber == "InOut2"){
      TitleInOut <- "PN 4 Produced vs Shipped (By Day)"
    }
    else if (input$PartNumber == "InOut3"){
      TitleInOut <- "PN 1 Produced vs Shipped (By Day)"
    }
    else if (input$PartNumber == "InOut5"){
      TitleInOut <- "PN 6 Produced vs Shipped (By Day)"
    }
    else if (input$PartNumber == "InOut6"){
      TitleInOut <- "PN 5 Produced vs Shipped (By Day)"
    }
  })
  
  TitleRunningDifferential <- reactive({
    #Return the appropriate data source depending on
    #the chosen Part Number button
    if (input$PartNumber == "InOut") {
      TitleRunningDifferential <- "PN 2 Inventory Log"
    } else if (input$PartNumber == "InOut1") {
      TitleRunningDifferential <- "PN 3 Inventory Log"
    }
    else if (input$PartNumber == "InOut2"){
      TitleRunningDifferential <- "PN 4 Inventory Log"
    }
    else if (input$PartNumber == "InOut3"){
      TitleRunningDifferential <- "PN 1 Inventory Log"
    }
    else if (input$PartNumber == "InOut5"){
      TitleRunningDifferential <- "PN 6 Inventory Log"
    }
    else if (input$PartNumber == "InOut6"){
      TitleRunningDifferential <- "PN 5 Inventory Log"
    }
  })
  
  data <- reactive({
    #Return the appropriate data source depending on
    #the chosen Part Number button
    if (input$PartNumber == "InOut") {
      data <- rivian_demand %>% filter(PartNumber == "C100061769-E")
    } else if (input$PartNumber == "InOut1") {
      data <- rivian_demand %>% filter(PartNumber == "C200061769-F")
    }
    else if (input$PartNumber == "InOut2"){
      data <- rivian_demand %>% filter(PartNumber == "C200053946-F")
    }
    else if (input$PartNumber == "InOut3"){
      data <- rivian_demand %>% filter(PartNumber == "C100061566-E")
    }
    else if (input$PartNumber == "InOut5"){
      data <- rivian_demand %>% filter(PartNumber == "C400000560-J")
    }
    else if (input$PartNumber == "InOut6"){
      data <- rivian_demand %>% filter(PartNumber == "C300000565-J")
    }
  })
  
  output$ProdvsDemand <- renderPlot({
    ggplot(subset(Production(), Date >= as.Date(min(input$ProdDates)) & Date <= as.Date(max(input$ProdDates))), aes(as.Date(Date),total)) + geom_line(color = "blue") +
      labs(title = TitleProdvsDemand(),x = "Date",y = "Total Units") + geom_point()
  })
  
  
  output$C1InOut <- renderPlot({
    
    #pal = c("forest green","red")
    #ggplot(subset(eval(as.name(input$PartNumber)), Date >= as.Date(min(input$OutDates)) & Date <= as.Date(max(input$OutDates))),aes(Date,Differential,fill=UpDown)) + geom_bar(stat = "identity") + 
    #theme(axis.text.x = element_text(angle = 55,hjust = 1)) +
    #scale_fill_manual(values = pal,labels = rev(c("Units Received","Units Shipped"))) + 
    #labs(title = TitleInOut(),x = "Date",y = "Over/Under") + 
    #geom_hline(color = "black", yintercept = 0)
    
    ggplot(subset(OutvsProdReac(), Date >= as.Date(min(input$OutDates)) & Date <= as.Date(max(input$OutDates))),aes(Date,value, fill = OutAndTotal)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      theme(axis.text.x = element_text(angle = 65,hjust = 1)) + 
      labs(title = TitleInOut(),x = "Date",y = "Over/Under") + 
      geom_hline(color = "black", yintercept = 75) +
      geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25) + guides(fill=guide_legend(title="Legend"))
    
  })
  
  output$RunningDifferential <- renderPlot({
    ggplot(eval(as.name(input$PartNumber)),aes(as.Date(Date),RunningDifferential)) + geom_line(color = "blue") + 
      labs(title = TitleRunningDifferential(),x = "Date",y = "Total Over/Under") + geom_point()
  })
  
  #output$Inventory <- renderTable({
   # Inventory()
  #})
  
  output$TotalProdTable <- renderTable({
    data.frame(Total = sum(subset(Production(), Date >= as.Date(min(input$ProdDates)) & Date <= as.Date(max(input$ProdDates)))$total))
  })
  
  
  
  
}

shinyApp(ui = ui,server = server)