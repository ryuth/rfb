boxscores <- list.files("oldbox/")

boxname <- c("GameDate","FirstName","LastName","POS","LW","Bonus","Bases.Taken","Outs.on.Base","Field","E","Zone","Block","Frame",
             "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB","LD","POPU",
             "BH","IFH","OUTS")

master_box <- data.frame(matrix(NA,nrow=1,ncol=length(boxname)))

colnames(master_box) <- boxname

for(i in 1:length(boxscores))
{
  box <- read.csv(paste(getwd(),"/oldbox/",boxscores[i],sep=""))
  
  colnames(box) <- boxname
  
  box <- box[,c("GameDate","FirstName","LastName","POS","LW","Bonus","Bases.Taken","Outs.on.Base","Field","E","Zone","Block","Frame",
                "PA","AB","R","H","X1B","X2B","X3B","HR","RBI","SAC","SF","HBP","BB","K","SB","CS","GIDP","HFC","GB","FB","LD","POPU",
                "BH","IFH","OUTS")]
  master_box <- rbind(master_box,box)
}

master_box$PlayerName <- paste(master_box$FirstName,master_box$LastName,sep=" ")

write.csv(master_box,"early_season_master_stat.csv",row.names=FALSE)


