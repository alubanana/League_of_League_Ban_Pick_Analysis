setwd("E:/1-xiaojing/A 清华硕/4-哥大学位/IEOR 4650 Business Analytics/4650-project")
origindata = read.csv("2020_LoL_esports_match_data_from_OraclesElixir_20201108.csv")
data = origindata[,-3]

team = data[which(data$position=="team"),]
player = data[which(data$position!="team"),]

#pick rate
player$champion = as.factor(player$champion)
library(dplyr)
champion<- group_by(player,champion)
champion_Group<- summarise(champion,count = n())
champion_Group["pick"] = champion_Group$count*1000/sum(champion_Group$count)
champion_Group<- champion_Group[order(champion_Group$pick,decreasing=T),]
par(mfrow=c(1,1))
barplot(champion_Group$pick[1:15],names.arg = champion_Group$champion[1:15],ylim = c(0,40),main = "Pick Rate 20% ~ 40%")
barplot(champion_Group$pick[16:36],names.arg = champion_Group$champion[16:36],ylim = c(0,40),main = "Pick Rate 10% ~ 20%")
barplot(champion_Group$pick[37:57],names.arg = champion_Group$champion[37:57],ylim = c(0,40),main = "Pick Rate 5% ~ 10%")

#ban rate
ban = group_by(team,ban1)
ban_Group<- summarise(ban,count = n())
colnames(ban_Group) = c("champion","ban1")
merger = merge(champion_Group,ban_Group,all.x=TRUE,sort=TRUE,by = "champion")
ban = group_by(team,ban2)
ban_Group<- summarise(ban,count = n())
colnames(ban_Group) = c("champion","ban2")
merger = merge(merger,ban_Group,all.x=TRUE,sort=TRUE,by = "champion")
ban = group_by(team,ban3)
ban_Group<- summarise(ban,count = n())
colnames(ban_Group) = c("champion","ban3")
merger = merge(merger,ban_Group,all.x=TRUE,sort=TRUE,by = "champion")
ban = group_by(team,ban4)
ban_Group<- summarise(ban,count = n())
colnames(ban_Group) = c("champion","ban4")
merger = merge(merger,ban_Group,all.x=TRUE,sort=TRUE,by = "champion")
ban = group_by(team,ban5)
ban_Group<- summarise(ban,count = n())
colnames(ban_Group) = c("champion","ban5")
merger = merge(merger,ban_Group,all.x=TRUE,sort=TRUE,by = "champion")
merger[is.na(merger)] <- 0
merger$ban = rowSums(merger[,4:8])

merger$banrate = merger$ban*1000/sum(champion_Group$count)
sum(merger$banrate)

banrate = merger[order(merger$banrate,decreasing=T),]
barplot(banrate$banrate[1:14],names.arg = banrate$champion[1:14],ylim = c(0,45),main = "ban rate 20% ~ 45%")
barplot(banrate$banrate[15:38],names.arg = banrate$champion[15:38],ylim = c(0,45),main = "ban rate 10% ~ 20%")
barplot(banrate$banrate[39:54],names.arg = banrate$champion[39:54],ylim = c(0,45),main = "ban rate 5% ~ 10%")

write.xlsx(x, file, sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE)

#pick not ban rate

write.csv(champion_Group,"E:/1-xiaojing/A 清华硕/4-哥大学位/IEOR 4650 Business Analytics/4650-project/pickrate.csv",row.names = F)
write.csv(banrate,"E:/1-xiaojing/A 清华硕/4-哥大学位/IEOR 4650 Business Analytics/4650-project/banrate.csv",row.names = F)

#descriptive
par(mfrow=c(2,2))
boxplot(team$kills~team$result,xlab = "game result",ylab = "kills",main ="result~kills")
boxplot(team$deaths~team$result,xlab = "game result",ylab = "deaths",main ="result~deaths")
boxplot(team$assists~team$result,xlab = "game result",ylab = "assists",main ="result~assists")
spineplot(as.factor(team$firstblood)~as.factor(team$result),xlab = "game result",ylab="firstblood",main ="result~firstblood")

boxplot(team$doublekills~team$result,xlab = "game result",ylab = "doublekills",main ="result~doublekills")
spineplot(as.factor(team$triplekills)~as.factor(team$result),xlab = "game result",ylab = "triplekills",main ="result~triplekills")
spineplot(as.factor(team$quadrakills)~as.factor(team$result),xlab = "game result",ylab = "quadrakills",main ="result~quadrakills")
spineplot(as.factor(team$pentakills)~as.factor(team$result),xlab = "game result",ylab = "pentakills",main ="result~pentakills")

boxplot(team$team.kpm~team$result,xlab = "game result",ylab = "team.kpm",main ="result~team.kpm")
boxplot(team$ckpm~team$result,xlab = "game result",ylab = "ckpm",main ="result~ckpm")

spineplot(as.factor(team$firstdragon)~as.factor(team$result),xlab = "game result",ylab = "firstdragon",main ="result~firstdragon")
spineplot(as.factor(team$dragons)~as.factor(team$result),xlab = "game result",ylab = "dragons",main ="result~dragons")
spineplot(as.factor(team$elementaldrakes)~as.factor(team$result),xlab = "game result",ylab = "elementaldrakes",main ="result~elementaldrakes")
spineplot(as.factor(team$elders)~as.factor(team$result),xlab = "game result",ylab = "elders",main ="result~elders")

spineplot(as.factor(team$infernals)~as.factor(team$result),xlab = "game result",ylab = "infernals",main ="result~infernals")
spineplot(as.factor(team$mountains)~as.factor(team$result),xlab = "game result",ylab = "mountains",main ="result~mountains")
spineplot(as.factor(team$clouds)~as.factor(team$result),xlab = "game result",ylab = "clouds",main ="result~clouds")
spineplot(as.factor(team$oceans)~as.factor(team$result),xlab = "game result",ylab = "oceans",main ="result~oceans")

spineplot(as.factor(team$firstherald)~as.factor(team$result),xlab = "game result",ylab = "firstherald",main ="result~firstherald")
spineplot(as.factor(team$heralds)~as.factor(team$result),xlab = "game result",ylab = "heralds",main ="result~heralds")
spineplot(as.factor(team$firstbaron)~as.factor(team$result),xlab = "game result",ylab = "firstbaron",main ="result~firstbaron")
spineplot(as.factor(team$barons)~as.factor(team$result),xlab = "game result",ylab = "barons",main ="result~barons")

spineplot(as.factor(team$firsttower)~as.factor(team$result),xlab = "game result",ylab = "firsttower",main ="result~firsttower")
spineplot(as.factor(team$towers)~as.factor(team$result),xlab = "game result",ylab = "towers",main ="result~towers")
spineplot(as.factor(team$firstmidtower)~as.factor(team$result),xlab = "game result",ylab = "firstmidtower",main ="result~firstmidtower")
spineplot(as.factor(team$firsttothreetowers)~as.factor(team$result),xlab = "game result",ylab = "firsttothreetowers",main ="result~firsttothreetowers")

boxplot(team$damagetochampions~team$result,xlab = "game result",ylab = "damagetochampions",main ="result~damagetochampions")
boxplot(team$dpm~team$result,xlab = "game result",ylab = "dpm",main ="result~dpm")
boxplot(team$damagetakenperminute~team$result,xlab = "game result",ylab = "damagetakenperminute",main ="result~damagetakenperminute")
boxplot(team$damagemitigatedperminute~team$result,xlab = "game result",ylab = "damagemitigatedperminute",main ="result~damagemitigatedperminute")

boxplot(team$wardsplaced~team$result,xlab = "game result",ylab = "wardsplaced",main ="result~wardsplaced")
boxplot(team$wpm~team$result,xlab = "game result",ylab = "wpm",main ="result~wpm")
boxplot(team$wardskilled~team$result,xlab = "game result",ylab = "wardskilled",main ="result~wardskilled")
boxplot(team$wcpm~team$result,xlab = "game result",ylab = "wcpm",main ="result~wcpm")

boxplot(team$controlwardsbought~team$result,xlab = "game result",ylab = "controlwardsbought",main ="result~controlwardsbought")
boxplot(team$visionscore~team$result,xlab = "game result",ylab = "visionscore",main ="result~visionscore")
boxplot(team$vspm~team$result,xlab = "game result",ylab = "vspm",main ="result~vspm")
boxplot(team$cspm~team$result,xlab = "game result",ylab = "cspm",main ="result~cspm")

boxplot(team$totalgold~team$result,xlab = "game result",ylab = "totalgold",main ="result~totalgold")
boxplot(team$earnedgold~team$result,xlab = "game result",ylab = "earnedgold",main ="result~earnedgold")
boxplot(team$earned.gpm~team$result,xlab = "game result",ylab = "earned gpm",main ="result~earned gpm")
boxplot(team$goldspent~team$result,xlab = "game result",ylab = "goldspent",main ="result~goldspent")

boxplot(team$minionkills~team$result,xlab = "game result",ylab = "minionkills",main ="result~minionkills")
boxplot(team$monsterkills~team$result,xlab = "game result",ylab = "monsterkills",main ="result~monsterkills")
boxplot(team$monsterkillsownjungle~team$result,xlab = "game result",ylab = "monsterkillsownjungle",main ="result~monsterkillsownjungle")
boxplot(team$monsterkillsenemyjungle~team$result,xlab = "game result",ylab = "monsterkillsenemyjungle",main ="result~monsterkillsenemyjungle")


boxplot(team$kills~team$result,xlab = "game result",ylab = "kills",main ="result~kills")
boxplot(team$deaths~team$result,xlab = "game result",ylab = "deaths",main ="result~deaths")
boxplot(team$assists~team$result,xlab = "game result",ylab = "assists",main ="result~assists")

spineplot(as.factor(team$dragons)~as.factor(team$result),xlab = "game result",ylab = "dragons",main ="result~dragons",col=cm.colors(6))
spineplot(as.factor(team$barons)~as.factor(team$result),xlab = "game result",ylab = "barons",main ="result~barons",col=cm.colors(3))
spineplot(as.factor(team$towers)~as.factor(team$result),xlab = "game result",ylab = "towers",main ="result~towers",col=cm.colors(6))
boxplot(team$earned.gpm~team$result,xlab = "game result",ylab = "earned gpm",main ="result~earned gpm")

library(ggplot2)
team$result = as.factor(team$result)
p<-ggplot(data=team, aes(x=result,y=kills))+geom_boxplot(aes(fill=result))
p+ facet_wrap(~ result, scales="free")

p<-ggplot(data=team, aes(x=result,y=deaths))+geom_boxplot(aes(fill=result))
p+ facet_wrap(~ result, scales="free")

p<-ggplot(data=team, aes(x=result,y=assists))+geom_boxplot(aes(fill=result))
p+ facet_wrap(~ result, scales="free")

p<-ggplot(data=team, aes(x=result,y=earned.gpm))+geom_boxplot(aes(fill=result))
p
