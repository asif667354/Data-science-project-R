View(head(df_Merge_ASDnVehiclenCompany))
str((df_Merge_ASDnVehiclenCompany))
getwd()
View(head(df_Merge_ASDnVehiclenCompany[,c(-0:-4,-8,-7,-23,-28,-33:-38,-51,-58)]))
df_Merge_ASDnVehiclenCompany(colnames(df_Merge_ASDnVehiclenCompany)=="LKR-ID")
new_df<-df_Merge_ASDnVehiclenCompany[,c(-0:-4,-7,-8,-23,-28,-33:-38,-51,-58)]
View(head(new_df))
summary(new_df)

plot(new_df$`Sum of Number of Tractors`,new_df$`Number of machinery`)
sub_new_df<-( subset(new_df,new_df$`Number of machinery`<200))
plot(sub_new_df$`Number of machinery`,sub_new_df$`Sum of Number of Tractors`)
plot(sub_new_df$`Fläche [ha]`,sub_new_df$`Sum of Number of Tractors`)
plot(sub_new_df$`Fläche [ha]`,sub_new_df$`Number of machinery`)
plot(sub_new_df$`LF gesamt [ha]`,sub_new_df$`Number of machinery`)
str(sub_new_df)
res<-cor(sub_new_df)
heatmap(res)
x<-round(matrix(cor(sub_new_df)), 2)

dependent<-sub_new_df[,c(1:52,54)]
indep<-sub_new_df[,53]
head(indep)
cor(indep,dependent)

hgh_cor<- cbind(dependent$`Betriebe 5-20 ha`,dependent$`Betriebe 20-50 ha`,dependent$`Betriebe 50-100 ha`,dependent$`Betriebe Viehhaltung`,dependent$`Betriebe Schweine`,dependent$`Betriebe Zuchtsauen`,dependent$Ackerbaubetriebe,dependent$`LW-Fläche 10-20 ha`,dependent$`LW-Fläche 20-50 ha`,dependent$`Betriebe 50-100 ha`,dependent$`Sum of Number of Tractors`,indep)
str(hgh_cor)
cor(hgh_cor)
x<-cor(hgh_cor)
heatmap(x)
pairs(hgh_cor)

y<-boxplot(hgh_cor$`Number of machinery`)
y

View(hgh_cor[,-12])
train.miss= apply(hgh_cor[,-12],2,function(x)sum(is.na(x)|x==""))
train.miss

str(hgh_cor)
install.packages("stringr")
install.packages("dplyr")
 library(stringr)
library(dplyr)
rename(hgh_cor,"dependent$`Betriebe 5-20 ha`"="Betriebe 5-20 ha")
colnames(hgh_cor)
names(hgh_cor)[names(hgh_cor) == "dependent$`Sum of Number of Tractors`"] <- "Sum of Number of Tractors"
names(hgh_cor)[names(hgh_cor) == "dependent$Ackerbaubetriebe"] <- "Ackerbaubetriebe"
View(hgh_cor)



 #hgh_cor1 <- hgh_cor %>% rename_all((funs(str_replace(., "dependent$`", "Dep"))))



newMod<- lm(`Number of machinery`~ hgh_cor$`Betriebe 5-20 ha`+hgh_cor$`Betriebe 20-50 ha`+hgh_cor$`Betriebe 50-100 ha`+ hgh_cor$`Betriebe Viehhaltung`+hgh_cor$`Betriebe Schweine`+hgh_cor$`Betriebe Zuchtsauen`+hgh_cor$Ackerbaubetriebe+hgh_cor$`LW-Fläche 10-20 ha`+hgh_cor$`LW-Fläche 20-50 ha`+hgh_cor$`Sum of Number of Tractors` , data = hgh_cor)
summary(newMod)

install.packages("psycho")
library(psycho)
new_std<-standardize(hgh_cor)
str(hgh_cor)
newMod_str<- lm(`Number of machinery`~ `Betriebe 5-20 ha`+`Betriebe 20-50 ha`+`Betriebe 50-100 ha`+ `Betriebe Viehhaltung`+`Betriebe Schweine`+`Betriebe Zuchtsauen`+Ackerbaubetriebe+`LW-Fläche 10-20 ha`+`LW-Fläche 20-50 ha`+`Sum of Number of Tractors` , data = new_std)
summary(newMod_str)
View(new_std)

#reading data from excel and omitting area=0
new_qry_2017<-qry_2017[qry_2017$area!=0,]
View(qry_2017)
View(new_qry_2017)
new_qry_2018<-qry_2018[qry_2018$area!=0,]
View(new_qry_2018)
#groupby area for 2017 and find sale of tractor
library(dplyr)
df_2017<-new_qry_2017%>%group_by(area)%>%summarise(aggSum=sum(number_of_units))
View(df_2017)
df_2018<-new_qry_2018%>%group_by(area)%>%summarise(aggSum=sum(number_of_units))

#company1 sales
View(Company1_2017_2018)
apply(Company1_2017_2018,2,function(x)sum(is.na(x)))
Company1_2017_2018[is.na(Company1_2017_2018)]<-0
Company1_2017_2018<-Company1_2017_2018[Company1_2017_2018$`LandKreis ID`!=0,]

#groupby lkID
company1_aggsum_LkID<-(Company1_2017_2018%>%group_by(`LandKreis ID`)%>%summarise(C1Sale2017=sum(`2017`),C1Sale2018=sum(`2018`)))
View(company1_aggsum_LkID)

#merge 2017 tractor and company1 sales
df_merge_c1_2017<-merge.data.frame(df_2017,company1_aggsum_LkID,by.x = "area",by.y = "LandKreis ID")
View(df_merge_c1_2017)

#plotting 2017 data for company1
plot(df_merge_c1_2017$aggSum,df_merge_c1_2017$C1Sale2017)
cor(df_merge_c1_2017)

#merge 2018 tractor and company1 sales
df_merge_c1_2018<-merge.data.frame(df_2018,company1_aggsum_LkID,by.x = "area",by.y = "LandKreis ID")
View(df_merge_c1_2018)

#plotting 2018 data for company1
plot(df_merge_c1_2018$aggSum,df_merge_c1_2018$C1Sale2018)
cor(df_merge_c1_2018)


#company2 sales
View(Company2_2017_2018)
(apply(Company2_2017_2018,2,function(x)sum(is.na(x)|x=="")))
Company2_2017_2018[is.na(Company2_2017_2018)]<-0
Company2_2017_2018<-Company2_2017_2018[Company2_2017_2018$`Landkreis ID`!=0,]
Company2_2017<-Company2_2017_2018[Company2_2017_2018$Jahr==2017,]
View(Company2_2017)
str(Company2_2017)
Company2_2018<-Company2_2017_2018[Company2_2017_2018$Jahr==2018,]
View(Company2_2018)

#groupby lkID company2
company2_2017_AggSale_LkID<-(Company2_2017%>%group_by(`Landkreis ID`)%>%summarise(C2Sale2017=sum(Absatz)))
View(company2_2017_AggSale_LkID)

company2_2018_AggSale_LkID<-(Company2_2018%>%group_by(`Landkreis ID`)%>%summarise(C2Sale2018=sum(Absatz)))
View(company2_2018_AggSale_LkID)

#merge 2017 tractor and company2 sales
df_merge_c2_2017<-merge.data.frame(df_2017,company2_2017_AggSale_LkID,by.x = "area",by.y = "Landkreis ID")
View(df_merge_c2_2017)

#merge 2018 tractor and company2 sales
df_merge_c2_2018<-merge.data.frame(df_2018,company2_2018_AggSale_LkID,by.x = "area",by.y = "Landkreis ID")
View(df_merge_c2_2018)

#plotting 2017 data for company2 and tractor sale
plot(df_merge_c2_2017$aggSum,df_merge_c2_2017$C2Sale2017)
cor(df_merge_c2_2017)

#plotting 2018 data for company2 and tractor sale
plot(df_merge_c2_2018$aggSum,df_merge_c2_2018$C2Sale2018)
cor(df_merge_c2_2018)

#company3 
View(Company3_2017_2018)
apply(Company3_2017_2018,2,function(x)sum(is.na(x)|x==""))
Company3_2017_2018[is.na(Company3_2017_2018)]<-0
Company3_2017_2018<-Company3_2017_2018[Company3_2017_2018$`Landkreis ID`!=0,]
Company3_2017<-Company3_2017_2018[Company3_2017_2018$Jahr==2017,]
View(Company3_2017)

Company3_2018<-Company3_2017_2018[Company3_2017_2018$Jahr==2018,]
View(Company3_2018)

#groupby lkID company3
company3_2017_AggSale_LkID<-(Company3_2017%>%group_by(`Landkreis ID`)%>%summarise(C3Sale2017=sum(Anzahl)))
View(company3_2017_AggSale_LkID)

company3_2018_AggSale_LkID<-(Company3_2018%>%group_by(`Landkreis ID`)%>%summarise(C3Sale2018=sum(Anzahl)))
View(company3_2018_AggSale_LkID)

#merge 2017 tractor and company3 sales
df_merge_c3_2017<-merge.data.frame(df_2017,company3_2017_AggSale_LkID,by.x = "area",by.y = "Landkreis ID")
View(df_merge_c3_2017)

#merge 2018 tractor and company3 sales
df_merge_c3_2018<-merge.data.frame(df_2018,company3_2018_AggSale_LkID,by.x = "area",by.y = "Landkreis ID")
View(df_merge_c3_2018)



#plotting 2017 data for company3 and tractor sale
plot(df_merge_c3_2017$aggSum,df_merge_c3_2017$C3Sale2017)
cor(df_merge_c3_2017)

#plotting 2018 data for company2 and tractor sale
plot(df_merge_c3_2018$aggSum,df_merge_c3_2018$C3Sale2018)
cor(df_merge_c3_2018)

#Company4





