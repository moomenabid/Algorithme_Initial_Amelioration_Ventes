### FIRST USE : 
#install.packages('Hmisc')
#install.packages("corrplot")
#install.packages('dplyr')

#infos = read.csv("C:/Users/abidm/Desktop/Projet/algo/infos_file.csv", sep=";",header=TRUE)
infos = read.csv("C:/Users/user/Desktop/Projects/Channel_flling/Algo_initial_channel_filling/infos_file.csv", sep=";",header=TRUE)
#Initialisation :
  #Country
country=as.character(infos$country[1])
  #Rules file:
rules = as.character(infos$rules.file.name[1])
  # file name :
file_name=as.character(infos$data.file.name[1])
  # Minimal number of tests/reportables to be a good input :
min_tests_pub = infos$minimal.nb.tests.pub[1]
min_tests_prv = infos$minimal.nb.tests.prv[1]
  #Atypics customers numbers to delete
nb_customers=0
for(i in 1:nrow(infos)){
  if (!is.na(infos$customers.to.delete[i])){
    nb_customers = nb_customers+1
  }
}
if (nb_customers != 0){
  to_delete = rep(NA, nb_customers)
  for (i in 1:nb_customers){
    to_delete[i] = as.character(infos$customers.to.delete[i])
  }
}
if (nb_customers == 0) {
  to_delete=c()
}
  #List of potential outputs
potential_outputs=as.character(infos$list_outputs)
  #Minimum number of customers for taking the correlations into account 
minimum_nb =  infos$Minimal.number.of.customers.for.correlations[1]
  #Studied value : "Tests" or "Reportables" ?
studied=as.character(infos$Studied.parameter[1])

# GPCH to delete
nb_gpch=0
for(i in 1:nrow(infos)){
  if (!is.na(infos$gpch.to.delete[i])){
    nb_gpch = nb_gpch+1
  }
}
if (nb_gpch != 0){
  gpch_to_delete = rep(NA, nb_gpch)
  for (i in 1:nb_gpch){
    gpch_to_delete[i] = as.character(infos$gpch.to.delete[i])
  }
}
if (nb_gpch == 0) {
  gpch_to_delete=c()
}

options(warn = -1)
library('Hmisc')
library('corrplot')
library('dplyr')

#Loading data
#file_name=paste("C:/Users/abidm/Desktop/Projet1",file_name,sep="/") #ajouté
file_name=paste("C:/Users/user/Desktop/Projects/Channel_flling/Algo_initial_channel_filling",file_name,sep="/") 
data <- read.csv(file_name, sep=";", header=TRUE)

#We keep data from 2019 (the last completed year)
data = data[which(as.integer(data$Month) >=201901 
                  & as.integer(data$Month)<=201912 ),]

#We only keep the GPCH for the customers where reportables>0

  data=data[which(!is.na(data[studied])),]
  data=data[which(data[studied] >0),]

#We delete the customers defined earlier
if (length(to_delete) != 0){
  data=data[which(!is.element(data$Customer.Num,to_delete )),]
  data=data[which(data[studied] >0),]
}
type=rep(NA, nrow(data))
type[grep("Gov",data$Industry)] = "Public"
type[grep("Prv",data$Industry)] = "Privé"
data=cbind(data,type)

write.csv(data, "data.csv")
#We distinguish public and private customers
data_prv = data[grep("Prv",data$Industry),]
data_pub = data[grep("Gov",data$Industry),]
reactifs=unique(data$Gpch)
nb_clients_prv=cbind(rep(NA,length(reactifs)),rep(NA,length(reactifs)))
for (i in 1:length(reactifs)){
  nb_clients_prv[i,1]= as.character(reactifs[i])
  nb_clients_prv[i,2]= length(unique(data_prv[which(data_prv$Gpch==as.character(reactifs[i])),1]))
}

nb_clients_pub=cbind(rep(NA,length(reactifs)),rep(NA,length(reactifs)))
for (i in 1:length(reactifs)){
  nb_clients_pub[i,1]= as.character(reactifs[i])
  nb_clients_pub[i,2]= length(unique(data_pub[which(data_pub$Gpch==as.character(reactifs[i])),1]))
}
write.csv(nb_clients_prv, "nb_clients_prv.csv")
write.csv(nb_clients_pub, "nb_clients_pub.csv")

########################################################################
############################début essai (ajouté)###############################
########################################################################

industry_type=1
if (industry_type==1){
  data=data_pub
}
if (industry_type==2){
  data=data_prv
}

clients=unique(data$Customer.Num)
mois=rep(NA, nrow(data))
indice_mois=mois
j=1
for (i in 1:(nrow(data)-1) ){
  if (data$Month[i] != data$Month[i+1]){
    mois[j]=data$Month[i]
    indice_mois[j]=i+1
    j=j+1
  }
}
mois<-mois[which(!(is.na(mois)))]
indice_mois<-indice_mois[which(!(is.na(indice_mois)))] 

#Agregation per month per customer of the reportables
if (studied=="Tests"){
  data=aggregate(Tests ~ Customer.Num + Gpch , data = data, FUN = sum)
}
if(studied=="Reportables"){
  data=aggregate(Reportables ~ Customer.Num + Gpch , data = data, FUN = sum)
}

data <-data[order(as.vector(data$Gpch)),]

reactifs= (unique(data$Gpch))

reportable = as.data.frame(matrix(data = NA, nrow = length(reactifs), 
                                  ncol = length(clients)+1))
for (i in 1:length(reactifs)){
  reportable[i,1] = as.character(reactifs[i])
}
for (i in 2:(length(clients)+1)){
  names(reportable)[i] <- as.character(clients[i-1])
}
i=1
for (r in reactifs){
  while (i< nrow(data) & data$Gpch[i] == r){
    reportable[which(reportable$V1 == r),
               which( names(reportable)== as.character(data$Customer.Num[i]))]= 
      as.integer(data[i,studied])
    i=i+1
  }
}

for (i in 1:length(reactifs)){
  rownames(reportable)[i] = as.character(reactifs[i])
}
reportable = reportable[,2:ncol(reportable)]


report = as.data.frame(t(reportable))
mat_correlations = cor(report, use ="pairwise.complete")
nb_clients_corr=rcorr(as.matrix(report, use ="pairwise.complete"))
nb_clients_corr=nb_clients_corr[["n"]]

#GPCH of output and input parameters

rules=paste("C:/Users/user/Desktop/Projects/Channel_flling/Algo_initial_channel_filling",rules,sep="/") #ajouté

TESTS = read.csv(rules,sep=";",header=TRUE)
outputs=as.vector(TESTS$output)
reactifs=as.vector(reactifs)
autres_output=unique(potential_outputs[which(!(is.element(potential_outputs,outputs)))])
inputs=rep(NA,length(autres_output))
industry=rep("All",length(autres_output))
X = cbind(autres_output,inputs,industry)
colnames(X)=c("output","input","type")
TESTS= rbind(TESTS,X)
ind_type = as.vector(TESTS$type)

TESTS2 = TESTS[which(ind_type == "All" | ind_type== -1),]
if(industry_type==1){
  TESTS2 = rbind(TESTS2,TESTS[grep("Gov:",ind_type),])
}
if(industry_type==2){
  TESTS2 = rbind(TESTS2,TESTS[grep("Prv:",ind_type),])
}
if (studied == "Reportables"){
  data_agg = aggregate(Reportables ~ Gpch , data = data, FUN = sum)
}
if (studied == "Tests"){
  data_agg = aggregate(Tests ~ Gpch , data = data, FUN = sum)
}

minimum_nb_tests=data_agg[which(data_agg[studied]>min_tests_pub),]
input_param = as.character(minimum_nb_tests$Gpch)

#h=union(TESTS2$output,TESTS2$input) ajouté
reactifs_a_tester= unique(union(TESTS2$output,TESTS2$input))
reactifs_a_tester=unique(union(reactifs_a_tester,input_param))
reactifs_a_tester=reactifs_a_tester[which(!is.element(reactifs_a_tester,gpch_to_delete))]
report2=report[,which(is.element(colnames(report), reactifs_a_tester))]
mat_corr2 = cor(report2, use ="pairwise.complete")
nb_clients_corr2=rcorr(as.matrix(report2, use ="pairwise.complete"))
nb_clients_corr2=nb_clients_corr2[["n"]]
if (industry_type==1){
  png(file = paste0("correlogramme_publics_",country,".png"), width = 6000, height = 6000)
  corrplot(mat_corr2, method="color",type="upper",na.label = " ",
           addCoef.col = "black",number.cex= 1.6,
           tl.col="black", tl.srt=45,tl.cex=2,cl.cex=5, insig = "blank",diag=FALSE)
  dev.off()
}
if (industry_type==2){
  png(file = paste0("correlogramme_prives_",country,".png"), width = 6000, height = 6000)
  corrplot(mat_corr2, method="color",type="upper",na.label = " ",
           addCoef.col = "black",number.cex= 1.6,
           tl.col="black", tl.srt=45,tl.cex=2,cl.cex=5, insig = "blank",diag=FALSE)
  dev.off()
}
test_output=minimum_nb_tests[which(is.element(minimum_nb_tests$Gpch,TESTS2$output)),]
test_output_ok=as.character(test_output$Gpch[which(test_output[studied]>min_tests_pub)])
reactifs_a_tester2=reactifs_a_tester[which((!is.element(reactifs_a_tester,TESTS2$output)))]
#l=unique(TESTS2$output)
#h=unique(reactifs_a_tester2)
reactifs_a_tester2=unique(union(reactifs_a_tester2,test_output_ok))

report2=report[,which(is.element(colnames(report), reactifs_a_tester))]
mat_corr2 = cor(report2, use ="pairwise.complete")
nb_clients_corr2=rcorr(as.matrix(report2, use ="pairwise.complete"))
nb_clients_corr2=nb_clients_corr2[["n"]]

#We don't take the correlations which are bases on less than x customers into 
#account (defined earlier)
for (i in 1:nrow(mat_corr2)){
  for (j in 1:ncol(mat_corr2)){
    if (nb_clients_corr2[i,j] <minimum_nb ){
      mat_corr2[i,j]<-NA
    }
  }
  
}

mat_corr3=mat_corr2
mat_corr2=mat_corr2[,which(is.element(colnames(mat_corr2),reactifs_a_tester2))]
nb_clients_corr2=nb_clients_corr2[,which(is.element(colnames(mat_corr3),reactifs_a_tester2))]

#data management
Top1=rep(NA,nrow(mat_corr2))
Top2=rep(NA,nrow(mat_corr2))
Top3=rep(NA,nrow(mat_corr2))
Top4=rep(NA,nrow(mat_corr2))
Top5=rep(NA,nrow(mat_corr2))
Nb_clients_input_output1=rep(NA,nrow(mat_corr2))
Nb_clients_input_output2=rep(NA,nrow(mat_corr2))
Nb_clients_input_output3=rep(NA,nrow(mat_corr2))
Nb_clients_input_output4=rep(NA,nrow(mat_corr2))
Nb_clients_input_output5=rep(NA,nrow(mat_corr2))
Top1_gpch=rep(NA,nrow(mat_corr2))
Top2_gpch=rep(NA,nrow(mat_corr2))
Top3_gpch=rep(NA,nrow(mat_corr2))
Top4_gpch=rep(NA,nrow(mat_corr2))
Top5_gpch=rep(NA,nrow(mat_corr2))
top_3=cbind(Top1,Top1_gpch,Nb_clients_input_output1,Top2,Top2_gpch,
            Nb_clients_input_output2,Top3,Top3_gpch,Nb_clients_input_output3,Top4,Top4_gpch,Nb_clients_input_output4,Top5,Top5_gpch,Nb_clients_input_output5)
rownames(top_3)=rownames(mat_corr2)
top_3=as.data.frame(top_3)

#######################################################################
#############################début {ok} ajouté#########################
#######################################################################
# 
# i=1
# ok=0
# for (k in 1:ncol(mat_corr2)){
#   if (!is.na(mat_corr2[i,k]) & mat_corr2[i,k]<1){
#     ok=ok+1
#   }
# }
# 
# #Test : if we do have a TOP1, we keep the correlation coefficient,
# #the GPCH and the number of customers (both output and input)
# if (ok>=1){
#   top_3$Top1[i] <- max(mat_corr2[i, which(mat_corr2[i,]!=1)], na.rm=TRUE)
#   top_3$Top1_gpch[i]<-colnames(mat_corr2)[which(top_3$Top1[i]==mat_corr2[i,])]
#   top_3$Nb_clients_input_output1[i] = nb_clients_corr2[i,
#                                                        which(mat_corr2[i,]==top_3$Top1[i])]
#   tmp_mat=mat_corr2
#   tmp_mat[i,which(tmp_mat[i,]==top_3$Top1[i])]=NA
# 
# }
# 
# #Test : same test if we do have a TOP2
# 
# if (ok>=2){
#   top_3$Top2[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
#   top_3$Top2_gpch[i]<-colnames(mat_corr2)[which(top_3$Top2[i]==mat_corr2[i,])]
#   top_3$Nb_clients_input_output2[i] = nb_clients_corr2[i,
#                                                        which(mat_corr2[i,]==top_3$Top2[i])]
#   tmp_mat[i,which(tmp_mat[i,]==top_3$Top2[i])]=NA
# }
# if (ok >=3){
# 
#   #Test : Same test if we do have a TOP3
# 
#   top_3$Top3[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
#   top_3$Top3_gpch[i]<-colnames(mat_corr2)[which(top_3$Top3[i]==mat_corr2[i,])]
#   top_3$Nb_clients_input_output3[i] = nb_clients_corr2[i,
#                                                        which(mat_corr2[i,]==top_3$Top3[i])]
#   tmp_mat[i,which(tmp_mat[i,]==top_3$Top3[i])]=NA
# 
# }
# 
# if (ok >=4){
# 
#   #Test : Same test if we do have a TOP3
# 
#   top_3$Top4[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
#   top_3$Top4_gpch[i]<-colnames(mat_corr2)[which(top_3$Top4[i]==mat_corr2[i,])]
#   top_3$Nb_clients_input_output4[i] = nb_clients_corr2[i,
#                                                        which(mat_corr2[i,]==top_3$Top4[i])]
#   tmp_mat[i,which(tmp_mat[i,]==top_3$Top4[i])]=NA
# }
# 
# if (ok >=5){
# 
#   #Test : Same test if we do have a TOP3
# 
#   top_3$Top5[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
#   top_3$Top5_gpch[i]<-colnames(mat_corr2)[which(top_3$Top5[i]==mat_corr2[i,])]
#   top_3$Nb_clients_input_output5[i] = nb_clients_corr2[i,
#                                                        which(mat_corr2[i,]==top_3$Top5[i])]
# }
# #Test : if we don't have any TOP1, TOP2 or TOP3 : missing value
# 
# if (ok ==0){
#   top_3$Top1[i] = NA
#   top_3$Top1_gpch[i] = NA
#   top_3$Nb_clients_input_output1[i] = NA
#   top_3$Top2[i] = NA
#   top_3$Top2_gpch[i] = NA
#   top_3$Nb_clients_input_output2[i] = NA
#   top_3$Top3[i] = NA
#   top_3$Top3_gpch[i] = NA
#   top_3$Nb_clients_input_output3[i] = NA
#   top_3$Top4[i] = NA
#   top_3$Top4_gpch[i] = NA
#   top_3$Nb_clients_input_output4[i] = NA
#   top_3$Top5[i] = NA
#   top_3$Top5_gpch[i] = NA
#   top_3$Nb_clients_input_output5[i] = NA
# }

#######################################################################
#############################fin {ok} ajouté et reprise algo ###########################
#######################################################################

for (i in 1:nrow(mat_corr2) ){
  ok=0
  for (k in 1:ncol(mat_corr2)){
    if (!is.na(mat_corr2[i,k]) & mat_corr2[i,k]<1){
      ok=ok+1
    }
  }
  
  #Test : if we do have a TOP1, we keep the correlation coefficient,
  #the GPCH and the number of customers (both output and input)
  if (ok>=1){
    top_3$Top1[i] <- max(mat_corr2[i, which(mat_corr2[i,]!=1)], na.rm=TRUE)
    top_3$Top1_gpch[i]<-colnames(mat_corr2)[which(top_3$Top1[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output1[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top1[i])]
    tmp_mat=mat_corr2
    tmp_mat[i,which(tmp_mat[i,]==top_3$Top1[i])]=NA
    
  }
  
  #Test : same test if we do have a TOP2
  
  if (ok>=2){
    top_3$Top2[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
    top_3$Top2_gpch[i]<-colnames(mat_corr2)[which(top_3$Top2[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output2[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top2[i])]
    tmp_mat[i,which(tmp_mat[i,]==top_3$Top2[i])]=NA
  }
  if (ok >=3){
    
    #Test : Same test if we do have a TOP3
    
    top_3$Top3[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
    top_3$Top3_gpch[i]<-colnames(mat_corr2)[which(top_3$Top3[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output3[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top3[i])]
    tmp_mat[i,which(tmp_mat[i,]==top_3$Top3[i])]=NA
    
  }
  
  if (ok >=4){
    
    #Test : Same test if we do have a TOP3
    
    top_3$Top4[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
    top_3$Top4_gpch[i]<-colnames(mat_corr2)[which(top_3$Top4[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output4[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top4[i])]
    tmp_mat[i,which(tmp_mat[i,]==top_3$Top4[i])]=NA
  }
  
  if (ok >=5){
    
    #Test : Same test if we do have a TOP3
    
    top_3$Top5[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
    top_3$Top5_gpch[i]<-colnames(mat_corr2)[which(top_3$Top5[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output5[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top5[i])]
  }
  #Test : if we don't have any TOP1, TOP2 or TOP3 : missing value
  
  if (ok ==0){
    top_3$Top1[i] = NA
    top_3$Top1_gpch[i] = NA
    top_3$Nb_clients_input_output1[i] = NA
    top_3$Top2[i] = NA
    top_3$Top2_gpch[i] = NA
    top_3$Nb_clients_input_output2[i] = NA
    top_3$Top3[i] = NA
    top_3$Top3_gpch[i] = NA
    top_3$Nb_clients_input_output3[i] = NA
    top_3$Top4[i] = NA
    top_3$Top4_gpch[i] = NA
    top_3$Nb_clients_input_output4[i] = NA
    top_3$Top5[i] = NA
    top_3$Top5_gpch[i] = NA
    top_3$Nb_clients_input_output5[i] = NA
  }
}


top_3_bis = top_3[which(is.element(rownames(top_3), TESTS$output)),]
#h=factor(data$Gpch, levels = reactifs) ajouté
#h1=table(factor(data$Gpch, levels = reactifs)) ajouté
Nb_clients_input=table(factor(data$Gpch, levels = reactifs))
Nb_clients_input=as.data.frame(Nb_clients_input)
Top1_nb_clients_input=rep(0,nrow(top_3_bis))
Top2_nb_clients_input=rep(0,nrow(top_3_bis))
Top3_nb_clients_input=rep(0,nrow(top_3_bis))
Top4_nb_clients_input=rep(0,nrow(top_3_bis))
Top5_nb_clients_input=rep(0,nrow(top_3_bis))
#For each TOP, we keep the number of customers for the input parameter
for (i in 1:nrow(top_3_bis)){
  if(!is.na(top_3_bis$Top1_gpch[i])){
    Top1_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                      top_3_bis$Top1_gpch[i]),2]
  }
  if(!is.na(top_3_bis$Top2_gpch[i])){
    Top2_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                      top_3_bis$Top2_gpch[i]),2]
  }
  if(!is.na(top_3_bis$Top3_gpch[i])){
    Top3_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                      top_3_bis$Top3_gpch[i]),2]
  }
  if(!is.na(top_3_bis$Top4_gpch[i])){
    Top4_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                      top_3_bis$Top4_gpch[i]),2]
  }
  if(!is.na(top_3_bis$Top5_gpch[i])){
    Top5_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                      top_3_bis$Top5_gpch[i]),2]
  }
}

TOP3 = cbind(top_3_bis[,c(1,2,3)],Top1_nb_clients_input,top_3_bis[,c(4,5,6)],
             Top2_nb_clients_input,top_3_bis[,c(7,8,9)],Top3_nb_clients_input,top_3_bis[,c(10,11,12)],Top4_nb_clients_input,top_3_bis[,c(13,14,15)],Top5_nb_clients_input)
#ratio output/input
Rapport_output_input1 = rep(NA,nrow(TOP3))
Rapport_output_input2 = rep(NA,nrow(TOP3))
Rapport_output_input3 = rep(NA,nrow(TOP3))
Rapport_output_input4 = rep(NA,nrow(TOP3))
Rapport_output_input5 = rep(NA,nrow(TOP3))
data=data[order(data$Customer.Num),]
######################################################################
############################O1I1######################################
######################################################################
# 
# out=rownames(TOP3)[1]
# inp1=TOP3$Top1_gpch[1]
# inp2=TOP3$Top2_gpch[1]
# inp3=TOP3$Top3_gpch[1]
# inp4=TOP3$Top4_gpch[1]
# inp5=TOP3$Top5_gpch[1]
# O = data[which(data$Gpch == out & data[studied]>0),]
# I1 = data[which(data$Gpch == inp1  & data[studied] >0),]
# I2 = data[which(data$Gpch == inp2  & data[studied] >0),]
# I3 = data[which(data$Gpch == inp3  & data[studied] >0),]
# I4 = data[which(data$Gpch == inp4  & data[studied] >0),]
# I5 = data[which(data$Gpch == inp5  & data[studied] >0),]
# I1=I1[which(is.element(I1$Customer.Num,O$Customer.Num)),]
# I2=I2[which(is.element(I2$Customer.Num,O$Customer.Num)),]
# I3=I3[which(is.element(I3$Customer.Num,O$Customer.Num)),]
# I4=I4[which(is.element(I4$Customer.Num,O$Customer.Num)),]
# I5=I5[which(is.element(I5$Customer.Num,O$Customer.Num)),]
# 
# O1=O[which(is.element(O$Customer.Num,I1$Customer.Num)),]
# if (!is.na(TOP3$Top1[1])){
#   Rapport_output_input1[1]=round(sum(O1[studied])/sum(I1[studied]),digits=4)
# }
# else{
#   Rapport_output_input1[1]=NA
# }
# O2=O[which(is.element(O$Customer.Num,I2$Customer.Num)),]
# if (!is.na(TOP3$Top2[1])){
#   Rapport_output_input2[1]=round(sum(O2[studied])/sum(I2[studied]),digits=4)
# }
# else{
#   Rapport_output_input2[1]=NA
# }
# O3=O[which(is.element(O$Customer.Num,I3$Customer.Num)),]
# if (!is.na(TOP3$Top3[1])){
#   Rapport_output_input3[1]=round(sum(O3[studied])/sum(I3[studied]),digits=4)
# }
# else{
#   Rapport_output_input3[1]=NA
# }
# O4=O[which(is.element(O$Customer.Num,I4$Customer.Num)),]
# if (!is.na(TOP3$Top4[1])){
#   Rapport_output_input4[1]=round(sum(O4[studied])/sum(I4[studied]),digits=4)
# }
# else{
#   Rapport_output_input4[1]=NA
# }
# O5=O[which(is.element(O$Customer.Num,I5$Customer.Num)),]
# if (!is.na(TOP3$Top5[1])){
#   Rapport_output_input5[1]=round(sum(O5[studied])/sum(I5[studied]),digits=4)
# }
# else{
#   Rapport_output_input5[1]=NA
# }

######################################################################
######################################################################
#####################################################################
for (i in 1:nrow(TOP3)){
  out=rownames(TOP3)[i]
  inp1=TOP3$Top1_gpch[i]
  inp2=TOP3$Top2_gpch[i]
  inp3=TOP3$Top3_gpch[i]
  inp4=TOP3$Top4_gpch[i]
  inp5=TOP3$Top5_gpch[i]
  O = data[which(data$Gpch == out & data[studied]>0),]
  I1 = data[which(data$Gpch == inp1  & data[studied] >0),]
  I2 = data[which(data$Gpch == inp2  & data[studied] >0),]
  I3 = data[which(data$Gpch == inp3  & data[studied] >0),]
  I4 = data[which(data$Gpch == inp4  & data[studied] >0),]
  I5 = data[which(data$Gpch == inp5  & data[studied] >0),]
  I1=I1[which(is.element(I1$Customer.Num,O$Customer.Num)),]
  I2=I2[which(is.element(I2$Customer.Num,O$Customer.Num)),]
  I3=I3[which(is.element(I3$Customer.Num,O$Customer.Num)),]
  I4=I4[which(is.element(I4$Customer.Num,O$Customer.Num)),]
  I5=I5[which(is.element(I5$Customer.Num,O$Customer.Num)),]
  
  O1=O[which(is.element(O$Customer.Num,I1$Customer.Num)),]
  if (!is.na(TOP3$Top1[i])){
    Rapport_output_input1[i]=round(sum(O1[studied])/sum(I1[studied]),digits=4)
  }
  else{
    Rapport_output_input1[i]=NA
  }
  O2=O[which(is.element(O$Customer.Num,I2$Customer.Num)),]
  if (!is.na(TOP3$Top2[i])){
    Rapport_output_input2[i]=round(sum(O2[studied])/sum(I2[studied]),digits=4)
  }
  else{
    Rapport_output_input2[i]=NA
  }
  O3=O[which(is.element(O$Customer.Num,I3$Customer.Num)),]
  if (!is.na(TOP3$Top3[i])){
    Rapport_output_input3[i]=round(sum(O3[studied])/sum(I3[studied]),digits=4)
  }
  else{
    Rapport_output_input3[i]=NA
  }
  O4=O[which(is.element(O$Customer.Num,I4$Customer.Num)),]
  if (!is.na(TOP3$Top4[i])){
    Rapport_output_input4[i]=round(sum(O4[studied])/sum(I4[studied]),digits=4)
  }
  else{
    Rapport_output_input4[i]=NA
  }
  O5=O[which(is.element(O$Customer.Num,I5$Customer.Num)),]
  if (!is.na(TOP3$Top5[i])){
    Rapport_output_input5[i]=round(sum(O5[studied])/sum(I5[studied]),digits=4)
  }
  else{
    Rapport_output_input5[i]=NA
  }
}

if (industry_type==1){
  Industry=rep("Gov", nrow(TOP3))
}
if(industry_type==2){
  Industry=rep("Prv", nrow(TOP3))
}

TOP3_bis=cbind(Industry,TOP3[,1:4],Rapport_output_input1,TOP3[,5:8], 
               Rapport_output_input2, TOP3[,9:12],Rapport_output_input3, TOP3[,13:16],Rapport_output_input4, TOP3[,17:20],Rapport_output_input5)
GPCH=rownames(TOP3_bis)
if(industry_type==1){
  TOP3_publics=cbind(GPCH,TOP3_bis)
  
}
if(industry_type==2){
  TOP3_prive=cbind(GPCH,TOP3_bis)
  
}

#Comparison with the initial rules
output=as.character(TESTS2$output)
coef=rep(NA,length(TESTS2$output))
input=as.character(TESTS2$input)
clients=rep(NA,length(TESTS2$output))
out_in=cbind(output,input,coef, clients, Industry)
out_in=as.data.frame(out_in)
out_in$coef = as.vector(out_in$coef)
out_in$clients = as.vector(out_in$clients)
mat_corr = mat_correlations
dispo = rep(NA,nrow(out_in))
#######################################################################
###############################dispo###################################
#######################################################################
# 
# 
#   dispo[1]= (is.element(out_in$input[1], rownames(mat_corr)) & 
#                is.element(out_in$output[1], rownames(mat_corr)))
#   
#   if (dispo[1]==TRUE ){
#     out_in$coef[1] = mat_corr[which( rownames(mat_corr) == out_in$output[1]),
#                               which(colnames(mat_corr) == out_in$input[1])]
#     out_in$clients[1] = nb_clients_corr[which( rownames(mat_corr) 
#                                                == out_in$output[1]),
#                                         which(colnames(mat_corr) 
#                                               == out_in$input[1])]   
#   } else{
#     out_in$coef[1]=NA
#     out_in$clients[1]=NA
#   }
# 
# 

#######################################################################
###############################dispo##################################
#######################################################################
for (i in 1:nrow(out_in)){
  dispo[i]= (is.element(out_in$input[i], rownames(mat_corr)) & 
               is.element(out_in$output[i], rownames(mat_corr)))
  
  if (dispo[i]==TRUE ){
    out_in$coef[i] = mat_corr[which( rownames(mat_corr) == out_in$output[i]),
                              which(colnames(mat_corr) == out_in$input[i])]
    out_in$clients[i] = nb_clients_corr[which( rownames(mat_corr) 
                                               == out_in$output[i]),
                                        which(colnames(mat_corr) 
                                              == out_in$input[i])]   
  }
  else{
    out_in$coef[i]=NA
    out_in$clients[i]=NA
  }
}
#ratio calculation
Rapport_output_input = rep(NA,nrow(out_in))
for (i in 1:nrow(out_in)){
  out=as.character(out_in$output[i])
  inp=as.character(out_in$input[i])
  O = data[which(data$Gpch == out & data[studied] >0),]
  I = data[which(data$Gpch == inp & data[studied] >0),]
  I=I[which(is.element(I$Customer.Num,O$Customer.Num)),]
  O=O[which(is.element(O$Customer.Num,I$Customer.Num)),]
  if(!is.na(out_in$coef[i])){
    Rapport_output_input[i]=round(sum(O[studied])/sum(I[studied]),digits=4)
  }
  else{
    Rapport_output_input[i]=NA
  }
}

if (industry_type==1){
  out_in2=cbind(out_in,Rapport_output_input)
  output_input_publics=out_in2
}

if(industry_type==2){
  out_in=cbind(out_in,Rapport_output_input)
}
########################################################################
################################fin essai (ajouté)########################################
########################################################################
# -  Algorithm - #
industry_type=2
for (industry_type in 1:2){
  if (industry_type==1){
    data=data_pub
  }
  if (industry_type==2){
    data=data_prv
  }

clients=unique(data$Customer.Num)
mois=rep(NA, nrow(data))
indice_mois=mois
j=1
for (i in 1:(nrow(data)-1) ){
  if (data$Month[i] != data$Month[i+1]){
    mois[j]=data$Month[i]
    indice_mois[j]=i+1
    j=j+1
  }
}
mois<-mois[which(!(is.na(mois)))]
indice_mois<-indice_mois[which(!(is.na(indice_mois)))] 

#Agregation per month per customer of the reportables
if (studied=="Tests"){
  data=aggregate(Tests ~ Customer.Num + Gpch , data = data, FUN = sum)
  }
if(studied=="Reportables"){
  data=aggregate(Reportables ~ Customer.Num + Gpch , data = data, FUN = sum)
}

data <-data[order(as.vector(data$Gpch)),]

reactifs= (unique(data$Gpch))

reportable = as.data.frame(matrix(data = NA, nrow = length(reactifs), 
                                  ncol = length(clients)+1))
for (i in 1:length(reactifs)){
  reportable[i,1] = as.character(reactifs[i])
}
for (i in 2:(length(clients)+1)){
  names(reportable)[i] <- as.character(clients[i-1])
}
i=1
for (r in reactifs){
  while (i< nrow(data) & data$Gpch[i] == r){
    reportable[which(reportable$V1 == r),
               which( names(reportable)== as.character(data$Customer.Num[i]))]= 
      as.integer(data[i,studied])
    i=i+1
  }
}

for (i in 1:length(reactifs)){
  rownames(reportable)[i] = as.character(reactifs[i])
}
reportable = reportable[,2:ncol(reportable)]


report = as.data.frame(t(reportable))
mat_correlations = cor(report, use ="pairwise.complete")
nb_clients_corr=rcorr(as.matrix(report, use ="pairwise.complete"))
nb_clients_corr=nb_clients_corr[["n"]]

#GPCH of output and input parameters
TESTS = read.csv(rules,sep=";",header=TRUE)
outputs=as.vector(TESTS$output)
reactifs=as.vector(reactifs)
autres_output=unique(potential_outputs[which(!(is.element(potential_outputs,outputs)))])
inputs=rep(NA,length(autres_output))
industry=rep("All",length(autres_output))
X = cbind(autres_output,inputs,industry)
colnames(X)=c("output","input","type")
TESTS= rbind(TESTS,X)
ind_type = as.vector(TESTS$type)

TESTS2 = TESTS[which(ind_type == "All" | ind_type== -1),]
if(industry_type==1){
  TESTS2 = rbind(TESTS2,TESTS[grep("Gov:",ind_type),])
}
if(industry_type==2){
  TESTS2 = rbind(TESTS2,TESTS[grep("Prv:",ind_type),])
}
if (studied == "Reportables"){
  data_agg = aggregate(Reportables ~ Gpch , data = data, FUN = sum)
}
if (studied == "Tests"){
  data_agg = aggregate(Tests ~ Gpch , data = data, FUN = sum)
}

minimum_nb_tests=data_agg[which(data_agg[studied]>min_tests_pub),]
input_param = as.character(minimum_nb_tests$Gpch)


reactifs_a_tester= unique(union(TESTS2$output,TESTS2$input))
reactifs_a_tester=unique(union(reactifs_a_tester,input_param))
reactifs_a_tester=reactifs_a_tester[which(!is.element(reactifs_a_tester,gpch_to_delete))]
report2=report[,which(is.element(colnames(report), reactifs_a_tester))]
mat_corr2 = cor(report2, use ="pairwise.complete")
nb_clients_corr2=rcorr(as.matrix(report2, use ="pairwise.complete"))
nb_clients_corr2=nb_clients_corr2[["n"]]
if (industry_type==1){
png(file = paste0("correlogramme_publics_",country,".png"), width = 6000, height = 6000)
corrplot(mat_corr2, method="color",type="upper",na.label = " ",
         addCoef.col = "black",number.cex= 1.6,
         tl.col="black", tl.srt=45,tl.cex=2,cl.cex=5, insig = "blank",diag=FALSE)
dev.off()
}
if (industry_type==2){
  png(file = paste0("correlogramme_prives_",country,".png"), width = 6000, height = 6000)
  corrplot(mat_corr2, method="color",type="upper",na.label = " ",
           addCoef.col = "black",number.cex= 1.6,
           tl.col="black", tl.srt=45,tl.cex=2,cl.cex=5, insig = "blank",diag=FALSE)
  dev.off()
}
test_output=minimum_nb_tests[which(is.element(minimum_nb_tests$Gpch,TESTS2$output)),]
test_output_ok=as.character(test_output$Gpch[which(test_output[studied]>min_tests_pub)])
reactifs_a_tester2=reactifs_a_tester[which((!is.element(reactifs_a_tester,TESTS2$output)))]
reactifs_a_tester2=unique(union(reactifs_a_tester2,test_output_ok))


report2=report[,which(is.element(colnames(report), reactifs_a_tester))]
mat_corr2 = cor(report2, use ="pairwise.complete")
nb_clients_corr2=rcorr(as.matrix(report2, use ="pairwise.complete"))
nb_clients_corr2=nb_clients_corr2[["n"]]

#We don't take the correlations which are bases on less than x customers into 
  #account (defined earlier)
for (i in 1:nrow(mat_corr2)){
  for (j in 1:ncol(mat_corr2)){
    if (nb_clients_corr2[i,j] <minimum_nb ){
      mat_corr2[i,j]<-NA
    }
  }

}

mat_corr3=mat_corr2
mat_corr2=mat_corr2[,which(is.element(colnames(mat_corr2),reactifs_a_tester2))]
nb_clients_corr2=nb_clients_corr2[,which(is.element(colnames(mat_corr3),reactifs_a_tester2))]

#data management
Top1=rep(NA,nrow(mat_corr2))
Top2=rep(NA,nrow(mat_corr2))
Top3=rep(NA,nrow(mat_corr2))
Top4=rep(NA,nrow(mat_corr2))
Top5=rep(NA,nrow(mat_corr2))
Nb_clients_input_output1=rep(NA,nrow(mat_corr2))
Nb_clients_input_output2=rep(NA,nrow(mat_corr2))
Nb_clients_input_output3=rep(NA,nrow(mat_corr2))
Nb_clients_input_output4=rep(NA,nrow(mat_corr2))
Nb_clients_input_output5=rep(NA,nrow(mat_corr2))
Top1_gpch=rep(NA,nrow(mat_corr2))
Top2_gpch=rep(NA,nrow(mat_corr2))
Top3_gpch=rep(NA,nrow(mat_corr2))
Top4_gpch=rep(NA,nrow(mat_corr2))
Top5_gpch=rep(NA,nrow(mat_corr2))
top_3=cbind(Top1,Top1_gpch,Nb_clients_input_output1,Top2,Top2_gpch,
            Nb_clients_input_output2,Top3,Top3_gpch,Nb_clients_input_output3,Top4,Top4_gpch,Nb_clients_input_output4,Top5,Top5_gpch,Nb_clients_input_output5)
rownames(top_3)=rownames(mat_corr2)
top_3=as.data.frame(top_3)

for (i in 1:nrow(mat_corr2) ){
  ok=0
  for (k in 1:ncol(mat_corr2)){
    if (!is.na(mat_corr2[i,k]) & mat_corr2[i,k]<1){
      ok=ok+1
    }
  }
  
  #Test : if we do have a TOP1, we keep the correlation coefficient,
  #the GPCH and the number of customers (both output and input)
  if (ok>=1){
    top_3$Top1[i] <- max(mat_corr2[i, which(mat_corr2[i,]!=1)], na.rm=TRUE)
    top_3$Top1_gpch[i]<-colnames(mat_corr2)[which(top_3$Top1[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output1[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top1[i])]
    tmp_mat=mat_corr2
    tmp_mat[i,which(tmp_mat[i,]==top_3$Top1[i])]=NA
    
  }
  
  #Test : same test if we do have a TOP2
  
  if (ok>=2){
    top_3$Top2[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
    top_3$Top2_gpch[i]<-colnames(mat_corr2)[which(top_3$Top2[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output2[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top2[i])]
    tmp_mat[i,which(tmp_mat[i,]==top_3$Top2[i])]=NA
  }
  if (ok >=3){
    
    #Test : Same test if we do have a TOP3
    
    top_3$Top3[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
    top_3$Top3_gpch[i]<-colnames(mat_corr2)[which(top_3$Top3[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output3[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top3[i])]
    tmp_mat[i,which(tmp_mat[i,]==top_3$Top3[i])]=NA
    
  }
  
  if (ok >=4){
    
    #Test : Same test if we do have a TOP3
    
    top_3$Top4[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
    top_3$Top4_gpch[i]<-colnames(mat_corr2)[which(top_3$Top4[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output4[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top4[i])]
    tmp_mat[i,which(tmp_mat[i,]==top_3$Top4[i])]=NA
  }
  
  if (ok >=5){
    
    #Test : Same test if we do have a TOP3
    
    top_3$Top5[i] = max(tmp_mat[i, which(tmp_mat[i,] !=1)], na.rm=TRUE)
    top_3$Top5_gpch[i]<-colnames(mat_corr2)[which(top_3$Top5[i]==mat_corr2[i,])]
    top_3$Nb_clients_input_output5[i] = nb_clients_corr2[i,
                                                         which(mat_corr2[i,]==top_3$Top5[i])]
  }
  #Test : if we don't have any TOP1, TOP2 or TOP3 : missing value
  
  if (ok ==0){
    top_3$Top1[i] = NA
    top_3$Top1_gpch[i] = NA
    top_3$Nb_clients_input_output1[i] = NA
    top_3$Top2[i] = NA
    top_3$Top2_gpch[i] = NA
    top_3$Nb_clients_input_output2[i] = NA
    top_3$Top3[i] = NA
    top_3$Top3_gpch[i] = NA
    top_3$Nb_clients_input_output3[i] = NA
    top_3$Top4[i] = NA
    top_3$Top4_gpch[i] = NA
    top_3$Nb_clients_input_output4[i] = NA
    top_3$Top5[i] = NA
    top_3$Top5_gpch[i] = NA
    top_3$Nb_clients_input_output5[i] = NA
  }
}


top_3_bis = top_3[which(is.element(rownames(top_3), TESTS$output)),]
Nb_clients_input=table(factor(data$Gpch, levels = reactifs))
Nb_clients_input=as.data.frame(Nb_clients_input)
Top1_nb_clients_input=rep(0,nrow(top_3_bis))
Top2_nb_clients_input=rep(0,nrow(top_3_bis))
Top3_nb_clients_input=rep(0,nrow(top_3_bis))
Top4_nb_clients_input=rep(0,nrow(top_3_bis))
Top5_nb_clients_input=rep(0,nrow(top_3_bis))
#For each TOP, we keep the number of customers for the input parameter
for (i in 1:nrow(top_3_bis)){
  if(!is.na(top_3_bis$Top1_gpch[i])){
    Top1_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                top_3_bis$Top1_gpch[i]),2]
  }
  if(!is.na(top_3_bis$Top2_gpch[i])){
    Top2_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                top_3_bis$Top2_gpch[i]),2]
  }
  if(!is.na(top_3_bis$Top3_gpch[i])){
    Top3_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                top_3_bis$Top3_gpch[i]),2]
  }
  if(!is.na(top_3_bis$Top4_gpch[i])){
    Top4_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                      top_3_bis$Top4_gpch[i]),2]
  }
  if(!is.na(top_3_bis$Top5_gpch[i])){
    Top5_nb_clients_input[i]=Nb_clients_input[which(Nb_clients_input[,1]==
                                                      top_3_bis$Top5_gpch[i]),2]
  }
}

TOP3 = cbind(top_3_bis[,c(1,2,3)],Top1_nb_clients_input,top_3_bis[,c(4,5,6)],
             Top2_nb_clients_input,top_3_bis[,c(7,8,9)],Top3_nb_clients_input,top_3_bis[,c(10,11,12)],Top4_nb_clients_input,top_3_bis[,c(13,14,15)],Top5_nb_clients_input)
#ratio output/input
Rapport_output_input1 = rep(NA,nrow(TOP3))
Rapport_output_input2 = rep(NA,nrow(TOP3))
Rapport_output_input3 = rep(NA,nrow(TOP3))
Rapport_output_input4 = rep(NA,nrow(TOP3))
Rapport_output_input5 = rep(NA,nrow(TOP3))
data=data[order(data$Customer.Num),]

for (i in 1:nrow(TOP3)){
  out=rownames(TOP3)[i]
  inp1=TOP3$Top1_gpch[i]
  inp2=TOP3$Top2_gpch[i]
  inp3=TOP3$Top3_gpch[i]
  inp4=TOP3$Top4_gpch[i]
  inp5=TOP3$Top5_gpch[i]
  O = data[which(data$Gpch == out & data[studied]>0),]
  I1 = data[which(data$Gpch == inp1  & data[studied] >0),]
  I2 = data[which(data$Gpch == inp2  & data[studied] >0),]
  I3 = data[which(data$Gpch == inp3  & data[studied] >0),]
  I4 = data[which(data$Gpch == inp4  & data[studied] >0),]
  I5 = data[which(data$Gpch == inp5  & data[studied] >0),]
  I1=I1[which(is.element(I1$Customer.Num,O$Customer.Num)),]
  I2=I2[which(is.element(I2$Customer.Num,O$Customer.Num)),]
  I3=I3[which(is.element(I3$Customer.Num,O$Customer.Num)),]
  I4=I4[which(is.element(I4$Customer.Num,O$Customer.Num)),]
  I5=I5[which(is.element(I5$Customer.Num,O$Customer.Num)),]

  O1=O[which(is.element(O$Customer.Num,I1$Customer.Num)),]
  if (!is.na(TOP3$Top1[i])){
    Rapport_output_input1[i]=round(sum(O1[studied])/sum(I1[studied]),digits=4)
    }
  else{
    Rapport_output_input1[i]=NA
    }
  O2=O[which(is.element(O$Customer.Num,I2$Customer.Num)),]
  if (!is.na(TOP3$Top2[i])){
    Rapport_output_input2[i]=round(sum(O2[studied])/sum(I2[studied]),digits=4)
    }
  else{
    Rapport_output_input2[i]=NA
    }
  O3=O[which(is.element(O$Customer.Num,I3$Customer.Num)),]
  if (!is.na(TOP3$Top3[i])){
    Rapport_output_input3[i]=round(sum(O3[studied])/sum(I3[studied]),digits=4)
  }
  else{
    Rapport_output_input3[i]=NA
  }
  O4=O[which(is.element(O$Customer.Num,I4$Customer.Num)),]
  if (!is.na(TOP3$Top4[i])){
    Rapport_output_input4[i]=round(sum(O4[studied])/sum(I4[studied]),digits=4)
  }
  else{
    Rapport_output_input4[i]=NA
  }
  O5=O[which(is.element(O$Customer.Num,I5$Customer.Num)),]
  if (!is.na(TOP3$Top5[i])){
    Rapport_output_input5[i]=round(sum(O5[studied])/sum(I5[studied]),digits=4)
  }
  else{
    Rapport_output_input5[i]=NA
  }
}

if (industry_type==1){
  Industry=rep("Gov", nrow(TOP3))
}
if(industry_type==2){
  Industry=rep("Prv", nrow(TOP3))
}

TOP3_bis=cbind(Industry,TOP3[,1:4],Rapport_output_input1,TOP3[,5:8], 
               Rapport_output_input2, TOP3[,9:12],Rapport_output_input3, TOP3[,13:16],Rapport_output_input4, TOP3[,17:20],Rapport_output_input5)
GPCH=rownames(TOP3_bis)
if(industry_type==1){
  TOP3_publics=cbind(GPCH,TOP3_bis)

}
if(industry_type==2){
  TOP3_prive=cbind(GPCH,TOP3_bis)

}

#Comparison with the initial rules
output=as.character(TESTS2$output)
coef=rep(NA,length(TESTS2$output))
input=as.character(TESTS2$input)
clients=rep(NA,length(TESTS2$output))
out_in=cbind(output,input,coef, clients, Industry)
out_in=as.data.frame(out_in)
out_in$coef = as.vector(out_in$coef)
out_in$clients = as.vector(out_in$clients)
mat_corr = mat_correlations
dispo = rep(NA,nrow(out_in))
for (i in 1:nrow(out_in)){
  dispo[i]= (is.element(out_in$input[i], rownames(mat_corr)) & 
               is.element(out_in$output[i], rownames(mat_corr)))
  
  if (dispo[i]==TRUE ){
    out_in$coef[i] = mat_corr[which( rownames(mat_corr) == out_in$output[i]),
                              which(colnames(mat_corr) == out_in$input[i])]
    out_in$clients[i] = nb_clients_corr[which( rownames(mat_corr) 
                                               == out_in$output[i]),
                                        which(colnames(mat_corr) 
                                               == out_in$input[i])]   
  }
  else{
    out_in$coef[i]=NA
    out_in$clients[i]=NA
  }
}
#ratio calculation
Rapport_output_input = rep(NA,nrow(out_in))
for (i in 1:nrow(out_in)){
  out=as.character(out_in$output[i])
  inp=as.character(out_in$input[i])
  O = data[which(data$Gpch == out & data[studied] >0),]
  I = data[which(data$Gpch == inp & data[studied] >0),]
  I=I[which(is.element(I$Customer.Num,O$Customer.Num)),]
  O=O[which(is.element(O$Customer.Num,I$Customer.Num)),]
  if(!is.na(out_in$coef[i])){
  Rapport_output_input[i]=round(sum(O[studied])/sum(I[studied]),digits=4)
  }
  else{
    Rapport_output_input[i]=NA
  }
}

if (industry_type==1){
  out_in2=cbind(out_in,Rapport_output_input)
  output_input_publics=out_in2
}

if(industry_type==2){
  out_in=cbind(out_in,Rapport_output_input)
}
}
TOP3_prv_pub = rbind(TOP3_prive,TOP3_publics)
out_in_prv_pub = rbind(out_in,output_input_publics)
out_in_prv_pub=out_in_prv_pub[which(!is.na(out_in_prv_pub$input)),]
out_in_prv_pub=out_in_prv_pub %>% distinct

#Creation of csv files containing the results of the algorithm
#They will be created in the repository defined previously
write.csv(TOP3_prv_pub,paste0("TOP5_",country,".csv"))
write.csv(out_in_prv_pub,paste0("output_input_",country,".csv"))

data=TOP3_prv_pub
nb_output=nrow(data)
output=nb_output
input=c("base","top1","top2","top3","top4","top5")
for (i in 1:(nrow(data)*(length(input)-1))){
  data=rbind(data, rep(NA,ncol(data)))
}
lignes = seq(nrow(data),1,-6)
for(i in lignes){
  data[i,]=data[output,]
  data[i-1,]=data[output,]
  data[i-2,]=data[output,]
  data[i-3,]=data[output,]
  data[i-4,]=data[output,]
  data[i-5,]=data[output,]
  output=output-1
}
input=rep(c("base","top1","top2","top3","top4","top5"),nb_output)
data=cbind(input,data)

regles=read.csv(paste0("output_input_",country,".csv"),header=TRUE,sep=",")
nb_clients_prv = read.csv("nb_clients_prv.csv",header=TRUE,sep=",")
nb_clients_pub = read.csv("nb_clients_pub.csv",header=TRUE,sep=",")
correlation = rep(NA,nrow(data))
opportunites = correlation
rapport = correlation
gpch = correlation
nb_clients_correlation = correlation

  if (studied=="Tests"){
    nb_tests_pub=aggregate(Tests ~ Gpch , data = data_pub, FUN = sum)
    nb_tests_prv=aggregate(Tests ~ Gpch , data = data_prv, FUN = sum)
  }
  if(studied=="Reportables"){
    nb_tests_pub=aggregate(Reportables ~ Gpch , data = data_pub, FUN = sum)
    nb_tests_prv=aggregate(Reportables ~ Gpch , data = data_prv, FUN = sum)
  }

#######################################################################
#################################last##################################

#################################fin last##################################
#######################################################################
for (i in 1:nrow(data)){
  if (data$input[i] == "base" & data$Industry[i] == "Gov"){
    if (length(which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i]) != 0)){
      opportunites[i]=nb_clients_pub$V2[which(nb_clients_pub$V1 == as.character(regles$input[which(regles$output == as.character(data$GPCH[i]))]))] - regles$clients[which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i])]
      }
    else{
      opportunites[i]=NA
    }
  }
  
  if (data$input[i] == "base" & data$Industry[i] == "Prv"){
    if (length(which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i]) != 0)){
      opportunites[i]= nb_clients_prv$V2[which(nb_clients_prv$V1 == as.character(regles$input[which(regles$output == as.character(data$GPCH[i]))]))] - regles$clients[which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i])]
    }
    else{
      opportunites[i]=NA
    }
  }
  if (data$input[i] == "base"){
    if (length(which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i]) != 0)){
      correlation[i]=regles$coef[which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i])]
      nb_clients_correlation[i]= regles$clients[which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i])]
    }
    else{
      correlation[i]=NA
      nb_clients_correlation[i]=NA
    }
    if (length(which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i]) != 0)){
      rapport[i] = regles$Rapport_output_input[which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i])]
    }
    else{
      rapport[i]=NA
    }
    if (length(which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i]) != 0)){
      gpch[i] = as.character(regles$input[which(regles$output == as.character(data$GPCH[i]) & regles$Industry == data$Industry[i])])
    } 
    else{
      gpch[i]=NA
    }
    
  }
  if (data$input[i] == "top1"){
    correlation[i]=data$Top1[i]
    opportunites[i] = data$Top1_nb_clients_input[i] - data$Nb_clients_input_output1[i]
    rapport[i] = data$Rapport_output_input1[i]
    gpch[i]=as.character(data$Top1_gpch[i])
    nb_clients_correlation[i]=data$Nb_clients_input_output1[i]
    }
  
  if (data$input[i] == "top2"){
    correlation[i]=data$Top2[i]
    opportunites[i] = data$Top2_nb_clients_input[i] - data$Nb_clients_input_output2[i]
    rapport[i] = data$Rapport_output_input2[i]
    gpch[i]=as.character(data$Top2_gpch[i])
    nb_clients_correlation[i]=data$Nb_clients_input_output2[i]
    }
  if (data$input[i] == "top3"){
    correlation[i]=data$Top3[i]
    opportunites[i] = data$Top3_nb_clients_input[i] - data$Nb_clients_input_output3[i]
    rapport[i] = data$Rapport_output_input3[i]
    gpch[i]=as.character(data$Top3_gpch[i])
    nb_clients_correlation[i]=data$Nb_clients_input_output3[i]
    }
  if (data$input[i] == "top4"){
    correlation[i]=data$Top4[i]
    opportunites[i] = data$Top4_nb_clients_input[i] - data$Nb_clients_input_output4[i]
    rapport[i] = data$Rapport_output_input4[i]
    gpch[i]=as.character(data$Top4_gpch[i])
    nb_clients_correlation[i]=data$Nb_clients_input_output4[i]
    }
  if (data$input[i] == "top5"){
    correlation[i]=data$Top5[i]
    opportunites[i] = data$Top5_nb_clients_input[i] - data$Nb_clients_input_output5[i]
    rapport[i] = data$Rapport_output_input5[i]
    gpch[i]=as.character(data$Top5_gpch[i])
    nb_clients_correlation[i]=data$Nb_clients_input_output5[i]
    
  }
}


GPCH=as.character(data$GPCH)
Industry=as.character(data$Industry)
input=input
Country=rep(country,length(GPCH))


data2 = cbind(Country,GPCH,Industry,input,gpch,correlation,opportunites,nb_clients_correlation,rapport)
data2=as.data.frame(data2)
nb_tests_input=rep(NA,nrow(data2))

for (i in 1:nrow(data2)){
  if (!is.na(data2$gpch[i]) & data2$Industry[i]=="Gov"){
    nb_tests_input[i] = nb_tests_pub[which(nb_tests_pub$Gpch == as.character(data2$gpch[i]) ),studied]
  }
  if(!is.na(data2$gpch[i]) & data2$Industry[i]=="Prv"){
    nb_tests_input[i] = nb_tests_prv[which(nb_tests_prv$Gpch == as.character(data2$gpch[i]) ),studied]
  }
}

data2=cbind(data2,nb_tests_input)
colnames(data2)=c("Country","Output","Industry","Top","Input","Correlation","Oppotunities","nombre clients corr","Rapport","tests input")

write.csv(data2,paste0("TOP5_",country,"_managed.csv"))

