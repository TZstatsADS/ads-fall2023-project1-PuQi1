install.packages("wordcloud2")
install.packages("stringr")  
install.packages('tm')
install.packages('tidytext')
install.packages('DT')
install.packages('jiebaR')
install.packages('jiebaRD')
install.packages('NLP')
install.packages('purrr')
install.packages('ggplot2')
install.packages("ggpubr")

library(tm)
library(tidytext)
library(purrr)
library(DT)
library(stringr)
library(wordcloud2)
library(dplyr)
library(jiebaRD) 
library(jiebaR)
library(NLP)
library(ggplot2)
library(ggpubr)

setwd("E:/GitHub/ads-fall2023-project1-PuQi1/data")

#combine the data of two csv
original_data=read.csv('original_hm.csv',na.strings='NA')
demo_data=read.csv('demographic.csv',na.strings='NA')

hm_data=merge(original_data,demo_data,
              by='wid',all.x=T)

#clean the data
hm_data[hm_data == ''] = NA
clear_data=na.omit(hm_data)
clear_data=clear_data %>% distinct(hm,.keep_all = TRUE)
clear_data$hm=tolower(clear_data$hm)
clear_data$hm=str_replace_all(clear_data$hm,'[[:punct:]]', '')
write.csv(clear_data,'clear_data.csv')

#remove stopwords
stopwords=read.csv('C:/Users/spq/Desktop/stop.csv',head=T)

mix=worker('mix')
seg=segment(clear_data$hm,mix)

result=removeWords(seg,stopwords[,2])
df=freq(result)

for (i in c(0:100)){
  df=filter(df, df$char!=as.character(i))
}

finaldata=df[-15678,]

#print the wordcloud of whole data
finaldata=finaldata[order(-finaldata$freq),]
finaldata=finaldata[1:100,]
wordcloud2(finaldata,shape= "circle")

#discuss different groups

#1. group by gender

data_g = as.data.frame(matrix(nrow=96362,ncol=2))
data_g[,1]=clear_data$gender
data_g[,2]=clear_data$hm

GID <- data_g$V1[!duplicated(data_g$V1)] 
GO <- matrix(NA,nrow=length(GID),ncol=2) 
for (i in 1:length(GID)) {
  a <- data_g[data_g$V1==GID[i],]
  GO[i,1] <- a[1,1]
  GO[i,2] <- paste(a[,2],collapse = ",")
}
GO_g <- as.data.frame(GO)

corpus_g=VCorpus(VectorSource(GO_g$V2))

list_g=corpus_g%>%
  tidy()%>%
  select(text)

dict_g <- tidy(corpus_g) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)

completed_g <- list_g %>%
  mutate(id = row_number()) %>%
  distinct()%>%
  unnest_tokens(stems, text)%>%
  bind_cols(dict_g) %>%
  anti_join(stopwords, by = c("dictionary" = "word"))

completed_g$dictionary<-NULL

g_1=as.data.frame(table(completed_g[completed_g$id==1,2]))
g1_1=left_join(completed_g[completed_g$id==1,],g_1,
              by='stems')

g_2=as.data.frame(table(completed_g[completed_g$id==2,2]))
g1_2=left_join(completed_g[completed_g$id==2,],g_2,
               by='stems')

gg=rbind.data.frame(g1_1,g1_2)
gg=gg[!duplicated(gg), ]
gg %>%
  bind_tf_idf(term = stems,document = id,n = Freq) -> tf_idf_gg

tf_idf_gg=tf_idf_gg[tf_idf_gg$stems!='3mth',]
tf_idf_gg=tf_idf_gg[tf_idf_gg$stems!='ï',]
tf_idf_gg=tf_idf_gg[tf_idf_gg$stems!='rs350',]

tf_idf_gg%>%
  group_by(id) %>%
  arrange(desc(tf_idf))%>%
  slice(1:10) ->ggfinal
  
p1=ggplot(ggfinal[1:10,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Male") 
p2=ggplot(ggfinal[11:20,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Female") 
pgg=ggarrange(p1,p2,ncol = 1,nrow =2,widths = c(1,1),heights = c(1,1,1))
ggsave('tf-idf for gender.png',plot=pgg,device='png',
       dpi = 300, width = 7, height = 4, units = "in")

#2.group by country

data_c = as.data.frame(matrix(nrow=96362,ncol=2))
data_c[,1]=clear_data$country
data_c[,2]=clear_data$hm

GID <- data_c$V1[!duplicated(data_c$V1)] 
GO <- matrix(NA,nrow=length(GID),ncol=2) 
for (i in 1:length(GID)) {
  a <- data_c[data_c$V1==GID[i],]
  GO[i,1] <- a[1,1]
  GO[i,2] <- paste(a[,2],collapse = ",")
}
GO_c <- as.data.frame(GO)

corpus_c=VCorpus(VectorSource(GO_c$V2))

list_c=corpus_c%>%
  tidy()%>%
  select(text)

dict_c<- tidy(corpus_c) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)

completed_c<- list_c%>%
  mutate(id = row_number()) %>%
  distinct()%>%
  unnest_tokens(stems, text)%>%
  bind_cols(dict_c) %>%
  anti_join(stopwords, by = c("dictionary" = "word"))

completed_c$dictionary<-NULL

c_1=as.data.frame(table(completed_c[completed_c$id==1,2]))
c1_1=left_join(completed_c[completed_c$id==1,],c_1,
               by='stems')

c_2=as.data.frame(table(completed_c[completed_c$id==2,2]))
c1_2=left_join(completed_c[completed_c$id==2,],c_2,
               by='stems')

cc=rbind.data.frame(c1_1,c1_2)
cc=cc[!duplicated(cc), ]
cc %>%
  bind_tf_idf(term = stems,document = id,n = Freq) -> tf_idf_cc

tf_idf_cc=tf_idf_cc[tf_idf_cc$stems!='3mth',]
tf_idf_cc=tf_idf_cc[tf_idf_cc$stems!='rs',]

tf_idf_cc%>%
  group_by(id) %>%
  arrange(desc(tf_idf))%>%
  slice(1:10) ->ccfinal

p1=ggplot(ccfinal[1:10,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("USA") 
p2=ggplot(ccfinal[11:20,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("IND") 
cgg=ggarrange(p1,p2,ncol = 1,nrow =2,widths = c(1,1),heights = c(1,1,1))
ggsave('tf-idf for country.png',plot=cgg,device='png',
       dpi = 300, width = 7, height = 4, units = "in")

#3. group by marriage

data_m = as.data.frame(matrix(nrow=96362,ncol=2))
data_m[,1]=clear_data$marital
data_m[,2]=clear_data$hm

GID <- data_m$V1[!duplicated(data_m$V1)] 
GO <- matrix(NA,nrow=length(GID),ncol=2) 
for (i in 1:length(GID)) {
  a <- data_m[data_m$V1==GID[i],]
  GO[i,1] <- a[1,1]
  GO[i,2] <- paste(a[,2],collapse = ",")
}
GO_m <- as.data.frame(GO)

corpus_m=VCorpus(VectorSource(GO_m$V2))

list_m=corpus_m%>%
  tidy()%>%
  select(text)

dict_m <- tidy(corpus_m) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)

completed_m<- list_m %>%
  mutate(id = row_number()) %>%
  distinct()%>%
  unnest_tokens(stems, text)%>%
  bind_cols(dict_m) %>%
  anti_join(stopwords, by = c("dictionary" = "word"))

completed_m$dictionary<-NULL

m_1=as.data.frame(table(completed_m[completed_m$id==1,2]))
m1_1=left_join(completed_m[completed_m$id==1,],m_1,
               by='stems')

m_2=as.data.frame(table(completed_m[completed_m$id==2,2]))
m1_2=left_join(completed_m[completed_m$id==2,],m_2,
               by='stems')

m_3=as.data.frame(table(completed_m[completed_m$id==3,2]))
m1_3=left_join(completed_m[completed_m$id==3,],m_3,
               by='stems')

m_4=as.data.frame(table(completed_m[completed_m$id==4,2]))
m1_4=left_join(completed_m[completed_m$id==4,],m_4,
               by='stems')

m_5=as.data.frame(table(completed_m[completed_m$id==5,2]))
m1_5=left_join(completed_m[completed_m$id==5,],m_5,
               by='stems')

mm=rbind.data.frame(m1_1,m1_2)
mm=rbind.data.frame(mm,m1_3)
mm=rbind.data.frame(mm,m1_4)
mm=rbind.data.frame(mm,m1_5)
mm=mm[!duplicated(mm), ]
mm %>%
  bind_tf_idf(term = stems,document = id,n = Freq) -> tf_idf_mm

tf_idf_mm=tf_idf_mm[tf_idf_mm$stems!='id',]
tf_idf_mm=tf_idf_mm[tf_idf_mm$stems!='â',]

tf_idf_mm%>%
  group_by(id) %>%
  arrange(desc(tf_idf))%>%
  slice(1:10) ->mmfinal

p1=ggplot(mmfinal[1:10,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Married") 
p2=ggplot(mmfinal[11:20,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Single") 
p3=ggplot(mmfinal[21:30,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Divorced") 
mgg=ggarrange(p1,p2,p3,ncol = 1,nrow =3,widths = c(1,1),heights = c(1,1,1))
ggsave('tf-idf for marriage.png',plot=mgg,device='png',
       dpi = 300, width = 7, height = 4, units = "in")

#4.group by whether being a parent

data_p = as.data.frame(matrix(nrow=96362,ncol=2))
data_p[,1]=clear_data$parenthood
data_p[,2]=clear_data$hm

GID <- data_p$V1[!duplicated(data_p$V1)] 
GO <- matrix(NA,nrow=length(GID),ncol=2) 
for (i in 1:length(GID)) {
  a <- data_p[data_p$V1==GID[i],]
  GO[i,1] <- a[1,1]
  GO[i,2] <- paste(a[,2],collapse = ",")
}
GO_p<- as.data.frame(GO)

corpus_p=VCorpus(VectorSource(GO_p$V2))

list_p=corpus_p%>%
  tidy()%>%
  select(text)

dict_p <- tidy(corpus_p) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)

completed_p <- list_p %>%
  mutate(id = row_number()) %>%
  distinct()%>%
  unnest_tokens(stems, text)%>%
  bind_cols(dict_g) %>%
  anti_join(stopwords, by = c("dictionary" = "word"))

completed_p$dictionary<-NULL

p_1=as.data.frame(table(completed_p[completed_p$id==1,2]))
p1_1=left_join(completed_p[completed_p$id==1,],p_1,
               by='stems')

p_2=as.data.frame(table(completed_p[completed_p$id==2,2]))
p1_2=left_join(completed_p[completed_p$id==2,],p_2,
               by='stems')

pp=rbind.data.frame(p1_1,p1_2)
pp=pp[!duplicated(pp), ]
pp %>%
  bind_tf_idf(term = stems,document = id,n = Freq) -> tf_idf_pp

tf_idf_pp=tf_idf_pp[tf_idf_pp$stems!='8th',]
tf_idf_pp=tf_idf_pp[tf_idf_pp$stems!='ï',]

tf_idf_pp%>%
  group_by(id) %>%
  arrange(desc(tf_idf))%>%
  slice(1:10) ->ppfinal

p1=ggplot(ppfinal[1:10,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Being a parent") 
p2=ggplot(ppfinal[11:20,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Not being a parent") 
pgg=ggarrange(p1,p2,ncol = 1,nrow =2,widths = c(1,1),heights = c(1,1,1))
ggsave('tf-idf for parenthoood.png',plot=pgg,device='png',
       dpi = 300, width = 7, height = 4, units = "in")

#5.group by age 

clear_data[clear_data$age<25,]$age=1
clear_data[clear_data$age<55&25<=clear_data$age,]$age=2
clear_data[55<=clear_data$age,]$age=3

data_a = as.data.frame(matrix(nrow=96362,ncol=2))
data_a[,1]=clear_data$age
data_a[,2]=clear_data$hm

GID <- data_a$V1[!duplicated(data_a$V1)] 
GO <- matrix(NA,nrow=length(GID),ncol=2) 
for (i in 1:length(GID)) {
  a <- data_a[data_a$V1==GID[i],]
  GO[i,1] <- a[1,1]
  GO[i,2] <- paste(a[,2],collapse = ",")
}
GO_a <- as.data.frame(GO)

corpus_a=VCorpus(VectorSource(GO_a$V2))

list_a=corpus_a%>%
  tidy()%>%
  select(text)

dict_a <- tidy(corpus_a) %>%
  select(text) %>%
  unnest_tokens(dictionary, text)

completed_a <- list_a %>%
  mutate(id = row_number()) %>%
  distinct()%>%
  unnest_tokens(stems, text)%>%
  bind_cols(dict_a) %>%
  anti_join(stopwords, by = c("dictionary" = "word"))

completed_a$dictionary<-NULL

a_1=as.data.frame(table(completed_a[completed_a$id==1,2]))
a1_1=left_join(completed_a[completed_a$id==1,],a_1,
               by='stems')

a_2=as.data.frame(table(completed_a[completed_a$id==2,2]))
a1_2=left_join(completed_a[completed_a$id==2,],a_2,
               by='stems')

a_3=as.data.frame(table(completed_a[completed_a$id==3,2]))
a1_3=left_join(completed_a[completed_a$id==3,],a_3,
               by='stems')

aa=rbind.data.frame(a1_1,a1_2)
aa=rbind.data.frame(aa,a1_3)
aa=aa[!duplicated(aa), ]
aa %>%
  bind_tf_idf(term = stems,document = id,n = Freq) -> tf_idf_aa

tf_idf_aa=tf_idf_aa[tf_idf_aa$stems!='iâ',]
tf_idf_aa=tf_idf_aa[tf_idf_aa$stems!='granddaughters',]
tf_idf_aa=tf_idf_aa[tf_idf_aa$stems!='grandsons',]

tf_idf_aa%>%
  group_by(id) %>%
  arrange(desc(tf_idf))%>%
  slice(1:10) ->aafinal

a1=ggplot(aafinal[1:10,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Middle aged") 
a2=ggplot(aafinal[11:20,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Elders") 
a3=ggplot(aafinal[21:30,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Youth") 
agg=ggarrange(a1,a2,a3,ncol = 1,nrow =3,widths = c(1,1),heights = c(1,1,1))
ggsave('tf-idf for age.png',plot=agg,device='png',
       dpi = 300, width = 9, height = 4, units = "in")
