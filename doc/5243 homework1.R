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

#数据整合
original_data=read.csv('original_hm.csv',na.strings='NA')
demo_data=read.csv('demographic.csv',na.strings='NA')

hm_data=merge(original_data,demo_data,
              by='wid',all.x=T)

#数据清洗
hm_data[hm_data == ''] = NA
clear_data=na.omit(hm_data)
clear_data=clear_data %>% distinct(hm,.keep_all = TRUE)
clear_data$hm=tolower(clear_data$hm)
clear_data$hm=str_replace_all(clear_data$hm,'[[:punct:]]', '')

#停用词
stopwords=read.csv('C:/Users/spq/Desktop/stop.csv',head=T)

mix=worker('mix')
seg=segment(clear_data$hm,mix)

result=removeWords(seg,stopwords[,2])
df=freq(result)

for (i in c(0:100)){
  df=filter(df, df$char!=as.character(i))
}

finaldata=df[-15678,]

#画出总体词云
wordcloud2(finaldata[50,],shape= "circle")


#按照性别分类
data_g = as.data.frame(matrix(nrow=96362,ncol=2))
data_g[,1]=clear_data$gender
data_g[,2]=clear_data$hm

GID <- data_g$V1[!duplicated(data_g$V1)] #data第一列去重#
GO <- matrix(NA,nrow=length(GID),ncol=2) #提前算好合并成的数据是几行几列，length(GID)行，2列#
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

g_3=as.data.frame(table(completed_g[completed_g$id==3,2]))
g1_3=left_join(completed_g[completed_g$id==3,],g_3,
               by='stems')

gg=rbind.data.frame(g1_1,g1_2)
gg=rbind.data.frame(gg,g1_3)
gg=gg[!duplicated(gg), ]
gg %>%
  bind_tf_idf(term = stems,document = id,n = Freq) -> tf_idf_gg

tf_idf_gg%>%
  group_by(id) %>%
  arrange(desc(tf_idf))%>%
  slice(1:10) ->ggfinal
  

#按照国籍分类
#按照婚否
data_m = as.data.frame(matrix(nrow=96362,ncol=2))
data_m[,1]=clear_data$marital
data_m[,2]=clear_data$hm

GID <- data_m$V1[!duplicated(data_m$V1)] #data第一列去重#
GO <- matrix(NA,nrow=length(GID),ncol=2) #提前算好合并成的数据是几行几列，length(GID)行，2列#
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

tf_idf_mm%>%
  group_by(id) %>%
  arrange(desc(tf_idf))%>%
  slice(1:10) ->mmfinal

p1=ggplot(mmfinal[1:10,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Married") 
p2=ggplot(mmfinal[11:20,],aes(x = stems,y=tf_idf))+geom_point()+
  ggtitle("Single") 
ggarrange(p1,p2,ncol = 2,nrow =1,widths = c(1,1),heights = c(1,1,1))
#按照父母

#按照年龄