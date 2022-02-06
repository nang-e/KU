library(rJava)
library(KoNLP)
library(wordcloud)
library(stringr)
library(memoise)
library(dplyr)
library(wordcloud2)

# 1. 데이터 입력 및 정리
useNIADic()
setwd('C:/Users/user/Desktop')
iu=readLines('아이유 가사.txt')
glimpse(iu)
iu=as.data.frame(iu,stringsAsFactors = F)
iu=iu[-which(iu==''),] # 빈 행 제거 
iu=as.data.frame(iu,stringsAsFactors = F)
iu%>%nrow # 10곡의 노래에 총 1213개의 텍스트 데이터(가사)가 있음을 알 수 있다.

# 2. 데이터 전처리 - 결측치, 중복 행, 의미 없는 테스트 제거
sum(is.na(iu)) # 결측치가 없는 것으로 확인되므로 따로 결측치 처리는 하지 않음
nrow(unique(iu))
iu=unique(iu) # bias 제거 위해 중복되는 가사를 제거한다. 행이 934개로 줄었음

iu_=gsub('[[:punct:][:digit:]]','',iu) # 불필요한 특수문자와 숫자 제거
iu_=gsub('[A-Za-z]','',iu_) # 영어는 제거하고 한글만 분석

# 3.형태소 분류
## 1) 음절별로 분류
step2.1=SimplePos22(iu_)
a <- unlist(step2.1) # 데이터 처리가 용이하도록 list type을 unlist함
a <- gsub('[+]',' ',a) # 분류하기 위해 +를 띄어쓰기로 바꾸기
a <- unlist(strsplit(a,' ')) # 띄어쓰기를 기준으로 분류 !
a_ <- unname(a)

## 2) 품사별로 분류
P <- a[grep('/P',a)] ; P <- unname(P) ; P <- gsub('[[:punct:][A-Za-z]','',P) # 용언 분류 + names 제거 + /P_ 제거
N <- a[grep('/N',a)] ; N <- N[-grep('/NN',N)] ; N <- unname(N) ; N <- gsub('[[:punct:][A-Za-z]','',N) # 채언 분류, 수사는 제거하기 + names 제거 + /N_ 제거
M <- a[grep('/M',a)] ; M <- unname(M) ; M <- gsub('[[:punct:][A-Za-z]','',M) # 수식언 분류 + names 제거 + /M_ 제거

EF <- unique(a[grep('/EF',a)]) # 종결어미

## 3) 분류한 품사 간단한 시각화 
P.30 <- head(sort(table(P),decreasing = T),30) # 용언 상위 30개
wordcloud2(P.30,fontFamily = '나눔바른펜',size=1)

N.30 <- head(sort(table(N),decreasing = T),30) # 체언 상위 30개
wordcloud2(N.30,fontFamily = '나눔바른펜',size=1)

M.30 <- head(sort(table(M),decreasing = T),30) # 수식언 상위 30개
wordcloud2(M.30,fontFamily = '나눔바른펜',size=1)

# Tokenizing by n-gram
library(dplyr)
library(tidytext)
library(tidyr)
iu_df=data_frame(line=1:length(iu),text=iu) # vector type인 텍스트를 데이터 프레임으로 넣기 
iu_df.t1=iu_df %>% 
  mutate_all(as.character) %>% 
  unnest_tokens(output=word,input=text)
sim=grep('[A-Za-z]',iu_df.t1$word)
sim2=iu_df.t1[-sim,]
sim2.2=sim2$word[2:nrow(sim2)]
iu.sep2=cbind(sim2$word[1:nrow(sim2)-1],sim2.2)
iu.sep2=as.data.frame(iu.sep2)
colnames(iu.sep2)=c('word1','word2')

iu_df.t2=iu_df %>% 
  mutate_all(as.character) %>% 
  unnest_tokens(output=word,input=text,token='ngrams',n=2)

iu.sep=iu_df.t2 %>% separate(word,c('word1','word2'),sep=' ')
iu.sep %>% filter(word1=='난') %>% count(word1,word2,sort=T)


for(i in 1:length(a_)){
  a_[i]=substr(a_[i],1,nchar(a_[i])-3)
}

a_df=data_frame(line=1:length(a_),text=a_)
a_df
a_df2=rbind(a_df[2:nrow(a_df),2],0)
a_df.tot=data.frame(a_df$text,a_df2) ; colnames(a_df.tot)=c('text1','text2')
a_df.tot %>% filter(text1=='나') %>% count(text1,text2,sort=T) %>% head(1)# 가장 많이 나온 어미 = ㄴ
iu.sep %>% filter(word1=='난') %>% count(word1,word2,sort=T) %>% head(1) # 너


N.30.n=names(N.30)
N.30.re=N.30.n[-c(4,8,13,14,17,23,28,29)] # 체언 중 주어로 불가능한(ex-의존 명사 등) 체언은 제거

aa=data.frame(text1=0,text2=0,n=0)
for(i in 1:length(N.30.re)){
  aa[i,]=a_df.tot %>% filter(text1==N.30.re[i]) %>% count(text1,text2,sort=T) %>% head(1) %>% rbind()
}
aa # 조사가 아닌 것이 붙은 경우는 삭제함
aa2=aa[-c(3,4,6,8,13,15,16,17,21),]
start=c('난','너의','사랑이','눈을','마음을','우리의','손을','밤도','시간이','얼굴이','당신은','속에','노래를')
start_list=list()
for(i in 1:length(start)){
  start_list[[i]]=start[i]
}


for(i in 1:length(EF)){
  EF[i]=substr(EF[i],1,nchar(EF[i])-3)
}
for(i in 1:length(EF)){
  EF[i]=substr(EF[i],nchar(EF[i]),nchar(EF[i]))
}
EF=unique(EF)
EF # 종결어미 추출


for(j in 1:length(start_list)){
  i=1
   repeat{
      a1=iu.sep2 %>% filter(word1==start_list[[j]][i]) %>% count(word1,word2,sort=T) %>% head(1)
      start_list[[j]]=c(start_list[[j]],a1$word2)
      if(substr(start_list[[j]][i+1],nchar(start_list[[j]][i+1]),nchar(start_list[[j]][i+1]))%in%EF) break
      i=i+1
    }
}

