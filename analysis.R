setwd('c:/Rtest/project')
library(readxl)
seoul <- read_excel('seoul.xlsx')
library(dplyr)
library(psych)
library(car)
library(plotly)
library(agricolae)
#우울증 척도 질문 신뢰도 검정
seoul3 <- seoul %>% select(C06,
                           C0601,
                           C0602,
                           C0603,
                           C0604,
                           C0605,
                           C0606,
                           C0607,
                           C0608,
                           C0609,
                           C0610)
alpha(seoul3)#0.69

seoul3 <- seoul3 %>%
  mutate(C0601 = 5-C0601) %>% 
  mutate(C0606 = 5-C0606)

alpha(seoul3)#0.83

#전처리
seoul2 <- seoul %>% 
  mutate(suicide = ifelse(C07==1,1,0)) %>% #죽고싶다는 생각한 적 있다 : 1, 없다 : 0
  mutate(insurance = C02) %>% #건강보험가입
  mutate(familysatis = D01) %>% #가족관계 만족도
  mutate(age = ifelse(A0141<1955,'65세 이상',
                      ifelse(A0141<1970, '50~64',
                             ifelse(A0141<1980, '40~49',
                                    ifelse(A0141<1990, '30~39', '20~29'))))) %>% #연령대
  mutate(marriage = A018) %>% #혼인 상태
  mutate(assets = YB131+YB132+YB133) %>% #자산
  mutate(income = B09+B0901+B0902+B0903+B0904) %>% #소득
  filter(income < 999999*5) %>% 
  mutate(debt = YB14) %>% #부채
  filter(debt<999999) %>% #결측치 제거
  mutate(gender = ifelse(A013==1,'남성','여성')) %>% #성별
  mutate(overcome = ifelse(C09==1,'가족',
                           ifelse(C09==2,'친구, 동료',
                                  ifelse(C09==3,'이웃',
                                         ifelse(C09==4,'심리상담 및 병원 치료',
                                                ifelse(C09==6,'혼자 해결',NA)))))) %>% #극복방법
  mutate(recog = C101) %>% #정신건강센터 인지여부
  mutate(depressed = (C06+(4-C0601)+C0602+C0603+C0604+C0605+(4-C0606)+C0607+C0608+C0609+C0610-9)*(20/11)) %>% #우울
  mutate(seperatepa = D19) %>% #부모님과 떨어져
  mutate(psyuse = ifelse(C102==3,0,1)) %>% #정신건강센터 이용여부 있음 : 1, 없음 : 0
  mutate(psyhelp = ifelse(C103==1,'전혀 도움이 안됨',
                          ifelse(C103==2, '별로 도움이 안됨',
                                 ifelse(C103==3,'보통',
                                        ifelse(C103==4,'약간 도움 됨', '매우 도움 됨'))))) %>% #정신건강센터 도움되는 정도
  mutate(incomecls = ifelse(income<1000, '1000만원 미만',
                            ifelse(income<3000,'1000~3000만원',
                                   ifelse(income<5000, '3000~5000만원',
                                          ifelse(income<7000,'5000~7000만원',
                                                 ifelse(income<10000,'7000만원~1억원','1억원 이상')))))) %>% 
  mutate(psyintent = ifelse(C104==1,0,1))#없음 : 0, 있음 : 1

#우울감을 경험한 적 있는 비율 : 23.1%
seoul_d <- filter(seoul2,depressed>16)
nrow(seoul_d)
nrow(seoul2)
(nrow(seoul_d)/nrow(seoul2))*100
prop.test(nrow(seoul_d),nrow(seoul2),0.2, alternative = 'greater')

#자살 요인 분석
##정신적
chisq.test(seoul2$suicide,seoul2$gender)#성별간 자살 차이가 있음이 통계적으로 유의하다.
seoul_lm4 <- glm(data=seoul2,suicide~factor(gender))
summary(seoul_lm4)
exp(cbind(OR=coef(seoul_lm4), confint(seoul_lm4)))

seoul_lm <- glm(data=seoul2, suicide~depressed+debt+assets+income+factor(seperatepa)+factor(insurance)+
                  familysatis+factor(age)+factor(marriage),family=binomial)
summary(seoul_lm)
vif(seoul_lm)
NROW(seoul_M)
NROW(seoul_F)

##경제적
seoul_lm <- glm(data=seoul2, suicide~debt+income+assets,family=binomial)
summary(seoul_lm)
exp(cbind(OR=coef(seoul_lm), confint(seoul_lm)))

vif(seoul_lm)
##성별,연령대에 따른
seoul4 <- filter(seoul2, suicide == 1)#죽고싶다고 생각한 적있는


#성별에 따른 죽싶생 비율
nrow(filter(seoul_M,suicide==1))
nrow(filter(seoul_F,suicide==1))
gendersui <- c(nrow(filter(seoul_M,suicide==1))/NROW(seoul_M),nrow(filter(seoul_F,suicide==1))/NROW(seoul_F))
barplot(gendersui, names = c('남성','여성'))
ggplot(data = seoul4, aes(age, fill=gender)) +
  geom_bar(position = 'dodge') +
  facet_grid(~gender)


chisq.test(seoul2$suicide,seoul2$gender)

lm1 <- glm(data=seoul2, suicide~factor(gender), family =binomial)
summary(lm1)
#연령대별 죽싶생 비율
ggplot(data = seoul4, aes(x=age)) + geom_bar()
plot_ly(seoul4,labels = ~age,type = 'pie',
       textposition = 'inside',
       textinfo = "label+percent",
       insidetextfont = list(color = 'white')) %>% layout(title=list(text = '연령대에 따른 \n죽고 싶은 생각 비율',x=0,y = 0.9))#연령대에 따른 죽싶생 원형 그래프

#depression 유의한 것들
seoul_lm2 <- lm(data=seoul2,depressed~factor(insurance)+familysatis+factor(age)+factor(marriage)+income+debt)
summary(seoul_lm2)
vif(seoul_lm2)
#depression에서 유의미한 친구들 ploting
a <- par(mfrow=c(1,3))
m <- lm(data=seoul2,depressed~A0141)
plot(x=seoul2$A0141,y=seoul2$depressed)
abline(m)
m <- lm(data=seoul2,depressed~income)
plot(x=seoul2$income,y=seoul2$depressed)
abline(m)
m <- lm(data=seoul2,depressed~familysatis)
plot(x=seoul2$familysatis,y=seoul2$depressed)
abline(m)

plot_ly(seoul2, x=~A0141, y=~familysatis, z=~depressed, color=~incomecls) %>% 
  add_markers()
plot_ly(seoul2, x=~A0141, y=~familysatis, z=~depressed, color=~income) %>% add_markers()
plot_ly(seoul2, x=~A0141, y=~familysatis, z=~depressed, color=~incomecls,
        colors = c('#F3C3A6', '#EBA17A', '#E48251', '#D84719','#B60B20', '#970044')) %>% add_markers()
#########
#dep_plot <- plot_ly(seoul2, x=~A0141, y=~familysatis, z=~depressed, color=~incomecls) %>% add_markers()
# my_df <- seoul2
# dep_lm <- lm(depressed ~A0141 + familysatis,data = my_df)
# graph_reso <- 0.05
# 
# #Setup Axis
# axis_x <- seq(min(my_df$A0141), max(my_df$A0141), by = graph_reso)
# axis_y <- seq(min(my_df$familysatis), max(my_df$familysatis), by = graph_reso)
# 
# #Sample points
# dep_lm_surface <- expand.grid(A0141 = axis_x,familysatis = axis_y,KEEP.OUT.ATTRS = F)
# dep_lm_surface$depressed <- predict.lm(dep_lm, newdata = dep_lm_surface)
# install.packages("reshape2")
# library(reshape2)
# dep_lm_surface <- acast(dep_lm_surface, A0141 ~ familysatis, value.var = "depressed")
# 
# dep_plot <- add_trace(p = dep_plot,
#                        z = dep_lm_surface,
#                        x = axis_x,
#                        y = axis_y,
#                        type = "surface")
# 
# dep_plot
######

#성별에 따른 depressed
seoul_F <- filter(seoul2, gender == '여성')
seoul_M <- filter(seoul2, gender == '남성')
tapply(seoul2$depressed, seoul2$gender, shapiro.test)#정규적이지 않다.
var.test(seoul_F$depressed, seoul_M$depressed) #등분산이다.
t.test(seoul_F$depressed,seoul_M$depressed, alternative = 'greater') #여성의 우울한 정도가 남성보다 높은것이 통계적으로 유의하다.
genderde <- c(mean(seoul_F$depressed),mean(seoul_M$depressed))
barplot(genderde, names = c('여성','남성'))#실제로 여성이 더 우울하다

#연령대별 우울차이
bartlett.test(seoul2$depressed, seoul2$age)#동질적X
seoulaov3 <- oneway.test(depressed~age, data=seoul2, var.equal = FALSE)
seoulaov3
seoulaov3 <- anova(lm(depressed~age,data = seoul2))
seoulaov3
seoulaov3 <- aov(data=seoul2, depressed~factor(age))
summary(seoulaov3)
scheffe.test(seoulaov3,"factor(age)",alpha = 0.05,console=T)
#남성, 여성의 연령대별 우울점수에 차이가 있는가
bartlett.test(seoul_F$depressed,seoul_F$age)
bartlett.test(seoul_M$depressed,seoul_M$age)
seoulaov <- aov(data=seoul_F, depressed~factor(age))
summary(seoulaov)
scheffe.test(seoulaov,"factor(age)",alpha = 0.05,console=T)
seoulaov2 <- aov(data=seoul_M, depressed~factor(age))
summary(seoulaov2)
scheffe.test(seoulaov2,"factor(age)",alpha = 0.05,console=T)
#연령대간에 우울감의 차이가 있는 것이 통계적으로 유의하다

seoulaov <- aov(data=seoul2, depressed~factor(age))
summary(seoulaov)

scheffe.test(seoulaov, "factor(age)", alpha = 0.05, console = T)

#소득에 따른 depressed
seoulaov2 <- aov(data=seoul2, depressed~factor(incomecls))
summary(seoulaov2)
scheffe.test(seoulaov2,'factor(incomecls)', alpha= 0.05, console = T)

#소득 유의한 것들
seoul_lm3 <- lm(data=seoul2,income~factor(insurance)+familysatis+factor(age)+factor(marriage)+factor(gender) )
summary(seoul_lm3)
vif(seoul_lm3)



#누구의 도움을 받았는가?
seoul4 <- filter(seoul2, suicide == 1)
ggplot(data = seoul4, aes(x=overcome, fill=age)) + geom_bar()#혼자 해결한다
#인지도에 따른 이용여부
chisq.test( seoul2$psyuse,seoul2$recog) #인지도에 따른 이용여부에 차이가 있음이 통계적으로 유의하다.
chisq.test(seoul2$overcome,seoul2$recog)#인지도에 따른 극복방법

seoul8 <- seoul2 %>% select(c('psyuse','recog'))
cor(seoul8)

seoul2 <- seoul2 %>%
  mutate(overcome2 = ifelse(overcome == '혼자 해결',1,0))#혼자 해결 : 1, 아니면 : 0
seoul_lm4 <- glm(data=seoul2,overcome2~factor(recog),family = binomial)
summary(seoul_lm4)
seoul_lm3 <- glm(data=seoul2,psyuse~factor(recog)+depressed,family = binomial)
summary(seoul_lm3)
#그러나 인지도가 이용여부,혼자극복했는지 여부에 영향을 미치는 것 같진 않다.

#성별에 따른 이용센터 이용 여부 1: 있다. 0 : 없다

chisq.test(seoul2$psyuse,seoul2$gender)

seoul_lm5 <- glm(data=seoul2, psyuse~factor(gender),family = binomial)
summary(seoul_lm5)
exp(cbind(OR=coef(seoul_lm5), confint(seoul_lm5)))

#여성이 남성보다 이용에 영향을 더 미친다.
#연령대에 따른 이용센터 이용 여부

chisq.test(seoul2$psyuse,seoul2$age)

seoul_lm6 <- glm(data=seoul2, psyuse~factor(age), family = binomial)
summary(seoul_lm6)
exp(cbind(OR=coef(seoul_lm6), confint(seoul_lm6)))

#정신건강 센터에 대한 인식(도움이 될 것 같은가?)
seoul2$incomecls <- factor(seoul2$incomecls, levels = c("1000만원 미만",
                                                        "1000~3000만원",
                                                        "3000~5000만원",
                                                        "5000~7000만원",
                                                        "7000만원~1억원",
                                                        "1억원 이상"))
ggplot(data = seoul2, aes(x=psyhelp, fill=incomecls)) + geom_bar() +
  scale_x_discrete(limits=c("전혀 도움이 안됨", "별로 도움이 안됨", "보통", "약간 도움 됨","매우 도움 됨"))

seoul2 <- seoul2 %>% 
  mutate(psyhelp2 = ifelse(psyhelp=="전혀 도움이 안됨" | psyhelp=="별로 도움이 안됨",0,1))
table(seoul2$psyhelp2)

#성별이 상담센터의 인식에 어떻게 영향을 미치는가?

chisq.test(seoul2$psyhelp2,seoul2$gender)

seoul_lm8 <- glm(data=seoul2,psyhelp2~factor(gender) ,family = binomial)
summary(seoul_lm8)
exp(cbind(OR=coef(seoul_lm8), confint(seoul_lm8)))

#연령대간에 상담센터 인식에 차이가 있는가?

chisq.test(seoul2$psyhelp2,seoul2$age)

#어떻게 영향을 미치는가

seoul_lm9 <- glm(data=seoul2,psyhelp2~factor(age) ,family = binomial)
summary(seoul_lm9)
exp(cbind(OR=coef(seoul_lm9), confint(seoul_lm9)))

#성별에 따라 상담센터 이용희망에 차이가 있는가?

chisq.test(seoul2$psyintent, seoul2$gender)

seoul_lm10 <- glm(data=seoul2,psyintent~factor(gender),family=binomial)
summary(seoul_lm10)
exp(cbind(OR=coef(seoul_lm10), confint(seoul_lm10)))

#연령대에 따라 상담센터 이용희망에 차이가 있는가?

chisq.test(seoul2$psyintent, seoul2$age)

seoul_lm11 <- glm(data=seoul2,psyintent~factor(age),family=binomial)
summary(seoul_lm11)
exp(cbind(OR=coef(seoul_lm11), confint(seoul_lm11)))

#연령대에 따라 인지여부에 차이가 있는가?
#recog2

chisq.test(seoul2$recog2, seoul2$age)

seoul_lm14 <- glm(data=seoul2,recog2~factor(age),family=binomial)
summary(seoul_lm14)
exp(cbind(OR=coef(seoul_lm14), confint(seoul_lm14)))

#recog3
chisq.test(seoul2$recog3, seoul2$age)
seoul_lm15 <- glm(data=seoul2,recog3~factor(age),family=binomial)
summary(seoul_lm15)
exp(cbind(OR=coef(seoul_lm15), confint(seoul_lm15)))

#성별에 따라 인지여부에 차이가 있는가?
table(seoul2$recog)
seoul2 <- seoul2 %>% 
  mutate(recog2 = ifelse(recog == 1, 0, 1)) %>% 
  mutate(recog3 = ifelse(recog == 1|recog==2,0,1))
#recog2

chisq.test(seoul2$recog2, seoul2$gender)
seoul_lm12 <- glm(data=seoul2,recog2~factor(gender),family=binomial)
summary(seoul_lm12)
exp(cbind(OR=coef(seoul_lm12), confint(seoul_lm12)))

#recog3
chisq.test(seoul2$recog3, seoul2$gender)
seoul_lm13 <- glm(data=seoul2,recog3~factor(gender),family=binomial)
summary(seoul_lm13)
exp(cbind(OR=coef(seoul_lm12), confint(seoul_lm12)))

#
seoul_lm16 <- lm(data=seoul2, income~)
seoul_lm17 <- lm(data=seoul2, debt~)

########################################3
getwd()
write.csv(
  seoul2,              # 파일에 저장할 데이터 프레임 또는 행렬
  file="seoul2.csv",        # 데이터를 저장할 파일명
  row.names=TRUE  # TRUE면 행 이름을 CSV 파일에 포함하여 저장한다.
)
seoul9 <- seoul2 %>% select(c('depressed','income','familysatis','A0141'))
k<- kmeans(seoul9,3)
plot(seoul9$income,seoul9$depressed,col=k$cluster)

seoul6 <- seoul2 %>% select(c('debt','income','depressed'))
seoul6
cor(seoul6)

seoul7 <- select(seoul2, c('C103','psyintent'))
alpha(seoul7)

