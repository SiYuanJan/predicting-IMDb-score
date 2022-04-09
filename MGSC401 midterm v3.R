film_dataset_2021 = read.csv("~/Desktop/MGSC401/midterm/film_dataset_2021 - dataset.csv")
attach(film_dataset_2021)
require(psych)#collinearity
library(car)
library(ggplot2)
require(methods)
require("olsrr")
require(lmtest)
require(plm)
library(splines)
library(boot)


###Data Exploration

#numerical predictors
hist(title_year) #right skewed
plot(title_year,imdb_score)

hist(actor_1_facebook_likes,breaks = 200)#left skewed
plot(actor_1_facebook_likes,imdb_score)

hist(actor_2_facebook_likes,breaks = 200)#left skewed
plot(actor_2_facebook_likes,imdb_score)

hist(actor_3_facebook_likes,breaks = 200)#left skewed
plot(actor_3_facebook_likes,imdb_score)

hist(director_facebook_likes,breaks = 200)#left skewed
plot(director_facebook_likes,imdb_score)

hist(movie_facebook_likes_2018,breaks = 200)#left skewed
plot(movie_facebook_likes_2018,imdb_score)

hist(duration_in_mins,breaks = 50)
plot(duration_in_mins,imdb_score)

hist(aspect_ratio, breaks = 50) #left skewed
plot(aspect_ratio,imdb_score)

hist(budget_local_currency, breaks = 200) #left skewed
plot(budget_local_currency, imdb_score)

hist(number_faces_poster,breaks=20) #left skewed
plot(number_faces_poster,imdb_score)

#removing outliers after visualizing the plots
film_dataset2=film_dataset_2021[-c(2693,63,3306,364,1496,250,1277,3052),] #remove outliers
attach(film_dataset2)

#categorical predictors
film_dataset_2021$main_genre=as.factor(film_dataset_2021$main_genre)
boxplot(imdb_score~main_genre)

film_dataset2$secondary_genre=as.factor(film_dataset2$secondary_genre)
boxplot(imdb_score~secondary_genre)

film_dataset2$language=as.factor(film_dataset2$language)
boxplot(imdb_score~language)

film_dataset2$country=as.factor(film_dataset2$country)
boxplot(imdb_score~country)

film_dataset2$content_rating=as.factor(film_dataset2$content_rating)
boxplot(imdb_score~content_rating)


### Non-linearity test
mreg=lm(imdb_score~title_year+duration_in_mins+movie_facebook_likes_2018+actor_1_facebook_likes+actor_2_facebook_likes+actor_3_facebook_likes+director_facebook_likes)
summary(mreg)
plot(predict(mreg),residuals(mreg))
abline(0,0,lty=2,col="red")
residualPlots(mreg) 


###regressions

#---director fb like
dir_like1=lm(imdb_score~director_facebook_likes)
dir_like2=lm(imdb_score~poly(director_facebook_likes,2))
dir_like3=lm(imdb_score~poly(director_facebook_likes,3))
dir_like4=lm(imdb_score~poly(director_facebook_likes,4))
dir_like5=lm(imdb_score~poly(director_facebook_likes,5))
anova(dir_like1,dir_like2,dir_like3,dir_like4) #d=3,4

plot=ggplot(film_dataset2,aes(y=imdb_score,x=director_facebook_likes))
scatter=geom_point()
line_poly_dirlike1=geom_smooth(method="lm",formula = y~x)
line_poly_dirlike4=geom_smooth(method="lm",formula = y~poly(x,4))
plot+scatter+line_poly_dirlike1
plot+scatter+line_poly_dirlike4

#Non linearity of director fb like
dir_like1=lm(imdb_score~director_facebook_likes)
plot(predict(dir_like1),residuals(dir_like1))
abline(0,0,lty=2)
residualPlots(dir_like1) #p>0.05, passed non-linearity test

#heteroskedasticity test
ncvTest(dir_like1) #p<0.05 => hetero
coeftest(dir_like1,vcov=vcovHC(dir_like1,type="HC1")) #pvalue <2e-16 -> < 2.2e-16 


###---actor_1 fb like 
act_like1=lm(imdb_score~actor_1_facebook_likes)
act_like2=lm(imdb_score~poly(actor_1_facebook_likes,2))
act_like3=lm(imdb_score~poly(actor_1_facebook_likes,3))
act_like4=lm(imdb_score~poly(actor_1_facebook_likes,4))
act_like5=lm(imdb_score~poly(actor_1_facebook_likes,5))
anova(act_like1,act_like2,act_like3,act_like4,act_like5) #d=3

plot=ggplot(film_dataset2,aes(y=imdb_score,x=actor_1_facebook_likes))
scatter=geom_point()
line_poly_actlike1=geom_smooth(method="lm",formula = y~x)
line_poly_actlike2=geom_smooth(method="lm",formula = y~poly(x,2))
plot+scatter+line_poly_actlike1

#non linearity
plot(predict(act_like1),residuals(act_like1))
abline(0,0,lty=2)
residualPlots(act_like1) #pvalue<0.5 -> non linear

#heteroskedasticity test
ncvTest(act_like1) #p<0.05 => hetero
coeftest(act_like1,vcov=vcovHC(act_like1,type="HC1")) #pvalue 0.007->0.01 

###---actor 2 fb like
act2_like1=lm(imdb_score~actor_2_facebook_likes)
act2_like2=lm(imdb_score~poly(actor_2_facebook_likes,2))
act2_like3=lm(imdb_score~poly(actor_2_facebook_likes,3))
act2_like4=lm(imdb_score~poly(actor_2_facebook_likes,4))
act2_like5=lm(imdb_score~poly(actor_2_facebook_likes,5))
anova(act2_like1,act2_like2,act2_like3,act2_like4,act2_like5) #linear

act2_like=lm(imdb_score~actor_2_facebook_likes)
plot=ggplot(film_dataset2,aes(y=imdb_score,x=actor_2_facebook_likes))
line_poly_act2like=geom_smooth(method="lm",formula = y~x)
plot+scatter+line_poly_act2like

#non linearity
plot(predict(act2_like),residuals(act2_like))
abline(0,0,lty=2)
residualPlots(act2_like) #pvalue>0.05 -> linear

#heteroskedasticity test
ncvTest(act2_like1) #p<0.05 => hetero
coeftest(act2_like1,vcov=vcovHC(act2_like1,type="HC1")) #pvalue 1.53e-09 -> 0.005386 

###---actor 3 fb like
act3_like1=lm(imdb_score~actor_3_facebook_likes)
act3_like2=lm(imdb_score~poly(actor_3_facebook_likes,2))
act3_like3=lm(imdb_score~poly(actor_3_facebook_likes,3))
act3_like4=lm(imdb_score~poly(actor_3_facebook_likes,4))
act3_like5=lm(imdb_score~poly(actor_3_facebook_likes,5))
anova(act3_like1,act3_like2,act3_like3,act3_like4,act3_like5) #d=2

plot=ggplot(film_dataset2,aes(y=imdb_score,x=actor_3_facebook_likes))
line_poly_act3like=geom_smooth(method="lm",formula = y~x)
plot+scatter+line_poly_act3like
#non linearity
plot(predict(act3_like1),residuals(act3_like1))
abline(0,0,lty=2)
residualPlots(act3_like1) #pvalue<0.05 -> non linear

#heteroskedasticity test
ncvTest(act3_like1) #p<0.05 => hetero
coeftest(act3_like1,vcov=vcovHC(act3_like1,type="HC1")) #pvalue 5.22e-05->1.571e-05

###--- movie fb like
movie_like1=lm(imdb_score~movie_facebook_likes_2018)
movie_like2=lm(imdb_score~poly(movie_facebook_likes_2018,2))
movie_like3=lm(imdb_score~poly(movie_facebook_likes_2018,3))
movie_like4=lm(imdb_score~poly(movie_facebook_likes_2018,4))
movie_like5=lm(imdb_score~poly(movie_facebook_likes_2018,5))
anova(movie_like1,movie_like2,movie_like3,movie_like4,movie_like5) #d=2,3

plot=ggplot(film_dataset2,aes(y=imdb_score,x=movie_facebook_likes_2018))
line_movielike=geom_smooth(method="lm",formula = y~x)
plot+scatter+line_movielike
#non linearity
plot(predict(movie_like1),residuals(movie_like1))
abline(0,0,lty=2)
residualPlots(movie_like1) #pvalue<0.05 -> non linear

#heteroskedasticity test
ncvTest(movie_like1) #p<0.05 => hetero
coeftest(movie_like1,vcov=vcovHC(movie_like1,type="HC1")) #pvalue <2e-16 -> < 2.2e-16

###---title_year
tityear1=lm(imdb_score~title_year)
tityear2=lm(imdb_score~poly(title_year,2))
tityear3=lm(imdb_score~poly(title_year,3))
tityear4=lm(imdb_score~poly(title_year,4))
tityear5=lm(imdb_score~poly(title_year,5))
anova(tityear1,tityear2,tityear3, tityear4,tityear5) #d=3

plot=ggplot(film_dataset2,aes(y=imdb_score,x=title_year))
line_tityear=geom_smooth(method="lm",formula = y~x)
plot+scatter+line_tityear
#non linearity
plot(predict(tityear1),residuals(tityear1))
abline(0,0,lty=2)
residualPlots(tityear1) #pvalue<0.05 -> non linear

#heteroskedasticity test
ncvTest(tityear1) #p<0.05 => hetero
coeftest(tityear1,vcov=vcovHC(tityear1,type="HC1")) #pvalue <2.e-16 ->2.2e-16

###---duration in min
dur1=lm(imdb_score~duration_in_mins)
dur2=lm(imdb_score~poly(duration_in_mins,2))
dur3=lm(imdb_score~poly(duration_in_mins,3))
dur4=lm(imdb_score~poly(duration_in_mins,4))
dur5=lm(imdb_score~poly(duration_in_mins,5))
anova(dur1,dur2,dur3,dur4,dur5) #d=4,5

plot=ggplot(film_dataset2,aes(y=imdb_score,x=duration_in_mins))
line_dur=geom_smooth(method="lm",formula = y~x)
plot+scatter+line_dur
#non linearity
plot(predict(dur1),residuals(dur1))
abline(0,0,lty=2)
residualPlots(dur1) #pvalue<0.05 -> non linear

#heteroskedasticity test
ncvTest(dur1) #p<0.05 => hetero
coeftest(dur1,vcov=vcovHC(dur1,type="HC1")) #pvalue <2e-16 -> < 2.2e-16

#see if there is any new outliers
mreg2=lm(imdb_score~poly(title_year,3)+poly(duration_in_mins,5)+main_genre+secondary_genre+
           language+country+content_rating+poly(movie_facebook_likes_2018,4)+poly(actor_1_facebook_likes,3)+
           actor_2_facebook_likes+actor_3_facebook_likes+poly(director_facebook_likes,4),data=film_dataset2)
summary(mreg2)

outlierTest(mreg2)#numerically find outliers->row 1594,3435,1184,3776,1191,3134,3901,1681
film_dataset2=film_dataset_2021[-c(2693,63,3306,364,1496,250,1277,3052,1594,3435,1184,3776,1191,3134,3901,1681),]
attach(film_dataset2)

#Check Collinearity --- Correlation Matrix
#quantitative variables are: columns{3,4,5,12,13,14,15,17,19,21,23}
quantvars = film_dataset2 [, c(3,4,5,12,13,14,15,17,19,21,23)]
pairs.panels(quantvars)


###Dummy variables
#----dummy
film_dataset2$language=as.factor(film_dataset2$language)
film_dataset2$lan_eng=ifelse(language=="English",1,0)

film_dataset2$country=as.factor(film_dataset2$country)
film_dataset2$country_USA=ifelse(country=="USA",1,0)
film_dataset2$country_UK=ifelse(country=="UK",1,0)

film_dataset2$main_genre=as.factor(film_dataset2$main_genre)
film_dataset2$main_g_action=ifelse(main_genre=="Action",1,0)
film_dataset2$main_g_com=ifelse(main_genre=="Comedy",1,0)
film_dataset2$main_g_bio=ifelse(main_genre=="Biography",1,0)
film_dataset2$main_g_hor=ifelse(main_genre=="Horror",1,0)
film_dataset2$main_g_cri=ifelse(main_genre=="Crime",1,0)
film_dataset2$main_g_dra=ifelse(main_genre=="Drama",1,0)

film_dataset2$content_rating=as.factor(film_dataset2$content_rating)
table(content_rating)
film_dataset2$content_R=ifelse(content_rating=="R",1,0)
film_dataset2$content_PG13=ifelse(content_rating=="PG-13",1,0)

film_dataset2$secondary_genre=as.factor(film_dataset2$secondary_genre)
film_dataset2$sec_animation=ifelse(secondary_genre=="Animation",1,0)

attach(film_dataset2)


###Using for loops to check if degrees are good #splines have smaller mse compared to poly
ki_1=quantile(duration_in_mins, 0.25)
ki_2=quantile(duration_in_mins,0.5)
ki_3=quantile(duration_in_mins,0.75)

kj_1=quantile(movie_facebook_likes_2018, 0.25)
kj_2=quantile(movie_facebook_likes_2018, 0.5)
kj_3=quantile(movie_facebook_likes_2018, 0.75)

km_1=quantile(actor_3_facebook_likes, 0.25)
km_2=quantile(actor_3_facebook_likes, 0.5)
km_3=quantile(actor_3_facebook_likes, 0.75)

ko_1=quantile(director_facebook_likes, 0.25)
ko_2=quantile(director_facebook_likes, 0.5)
ko_3=quantile(director_facebook_likes, 0.75)

kq_1=quantile(title_year, 0.25)
kq_2=quantile(title_year, 0.5)
kq_3=quantile(title_year, 0.75)

mse = rep(NA,4095) 
countt=0

for (duration_d in 1:4){
  for (movie_like_d in 1:4){
    for (act3_like_d in 1:4) {
      for (dir_like_d in 1:4) {
        for (act1_d in 1:4) {
          for (tityear_d in 1:4) {
            fit1=glm(imdb_score~bs(duration_in_mins,knots=c(ki_1,ki_2,ki_3),degree=duration_d)+
                       bs(movie_facebook_likes_2018,knots=c(kj_1,kj_2,kj_3),degree=movie_like_d)+
                       bs(actor_3_facebook_likes,knots=c(km_1,km_2,km_3),degree=act3_like_d)+
                       bs(director_facebook_likes,knots=c(ko_1,ko_2,ko_3),degree=dir_like_d)+
                       poly(actor_1_facebook_likes,act1_d)+
                       bs(title_year,knots=c(kq_1,kq_2,kq_3),degree = tityear_d)+
                       lan_eng+main_g_bio+main_g_action+main_g_hor+main_g_dra+main_g_cri+
                       content_R+content_PG13+sec_animation+country_UK, data=film_dataset2)
            
            mse[countt]=cv.glm(film_dataset2,fit1,K=20)$delta[1]
            countt=countt+1
          }}}}}}
mse
which.min(mse) #4078 duration_d=4; movie_like_d=4; act3_like_d=4; dir_like_d=3; act1_d=3; tityear_d=2
#mse=0.6039

###Play around the numbers and see which degree is the most appropriate (having smallest MSE)
#---final model---
fit2=glm(imdb_score~bs(duration_in_mins,knots=c(ki_1,ki_2,ki_3),4)+
           bs(movie_facebook_likes_2018,knots=c(kj_1,kj_2,kj_3),3)+
           bs(actor_3_facebook_likes,knots=c(km_1,km_2,km_3),2)+
           bs(director_facebook_likes,knots=c(ko_1,ko_2,ko_3),4)+
           poly(actor_1_facebook_likes,3)+
           bs(title_year,knots = c(kq_1,kq_2,kq_3),3)+
           lan_eng+main_g_bio+main_g_action+main_g_hor+main_g_dra+main_g_cri+content_R+content_PG13+sec_animation+country_UK,
         data=film_dataset2)
mse2=cv.glm(film_dataset2,fit2,K=50)$delta[1]

mse2


coeftest(fit2,vcov=vcovHC(fit2,type="HC1"))






