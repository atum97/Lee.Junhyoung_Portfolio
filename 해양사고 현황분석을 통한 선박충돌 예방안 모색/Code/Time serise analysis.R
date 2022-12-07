1.데이터 준비

getwd()
setwd('C:/Users/user/Desktop/해심원')
df <- read.table('arima_month.txt', fileEncoding = 'utf-8', header = T)
df
summary(df)

#시계열 데이터로 변환
library(ggplot2)
library(tseries)
library(forecast)

df$해양사고발생일 <- as.Date(df$해양사고발생일)
df <- ts(df[2], start = c(2016,1), frequency = 12)
df

autoplot(df,lwd=1.2, col='red', xlab = '년도', ylab='사건수', 
         main='년도별 해양사고 현황')+theme(axis.title = element_text(size=15),
                                   plot.title = element_text(size=20, hjust = 0.5,
                                                             face='bold'))

2. 데이터 검증

#정상성 검증
adf.test(df)          #p-value<0.05 이므로 정상성 만족

ndiffs(df)            #차분이 필요하지 않음

autoplot(decompose(df)) + theme(plot.title = element_text(size=15,
                                                          face='bold',hjust=0.5))

autoplot(df - decompose(df)$seasonal, main='계절요인 제거 그래프', lwd=1.2,col='red',ylab='사건수', xlab='년도')+ 
  theme(plot.title = element_text(size=15,face='bold',hjust=0.5))

autoplot(df, main='기존 그래프',ylab='사건수', xlab='년도',lwd=1.2,col='red')+ theme(plot.title = element_text(size=15,
                                                                                                     face='bold',hjust=0.5))

3.예측모델 생성

#Acf / PAcf 확인
Acf(df, lwd=2, main="Autocorrelation")
Pacf(df, lwd=2, main="Partial Autocorrelation")
#AR(1)/ARMA(1,0), MA(1)/ARMA(0,1), ARMA(1,1)

#최적(p,d,q)도출

auto.arima(df) #(1,0,0) 
arima(df, order = c(1,0,0))  # AIC=480.06
arima(df, order = c(0,0,1))  # AIC=483.17
arima(df, order = c(1,0,1))  # AIC=481.73
# => 실제로 AR(1)모형이 가장 적합하다는것을 확인.(AIC가장 낮음)

df.arima <- arima(df, order = c(1, 0, 0))
df.arima


4.예측모델 평가
qqnorm(df.arima$residuals, pch=21, col='black', 
       bg='gold', main='Q-Q Plot of Residuals')

qqline(df.arima$residuals, col='royalblue', lwd=2)
#직선 주변에 몰려있으므로 정규분포를 따른다고 볼 수 있음.

#자기상관이 0이라는 귀무가설을 검정
Box.test(df.arima$residuals, type = 'Ljung-Box')  
# p-value 0.05이상이므로 자기상관(잔차간의 상관관계) 0이라는 귀무가설 기각못함 
# => 데이터가 독립적으로 분포되어있다.
# 모형의 잔차를 이용하여 카이제곱검정 방법으로 검증

accuracy(df.arima)
accuracy(arima(df, order = c(0,0,1)))
accuracy(arima(df, order = c(1,0,1)))


tsdiag(df.arima)
#모두 파란선(임계치) 안에 들어있음
# => 자기 상관관계가 없음 = 규칙성이 없음 = 불규칙성
# => p-value값이 0 이상으로 분포 => 매우 양호한 시계열 모형
# => 모델이 적합하다!

library(rsq)
rsq(df.arima)


5. 향후 3년 예측
df.arima.pred <- forecast(df.arima, h = 2*12)
df.arima.pred

autoplot(df.arima.pred, xlab='년도', ylab='사건수',
         main='향후 2년 충돌사고 예측', flwd=2, fcol='red')+
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size = 20,
                                  face = 'bold',
                                  hjust = 0.5))

autoplot(forecast(arima(df, order = c(1,0,1)), h = 2*12), xlab='년도', ylab='사건수',
         main='향후 2년 충돌사고 예측', flwd=2, fcol='red')+
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size = 20,
                                  face = 'bold',
                                  hjust = 0.5))

