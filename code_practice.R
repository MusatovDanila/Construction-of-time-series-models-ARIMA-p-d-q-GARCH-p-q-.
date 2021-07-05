library("lubridate") 
library("zoo") 
library("xts") 
library("dplyr") 
library("ggplot2")
library("forecast")
library("quantmod")
library("sophisthse") # загрузка с sophist.hse.ru
library("urca")
library("tseries")
library("xlsx")

#получение данных=
invest <- sophisthse("INVFC_M")
# графики данных, ACF и PACF
tsdisplay(invest)

Invest_d <-decompose(invest)
plot(Invest_d)

ur.df(invest) %>% summary
ur.kpss(invest) %>% summary

ndiffs(invest)
nsdiffs(invest)
cbind("Monthly invest" = invest,
      "Monthly log invest" = log(invest),
      "Dayly change in log invest "=diff(log(invest)),
      "Annual change in log invest" = diff(diff(log(invest),12)))  %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Indicators for fixed capital investments")

#Работа с периодом ряда с 1995 по 2005

In_95_05 = window(invest, start=c(1995, 1),end=c(2005, 12) ) # часть выборки 1995-2005
In_95_06 = window(invest, start=c(1995, 1),end=c(2006, 12) ) # часть выборки 1995-2006

tsdisplay(In_95_05)
ur.df(In_95_05) %>% summary
ur.kpss(In_95_05) %>% summary

ndiffs(In_95_05)
nsdiffs(In_95_05)
cbind("Monthly invest" = In_95_05,
      "Monthly log invest" = log(In_95_05),
      "Dayly change in log invest "=diff(log(In_95_05)),
      "Annual change in log invest" = diff(diff(log(In_95_05),12)))  %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Indicators for fixed capital investments")

tsdisplay(diff(diff(log(In_95_05),12)))	

ur.df(diff(diff(log(In_95_05),12))) %>% summary
ur.kpss(diff(diff(log(In_95_05),12))) %>% summary

# перебор всех моделей с составлением таблицы
get_models <- function(time_series,val_p, d, val_q, P, D, Q, s){
  mod_names = c()
  i = 1
  m = matrix(0, nrow=(val_p+1)*(val_q+1), ncol = 5)
  for(p in 0:val_p){
    for(q in 0:val_q){
      get_mod <- tryCatch(
        {
          mod = Arima(time_series,order=c(p,d,q), seasonal = c(P,D,Q))
        },
        error = function(cond)
        {return(NA)}
      )
      if(!is.na(get_mod)){
        invisible(capture.output( {
          res_summ = summary(mod) 
          m[i,1] = res_summ[2]
          m[i,2] = res_summ[3]
          m[i,3] = mod[["aic"]]
          m[i,4] = mod[["aicc"]]
          m[i,5] = mod[["bic"]]
        }))
      }else{
        m[i,1] = m[i,2] = m[i,3] = m[i,4] = m[i,5] = "err"
      }
      p
      mod_names = append(mod_names,  paste0("ARIMA(",p,",",1,",",q,")","(",P,",",D,",",Q,")","[",s,"]"   ))
      i = i + 1
    }
  }
  colnames(m) <- c("RMSE", "MAE", "AIC", "AICc","BIC")
  rownames(m) <- mod_names
  m
}

mod_a_95_05 <- auto.arima(In_95_05)
summary(mod_a_95_05)

models_95_05 = get_models(In_95_05,8,1,9,1,1,1,12)

write.csv(models, "Arima_models.csv")

prognoz_a <- forecast(mod_a_95_05,h=12)
plot(prognoz_a)
lines(In_95_06,col = "red")

mod_1<- Arima(In_95_05,order=c(8,1,8),seasonal = c(1,1,1))
prognoz_1 <- forecast(mod_1,h=12)
plot(prognoz_1)
lines(In_95_06,col = "red")

mod_a_1 <- auto.arima(In_95_05,stepwise=FALSE, approximation=FALSE )
summary(mod_a_1)

In_10_14 = window(invest, start=c(2010, 1),end=c(2014, 12) ) # часть выборки 2010-2014
In_10_15 = window(invest, start=c(2010, 1),end=c(2015, 12) ) # часть выборки 2010-2015  
tsdisplay(In_10_14)

ur.df(In_10_14) %>% summary
ur.kpss(In_10_14) %>% summary

ndiffs(In_10_14)
nsdiffs(In_10_14)
cbind("Monthly invest" = In_10_14,
      "Monthly log invest" = log(In_10_14),
      "Dayly change in log invest "=diff(log(In_10_14)),
      "Annual change in log invest" = diff(diff(log(In_10_14),12)))  %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Indicators for fixed capital investments")

ur.df(diff(diff(log(In_10_14),12))) %>% summary
ur.kpss(diff(diff(log(In_10_14),12))) %>% summary

tsdisplay(diff(diff(log(In_10_14),12)))

lines(In_2_E,col = "red")
ts.plot(In_1_E,In_2_E,gpars = list(col = c("black", "red")))

mod_a_10_14 <- auto.arima(In_10_14)
summary(mod_a_10_14)

models_10_14 = get_models(In_10_14,6,1,6,1,1,0,12)

write.csv(models_10_14, "Arima_models_10_14.csv")

prognoz_a_10_14 <- forecast(mod_a_10_14,h=12)
plot(prognoz_a_10_14)
lines(In_10_15,col = "red")

mod_2<- Arima(In_10_14,order=c(6,1,5),seasonal = c(1,1,0))
prognoz_2 <- forecast(mod_2,h=12)
plot(prognoz_2)
lines(In_10_15,col = "red")

mod_3<- Arima(In_10_14,order=c(3,1,6),seasonal = c(1,1,0))
prognoz_3 <- forecast(mod_3,h=12)
plot(prognoz_3)
lines(In_10_15,col = "red")

mod_a_2 <- auto.arima(In_10_14,stepwise=FALSE, approximation=FALSE )
summary(mod_a_2)

tsdisplay((In_95_05-mean(In_95_05))^2)
tsdisplay(diff(diff(In_95_05,12))^2)

get_models_g_1 <- function(time_series,val_p, val_q, P, Q){
  mod_names = c()
  i = 1
  m = matrix(0, nrow=(val_p+1)*(val_q+1), ncol = 4)
  for(p in 0:val_p){
    for(q in 0:val_q){
      get_mod <- tryCatch(
        {
          model_t = ugarchfit(spec = ugarchspec(variance.model = list(garchOrder = c(p, q)), mean.model = list (armaOrder = c (P,Q), arfima=TRUE),fixed.pars=list(d=1)), data = time_series, solver ='hybrid')
          res_summ = infocriteria(model_t)
        },
        error = function(cond)
        {
          return(NULL)}
      )
      if(!is.null(get_mod)){
        invisible(capture.output( {
          m[i,1] = get_mod[1]
          m[i,2] = get_mod[2]
          m[i,3] = get_mod[3]
          m[i,4] = get_mod[4]
        }))
      }else{
        m[i,1] = m[i,2] = m[i,3] = m[i,4] = "err"
      }
      mod_names = append(mod_names,  paste0("  GARCH(",p,",",q,")  ARIMA(",P,",",1,",",Q,")"   ))
      i = i + 1
    }
  }
  colnames(m) <- c("Akaike", "Bayes", "Shibata", "Hannan-Quinn")
  rownames(m) <- mod_names
  m
}

model_test_1= get_models_g_1(In_95_05,5,5,0,1)

model_test_3= get_models_g_1(In_10_14,6,6,0,1)

write.csv(model_test_1, "Garch_models.csv")
write.csv(model_test_3, "Garch_models_2.csv")

ug_spec_95_05_2<- ugarchspec ( variance.model = list(garchOrder = c(1, 0)), mean.model = list (armaOrder = c (0,1),arfima=TRUE),fixed.pars=list(d=1))
ug_spec_95_05_2
model_2_G <- ugarchfit(spec = ug_spec_95_05_2, data = In_95_05, solver ='hybrid')
model_2_G   

plot((model_2_G@fit$residuals)^2, type = "l")
lines(model_2_G@fit$var, col = "red")

mod_2_g_fore <- ugarchforecast(model_2_G, n.ahead = 12)
mod_2_g_fore

ug_spec_10_14_1<- ugarchspec ( variance.model = list(garchOrder = c(1, 0)), mean.model = list (armaOrder = c (0,1),arfima=TRUE),fixed.pars=list(d=1))
ug_spec_10_14_1
ug_spec_10_14_2<- ugarchspec ( variance.model = list(garchOrder = c(6, 6)), mean.model = list (armaOrder = c (0,1),arfima=TRUE),fixed.pars=list(d=1))
ug_spec_10_14_2

model_3_G <- ugarchfit(spec = ug_spec_10_14_1, data = In_10_14, solver ='hybrid')
model_3_G  
model_4_G <- ugarchfit(spec = ug_spec_10_14_2, data = In_10_14, solver ='hybrid')
model_4_G  

plot((model_3_G@fit$residuals)^2, type = "l")
lines(model_3_G@fit$var, col = "red")

mod_3_g_fore <- ugarchforecast(model_3_G, n.ahead = 12)
mod_3_g_fore

plot((model_4_G@fit$residuals)^2, type = "l")
lines(model_4_G@fit$var, col = "red")

mod_4_g_fore <- ugarchforecast(model_4_G, n.ahead = 12)
mod_4_g_fore

