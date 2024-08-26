library(fpp3)

# Load data
remove(list=ls())

#B.

temp=readr::read_csv("project_5337.csv")
DATE=seq(as.Date("2016/07/01"),by="month",length.out=93)%>%
  yearmonth()%>%as_tibble()%>%rename(DATE=value)
temp=temp%>%select(-DATE)
Full=DATE%>%add_column(temp)%>%as_tsibble(index=DATE)
Hold=Full%>%filter_index(~"2023 Nov")
Hold

#plot of Data
Hold%>%autoplot(MEDLISPRIPERSQUFEETX)

#SARIMA

#unitroot tests
Hold%>%features(MEDLISPRIPERSQUFEETX,unitroot_kpss)
Hold%>%features(MEDLISPRIPERSQUFEETX,unitroot_nsdiffs)
Hold%>%features(MEDLISPRIPERSQUFEETX,unitroot_ndiffs)
#Differencing Needed
Hold%>%features(MEDLISPRIPERSQUFEETX,unitroot_ndiffs)
Hold%>%features(difference(MEDLISPRIPERSQUFEETX, 12),unitroot_ndiffs)
Hold%>%features(difference(MEDLISPRIPERSQUFEETX,12),unitroot_kpss)
#Differencing for seasonal and non-seasonal needed

#E.

#acf/pacf
Hold%>%gg_tsdisplay(difference(MEDLISPRIPERSQUFEETX, 12)%>%difference(),lag_max=60,plot_type="partial")

#model testing
Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,0)+PDQ(1,1,1)))%>%report()

Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(1,1,1)+PDQ(1,1,0)))%>%report()
Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(1,1,1)+PDQ(1,1,0)))%>%report()
Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,1)+PDQ(1,1,0)))%>%report()
Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,1)+PDQ(2,1,1)))%>%report()
Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,1)+PDQ(1,1,2)))%>%report()
Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,0)+PDQ(2,1,1)))%>%report()
Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,0)+PDQ(1,1,2)))%>%report()
Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,1)+PDQ(1,1,1)))%>%report()
Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(1,1,1)+PDQ(1,1,1)))%>%report()



#model
final=Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,0)+PDQ(1,1,1)))

#ggplot
residuals(final)%>%gg_tsdisplay(.resid,lag_max=60, plot_type="partial")

#ljung-boxtest
ljung1=final%>%augment()%>%features(.innov,ljung_box,lag=60,dof=4)
print(ljung1)

#F.

#Hold forecast
final%>%forecast(h=4)%>%autoplot(Full%>%filter_index('2023 Mar'~'2024 Mar'))+labs(x = "Date", y = "Median Price/Sqft.")+ggtitle("SARIMA(2,1,0)x(1,1,1) Forecast")

#G.

#multivariate
modelmulti=Hold%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,0)+PDQ(1,1,1)+CPInumber+lag(Lumber,n=12)))

#ggplot
residuals(modelmulti)%>%
  gg_tsdisplay(.resid,plot_type="partial",lag_max=60)
#ljung-boxtest
ljung2=modelmulti%>%augment()%>%features(.innov,ljung_box,lag=60,dof=6)
print(ljung2)

#Hold forecast
futureHOLD=new_data(Hold,4)%>%
  mutate(CPInumber=c(
    308.742,
    309.685,
    311.054,
    312.23),
    Lumber=c(
      263.388,
      266.869,
      263.853,
      261.22)
  )
modelmulti%>%forecast(futureHOLD)%>%
  autoplot(Full%>%filter_index('2023 Mar'~'2024 Mar'))+labs(x = "Date", y = "Median Price/Sqft.")+ggtitle("Multivariate Forecast")

accuracy2=modelmulti%>%forecast(futureHOLD)%>%accuracy(Full)
print(accuracy2)

#comparison
Full%>%filter_index("2023 Mar"~"2024 Mar")%>%autoplot(MEDLISPRIPERSQUFEETX)+
  autolayer(final%>%forecast(h=4),colour="RED",level=NULL)+
  autolayer(modelmulti%>%forecast(futureHOLD),colour="BLUE",level=NULL)+ggtitle("Forecast Comparisons")+
  scale_colour_manual(values = c("SARIMA Model" = "red", "Multivariate Model" = "blue"))+ggtitle("Model Comparison") +
  labs(x = "Date", y = "Median Price/Sqft.")

#accuracy
accuracy1=final%>%forecast(h=4)%>%accuracy(Full)
accuracy2=modelmulti%>%forecast(futureHOLD)%>%accuracy(Full)
print(accuracy1)
print(accuracy2)

#H.

#real forecast
future=new_data(Full,6)%>%
  mutate(CPInumber=c(
    312.31296,
    312.46595,
    313.12309,
    313.76684,
    314.760236,
    315.277488),
    Lumber=
      c(261.417,
        257.664,
        272.345,
        255.709,
        256.492,
        257
      )
  )

modelmulti=Full%>%model(ARIMA(MEDLISPRIPERSQUFEETX~0+pdq(2,1,0)+PDQ(1,1,1)+CPInumber+lag(Lumber,n=12)))
modelmulti%>%forecast(future)%>%autoplot(Full%>%filter_index('2016 Jul'~'2024 Apr'))+
  labs(x = "Date", y = "Median Price/Sqft.")+ggtitle("Multivariate Forecast")
forecast1=modelmulti%>%forecast(future)
print(forecast1$.mean)






