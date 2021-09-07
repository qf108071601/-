library(qs)
library(tidyverse)

Fianace = qread("D:/資料整理/股價檔案/金融股_公司別")
L = bind_rows(Fianace)
L$Date = as.Date(L$Date)

for (i in c(4:10,12:17)) {
  L[,i] = as.numeric(L[,i])
}

DT::datatable(L)



library(highcharter)
library(xts)

Monthly_Volume = L %>% 
  filter(Date>Sys.Date()-30) %>%
  group_by(證券代號,證券名稱) %>%
  summarise(近一月平均成交張數 = sum(成交股數)/1000)

i = which(Monthly_Volume$近一月平均成交張數 > 
        quantile(Monthly_Volume$近一月平均成交張數,0.75))

num = Monthly_Volume[i,"證券代號"]

L_choose = L %>%
  filter(證券代號 %in% num$證券代號) %>%
  summarise(Date,證券名稱,收盤價)


L_spread = L_choose %>% spread(.,key = 證券名稱,value=收盤價)
L_xts = xts(L_spread[,2:ncol(L_spread)],
            order.by = L_spread$Date)


h = highchart(type = "stock")
for (i in 1:ncol(L_xts)) {
  h = hc_add_series(hc = h,
                    data = L_xts[,i],
                    name = colnames(L_xts)[i],
                    type = "line")
  h = h
}
h




L_choose %>% 
  hchart(., 
         type = "line", 
         hcaes(x = Date, 
               y = 收盤價, 
               group = 證券名稱)) %>% 
  hc_yAxis(opposite = TRUE,
           labels = list(format = "{value}")) %>% 
  hc_tooltip(pointFormat = '{point.x:%Y-%m-%d} {point.y: .4f}',
             borderWidth = 5)
