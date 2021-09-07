require(PerformanceAnalytics)  
library(data.table)  
library(dplyr)  
library(tibble)  
library(TTR)  
library(tidyr)  
library(tidyquant)  
library(tsfeatures)  
library(rsample)  
library(purrr)  
library(stringr)  
library(tibbletime) # tsibble clashes with the base R index() function  
library(xgboost)  
library(rvest)  




set.seed(1234)  
##########################################################  

Scale_Me <- function(x){  
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)  
}  

###########################################################  

start_date <- "2018-01-01"  
end_date <- "2020-01-01"  

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"  
symbols <- url %>%  
  read_html() %>%  
  html_nodes(xpath = '//*[@id="constituents"]') %>%  
  html_table() %>%  
  .[[1]] %>%  
  filter(!str_detect(Security, "Class A|Class B|Class C")) %>%     # Removes firms with Class A, B & C shares  
  sample_n(30) %>%  
  pull(Symbol)  


symbols <- c( 'GOOG', 'AAPL', 'FB','AMZN',
              'EBAY', 'IBM', 'NFLX', 'NVDA',
              'XRX', 'INTC')

dataEnv <- new.env()  
getSymbols(symbols,  
           from = start_date,  
           to = end_date,  
           #hide = "yahoo",  
           #adjust = TRUE,  
           env = dataEnv) 



df <- eapply(dataEnv, function(x){  
  as.data.frame(x) %>%  
    rename_all(function(n){  
      gsub("^(\\w+)\\.", "", n, perl = TRUE)  #將資料清理存成dataframe
    }  
    ) %>%  
    rownames_to_column("date")   #row名稱
}) %>%  
  rbindlist(idcol = TRUE) %>%      #自動列出.id 
  mutate(date = as.Date(date)) %>%  
  group_by(.id) %>%  
  tq_mutate(   #可以用來算各種值,指標 (periodReturn)
    select = Adjusted,  
    mutate_fun = periodReturn,  
    period = "daily",  
    type = "arithmetic"  
  ) %>%  
  mutate(  
    Adj_lag = lag(Adjusted),  
    chng_Adj = ifelse(Adjusted > Adj_lag, 1, 0) #看上漲還下跌
  ) %>%  
  select("date", ".id", "Adjusted", "daily.returns", "chng_Adj", "Open", "High", "Low", "Close") %>%  
  as_tibble() %>%  
  as_tbl_time(index = date) %>%  
  setNames(c("date", "ID", "prc", "ret", "chng", "open", "high", "low", "close")) %>%  
  drop_na(chng)  

#依照ID 收折起來
nested_df <- df %>%  
  mutate(duplicate_ID = ID) %>%  
  nest(-ID)  


rolled_df <- map(nested_df$data, ~ rolling_origin(.,  
                                                  initial = 100,  
                                                  assess = 1,  
                                                  cumulative = FALSE,  
                                                  skip = 0))  #取前100天為train 後面為test rolling去測驗




functions <- c(  
  "entropy",                   
  # Measures the "forecastability" of a series 
  #- low values = high sig-to-noise, 
  #  large vals = difficult to forecast  
  "stability",                
  # means/variances are computed for all tiled windows 
  # - stability is the variance of the means  
  "lumpiness",                
  # Lumpiness is the variance of the variances  
  "max_level_shift",           
  # Finds the largest mean shift between two consecutive windows (returns two values, size of shift and time index of shift)  
  # 衡量兩個連續windows的Mean的最大偏移
  "max_var_shift",            
  # Finds the max variance shift between two consecutive windows (returns two values, size of shift and time index of shift)  
  # 衡量兩個連續windows的Variance的最大偏移
  "max_kl_shift",             
  # Finds the largest shift in the Kulback-Leibler pergence between two consecutive windows (returns two values, size of shift and time index of shift)  
  # KL 散度，這是一個用來衡量兩個概率分佈的相似性的一個度量指標
  "crossing_points",            
  # Number of times a series crosses the mean line 
  "acf_features",
  #
  "hurst",
  "pacf_features"
)


Prediction_Model <- function(SYMB){
  data <- bind_cols( #水平合併
    map(rolled_df[[SYMB]]$splits, ~ assessment(.x)) %>%  bind_rows(), #垂直合併
    
    map(rolled_df[[SYMB]]$splits, ~ analysis(.x)) %>%  
      map(., ~tsfeatures(.x[["ret"]], functions)) %>% #tsfeatures可提取時間序列的特徵 
      bind_rows() 
    ) %>%  
    rename(Y = chng) %>%  
    rename_at(vars(-c(1:9)), ~str_c("X", seq_along(.)))  #seq_along 可以產生一個跟輸入向量相同長度的序列，而序列的內容就是從 1 到輸入向量的長度值 
  
  ml_data <- data %>%  
    as_tibble() %>%  
    rolling_origin(  
      initial    = 200,  
      assess     = 1,    
      cumulative = FALSE,  
      skip       = 0)  
  
  X_train <- map(  
    ml_data$splits, ~ analysis(.x) %>%  
      as_tibble(., .name_repair = "universal") %>%  
      select(starts_with("X")) %>%  
      as.matrix()  
  )  
  
  Y_train <- map(  
    ml_data$splits, ~ analysis(.x) %>%  
      as_tibble(., .name_repair = "universal") %>%  
      select(starts_with("Y")) %>%  
      as.matrix()  
  )  
  
  X_test <- map(  
    ml_data$splits, ~ assessment(.x) %>%  
      as_tibble(., .name_repair = "universal") %>%  
      select(starts_with("X")) %>%  
      as.matrix()  
  )  
  
  Y_test <- map(  
    ml_data$splits, ~ assessment(.x) %>%  
      as_tibble(., .name_repair = "universal") %>%  
      select(starts_with("Y")) %>%  
      as.matrix()  
  )  
  
  #############################################################  
  
  dtrain <- map2(  
    X_train, Y_train, ~ xgb.DMatrix(data = .x, label = .y, missing = "NaN")  
  )  
  
  dtest <- map(  
    X_test, ~ xgb.DMatrix(data = .x, missing = "NaN")  
  )  
  
  # Parameters:  
  watchlist <- list("train" = dtrain)  
  params <- list("eta" = 0.1, 
                 "max_depth" = 5, 
                 "colsample_bytree" = 1, 
                 "min_child_weight" = 1, 
                 "subsample"= 1,  
                 "objective"="binary:logistic", 
                 "gamma" = 1, 
                 "lambda" = 1, 
                 "alpha" = 0, 
                 "max_delta_step" = 0,  
                 "colsample_bylevel" = 1,
                 "eval_metric"= "auc",  
                 "set.seed" = 176)  
  
  # Train the XGBoost model  
  xgb.model <- map(  
    dtrain, ~ xgboost(params = params, data = .x, nrounds = 10, watchlist)  
  )  
  
  xgb.pred <- map2(  
    .x = xgb.model,  
    .y = dtest,  
    .f = ~ predict(.x, newdata = .y, type = 'prob')  
  )  
  
  preds <- cbind(plyr::ldply(xgb.pred, data.frame),  
                 plyr::ldply(Y_test, data.frame)) %>%  
    setNames(c("pred_probs", "actual")) %>%  
    bind_cols(plyr::ldply(map(ml_data$splits, ~assessment(.x)))) %>%  
    rename(ID = duplicate_ID) %>%  
    #select(pred_probs, actual, date, ID, prc, ret) %>%  
    as_tibble()  
  return(preds)  
}




Sys_t_start <- Sys.time()  
Resultados <- lapply(seq(1:length(rolled_df)), Prediction_Model)  
Sys_t_end <- Sys.time()  
round(Sys_t_end - Sys_t_start, 2)  



top_assets <- plyr::ldply(Resultados) %>%  
  #select(pred_probs, actual, date, open, high, low, close, prc, ret) %>%  
  group_by(date) %>%  
  arrange(desc(pred_probs)) %>%  
  dplyr::slice(1) %>%  
  ungroup() %>%  
  select(date, everything()) %>%  
  rename(score = pred_probs) %>%  
  select(-actual)  



top_assets <- xts(top_assets[,c(2:ncol(top_assets))], order.by = top_assets$date) # put top_assets into xts format  

# Analyse strategy  
getSymbols("SPY",  
           from = start_date,  
           to = end_date,  
           hide = "yahoo") 




#detach("package:tsibble", unload = TRUE) # tsibble clashes with the base R index() function  
SPY$ret_Rb <- Delt(SPY$SPY.Adjusted)  
SPY <- SPY[index(SPY) >= min(index(top_assets))]  

RaRb <- cbind(top_assets, SPY)  

charts.PerformanceSummary(RaRb[, c("ret", "ret_Rb")], geometric = FALSE, wealth.index = FALSE,  
                          main = "Strategy vs. Market")



chart.Boxplot(RaRb[,c("ret", "ret_Rb")],  main = "Returns")  

lapply(RaRb[, c("ret", "ret_Rb")], function(x){SharpeRatio(x)})  


chart.RiskReturnScatter(RaRb[, c("ret", "ret_Rb")],   
                        Rf=.03/252, scale = 252,  
                        main = "Risk - Return over the period")  
