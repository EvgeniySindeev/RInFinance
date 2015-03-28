# Подключаем пакеты и загружаем данные:
library(quantmod)
library(PerformanceAnalytics)
start.date <- "2009-01-01"
end.date <- "2015-01-01"
getSymbols("MA", from = start.date, to = end.date)
getSymbols("V", from = start.date, to = end.date)

# Считаем доходности. 
V.ret <- lag(Return.calculate(Ad(V)), -1)
MA.ret <- lag(Return.calculate(Ad(MA)), -1)

# Определимся с датой начала торговли.
startTrade <- "2011-01-01"

# Создадим массив, содержащий торговые дни:

tradingDays <- seq(as.Date(startTrade), as.Date(end.date), by = 'days')

# И создадим временной ряд доходностей, заполненный нулями.

returns <- xts(rep(0, length(tradingDays)), order.by = tradingDays)

# Определимся с длинной нашей скользящей обучающей выборки:

length <- 50

Теперь нужно пройтись циклом по массиву торговых дней и в каждой точке проделать следующее:
  
# В каждой точке выделить обучающую выборку размера length, по которой мы будем строить модель.
# Построить модель.
# Найти отклонение от модели сегодня. Это будет последний элемент вектора всех отклонений.
# Определиться с тем, какое отклонение будем называть допустимым. Допустим, одно стандартное отклонение.
# Если отклонение сегодня выше допустимого, то одну акцию шортим, другую покупаем. Если ниже допустимого, то наоборот. Если в допустимых пределах, то получаем доходность на 0.
# Делаем:
  
  for (i in seq_along(tradingDays))
    { 
    # Step 1
    V.model <- window(Ad(V), 
                      start = as.Date(tradingDays[i]) - length, 
                      end = tradingDays[i])
    MA.model <- window(Ad(MA), 
                       start = as.Date(tradingDays[i]) - length, 
                       end = tradingDays[i])
    # Step 2
    model <- lm(MA.model ~ V.model)
    # Step 3
    residual <- tail(model$residuals, 1)
    # Step 4
    border <- sd(model$residuals)
    # Step 5
    returns[i] <- ifelse(
      residual > border, 
      (-MA.ret[tradingDays[i]] + V.ret[tradingDays[i]])/2, 
      ifelse (
        residual < -border, 
        (MA.ret[tradingDays[i]] - V.ret[tradingDays[i]])/2, 
        0)
    )
  }  
# Теперь можно проанализировать полученный вектор доходностей. 
# (В него попали NA-шки, поэтому опустим их предварительно):

returns <- na.omit(returns)
charts.PerformanceSummary(returns)

table.AnnualizedReturns(returns)

maxDrawdown(returns)
