require(stringr)
require(dplyr)
require(caret)
require(doParallel)

cl = registerDoParallel(cores = 3)


url = 'https://pl.wikipedia.org/wiki/Kr%C4%85g_kultur_p%C3%B3l_popielnicowych'
url = 'https://pl.wikipedia.org/wiki/Polska'
str = paste(readLines(url, encoding = 'UTF-8'), collapse = ' ')
x = str_extract_all(string = str, pattern = '<p>.*?</p>')
x = str_replace_all(string = x, pattern = '(<.+?>)', replacement = '')
x = tolower(x)
x = str_extract_all(string = x, pattern = '\\w+')
x = x[[1]]
x = x[x!='c']
tr = data_frame(lag2 = lag(x,2), lag1 = lag(x), y = x)

fltr = tr %>% group_by(y) %>% summarise(n = n()) %>% filter(!grepl(x = y, pattern = '\\d+') & n > 20)

tr = inner_join(x = tr, y = fltr, by = c('y' = 'y'))

mdl = train(data = tr, y ~ lag1, 
            method = 'rf', 
            trControl = trainControl(summaryFunction = multiClassSummary,
              method = 'repeatedcv', 
              number = 5, 
              repeats = 5, 
              verboseIter = T,
              )
            )


stopCluster(cl)
