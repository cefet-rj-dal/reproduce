#library(devtools)
#devtools::install_github("cefet-rj-dal/tspred-it", force = TRUE, dep = FALSE, upgrade = "never")
library(tspredit)
library(daltoolbox)

data(fertilizers)

ts <- ts_data(fertilizers$brazil_n, sw = 8)

samp <- ts_sample(ts, test_size = 4)

io_train <- ts_projection(samp$train)

tune <- ts_maintune(preprocess = list(ts_norm_swminmax()),
    input_size = c(3:7),
    base_model = ts_mlp(),
    augment = list(ts_aug_none())
  )
ranges <- list(size = 1:10,
               decay = seq(0, 1, 1 / 9),
               maxit = 10000)
model <- fit(tune, 
             x = io_train$input, 
             y = io_train$output, 
             ranges)

adjust <- predict(model, io_train$input)
ev_adjust <- evaluate(model, io_train$output, adjust)
print(ev_adjust$metrics$smape)


io_test <- ts_projection(samp$test)
prediction <- predict(model, x = io_test$input, steps_ahead = 1)
ev_test <- evaluate(model, io_test$output, prediction)
print(ev_test$metrics$smape)


hyper <- attr(model,"hyperparameters")

