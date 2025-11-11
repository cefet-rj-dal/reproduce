# Har Datasets

This Markdown is an English conversion of the original Jupyter notebook. It includes didactic explanations and commented code blocks.

## Overview
- Purpose: demonstrate usage of Harbinger on time series (loading data, configuring models/detectors, training, predicting, and plotting results).
- Environment: R (via `harbinger`) unless specified.

### Step
- Setup: load libraries and helper scripts.

```r
# Setup: load libraries and helper scripts.
# DAL Library
# version 0.1.0

source("load_harbinger.R")

#loading Harbinger
load_harbinger() # see ../load_harbinger.R 
```

### Step
- Load example dataset into memory.

```r
# Load example dataset into memory.
#loading the example database
data(har_examples)
model <- har_arima()
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 1 
dataset <- har_examples[[1]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 2 
dataset <- har_examples[[2]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 3 
dataset <- har_examples[[3]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 4 
dataset <- har_examples[[4]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 5 
dataset <- har_examples[[5]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 6 
dataset <- har_examples[[6]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 7 
dataset <- har_examples[[7]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 8 
dataset <- har_examples[[8]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 9 
dataset <- har_examples[[9]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Fit or tune the model on training windows.
- Plot results for visual inspection.

```r
# Fit or tune the model on training windows.
#Using the time series 10
dataset <- har_examples[[10]]
model <- fit(har_arima(), dataset$serie)
detection <- detect(model, dataset$serie)
grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
plot(grf)
```

### Step
- Execute computation step.

```r
# Execute computation step.
```
