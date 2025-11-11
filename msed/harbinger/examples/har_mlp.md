# Har Mlp

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
```

### Step
- Execute computation step.

```r
# Execute computation step.
#Using the time series 1 
dataset <- har_examples[[1]]
head(dataset)
```

### Step
- Plot results for visual inspection.

```r
# Plot results for visual inspection.
#ploting serie #1

plot(x = 1:length(dataset$serie), y = dataset$serie)
lines(x = 1:length(dataset$serie), y = dataset$serie)
```

### Step
- Setup: load libraries and helper scripts.

```r
# Setup: load libraries and helper scripts.
# establishing arima method 
  library(nnet)
  model <- har_tsreg_sw(ts_mlp(ts_gminmax(), input_size=3, size=3, decay=0))
```

### Step
- Fit or tune the model on training windows.

```r
# Fit or tune the model on training windows.
# fitting the model
  model <- fit(model, dataset$serie)
```

### Step
- Execute computation step.

```r
# Execute computation step.
# making detections using fbiad
  detection <- detect(model, dataset$serie)
```

### Step
- Execute computation step.

```r
# Execute computation step.
# filtering detected events
  print(detection |> dplyr::filter(event==TRUE))
```

### Step
- Evaluate the forecast using sMAPE or related metrics.

```r
# Evaluate the forecast using sMAPE or related metrics.
# evaluating the detections
  evaluation <- evaluate(model, detection$event, dataset$event)
  print(evaluation$confMatrix)
```

### Step
- Plot results for visual inspection.

```r
# Plot results for visual inspection.
# ploting the results
  grf <- plot.harbinger(model, dataset$serie, detection, dataset$event)
  plot(grf)
```

### Step
- Execute computation step.

```r
# Execute computation step.
```
