---
title: "Experiment Report"
author: "BCI Lab"
output: pdf_document
params:
  path: character
---



## Experiment path

```
## [1] "/home/mayenok/tmp/newEyelinesOnline/06"
```

## Dwells


```r
dwell_histogram(experiment)
```

![](/home/mayenok/tmp/newEyelinesOnline/report06_files/figure-latex/unnamed-chunk-2-1.pdf)<!-- --> 

\pagebreak

## EEG Epochs

### True positive epochs


```r
draw_eeg_epochs(experiment, 'true_positive')
```

```
## Warning: Removed 58 rows containing missing values (geom_path).
```

![](/home/mayenok/tmp/newEyelinesOnline/report06_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

\pagebreak

### True negative epochs


```r
draw_eeg_epochs(experiment, 'true_negative')
```

```
## Warning: Removed 108 rows containing missing values (geom_path).
```

![](/home/mayenok/tmp/newEyelinesOnline/report06_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

\pagebreak

### False negative epochs


```r
draw_eeg_epochs(experiment, 'false_negative')
```

![](/home/mayenok/tmp/newEyelinesOnline/report06_files/figure-latex/unnamed-chunk-5-1.pdf)<!-- --> 

\pagebreak

## Eye speed epochs


```r
draw_eye_epochs(experiment)
```

![](/home/mayenok/tmp/newEyelinesOnline/report06_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 
