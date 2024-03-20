---
title: "Bommeleeër Affär"
author: "Bryan Nelson"
date: '2022-02-05'
slug: bommeleeer-affar
categories: []
tags: Data Reflection
subtitle: ''
summary: Add summary here
authors: []
lastmod: '2022-02-05'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: no
projects: []
---

<script src="{{< blogdown/postref >}}index.en_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/pymjs/pym.v1.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/widgetframe-binding/widgetframe.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/pymjs/pym.v1.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/widgetframe-binding/widgetframe.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/jquery/jquery.min.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/ionrangeslider-javascript/js/ion.rangeSlider.min.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/strftime/strftime-min.js"></script>
<link href="{{< blogdown/postref >}}index.en_files/ionrangeslider-css/css/ion.rangeSlider.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index.en_files/crosstalk/css/crosstalk.min.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index.en_files/crosstalk/js/crosstalk.min.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/htmlwidgets/htmlwidgets.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/pymjs/pym.v1.js"></script>
<script src="{{< blogdown/postref >}}index.en_files/widgetframe-binding/widgetframe.js"></script>
<p>
Check out the final <a href="Bommeleeer-Affar.html" target="_blank">Bommeleeër Affär Dashboard</a>.
</p>

# Background

# Data Collection

# Setup

``` r
# library(flexdashboard)
library(tidyverse)
library(lubridate)
library(leaflet)
library(DT)
library(timevis)
library(crosstalk)
library(widgetframe)
library(htmlwidgets)
```

``` r
bomm <- read_csv("C:/Users/bryan/Dropbox/Computer/Andromeda-Tau/data-reflections/blog-data/bommeleeer-affar/data/bommeleeër-affär.csv")

bomm$Date <- mdy(bomm$Date)
```

## Locations

Shared data

``` r
shared_bomm <- SharedData$new(bomm)
```

Custom icon

``` r
expl_icon <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/831/831687.png",
  iconWidth = 24, iconHeight = 24)
```

Map

``` r
bomm_map <- leaflet() %>%
  addTiles(group = "Map") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addMarkers(data = shared_bomm,
             lng = ~longitude, lat = ~latitude,
             popup = ~paste0("Target: ", Target, "<br/>Location: ", Location, "<br/>Date: ", Date, "<br/>", Notes),
             label = ~Target,
             icon = expl_icon) %>%
  addLayersControl(baseGroups=c("Map", "Satellite"),
                   options=layersControlOptions(collapsed=FALSE))

frameWidget(bomm_map, width = "100%")
```

<div id="htmlwidget-1" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"url":"index.en_files/figure-html//widgets/widget_unnamed-chunk-5.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

Table

``` r
bomm_table <- shared_bomm %>%
  datatable(rownames = FALSE)

frameWidget(bomm_table, width = "100%")
```

<div id="htmlwidget-2" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"url":"index.en_files/figure-html//widgets/widget_unnamed-chunk-6.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

Filter

``` r
bomm_filter <- filter_slider("Date", "Date", shared_bomm, column = ~Date)
bomm_filter
```

<div class="form-group crosstalk-input crosstalk-input-slider js-range-slider" id="Date">
<label class="control-label" for="Date">Date</label>
<input data-skin="shiny" data-type="double" data-min="454723200000" data-max="517363200000" data-from="454723200000" data-to="517363200000" data-step="86400000" data-grid="true" data-grid-num="9.93150684931507" data-grid-snap="false" data-prettify-separator="," data-keyboard="true" data-keyboard-step="0.137931034482759" data-drag-interval="true" data-data-type="date" data-time-format="%F"/>
<script type="application/json" data-for="Date">{
  "values": ["1984-05-30", "1984-06-02", "1985-04-12", "1985-04-27", "1985-05-07", "1985-05-27", "1985-05-29", "1985-06-23", "1985-07-05", "1985-07-05", "1985-07-26", "1985-08-29", "1985-08-29", "1985-09-30", "1985-10-20", "1985-11-09", "1985-11-30", "1985-12-02", "1986-02-17", "1986-05-25"],
  "keys": ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"],
  "group": ["SharedData9485389d"]
}</script>
</div>

## Timeline

``` r
bomm_time <- bomm %>%
  mutate(start = Date) %>%
  mutate(content = Target) %>%
  timevis()

frameWidget(bomm_time, width = "100%", height = 1000)
```

<div id="htmlwidget-3" style="width:100%;height:1000px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"url":"index.en_files/figure-html//widgets/widget_unnamed-chunk-8.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script>

## Documents

# Summary

<iframe class="flexdashboard" src="Bommeleeer-Affar.html" style="height: 1070px; width: 720px">
</iframe>

## Future Explorations
