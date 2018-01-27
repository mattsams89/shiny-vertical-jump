# Shiny Vertical Jump Analysis
An open-source solution for vertical jump analysis

## What is this?
This app is geared toward strength and conditioning professionals and researchers who can't afford vertical jump analysis software and those who don't have the coding knowledge to design their own analyses. It's my hope these individuals will have an alternative to analyzing their force-time data in Excel (we've all been there). This allows for more powerful analysis than just simply jump height, peak force, etc. Additionally, this app is publicly available to take the cover off the black box that is force-time data analysis. A consistent set of filtering, plotting, and variable calculation functions allow for consistent analysis between individuals using the app. 

Importantly, this app is only designed to analyze data from dual PASCO force plates. Users are welcome to create forks that are compatible with other force platforms.

## Running the app
A more detailed tutorial is in the works. For now, if you want to play around with the app, you can either download it or download ```shiny``` and call

```
library(shiny)

runGitHub('shiny-vertical-jump', 'mattsams89')
```

For this app to run, however, you will need the dependencies ```shiny```, ```data.table```, ```dplyr```, ```signal```, ```ggplot2```, ```MESS```, ```shinythemes``` (although you can get away with removing this in line 38), and ```TTR```. Lines 15 - 22 in the source code detail why these packages are dependencies.
