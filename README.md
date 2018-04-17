# Shiny Vertical Jump Analysis
An open-source solution for vertical jump analysis

## What is this?
This Shiny app is geared toward strength and conditioning professionals and researchers who can't afford vertical jump analysis software and those who don't have the coding knowledge to design their own analyses. It's my hope these individuals will have an alternative to analyzing their force-time data in Excel (we've all been there). This allows for more powerful analysis than just simply jump height, peak force, etc. Additionally, this app is publicly available to take the cover off the black box that is force-time data analysis. A set of visible filtering, plotting, and variable calculation functions allow for consistent analysis procedures between users. 

Importantly, this app is only designed to analyze data from dual PASCO force plates. Users are welcome to create forks that are compatible with other force platforms. Further, this app is written for use with the R package ```shiny```. R was chosen for two reasons: 1) it's free to download (great for budget-strapped users) and 2) I have no experience with other languages. Maybe one day that will change...

## Running the app
This analysis app is written in R and depends on the package ```shiny``` among others to function. ```shiny``` allows for (relatively) easy creation of interactive web apps. These web apps can either run locally (in the case of the current app) or can be accessed remotely via a server application. I wanted to keep things simple, though, so the app runs locally. This allows you to save your results wherever your place the app.R file. I tried to make things as easy as possible when it comes to saving data, etc., which you'll see in the rest of the Tutorial. Importantly, I've only tested the app in Windows. There's a good chance some of the functions may not be supported in the Mac version of R, so contact me if you are running into issues.

First, you'll need to download and install [R](https://cran.r-project.org/mirrors.html) and [R Studio](https://www.rstudio.com/products/rstudio/). As of writing R is on version 3.4.4 while R Studio is on version 1.1.442. Having said that, older installations should also work. I think the bare minimums are R 3.0 and R Studio 1.0. Some required packages may not support older versions, though, so if you're running into issues, update your R installation before writing me angry emails!

Once installed, you'll need several packages that are dependencies for the code to run correctly. Packages expand the functionality of R past the ```base``` functions and can be written to solve virtually any issue. Most of the dependencies for this app are used to manipulate and filter your data. Lines 15 - 23 of the code provide a bit more detail, and if you're super curious you can read through the code yourself to get a better idea of what's going on. Packages are installed by calling the install.packages() function. E.g. install.packages('shiny'). Make sure to enclose the package name in '' or "", or you will receive an error. The current required packages are ```shiny```, ```data.table```, ```dplyr```, ```signal```, ```ggplot2```, ```MESS```, ```shinythemes``` (although you can disable this by removing line 39), ```TTR```, & ```purrr```.

Once you have R and all the required packages, you can download the app as a .zip folder (click the green 'Clone or download' button on GitHub and select 'Download ZIP') or manually copy the [raw code](https://raw.githubusercontent.com/mattsams89//app.R) to a new R script file (see below).

![New file](/Tutorial/New%20script%20file.png)  

If you download the app as a .zip, you'll need to extract the app.R file. Importantly, wherever the app.R file is stored will determine where the analysis files are created. So regardless of whether you download the .zip file or manually copy the code, be sure to save the file to the correct directory. For instance, if you work with several teams and want to keep their data separate you might save a copy of the app to folders for each team (see example below). 

![VB app example](/Tutorial/VB%20app.png)

![MBB app example](/Tutorial/MBB%20app.png)  

If you decide to have multiple copies of the app.R file, be sure to load the correct one by selecting it in the file viewer pane in the bottom right-hand corner of your R Studio environment.

![App location](/Tutorial/app%20location.png)

I normally get around this by having a different project for each team. You can create new projects by selecting your project's name > New Project and following the prompts to add a new project file.

![New project](/Tutorial/New%20project%201.png) 

![New folder](/Tutorial/New%20project%202.png) 

![Project type](/Tutorial/New%20project%203.png) 

![Project name](/Tutorial/New%20project%204.png) 

Again, you only have to follow these steps if you have multiple copies of the app.R file dedicated to different teams. If you only have a single copy of the file, don't worry about creating a new directory, etc. Just open the app.R file in your current R studio project / environment. 

The app can be run by either pressing ctrl + shift + enter or by pressing the 'Run App' button that appears. If you don't see the 'Run App' button, you're likely running an old version of R studio that doesn't support Shiny applications.

![Run app](/Tutorial/Run%20app.png)

## Preparing your data

Once you've collected data in Capstone, all you need to do is export the data as .csv format--no modification required! That is, as long as your vertical force data are named some variant of 'Normal Force.' You'll see what I mean below. If your data are named something different, you will need to change line ~ 575 in the code:

```
jump.data <- dplyr::select(jump.data, dplyr::starts_with('Normal'))
```

For instance, if your vertical force data were instead named 'Vertical Force Left' and 'Vertical Force Right' you would change the code to

```
jump.data <- dplyr::select(jump.data, dplyr::starts_with('Vertical')
```

Avoid names starting with 'Force,' as Capstone names each of the force beams (2-axis plates; not sure if present on single-axis plates) 'Force Beam 1,' 'Force Beam 2,' etc. This will break the app as the above code excludes all non-normal force data. That is, any force beam data, user-defined calculations, etc. are excluded *as long as* they do not start with the word 'Normal.' This is what allows you to page through the trials with the trial selector I'll cover a little later.

Below is an example our our Capstone data collection dashboard. You can see it's pretty cluttered. Graphs of the normal forces are all you really need, but I like to have this other information available to the athletes after each jump. These digit widgets also help me determine if an athlete needs to perform additional trials if the values are inconsistent from one trial to the next.

![Dashboard example](/Tutorial/Capstone%20dashboard.png)

If you've used Capstone before, you know each sensor data variable and all calculated variables are exported during the data export. For instance, our export includes 19 columns per trial (ignore Force Left and Force Right on the top left; six individual variables per plate plus six additional calculated variables plus a time column). This is why the 
```
jump.data <- dplyr::select(jump.data, dplyr::starts_with('Normal'))
```
bit above is central to the code functioning correctly.

![Data fields](/Tutorial/Capstone%20data%20summary.png)

![Capstone export](/Tutorial/Capstone%20export.png)