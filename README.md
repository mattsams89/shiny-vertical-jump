# Shiny Vertical Jump Analysis
An open-source solution for vertical jump analysis

# Table of contents
* [What is this?](#what-is-this)
* [Running the app](#running-the-app)
* [Preparing your data](#preparing-your-data)
* [Analyzing your data](#analyzing-your-data)
* [Wrapping up](#wrapping up)

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

Below is an example of our Capstone data collection dashboard. You can see it's pretty cluttered. Graphs of the normal forces are all you really need, but I like to have this other information available to the athletes after each jump. These digit widgets also help me determine if an athlete needs to perform additional trials if the values are inconsistent from one trial to the next.

![Dashboard example](/Tutorial/Capstone%20dashboard.png)

If you've used Capstone before, you know each sensor data variable and all calculated variables are exported during the data export. For instance, our export (below) includes 19 columns per trial (ignore Force Left and Force Right on the top left; six individual variables per plate plus six additional calculated variables plus a time column). This is why the 
```
jump.data <- dplyr::select(jump.data, dplyr::starts_with('Normal'))
```
explanation above is central to the code functioning correctly.

![Data fields](/Tutorial/Capstone%20data%20summary.png)

![Capstone export](/Tutorial/Capstone%20export.png)

## Analyzing your data

Once you have everything installed and have exported your data, all that's left is the analysis. I've attempted to make analysis as pain-free as possible as you're only required to set a few pieces of information on the left-hand side of the application. The analysis and saving the results are essentially automated after that.

![File structure](/Tutorial/Analysis%201.png)

As an example, here is how my files are organized for one of our teams. You'll notice the app.R file is present and has been added to the project environment by clicking on it. There are several other folders present, none of which are required. I just like to keep things tidy. The application will create its own 'Analyses' folder in the same directory as the app.R file to save results if one doesn't already exist.

Run the app by pressing ctrl + shift + enter or by pressing the 'Run App' button. Depending on your R Studio settings (the tiny dropdown menu next to the 'Run App' button), this will open a new window either in your web browser or via R's own web browser. They work the same way, so it doesn't really matter which you have selected. Or at least I think so. I've only tested the app with Chrome, so if IE, Edge, Opera, Firefox, etc. is having issues, I would suggest running Chrome or the internal browser instead.

![App start](/Tutorial/Analysis%202.png)

You have the following in the left-hand panel:
1. A file selector  
    Currently, files up to 40 MB are supported. You can change this in the app settings by altering line 32: `options(shiny.maxRequestSize = 40 * 1024 ^ 2)`.
2. A date selector
3. A text box to enter your athlete's name
4. A dropdown to select the jump type  
    Required for the analysis to function correctly
5. Boxes to change your plates' slope and intercept values if you've calibrated
6. A dropdown for different filters  
    None, 2nd order 10 Hz Butterworth, and 10-point SMA available by default  
    Other filters can be written to suit your needs
7. A trial selector
8. A dropdown for different bar loads  
    0, 11, 20 are the default options  
    These can be amended in the code

![File selection](/Tutorial/Analysis%203.png)

Start by selecting an exported .csv for analysis. It will take it a moment to upload and process depending on the file size and number of trials.

![Pre analysis](/Tutorial/Analysis%204.png)

I've gone ahead and set the necessary information to analyze the trial but haven't run the analysis yet ('Perform analysis' button). The app requires you to select the appropriate jump type because the jump start is found differently depending on whether your analyzing an SJ or CMJ. If you run the analysis and notice a really funky jump start, there's a good chance you set the incorrect jump type. 

Because I monitor the baseline values for the force plates (ensuring they are zeroed) and require the athletes to stand still for several seconds before performing their jumps, I haven't set a system mass or offset area for this example. As long as your plates are zeroed and your data look similar to what's pictured above (quiet standing for at least 1 second at the start, flight time being the only time the force plates are unloaded), the app should carry out analysis without issue. Capstone allows you to crop your trial data in case the plates are unloaded at the start or end of the trial, though that's a discussion for another tutorial.

![Auto analysis](/Tutorial/Analysis%205.png)

Here's an example of how the app changes once you've run an analysis. You'll notice there's a new plot below the original. This plot is zoomed in on the trial. There are also three red vertical lines. These lines denote the jump start, takeoff, and landing. Takeoff and landing represent the first and last point below the force threshold, respectively. In this case, the threshold is 20 N, but you can alter this in the code at line 940 (or search for 'threshold'). Jump start is found in accordance with Owen et al. (2014) and McMahon et al. (2018): the point at which force exceeds/falls below 5x the standard deviation of the system mass area (or first second of the data) is found. The point is then moved backward an additional 30 ms.

![Bilateral analysis](/Tutorial/Bilateral%20analysis.png)

![Unilateral analysis](/Tutorial/Unilateral%20analysis.png)

![Phasic analysis](/Tutorial/Phasic%20analysis.png)

For both SJ and CMJ, the analyzed trial will update the 'Bilateral Analysis' and 'Unilateral Analysis' tabs. This process may take a second once you actually click on the tab. Phasic analysis, on the other hand, is only carried out for CMJs. Briefly, the app determines the absolute impulse of the unweighting phase to then find the length of the braking phase (see the code comments for a deeper discussion). The propulsive phase then extends from the end of the braking phase to takeoff. Some very basic phasic analysis variables are included, but these can be extended depending on your needs (e.g. shape factor).

![Analysis output](/Tutorial/Analysis%20output.png)

If you didn't already have an 'Analyses' folder in the same directory as your app.R file, you'll notice one has been created by the application. Furthermore, several files have been created in this folder depending on the date you selected in the app. These are updated automatically each time you press the 'Perform analysis' button. Importantly, do not have these files open when you press 'Perform analysis' as the application will break and will need to be restarted. If you're in Chrome, the app will gray over and you'll see an Access Denied error in R Studio. Likewise, if you're using the internal browser the browser will forcibly close.

![Brushed system mass](/Tutorial/Brushed%20system%20mass.png)

Data collection doesn't always go perfectly, so you might have trials that look like this--the athlete is moving around at the beginning of collection, so the automatic selection of the first second of data as your system mass is no longer an option. Therefore, I've included the ability to 'brush' the force-time data in the upper plot by clicking and dragging across the plot. Once you've selected a system mass area, press the 'Set system mass' button. This will automatically fill the 'System mass start' and 'System mass end' fields. Likewise, if the plates haven't been zeroed, you can manually set an area to offset your force data by brushing anywhere along the flight time (see below).

![Brushed offset](/Tutorial/Brushed%20offset.png)

Importantly, if you set a system mass area and/or offset area in one trial and don't need to set one (or both) in the next, **be absolutely sure** to press the 'Clear fields' button. Manually deleting the values from the fields will not work, because the values are saved to vectors when you press their respective buttons. These vectors can only be cleared by pressing 'Clear fields.'

Once you're done analyzing data, be sure to stop the application by pressing the stop sign in R studio (if using Chrome or another external browswer) or by closing the internal browser.

## Wrapping up

And there you have it. If you've followed along, you should now have some analyzed vertical jump data. If you received errors or ran into other problems, please don't hesitate to contact me: <Matt.L.Sams@gmail.com> or [@DrMattSams](https://twitter.com/DrMattSams).