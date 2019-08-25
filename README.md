# Shiny Vertical Jump Analysis
An open-source solution for vertical jump analysis

# Table of contents
* [What is this?](#what-is-this)
* [Downloading R / Installing Required Packages](#downloading-r)
* [Running the app](#running-the-app)
* [Preparing your data](#preparing-your-data)
* [Analyzing your data](#analyzing-your-data)
* [Wrapping up](#wrapping-up)

## What is this?
**_This Readme is currently out of date._**
This Shiny app is geared toward strength and conditioning professionals and researchers who can't afford vertical jump analysis software and those who don't have the coding knowledge to design their own analyses. It's my hope these individuals will have an alternative to analyzing their force-time data in Excel (we've all been there). This allows for more powerful analysis than just simply jump height, peak force, etc. Additionally, this app is publicly available to take the cover off the black box that is force-time data analysis. A set of visible filtering, plotting, and variable calculation functions allow for consistent analysis procedures between users. 

Importantly, this app is only designed to analyze data from dual PASCO force plates or datasets that closely mimic the PASCO .txt or .csv export. Users are welcome to create forks that are compatible with other formats. This app is published under the GNU GPL-3 license; you're free to use, modify, and share the source code and your own modified code as long as you release your modification(s) under the same license. See [this site](https://resources.whitesourcesoftware.com/blog-whitesource/top-10-gpl-license-questions-answered) for a plain-English explanation of GPL-3.

## Downloading R
(I haven't tested compatibility with MacOS or Linux. Feel free to share any hiccups you encounter if you use either platform.) The first step in running an R-based program is to, well, have R installed on your computer. The most up-to-date version of R can be found [here](https://cran.r-project.org/bin/windows/base/). The app was built with R version 3.5.1 and packages that were current as of 2019-07-01. I can't promise everything will work correctly post-installation, as package updates can randomly break parts of the code. If you ever run into any errors, please [email me](mailto:samsperformancetraining@gmail.com) and I will look into the problem and release a hotfix ASAP. Once you have R installed, I would strongly recommend you install [RStudio](https://www.rstudio.com/products/rstudio/download/#download) as well. You interact with base R via an ugly, bare-bones IDE, whereas RStudio has all sorts of bells and whistles to make your experience more tolerable.

Aside from R and RStudio, you will need to install a number of support packages before the app will function. Packages expand the functionality of R past the ```base``` functions and can be written to solve virtually any issue. Most dependencies for this app are used to manipulate and visualize the data; lines 11-20 in the .Rmd posted on GitHub briefly explain what each package is used for. Packages are installed via the `install.packages()` function. E.g. `install.packages("flexdashboard")`. Make sure to enclose the package names in '' or "", although newer versions of RStudio may automatically prompt you to install missing packages when you open the .Rmd file. The currently required packages are the following: ```shiny```, ```flexdashboard```, ```purrr```, ```signal```, ```data.table```, ```plotly```, ```zoo```, ```changepoint```, ```pracma```, ```shinyWidgets```, and ```kableExtra```. Installing these packages should install any other requisite packages, but if the app refuses to load or crashes on certain functions, examine the error message before emailing me. If a package is missing, the error will tell you as much.

## Running the app
Once you have R and all the required packages, you can download the app as a .zip folder (click the green 'Clone or download' button on GitHub and select 'Download ZIP') or manually copy the [raw code](https://raw.githubusercontent.com/mattsams89/shiny-vertical-jump/master/Vertical%20Jump%20Analysis%20V2.Rmd) to a new RMarkdown file (see below). If you go this route and have already installed flexdashboard, clicking new R Markdown will open a dialog box. Select "from template" > "Flex Dashboard" and copy-paste the code to completely overwrite the default text in the file.

![New file](/Tutorial/new_rmd.png)

![Flexdashboard](/Tutorial/flex.png)

If you download the app as a .zip, you'll need to extract the .Rmd file. Importantly, wherever the .Rmd file is stored will determine where the analysis files are created. So regardless of whether you download the .zip file or manually copy the code, be sure to save the file to the correct directory. For instance, if you work with several teams and want to keep their data separate you might save a copy to folders for each team. 

If you decide to have multiple copies of the .Rmd file, be sure to load the correct one by selecting it in the file viewer pane in the bottom right-hand corner of your R Studio environment.

![App location](/Tutorial/app_location.png)

I normally get around this by having a different project for each team. You can create new projects by selecting your project's name > New Project and following the prompts to add a new project file.

![New project](/Tutorial/new_project_1.png) 

![New folder](/Tutorial/new_project_2.png) 

![Project type](/Tutorial/new_project_3.png) 

![Project name](/Tutorial/new_project_4.png) 

Again, you only have to follow these steps if you have multiple copies of the file dedicated to different teams. If you only have a single copy of the file, don't worry about creating a new directory, etc. Just open the .Rmd in your current R studio project / environment. 

The app can be run by either pressing ctrl + shift + K or by pressing the 'Run Document' button that appears. If you don't see the 'Run Document' button, you're likely running an old version of R studio that doesn't support Shiny applications.

## Preparing your data

Once you've collected data in Capstone, all you need to do is export the data as .txt or .csv format--no modification required! That is, as long as your vertical force data are named some variant of "Normal" force. See the example data for...examples...of how the data should be structured. You can use other names if you'd like, but that will require you changing line 88 in the code to reflect your chosen varible name. Avoid names starting with 'Force,' as Capstone names each of the force beams (2-axis plates; not sure if present on single-axis plates) 'Force Beam 1,' 'Force Beam 2,' etc. This will break the app because the filter is written to only retain variables containing "Normal" or whatever word you choose to use at line 88. That is, any force beam data, user-defined calculations, etc. are excluded *as long as* they do not start with the word "Normal."

## Analyzing your data

Once you have everything installed and have exported your data, all that's left is the analysis. I've attempted to make analysis as pain-free as possible as you're only required to set a few pieces of information on the left-hand side of the application. The analysis and saving the results are essentially automated after that.

Run the app by pressing ctrl + shift + K or by pressing the 'Run Document' button. 

You'll see the following in the left-hand panel:
1. A file selector  
    Currently, files up to 40 MB are supported. You can change this in the app settings by altering line 22: `options(shiny.maxRequestSize = 40 * 1024 ^ 2)`.
2. A trial selector
3. A dropdown for trial descriptive information, including a manual selector for vertical jump type in case the app guesses incorrectly
4. A dropdown to adjust the force plate calibration, sampling frequency, quiet standing length, and applied filter

Start by selecting an exported .csv for analysis. It will take it a moment to upload and process depending on the file size and number of trials.

![Upload](/Tutorial/file_upload.gif)

The app is smart enough to (hopefully) auto-recognize the jump type, but it can't yet find the quiet standing phase on its own. That isn't an issue for trials that look like this:

![](/Tutorial/easy_trial.png)

But trials that look like this are a problem:

![](/Tutorial/angry_trial.png)

In the latter case, you can "brush" the top plot by clicking and dragging across the plot.

![](/Tutorial/brushed-plot.png)

The height of your brush doesn't matter; only the X-axis coordinates of the brush are used to update the trial information. For best results, grab at least half a second of quiet standing at the start and end at any point after landing but before the athlete steps off the force plates.

Once your trial is selected correctly, a quick glance at the plots on the right (and eventually the bottom plot...need to update that) will tell you whether the app recognized the jump type correctly. I rather arbitrarily chose a 250N difference between quiet standing and minimum force prior to peak force as the delineation between SJ and CMJ. That worked well with my 65 - 90 kg soccer athletes, but it may not work as well for smaller athletes. You can change this cutoff at line 260 OR use the Descriptive Information dropdown to change the selected jump type. Once you're confident in the app's selection, press save and move on to the next trial via the "Select Trial" input. You can manually type a number, use the up arrow key inside the box, or click the little up arrow on the right-hand side.

Some quick insight into how the auto analysis works: quiet standing is set as the first 0.5 seconds (adjustable in the force plate adjustments dropdown) of the trial. The ```changepoints``` package is used to identify the flight time's location. This span is trimmed to remove any ringing in the plates, and the resultant data are used to offset FP1, FP2, and total force. Peak force and peak landing force are used to set the true locations for takeoff and landing via finding the first points where the flight time threshold (currently 10N; can be changed at line 196) is broken. The difference between peak and minimum force (in the case of automatic analysis) is used to determine if the jump is an SJ or CMJ, which leads to different search rules for jump initiation. In the case of SJ, body weight (in N) + 5x the SD of quiet standing is used; the opposite (-5x) is used for CMJ. SJ searches backwards from peak force to find this point, while CMJ searches backward from minimum force. In accordance with Owen et al. (2014) and McMahon et al. (2018), an additional 30ms step back is added.

Here's an example of how the app changes once you've run an analysis. You'll notice there's a new plot below the original. This plot is zoomed in on the trial. There are also three red vertical lines. These lines denote the jump start, takeoff, and landing. Takeoff and landing represent the first and last point below the force threshold, respectively. In this case, the threshold is 20 N, but you can alter this in the code at line 940 (or search for 'threshold'). Jump start is found in accordance with Owen et al. (2014) and McMahon et al. (2018): the point at which force exceeds/falls below 5x the standard deviation of the system mass area (or first second of the data) is found. The point is then moved backward an additional 30 ms.

From there, lots of math happens between lines 287 - 411. What you probably need to know: impulse is determined via the trapezoidal rule (`cumtrapz` from ```pracma```), average RFD is onset to peak force, FP1-FP2 asymmetry are represented by symmetry index values, and the math in the phase analysis chunk (387 - 411) is explained in Dr. Satoshi Mizuguchi's [dissertation](https://dc.etsu.edu/etd/1459/). All these data are saved to a date-stamped CSV in a folder called "Analyses." If the file or folder doesn't exist, the app will create it. 

Once you're done analyzing data, you can stop the application by pressing the stop sign in Rstudio or by closing the browser window.

## Wrapping up

And there you have it. If you've followed along, you should now have some analyzed vertical jump data. If you received errors or ran into other problems, please don't hesitate to contact me: <SamsPerformanceTraining@gmail.com> or [@DrMattSams](https://twitter.com/DrMattSams).