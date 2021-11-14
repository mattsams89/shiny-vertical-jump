# Shiny Vertical Jump Analysis

[![DOI](https://zenodo.org/badge/119111981.svg)](https://zenodo.org/badge/latestdoi/119111981)

An open-source solution for vertical jump analysis

# Table of contents

-   [What is this?](#what-is-this)
-   [Downloading R / Installing Required Packages](#downloading-r)
-   [Running the app](#running-the-app)
-   [Preparing your data](#preparing-your-data)
-   [Analyzing your data](#analyzing-your-data)
-   [Data interpolation](#interpolated-data)
-   [Wrapping up](#wrapping-up)

## What is this?

This Shiny app is geared toward strength and conditioning professionals and researchers who can't afford vertical jump analysis software and those who don't have the coding knowledge to design their own analyses. It's my hope these individuals will have an alternative to analyzing their force-time data in Excel (we've all been there). This allows for more powerful analysis than just simply jump height, peak force, etc. and allows for much faster processing of large amounts of data. Additionally, this app is publicly available to take the cover off the black box that is force-time data analysis. A freely visible set of filtering, plotting, and variable calculation functions allow for consistent analysis procedures between users.

## Downloading R

(I haven't tested compatibility with MacOS or Linux. Feel free to share any hiccups you encounter if you use either platform.) The first step in running an R-based program is to, well, have R installed on your computer. The most up-to-date version of R can be found [here](https://cran.r-project.org/bin/windows/base/). The app was last tested against **R version 4.1.0** and packages that were current as of **2021-07-17**. I can't promise everything will work correctly post-installation, as package updates can randomly break parts of the code. If you ever run into any errors, please [email me](mailto:matt.l.sams@gmail.com) and I will look into the problem and release a hotfix ASAP. Once you have R installed, you also need [RStudio](https://www.rstudio.com/products/rstudio/download/#download). You interact with base R via an ugly, bare-bones IDE, whereas RStudio has all sorts of bells and whistles to make your experience more tolerable.

Aside from R and RStudio, you will need to install a number of support packages before the app will function. Packages expand the functionality of R past the `base` functions and can be written to solve virtually any issue. Most dependencies for this app are used to manipulate and visualize the data. Packages are installed via the `install.packages()` function. E.g. `install.packages("data.table")`. Make sure to enclose the package names in '' or "", although newer versions of RStudio may automatically prompt you to install missing packages when you open the global.R file. The currently required packages are the following: `pacman`, `shiny`, `shinydashboard`, `data.table`, `tidytable`, `signal`, `changepoint`, `pracma`, `plotly`, `knitr`, `shinyjs`, and `kableExtra`. Installing these packages should install any other requisite packages, but if the app refuses to load or crashes on certain functions, examine the error message before emailing me. If a package is missing, the error will tell you.

## Running the app

Once you have R and all the required packages, you can download the app as a .zip folder (click the green 'Clone or download' button on GitHub and select 'Download ZIP') or manually copy the raw code (App \> app.R and global.R) to global.R and app.R files. The most recent release does away with the online version of the app (hostable on shinyapps.io) in favor of an offline-only version. If that's a problem, please reach out and I'll see what I can do.

If you download the app as a .zip, you'll need to extract the files. Importantly, wherever the global.R and app.R files are stored will determine where the analysis files are created. So regardless of whether you download the .zip file or manually copy the code, be sure to save the files to the correct directory. For instance, if you work with several teams and want to keep their data separate you might save copies to folders for each team.

If you decide to have multiple copies of the app, be sure to load the correct one by selecting the correct app.R in the file viewer pane in the bottom right-hand corner of your R Studio environment.

![App location](/Tutorial/app_location.png)

I normally get around this by having a different project for each team. You can create new projects by selecting your project's name \> New Project and following the prompts to add a new project file.

![New project](/Tutorial/new_project_1.png)

![New folder](/Tutorial/new_project_2.png)

![Project type](/Tutorial/new_project_3.png)

![Project name](/Tutorial/new_project_4.png)

Again, you only have to follow these steps if you have multiple copies of the file dedicated to different teams. If you only have a single copy of the file, don't worry about creating a new directory, etc. Just open the app.R in your current R studio project / environment.

The app can be run by either pressing ctrl + shift + enter or by pressing the 'Run App' button that appears.

## Preparing your data

Unlike previous versions of the app that only worked with Pasco Capstone data, the newest version supports multiple file formats. Supported formats now include Pasco, wide format, long format, and single trial. With the exception of Pasco export data, the other files should all follow the same format. The [example data](https://github.com/mattsams89/shiny-vertical-jump/tree/master/Example%20Data) folder contains examples for each supported layout. Your data *must* follow one of these formats because the file parser function will not know how to handle other layouts. For Pasco data, be sure your vertical force data are named some variant of "Normal" force; the parser is looking for columns that include "Normal" in their name and will remove any other columns.

## Analyzing your data

**You may notice a few out of date images and gifs. I'm working on updating these.**

Once you have everything installed and have exported your data, all that's left is the analysis. With the most recent release (v2.1.0), things aren't *quite* as automated as they were before, but where we lost automation, we gained fewer headaches...don't think about that statement too hard.

Run the app by pressing ctrl + shift + enter or by pressing the 'Run App' button.

You'll see the following in the left-hand panel:

1.  A file selector  
    Currently, files up to 40 MB are supported. You can change this in the app settings by altering line 14 of the global.R file: `options(shiny.maxRequestSize = 40 * 1024 ^ 2)`.
2.  A trial selector
3.  Dropdowns for plate layout (**NEW**), filter application, jump type, and jump start location
4.  Inputs for the athlete's name, bar load, sampling frequency, quiet standing length (in seconds), and calibration values for FP1 and FP2.

Start by selecting your file layout in the "File Type" input. If your data don't match the "typical" layout of FP1 being left and FP2 being right, you can change that by changing the Plate Layout dropdown to Right-Left. **NEW** Next, enter your sampling frequency in Hz (e.g. 1000, 1200, 1500) and then press the "Analyze Data" button. For those of you who have used the app previously, this is a little different from the original automatic method; the app was crashing when trying to enter new sampling frequencies, so starting the analysis with a button click seemed like the easiest fix.

**Importantly, with the new button, you'll need to press the button again if you 1) upload new data, 2) change the plate layout, 3) change the filter or calibration equation, or 4) change the sampling rate.** Each of the aforementioned inputs control functions in the analysis process that are dependent on the sampling rate input. 

![Upload](/Tutorial/new_upload.png)

The app is smart enough to (hopefully) auto-recognize the jump type and find quiet standing. The former is accomplished by examining the difference between body weight during quiet standing and the minimum force prior to peak force, whereas the latter is accomplished by finding the area with lowest variance in the data prior to jump initiation. If the app incorrectly guesses the jump type, you can manually set the jump type with the "Select Jump Type" dropdown. Likewise, if you don't like what the app selected for quiet standing, you can manually "brush" the top plot by clicking and dragging across the data.

![Brushing](/Tutorial/brushed_trial.gif)

The height of your brush doesn't matter; only the X-axis coordinates of the brush are used to update the trial information. When brushing the plot, make sure to grab from quiet standing to any point after peak landing force. There's a good chance the app will crash otherwise. In trials where the athlete was squirrelly prior to takeoff, grabbing a full second for quiet standing may not be feasible. In such cases, you can adjust the quiet standing length with the "Quiet Standing" input on the left. The default is 1 second, but it accepts values from 0.2 - 1 in 0.1 second increments.

Once your trial is selected correctly, a quick glance at the bottom plot and plots on the right will tell you whether the app recognized the jump type correctly. I rather arbitrarily chose a 250N difference between quiet standing and minimum force prior to peak force as the delineation between SJ and CMJ. That worked well with athletes ranging from 65kg - 110kg, but it may not work as well for smaller athletes.

Because this wasn't very clear in earlier versions of the app, pay special attention to the "Jump Start Location" and "Check Inverse Threshold?" inputs. By default, they're set to "5SD - BW" and "Yes." Briefly, these determine how the app defines the jump start. As you can see in some of the example trials, the athletes produced a pre-SJ countermovement or a slight uptick in their force prior to performing a CMJ. In either case, we've missed the *true* jump start location. Setting the Inverse Threshold to Yes causes the app to look back 100ms to see if the trial violates the "inverse threshold," or body weight +/- 5*SD. If it does, the app steps back to the last point before the athlete broke the inverse threshold.

From there, the jump start is affected by the Jump Start Location input. The default (5SD - BW) finds the first point the force-time curve returns to body weight, while (5SD - 30ms) steps back an arbitrary 30ms (in line with some of the recommendations that are out there). I have found 5SD - BW isn't always perfect so I may change "body weight" to mean +/- 1SD of body weight, but have no concrete plans right now.

Alternatively, you can disable the inverse threshold check by setting the input to "No."

**Back to our regularly scheduled program.**

Some quick insight into how the analysis works: the `findpeaks` function from `pracma` determines the locations for peak takeoff force and peak landing force. These points are confirmed by checking the number of points \< 10N between pairs of identified peaks (202 - 903ms, or \~ 5 - 100cm, represent the cutoffs). Based on the literature, initial flight time and landing are found by determining the first and last points \< 10N between these two peaks. The middle 50% of the data between initial takeoff and landing are used to calculate the mean + 5\*SD of the unloaded plates' force values, and true takeoff and landing are the first and last points above this threshold value. `cpt.meanvar` is used to find a 1 second interval with the lowest variance prior to takeoff; this is the app's best guess for the quiet standing phase. Like I mentioned above, jump type is guessed by examining the difference between quiet standing mean force and minimum force prior to takeoff. The app employs different rules for SJ and CMJ. For SJ, quiet standing mean force + 5x the SD of quiet standing is used as the jump initiation threshold, whereas the opposite is used for CMJ.

Once jump start is identified, lots of math happens. What you probably need to know: impulse is determined via the trapezoidal rule (`cumtrapz` from `pracma`), average RFD is onset to peak force, FP1-FP2 asymmetry are represented by % symmetry index values ([Left - Right] / [Left + Right] * 100), and the method for determining unweighting, braking, and propulsion is explained in Dr. Satoshi Mizuguchi's [dissertation](https://dc.etsu.edu/etd/1459/).

**NEW**

The most recent release adds two additional RFD variables to the CMJ analysis--Braking RFD and Deceleration RFD. These variables match the definitions of some commercial jump analysis software providers, where "braking" is defined as minimum force -> F@0V and "deceleration" is defined as force at minimum velocity -> F@0V. These definitions are separate to the phase names used in the app (unweighting, braking, propulsion)

## Data interpolation

![Interpolation](/Tutorial/interpolation.gif)

The biggest additions in version 2.0 are the ability to export raw force-time data and interpolate those data to new lengths (aka, "time normalization" or "registration"). Two methods are available, linear interpolation and piecewise linear interpolation. The former interpolates a given force-time curve to a specified length, while the latter allows you to define lengths for each of the primary CMJ phases (unweighting, braking, and propulsion). In either case, these methods help prep your data for analysis methods such as statistical parametric mapping (SPM), which is the new hotness in the vertical jump analysis world.

Importantly (and hopefully obviously), piecewise interpolation is only available for CMJs--linear interpolation is the only method available for SJs. In addition to defining lengths for each phase of the CMJ, it's also possible to lump unweighting and braking together into a single "stretching" phase.

I will say I wasn't completely sure on the correct way to interpolate the force values at the beginning of braking and propulsion, so the current method adds the last point from the previous phase and interpolates the data to your specified length + 1 before immediately removing the first point from the resultant interpolated data. This causes the interpolated curve to better match the shape of the original data. If I'm off on my thinking here, please let me know!

I don't have plans to add additional registration methods (e.g. dynamic time warping), but if there's interest, I'll see what I can do.

## Wrapping up

If you receive errors or run into other problems, please don't hesitate to contact me: [matt.l.sams@gmail.com](mailto:matt.l.sams@gmail.com) or [\@DrMattSams](https://twitter.com/DrMattSams).
