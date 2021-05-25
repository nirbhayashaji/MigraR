
# MigraR

## How to cite

- Nirbhaya Shajia, Florbela Nunes, M.Ines Rocha, Elsa Ferreira Gomes, Helena Castro. MigraR: an open-source, R-based application for analysis and
quantification of cell migration parameters. Computer Methods and Programs in Biomedicine (Submitted). 


MigraR is a graphical user interface (GUI) software  implemented in R using the Shiny framework. MigraR is accessible online via the [Shinyapps.io server](https://nirbhaya-shaji.shinyapps.io/migrar/). It also runs locally in MS Windows, Linux and macOS and requires prior installation of [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download/). 

MigraR is cross-platform, with an intuitive graphical user interface, running from open-source R and RStudio. With no need for expert knowledge in mathematical data analysis software, MigraR drastically simplifies the plotting and analysis of migration parameters and cell trajectories. 

MigraR was developed to assist biomedical researchers in the analysis of cell trajectories and migration parameters. The input of MigraR are data sheets of x,y coordinates along time that are delivered by upstream image processing and cell tracking software. Accepted formats are xlxs, csv and txt.  

## Installation/Running

MigraR sources can be downloaded from [GitHub](https://github.com/nirbhayashaji/MigraR.git). To execute MigraR locally, users must download the files **server.R** and **ui.R** to the working directory of RStudio and double click on the **server.R** file or open it using the file browser. The file will open in RStudio, wherefrom it can be executed by clicking _Run App_. MigraR will open in your default browser. To upload the cell tracking data, users simply have to click on the _Browse_ button listed first on the left panel as shown in the below figure:

![plot](https://github.com/nirbhayashaji/MigraR/blob/main/images/Browse.png)

Users must be connected to the internet for the initial run so that the required packages can be installed. From there, no internet connection is necessary for executing the application.

## Data sets

The data sources of MigraR are files (txt, csv or xlsx format) generated by dedicated cell-tracking software. The structure of the data is as shown below:

![plot](https://github.com/nirbhayashaji/MigraR/blob/main/images/DataStructure.png)

In these files, data must be organized in four columns, as follows:
- column A) x coordinates
- column B) y coordinates
- column C) z coordinates
- column D) time
- column E) TrackID (i.e., the cell track identifier, attributed by the cell tracking software). 


Some cell tracking software might also deliver $z$ coordinates listed in one additional column – column C. If the data file does not display $z$ coordinates, users must introduce one mock (empty or randomly filled) column C, in order to get the data processed in MigraR. The upload of input files to MigraR is performed by clicking the _Browse_ button bellow the _Choose File_ text. 

## The four windows of MigraR
MigraR offers an intuitive graphical user interface for plotting and calculating cell migration parameters. MigraR automatically calculates velocity, straightness and direction of movement based on the spatial and temporal coordinates listed on the uploaded data file. The workflow of MigraR is organized in four windows _Trajectories_, _Velocity_, _Straightness_, and _Direction_, which can be selected from the respective tabs at the top of the large right side panel.

### The **Trajectories** window
The **Trajectories** window (accessible from the _Trajectories_ tab). The **Trajectories** window depicts the visual projection of single-cell trajectories. These can be visualized according to three _View Mode_ options, available on the left panel:

![plot](https://github.com/nirbhayashaji/MigraR/blob/main/images/Trajectories.png)

- _Normal_ view mode is the default option, which provides a close-to-real projection of cell trajectories along the x,y axes
- _Track ID_ mode, individual cell trajectories are distinguished based on color
- _Rose Plot_ option, the starting positions of each cell trajectory are normalized to x,y origin (0,0). Scales of x,y axes can be adjusted using scroll bars on the bottom-right.

By clicking on the _Download plot_ button at the bottom of the charts, users can download the plot as an image file of **.png** ype.

### The _Velocity_ and _Straightness_ windows

The _Velocity_ and _Straightness_ windows display box and whisker charters, plotted from the velocity and straightness values that MigraR computes for the set of cell trajectories under analysis. Velocity values refer to the average velocity that each cell assumes along its trajectory. Straightness values refer to the behavior of each cell between the first and the last time point of its trajectory. By pressing the _Download plot_ buttons, users can generate and save the plot as **.png** files.

![plot](https://github.com/nirbhayashaji/MigraR/blob/main/images/VelocityStraightness.png)

_Download the Velocity data_ and _Download the Straightness data_ buttons generate **.csv** files listing the values of velocity and straightness displayed on the corresponding plots. These **.csv** files can be used for subsequent comparative and statistical analyses between different data sets using dedicated software.

### The _Direction_ window

The _Direction_ window is presented with two options, which can be chosen from the radio buttons on the left panel. On the first, _Direction of Trajectories,_ users can visualize and download plots where cell trajectories appear in different colors based on the direction of the movement. These cell trajectories plots are available in both their _Normal View_ and _Rose Plot_ modes. 

![plot](https://github.com/nirbhayashaji/MigraR/blob/main/images/Directions.png)


In the first pair of plots, cells are colored with a red-to-yellow gradient according to their horizontal directional choices: cells moving to the left are colored red; cells moving to the right are colored yellow. In the second pair of plots, cells are colored with a blue-to-green gradient according to their vertical directional choices: cells moving to the up are colored blue; cells moving to the down are colored green. 

The second option, _Angle of Trajectories_, shows the frequency of cell trajectories moving within the range of  $0-360\degree$ angles, over intervals of 4 degrees. Data are plotted both as column chart _Angle Frequency_ and _Rose Plot_. These plots, as well as the corresponding data sets are available for download using buttons similar to the ones in the previous windows. 


### The adjustments function of MigraR

All MigraR windows run in parallel with adjustment functions, which can be operated from the left side panel of the graphical interface. Here, users can find three sliders to filter the data sets based on the following parameters: Time, Velocity (Speed), and Straightness.

