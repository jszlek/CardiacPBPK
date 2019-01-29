/**
*
* CardiacPBPK v0.9 - Cardiac mechanistic physiologically based pharmacokinetic simulation application
Copyright (C) 2018 

Authors: Zofia Tylutki, Jakub Szlęk


Affiliation: 
Jagiellonian University Medical College,
Faculty of Pharmacy,
Medyczna 9 st.,
30-688 Kraków
Poland

Bugs, issues, please e-mail to maintainer
Jakub Szlęk: j.szlek@uj.edu.pl

Copyright (C) 2018

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General 
Public License as published by the Free Software Foundation, either version 3 of the License,
or (at your option) any later version.
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this program.
If not, see <http://www.gnu.org/licenses/>.

========================
REQUIREMENTS
========================
1) R environment version >= 3.3.0 (for download check https://cran.r-project.org/)
2) Additional R packages: shiny, shinyjs, DT, shinyBS, dplyr, plyr, ggplot2, ggthemes, distr, data.table, deSolve


========================
INSTALLATION AND RUNNING
========================

1)  Download the tar.gz archive from the https://sourceforge.net/projects/cardiacpbpk/files/ or clone github repository (if you have git packages installed in konsole/terminal type ‘git clone https://github.com/jszlek/CardiacPBPK.git’). It is also possible to download master branch from github (https://github.com/jszlek/CardiacPBPK/archive/master.zip)
2) Unpack downloaded package (on linux-like systems use "tar xzvf (...).tar.gz -C /path/to/my/.../folder")
3) Run the konsole/terminal or cmd.exe
4) Navigate to the folder (on linux-like systems use "cd /path/to/my/.../folder")
5) On Windows run the run_R.bat file or on linux-like systems run run_R.sh (make sure the *.bat or *.sh files are executable)

Alternativley:
5) Run R console (on linux type "R", on Windows 
6) Check if you are in the folder by typing "getwd()" in R console
7) Load shiny library by typing "library(shiny)"
8) Run the shiny application by typing "runApp()"
9) This command should run the app in a browser

*
*
**/
