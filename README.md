1.  Overview

    The files and subdirectories in this directory facilitate
    reproduction of the results from the paper “Manpower, Overtime, and
    Shootouts in American professional hockey” by Darren Colby and Rylan
    Tribush. Before attempting to reproduce these results, R version
    4.2.1 or greater and the latest versions of RStudio, the Tidyverse
    metapackage, and the jsonlite packages must be installed.

2.  Directory Structure

    This directory contains the following subdirectories:

         1. data

         2. documents

         3. figures

         4. scripts

    Within the data folder is the data we dowloaded from the NHL using
    the NHL’s API. The documents folder contains a codebook. In the
    figures folder are the figures from the paper and the scripts folder
    contains the scripts we used to download and clean our data, conduct
    our analysis, and produce the figures in our paper. In addition, the
    directory contains an RProj file to enable the use of relative paths
    and avoid hardcoding filepaths when analysis is conducted on a
    different maching. It also contains a gitignore file, which we used
    with GitHub.

3.  Reproducing the Results

    To reproduce the results open the project by double clicking on the
    SportsAnalyticsProject.RProj file, which will open the project in
    RStudio. Highlight all the code in the step1.R script and run it.
    Not that this will take a while because the script fetches data from
    every regular season game played in the NHL between 2011 and 2019.
    Once the script is done running, restart the R interpreter,
    highlight all the code in step2.R and run it. Running this script
    will load the data downloaded in the step1.R script, transform it,
    reproduce the analysis from our paper, and create the same figures
    from our paper, which will be saved in the figures directory.
