1.  Overview

    The files and subdirectories in this directory facilitate
    reproduction of the results from the paper "Evaluating the effect of the 
    National Hockey League's 3-on-3 rule on shootout occurrences and home team 
    win probability" by Darren Colby and Rylan Tribush. Before attempting to 
    reproduce these results, R version 4.2.1 or greater and the latest versions 
    of RStudio, the Tidyverse metapackage, and the jsonlite packages must be 
    installed.

2.  Directory Structure

    The structure of our project directory is listed below in the format 
    directory: description.

         1. data: data from the NHL API used in our analysis
         
            1.1 all_games.RDS: a dataframe stored in an RDS file for the 
                2011-2012 to 2018-2019 NHL linescores

         2. documents: miscellaneous documents
         
            2.1 Codebook.pdf: a codebook for the data downloaded from the NHL
         
            2.2 stats-api.md: documentation for the NHL API

         3. figures: figures and tables from the paper
         
            3.1 figure1.jpg: figure 1 from the paper
            
            3.2 figure2.jpg: figure 2 from the paper
            
            3.3 figure3.jpg: figure 3 from the paper
            
            3.4 table1.html: table 1 from the paper
            
            3.5 table2.html: table 2 from the paper

         4. scripts: containts R scripts needed to replicate our results
         
            4.1 step1.R: downloads and saves the data
            
            4.2 step2.R: runs Chi-squared analysis and creates plots and figures
            
            4.3 step3.R: runs logistic regression and creates table2
            
        5. .gitignore.txt: list of files to be ignored by Git
        
        6. README.md: this document
        
        7. SportsAnalyticsProject.RProj; RProj file to enable relative paths

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
    from our paper, which will be saved in the figures directory. Finally, 
    repeat these steps using the step3.R file, which will run the logistic 
    regression and generate table 2. Note that reproducing the resulst does not
    require typing anything.
