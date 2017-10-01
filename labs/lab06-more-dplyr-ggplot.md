Lab 6: More dplyr, ggplot2, and files' stuff
================
Gaston Sanchez

> ### Learning Objectives:
>
> -   Learn about `"dplyr"` pipelines
> -   Get to know the pipe operator `%>%`
> -   Chain dplyr operations with the piper
> -   Produce basic plots with `ggplot()`
> -   Saving plot images
> -   Exporting R output (as is)
> -   Exporting tables

------------------------------------------------------------------------

Manipulating and Visualizing Data Frames
----------------------------------------

In this lab, you will continue manipulating data frames with `"dplyr"`, and plotting graphics with `"ggplot2"`. In addition, you will also use various functions to export (or save) tables, images, and R output to external files.

While you follow this lab, you may want to open these cheat sheets:

-   [dplyr cheatsheet](../cheat-sheets/data-transformation-cheatsheet.pdf)
-   [ggplot2 cheatsheet](../cheat-sheets/ggplot2-cheatsheet-2.1.pdf)

### Filestructure

To help you better prepare for HW03, we want you to practice working with a more sophisticated file structure. Follow the steps listed below to create the necessary subdirectories like those depicted in this scheme:

        lab06/
          README.md
          data/
          code/
          images/

-   Open a Bash terminal (e.g. command line or GitBash)
-   Change your working directory to a location where you will store all the materials for this lab
-   Create a directory `lab06` for the lab materials

    ``` bash
    mkdir lab06
    ```

-   Change directory to `lab06`

    ``` bash
    cd lab06
    ```

-   Make other subdirectories: `data`, `code`, and `images`

    ``` bash
    mkdir data code images
    ```

-   List the contents of `lab06` to confirm that you have all the subdirectories

    ``` bash
    ls
    ```

-   Initialize (or create) an empty `README.md` text file

    ``` bash
    touch README.md
    ```

-   Change directory to the `data/` folder

    ``` bash
    cd data
    ```

-   Download the data file with the command `curl`, and the `-O` option (letter O)

    ``` bash
    curl -O https://github.com/ucb-stat133/stat133-fall-2017/raw/master/data/nba2017-players.csv
    ```

-   Use `ls` to confirm that the csv file is in `data/`
-   Use *word count* `wc` to count the lines of the csv file

    ``` bash
    wc nba2017-players.csv
    ```

-   Take a peek at the first rows of the csv file with `head`

    ``` bash
    head nba2017-players.csv
    ```

-   Take a peek at the last 5 rows of the csv file with `tail`

    ``` bash
    tail -n 5 nba2017-players.csv
    ```

------------------------------------------------------------------------

R script
--------

-   Once you have the filestructure for this lab, go to RStudio and open a new `R` script file (do NOT confuse with an `Rmd` file).
-   Save the `R` script file as `lab06-script.R` in the `code/` folder of `lab06/`

R script files are used to write R code only, using R syntax. In other words, you should NOT use Markdown syntax or LaTeX inside an R script file. Why? Because if you run the entire script, R will try to execute all the commands, and won't be able to recognize Markdown, LaTeX, yaml, or other syntaxes.

### File Header

Let's start with some good coding practices by adding a header to the `R` script file in the form of R comments. In general, the header section should contain a title, a description of what the script is about, what are the inputs, and what are the main outputs produced when executing the code in the script. Optionally, you can also include the name of the author, the date, and other details. Something like this:

    # Title: Short title (one sentence)
    # Description: what the script is about (one paragraph or two)
    # Input(s): what are the main inputs (list of inputs)
    # Output(s): what are the main outputs (list of outputs)
    # Author: First Last
    # Date: mm-dd-yyyy

Think of the header of a script file as the yaml header used in `Rmd` files. The header should be the very first thing that appears at the top of the script file. Personally, I like to surround the header in my R script files with some delimiting characters that help the reader to visually identify main parts of the script. Here's a hypothetical example of a header:

``` r
# ===================================================================
# Title: Cleaning Data
# Description:
#   This script performs cleaning tasks and transformations on 
#   various columns of the raw data file.
# Input(s): data file 'raw-data.csv'
# Output(s): data file 'clean-data.csv'
# Author: Gaston Sanchez
# Date: 10-05-2017
# ===================================================================
```

Another good coding practice is to avoid writing very long lines of code. Most coding style guides stick to a maximum line width of 80 characters, and this is the [magic number](https://softwareengineering.stackexchange.com/questions/148677/why-is-80-characters-the-standard-limit-for-code-width) that I also use for my scripts.

**Your turn**: Include a header in your R script file, respecting the width limit of 80 characters.

### Required Packages

The next thing that you need to include in your script file are the required packages. Not all script files need packages, but many do. When this is the case, loading the packages should be the first lines of code to be executed.

Include the commands to load the following packages in your script:

``` r
# packages
library(readr)    # importing data
library(dplyr)    # data wrangling
library(ggplot2)  # graphics
```

In addition to loading the packages, sometimes you will also need to load code from other script files. We won't do that today, but you should know that this is very common as the complexity and size of your projects grow.

### Exporting some data tables

After the header, and the loading-packages sections, the next part in your script involves importing the data.

-   Use `read_csv()` from the package `"readr"` to import the data `nba2017-players.csv` in R.
-   Create one data frame `warriors` by selecting rows---e.g. `filter()`---of Golden State Warriors, arranging rows by salary (increasingly).
-   Use the function `write.csv()` to export (or save) the data frame `warriors` to a data file `warriors.csv` in the `folder/` directory. You will need to use a relative path to specify the `file` argument.
-   Create another data frame `lakers` by selecting rows of Los Angeles Lakers, this time arranging rows by experience.
-   Now use the function `write_csv()` to export (or save) the data frame `lakers` to a data file `lakers.csv` in the `folder/` directory. You will also need to use a relative path to specify the `file` argument.
-   Inspect the contents of the `data/` folder and confirm that the csv files are there.

### Exporting some R output

After exporting the tables to the corresponding csv files, you will produce some summary statistics, and then save the generated output to external text files. To do this, you will have to learn about the `sink()` function, which sends R output to a specified file.

Say you are interested in exporting the summary statistics of `height` and `weight`, exactly in the same way they appear on the console:

``` r
summary(dat[ ,c('height', 'weight')])
```

    ##      height          weight     
    ##  Min.   :69.00   Min.   :150.0  
    ##  1st Qu.:77.00   1st Qu.:200.0  
    ##  Median :79.00   Median :220.0  
    ##  Mean   :79.15   Mean   :220.2  
    ##  3rd Qu.:82.00   3rd Qu.:240.0  
    ##  Max.   :87.00   Max.   :290.0

One naive option would be to manually copy the text displayed on the console, and then paste it to a text file. While this may work, it is labor intensive, error prone, and highly irreproducible. A better way to achieve this task is with the `sink()` function:

``` r
# divert output to the specified file
sink(file = 'summary-height-weight.txt`)
summary(dat[ ,c('height', 'weight')])
sink()
```

The fist call to `sink()` opens a connection to the specified file, and then all outputs are diverted to that location. The second call to `sink()`, i.e. the one without any arguments, closes the connection.

**Your turn:**

-   Export the `summary()` of the entire data frame `warriors` to a text file `summary-warriors.txt`
-   Export another `summary()` of the entire data frame `lakers` to a text file `summary-lakers.txt`