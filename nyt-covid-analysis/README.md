# New York Times COVID-19 Data

For my students working on COVID-19 projects, this repo gets them started with the [New York Times data](https://github.com/nytimes/covid-19-data) and illustrates the use of submodules in git.

## Prereqs and Setup

### R Packages

You will need to install the following packages:

```{r eval = FALSE}
install.packages(c(
  "tidyverse",
  "reactable",
  "mapview"
))
devtools::install_github("UrbanInstitute/urbnmapr")
```


Some of these packages require package **sf**, which cannot be installed on the GC RStudio Server.  You will need to work on your own machine.


### Create Your RStudio Project

You will work with a repository that contains the NYT data as a submodule, so the process of creating an RStudio project will be different than usual.

1. Open your terminal and `cd` to wherever you keep your git-controlled projects.
2. Clone my repository along with its submodule:

    ```
    git clone --recursive https://github.com/homerhanumat/nyt-covid-analysis.git
    ```
    
3.  In Rstudio ask to create a new Project, in an existing directory.  Specify the directory that was created by the clone in the previous step.

### Updating

The purpose of having the NYT data as a submodule in our repository is to permit you to keep you to track your analysis work separately from the data on which your analysis is based.  With your temrinal's working directory set to the root directory of your project, git commands such as `add`, `commit` and (should you choose to create a remote on Github) `push` will apply to your project only and will have no effect on the contents of the `covid-19-data` folder within it.

Always rememeber that the `covid-19-data` directory in your repo is a git submodule that links to the NYT repo.  Don't modify anything in this directory!

The New York Times updates its repository every day, and you will want to keep up with those changes.  The submodule allows you to accomplish this without interfering with any of your analysis work.   In order to update the data, take the following to steps:

1. Run the following command in your terminal:

    ```
    git submodule update --remote
    ```

2. Then source the `import.R` file in your project.

The data tables in your `data` directory will be updated.  To do your analysis, you'll load them into your Global Environment:

```{r eval = FALSE}
load("data/nyt_counties.rda")
load("data/nyt_states.rda")
```

Consult `getting_started.Rmd` for ideas about where to go from here.
