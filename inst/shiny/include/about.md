This application was written by Dieter Menne at [Menne Biomed Consulting](https://www.menne-biomed.de), in cooperation with the Department of Gastroenterology of the University Hospital of Zürich and Claraspital Basel. Thanks to Benjamin Misselwitz, Mark Fox, Werner Schwizer and Andreas Steingötter.

- All code is GPL-3 Open Source and available on github.
- The core routines to fit data are in package [breathtestcore](https://github.com/dmenne/breathtestcore), also on [CRAN](https://cran.r-project.org/web/packages/breathtestcore/index.html). [Code examples](https://dmenne.github.io/breathtestcore/reference/nlme_fit.html) are provided to run an analysis under R.
- For faster compilation, the Bayesian [Stan](http://mc-stan.org/)-based fit functions are moved to package [breathteststan](https://github.com/dmenne/breathteststan), also on [CRAN](https://cran.r-project.org/web/packages/breathteststan/index.html).
- Code for the [Shiny web app](https://shiny.rstudio.com/) is under development and currently only on [github](https://github.com/dmenne/breathteststan). 
- For a runnable demo of the web app (i.e. the program you are looking at), see [breathtestshiny](https://apps.menne-biomed.de/breathtestshiny).
- [R packages](http://cran.r-project.org/) shiny, rstan and nlme are used; for a full list, see [here](https://github.com/dmenne/breathtestcore/blob/master/DESCRIPTION) and [here](https://github.com/dmenne/breathteststan/blob/master/DESCRIPTION").
- To avoid installation hassles, use the Docker container on  [Docker hub](https://hub.docker.com/r/dmenne/gastro-docker/) or [github](https://github.com/dmenne/gastro-docker) which bundles packages `dmenne/gastempt` and `dmenne/breathtestshiny`.

Package versions used:
