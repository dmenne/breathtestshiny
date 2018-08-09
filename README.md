breathtestshiny: Web application to fit 13C time series for gastric emptying
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

The software is being developed in cooperation with the ETH, the Department of Gastroenterology of the University Hospital of Zürich, and Claraspital Basel. Thanks to Andreas Steingötter, Benjamin Misselwitz, Mark Fox and Werner Schwizer.

Shiny web app to fit 13C data with R package [breathtestcore](https://github.com/dmenne/breathtestcore). You can try the [web app](https://apps.menne-biomed.de/breathtestshiny/) online. No data are stored, but you can download all results and group-pairwise comparisons for studies.

For an easy installation, use the [Docker image](https://hub.docker.com/r/dmenne/breathtestshiny/).


## Shinytest

To generate a new test set, use the following sequence:

* Delete `default_demo_test-expected` and `default_demo_test-current` subdirectories of `inst/shiny/tests`
* Set the working directory to `inst/shiny`
* Run `shinytest::testApp()`
* Check if the files in `inst/shiny/tests/default_demo_test-expected` are what you expect
* Run `shinytest::testApp()` again.

During Check and on Travis CI, only the `json` files are compared to the `expected, because images can look different on different platforms.

For testing on Travis CI, it is important to generated the expected output on a Linux machine, since plots use different left/right ranges. However, this also give slightly different ranges, so that testing with shinytest __currently is not possible__.


 
