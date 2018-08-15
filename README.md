breathtestshiny: Web application to fit 13C time series for gastric emptying
===========================================

Dieter Menne   
Menne Biomed Consulting Tübingen, Germany    
http://www.menne-biomed.de   

dieter.menne@menne-biomed.de 

The software is being developed in cooperation with the ETH, the Department of Gastroenterology of the University Hospital of Zürich, and Claraspital Basel. Thanks to Andreas Steingötter, Benjamin Misselwitz, Mark Fox and Werner Schwizer.

Shiny web app to fit 13C data with R package [breathtestcore](https://github.com/dmenne/breathtestcore). You can try the [web app](https://apps.menne-biomed.de/breathtestshiny/) online. No data are stored in this online version, but you can download all results and group-pairwise comparisons for studies.

## Where to find the code

- On Github: https://github.com/dmenne/breathtestshiny

- On Docker Hub: https://hub.docker.com/r/dmenne/breathtestshiny

## Security

When used online, no data are stored after you close the browser, or when you stop the container.

## Docker image

### Installing Docker 
- Docker works best on Windows 10 64-bit where it can be run as a native application;  on earlier Windows 64-bit versions, Docker requires the Oracle Virtual Box which is automatically installed. 
- For Windows 10, you can get the installer from the [Docker store](https://store.docker.com/editions/community/docker-ce-desktop-windows). For earlier versions of Windows, use the [Docker toolbox](https://www.docker.com/products/docker-toolbox). For installation details, see [here](https://docs.docker.com/docker-for-windows/install/).  
- Linux users know how to install Docker anyway. 
- Docker should have at least 2 GB of memory; on Windows, use Settings from the Docker tray icon. 
- Have plenty of memory available to Docker when you want to build the package from sources; 10 GB might be required. When you see segmentation faults during C++ builds, this indicates a lack of memory.

### Installing `breathtestshiny` 

- From the command line, enter the following to start the container

```
docker run --name breathtestshiny  -p 3838:3838 -d dmenne/breathtestshiny
```
- The first startup needs some time because 3 GB of applications are downloaded. Subsequent startups require only a few seconds.
- Connect to the apps with your browser: [localhost:3838](`localhost:3838`). <- This link does not work when your read this from github or the Docker hub, only on your local installation.

### Using docker-compose

Manually copy the file `docker-compose.yml` to your work directory, and start the container  with

`# docker-compose up -d` 

By default, in this installation port 3839 is used, to avoid clashes with a Shiny server running on your server.
Use [localhost:3839](`localhost:3839`) in your browser, or edit the file `docker-compose.yml`.

## Shinytest

A test to be used with [shinytest](https://github.com/rstudio/shinytest) is included.

To generate a new test set, use the following sequence:

* Delete `default_demo_test-expected` and `default_demo_test-current` subdirectories of `inst/shiny/tests`
* Set the working directory to `inst/shiny`
* Run `shinytest::testApp()`
* Check if the files in `inst/shiny/tests/default_demo_test-expected` are what you expect
* Run `shinytest::testApp()` again.

During Check and on Travis CI, only the `json` files are compared because images can look different on different platforms.

For testing on Travis CI, it is important to generate the expected output on a Linux machine, since plots use different left/right ranges. This also give slightly different ranges, so that testing with shinytest on Travis CI __currently is skipped__.



