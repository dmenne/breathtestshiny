# breathtestshiny: Web application to fit 13C time series for gastric emptying

Dieter Menne  
Menne Biomed Consulting <http://www.menne-biomed.de>  
[dieter.menne\@menne-biomed.de](mailto:dieter.menne@menne-biomed.de)

![](man/figures/BreathTestShiny.png)

The software was developed in cooperation with the ETH, the Department of Gastroenterology of the University Hospital of Zürich, and Claraspital Basel. Thanks to Andreas Steingötter, Benjamin Misselwitz, Mark Fox and Werner Schwizer.

Shiny web app to fit 13C data with R package [breathtestcore](https://github.com/dmenne/breathtestcore). You can try the [web app](https://apps.menne-biomed.de/breathtestshiny/) online. No data or cookies are stored in the online version, but you can download all results and group-pairwise comparisons for studies.

## Where to find the code

-   On Github: <https://github.com/dmenne/breathtestshiny>

-   On Docker Hub: <https://hub.docker.com/r/dmenne/breathtestshiny>

## Security

No data are stored after you close the browser or when you stop the container.

## Docker image

### Installing Docker

-   Windows users must install Docker Desktop from [Docker store](https://docs.docker.com/desktop/windows/install/). For installation details, see [here](https://docs.docker.com/docker-for-windows/install/).
-   Linux users know how to install Docker anyway.
-   Docker should have at least 2 GB of memory. If you want to build the Docker image, you need at least 4 GB and 2 cores; confusing error messages are being emitted when memory is low.

### Installing `breathtestshiny`

-   From the command line, enter the following to start the container (this is ONE line)

`docker run --name breathtestshiny --restart unless-stopped -p 3838:3838 -d dmenne/breathtestshiny`

-   The first startup needs some time because 1 GB has to be downloaded. Subsequent startups require only a few seconds.

-   Connect to the app with your browser using `localhost:3838`.
