### `breathtestshiny`: Docker image to analyze gastric emptying from 13C Breath Test

Author: Dieter Menne, Menne Biomed Consulting, Tübingen in cooperation with the University Hospital of Zürich (Benjamin Misselwitz) and Claraspital Basel (Mark Fox)

This is the detailed description for use on the Docker hub.

## Where to find the code

- On Github: https://github.com/dmenne/breathtestshiny

- On Docker Hub: https://hub.docker.com/r/dmenne/breathtestshiny

## What it does

- 13C breath test data for gastric emptying using packages [breathtestshiny](https://github.com/dmenne/breathtestshiny), [breathtestcore](https://github.com/dmenne/breathtestcore) and [breathteststan](https://github.com/dmenne/breathteststan). Figures to import into your clinical reporting system for and tabular reports in Excel/PDF format can be downloaded. [Online demo](https://apps.menne-biomed.de/breathtestshiny/)

- Security: When used online, no data are stored after you close the browser, or when you stop the container.

- We welcome your feedback on data and report formats and your experience with not-so-well-behaved time series, especially ones with multiple peaks.   

## How to run

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
Use localhost:3839](`localhost:3839`) in your browser, or edit the file `docker-compose.yml`.


