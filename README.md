# Installation of the Code Repository

## Setting up the Ubuntu VM

First you need to get your basic prerequisites:

```
sudo apt update
sudo apt -y install r-base gdebi-core
```

## Setting up new R Environment

```
sudo apt install r-base
```

You can check the version using 

```
R --version
```

Then, you want to head to the [RStudio Website](https://rstudio.com/products/rstudio/download/) and download the Ubuntu 18 file and save it to your downloads folder.

Then, using `gdebi` install RStudio and it's dependencies:

```
sudo gdebi <rstudio local file here>
```

## Setting up the RStudio Project

Some files in the project won't be tracked, which includes the RStudio project file, so a new project will have to be created every time you set up a new device.

## Installing RStudio Dependencies

First, open the `app.js` file and install the dependencies that the file needs. You will need to run the following commands in order to prevent the errors and have the right packages for the installation to go through.

```
sudo apt-get install curl
sudo apt-get install libcurl14-openssl-dev
sudo apt-get install libssl-dev
sudo apt-get install libxml2-dev
sudo apt-get install libgdal-dev
```
