# FlightPassengersData
A simulation of passengers on a flight.


## Load the R file

``` R
source('https://raw.githubusercontent.com/ardimirzaei/FlightPassengersData/master/FlightPassengersManifest.R')
```

## Using the data set
To create a simple database run as
```r
df<-FlightPassengersData()
```
To change how many passengers you may wish to load:
```r
df<-FlightPassengersData(200)
```
Note: the default is 100. 

## Options

The function fits the following options:
``` r
Passengers=100
RandomSeed=NA
plot_Results=TRUE
```

RandomSeed can be set as any number if you want to reproduce a particular data set.
Plot_Results will always show, unless you wish to disable it from showing the results when you run the function. 
