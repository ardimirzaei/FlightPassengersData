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
To change how many you wish to load:
```r
df<-FlightPassengersData(200)
```
Note: the default is 100. 

