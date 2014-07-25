# Draft script

# leer datos
library(ggplot2)
stormdata <- read.csv(bzfile('repdata-data-StormData.csv.bz2'),
                      stringsAsFactors = F)

# extraer columnas.
# fecha. estado. evtype. fatalities. injuries. propdmg cropdmg
todollars <- function(ammount, expfactor) {
    if (expfactor == "K") { ammount * 1000 }
    else if (expfactor == "M") { ammount * 1000000 }
    else if (expfactor == "B") { ammount * 1000000000 }
    else { ammount }
}

storms <- data.frame(as.Date(stormdata$BGN_DATE, '%m/%d/%Y'),
                     stormdata$STATE,
                     as.factor(stormdata$EVTYPE),
                     stormdata$FATALITIES,
                     stormdata$INJURIES,
                     mapply(todollars, stormdata$PROPDMG, stormdata$PROPDMGEXP),
                     mapply(todollars, stormdata$CROPDMG, stormdata$CROPDMGEXP),
                     stringsAsFactors=F
)

names(storms) <- c("date","state","evtype",
                   "fatalities","injuries","propdmg","cropdmg")


# ver N/A. filtrar fechas si hace falta.
# desde 1996 segun http://www.ncdc.noaa.gov/stormevents/details.jsp

# convertir a dolares 2011.

