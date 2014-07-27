
# agregado por tipo
fatlpertype <- aggregate(fatalities ~ evtype,
                    data=storms96[storms96$fatalities > 0,],
                    FUN=sum)

injrpertype <- aggregate(injuries ~ evtype,
                         data=storms96[storms96$injuries > 0,],
                         FUN=sum)

dmgspertype <- aggregate(damages ~ evtype,
                         data=storms96[storms96$damages > 0,],
                         FUN=sum)



# eventos sin victimas
round(sum(storms96$fatalities != 0 | storms96$fatalities != 0)
            / nrow(storms96) * 100, 1)

# muchas victimas
storms96[storms96$fatalities >= 50,]
# joplin tornado may22 2011  150.


# 10 worst.
worstfatl <- order(fatlpertype$fatalities, decreasing=T)
worstinjr <- order(injrpertype$injuries, decreasing=T)

# per year.
fatlpertype$fatalitiesperyear <- round(fatlpertype$fatalities / 6)
injrpertype$injuriesperyear <- round(injrpertype$injuries / 6)

head(fatlpertype[worstfatl, c("evtype","fatalitiesperyear")], n=10)
head(injrpertype[worstinjr, c("evtype","injuriesperyear")], n=10)


# damages

worstdmgs <- order(dmgspertype$damages, decreasing=T)
# millions per year
dmgspertype$damagesperyear <- round(dmgspertype$damages / 6000000)
head(dmgspertype[worstdmgs, c("evtype","damagesperyear")], n=10)


# per state
dmgspertypestate <- aggregate(damages ~ evtype + state,
                         data=storms96[storms96$damages > 0,],
                         FUN=sum)
maxdmgsperstate <- aggregate(damages ~ state,
                             data=dmgspertypestate, FUN=max)
worststate <- merge(maxdmgsperstate, dmgspertypestate)

# 12 events make >100M each state.
unique(worststate[worststate$damages > 100000000,3])



dmgspermonth <- aggregate(damages ~ evtype + format(date, "%m %Y"), data=storms96, FUN=sum)
names(dmgspermonth)[2] <- "when"

worstevents <- head(dmgspertype[worstdmgs, c("evtype")], n=5)
dmgspermonthworst5 <- dmgspermonth[dmgspermonth$evtype %in% worstevents,]


dmgspermonthworst5$dwhen <- as.Date(paste("1 ", dmgspermonthworst5$when),
                                    format="%d %m %Y")




#outliers<-dmgspermonthworst5[head(order(dmgspermonthworst5$damages, decreasing=T),n=5),]
outl <- boxplot(dmgspermonthworst5$damages)$out

ggplot(subset(dmgspermonthworst5, ! damages %in% outl),
       aes(x=dwhen, y=damages/1000000)) +
    geom_line(aes(group=1)) +
    facet_grid(evtype~.) +
    theme(strip.text.y = element_text(
        size = 8, colour = "red", angle = 90)) +
    labs(x="Date of Event", y="Damages (millions of US$)",
         title="Damages per type since December 1995 (outliers filtered).")





