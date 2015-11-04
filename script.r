# http://docs.ggplot2.org/current/coord_polar.html
# http://www.r-chart.com/2010/07/pie-charts-in-ggplot2.html
# http://stackoverflow.com/questions/13615562/ggplot-donut-chart
# http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization

library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)

gss$decade <- ifelse(gss$year<1980,'1970s',
                     ifelse(gss$year<1990,'1980s',
                            ifelse(gss$year<2000,'1990s',
                                   ifelse(gss$year<2010,'2000s',
                                          '2010s'
                                    )
                             )
                      )
               )

gss$partyidRecode <- ifelse(gss$partyid=='STRONG DEMOCRAT' | gss$partyid=='NOT STR DEMOCRAT', 'DEMOCRAT',
  ifelse(gss$partyid=='IND,NEAR DEM' | gss$partyid=='INDEPENDENT' | gss$partyid=='IND,NEAR REP', 'INDEPENDENT',
    ifelse(gss$partyid=='NOT STR REPUBLICAN' | gss$partyid=='STRONG REPUBLICAN', 'REPUBLICAN',
      NA
    )
  )
)

gss$polviewsRecode <- ifelse(grepl('LIBERAL',gss$polviews),'LIBERAL',
 ifelse(grepl('MODERATE',gss$polviews),'MODERATE',
    ifelse(grepl('CONSERVATIVE',gss$polviews),'CONSERVATIVE',
      NA
    )
  )
)

# Checking difference between parties' and ideologies' intelligence thru GSS' vocab test. For each decade. 
gss.partiesPolviewsWordsum <- gss[
  c(
    'year',
    'decade',
    'partyid',
    'partyidRecode',
    'polviews',
    'polviewsRecode',
    'wordsum'
  )
]

ggplot(gss.partiesPolviewsWordsum) + 
  geom_boxplot(  
    aes(
      x = partyidRecode,
      y = wordsum
    )
  ) + 
  coord_flip() +
  facet_wrap(~ decade)

ggplot(gss.partiesPolviewsWordsum) + 
  geom_boxplot(  
    aes(
      x = polviewsRecode,
      y = wordsum
    )
  ) + 
  coord_flip() +
  facet_wrap(~ decade)
# DONE
#Note: No difference between vocab test scores of parties and ideologies.


# Cateogrizing parties by ideology, for each year.
gss.partiesPolviews <- gss.partiesPolviewsWordsum[,-7]

gss.partiesPolviews <- gss.partiesPolviews[
  is.na(gss.partiesPolviews$partyidRecode)==FALSE,
]

gss.partiesPolviews <- gss.partiesPolviews[
  is.na(gss.partiesPolviews$polviewsRecode)==FALSE,
]

ggplot(
  gss.partiesPolviews,
  aes(
    x = partyidRecode,
    fill = polviewsRecode
  )
) +
  geom_bar(
    position = 'fill',
    stat = 'bin'
  ) +
  scale_y_continuous(
    labels = percent_format()
  ) +
  facet_grid(decade ~ partyidRecode) + 
  coord_polar(theta = 'y') 



