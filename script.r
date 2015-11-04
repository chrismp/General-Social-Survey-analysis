# http://docs.ggplot2.org/current/coord_polar.html
# http://www.r-chart.com/2010/07/pie-charts-in-ggplot2.html
# http://stackoverflow.com/questions/13615562/ggplot-donut-chart
# http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization

install.packages('dplyr')
install.packages('ggplot2')

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


gss.partiesPolviews <- gss[c('year','decade','partyid','partyidRecode','polviews','polviewsRecode')]

# A pie chart = stacked bar chart + polar coordinates


df <- data.frame(
  variable = c("resembles", "does not resemble"),
  value = c(80, 20)
)

ggplot(
  df,
  aes(
    x='',
    y=value,
    fill=variable
  )
) +
  geom_bar(
    width=1,
    stat='identity'
  ) +
  scale_fill_manual(values = c('red','yellow')) + 
  coord_polar('y', start=pi/1.4) + 
  labs(title='Pac Man')


ggplot(
  gss.partiesPolviews[is.na(gss.partiesPolviews$polviewsRecode)==FALSE,],
  aes(
    x=factor(year),
    fill=polviewsRecode
  )
) + 
  geom_bar(
    width=1,
    stat='bin'
  ) +
  scale_fill_manual(
    values = c('#fc8d62','#66c2a5','#8da0cb')
  )







