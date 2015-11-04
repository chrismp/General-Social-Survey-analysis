# Import SPSS into R tutorial: http://dmwiig.net/2014/08/03/r-tutorial-using-r-to-work-with-datasets-from-the-norc-general-social-science-survey/

install.packages('Hmisc')
install.packages('foreign')

library(Hmisc)

gss <- spss.get('raw/GSS7214_R4.sav', use.value.labels=TRUE)
