#'Calculates per share Profit and Loss (PnL) at expiration for Short Call Condor Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.
#'@description
#'This is a volatility strategy consisting of a short position in an ITM call option with a strike price X1L, a long position in an ITM call option with a higher strike price X2Ml, a long position in an OTM call option with a strike price X3Mu, and a short position in an OTM call option with a higher strike price X4H. All strikes are equidistant: X4H minus X3Mu equals to X3Mu minus X2Ml; equals to X2Mu minus X1L. This is a relatively low net credit trade. The trader or investor has a neutral  outlook (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Short Call Condor Option Strategy and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price for one ITM shorted Call.
#'@param X2Ml Middle-low Strike Price or eXercise price for two middle strike bought Calls.
#'@param X3Mu Middle-upper Strike Price or eXercise price for two middle strike bought Calls.
#'@param X4H Higher Strike Price or eXercise price for one OTM shorted Call.
#'@param C1L Call Premium or Call Price received for the one ITM shorted Call.
#'@param C2Ml Call Premium or Call Price paid for the middle-low bought Call.
#'@param C3Mu Call Premium or Call Price paid for the middle-upper bought Call.
#'@param C4H Call Premium or Call Price received for the one OTM shorted Call.
#'@param PnL Profit and Loss
#'@param spot Spot Price
#'@param pl Profit and Loss
#'@param myData Data frame
#'@param myTibble tibble
#'@param hl lower bound value for setting lower-limit of x-axis displaying spot price.
#'@param hu upper bound value for setting upper-limit of x-axis displaying spot price.
#'@return graph of the strategy
#'@importFrom magrittr %>%
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 scale_fill_manual
#'@importFrom ggplot2 scale_color_manual
#'@importFrom ggplot2 geom_col
#'@importFrom tibble as_tibble
#'@importFrom dplyr mutate
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 element_line
#'@importFrom ggplot2 element_rect
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 geom_text
#'@importFrom ggplot2 labs
#'@importFrom ggplot2 scale_colour_manual
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom ggplot2 theme
#'@examples
#'shortCallCondorPnL(52,45,50,55,60,10,7,4,3.)
#'shortCallCondorPnL(415,400,420,440,460,50,35,22,16,hl=0.95,hu=1.125)
#'@export
shortCallCondorPnL <- function (ST,X1L,X2Ml,X3Mu,X4H,C1L,C2Ml,C3Mu,C4H,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Cr= C1L+ C4H - C2Ml -C3Mu
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (pmax((myData$spot-X2Ml),0))+ (pmax((myData$spot-X3Mu),0)) - (pmax((myData$spot-X1L),0))- (pmax((myData$spot-X4H),0)) + V0Cr
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  myTibble <- as_tibble(myData)
  myTbColored <- myTibble %>% mutate(PnL = pl >= 0)
  ggplot(myTbColored,aes(x=spot,y=pl,fill=PnL,label=pl)) +
    geom_col(position = "identity") +
    scale_fill_manual(values = c("#D47188","#1B7979"), guide= "none" ) +
    geom_point(aes(color=PnL))+
    scale_color_manual(values =  c("red","chartreuse"), guide= "none" ) +
    geom_text(nudge_y = 0.6,size= 3, color="navyblue")+
    theme(plot.caption = element_text(colour  =  'lightsteelblue3'))+
    theme(axis.line = element_line(linetype  =  'solid',colour ="darkmagenta" ))+
    theme(axis.ticks = element_line(size  =  1,colour = "deeppink1"))+
    theme(panel.grid.major = element_line(colour  =  'lightsteelblue1'))+
    theme(panel.grid.minor = element_line(colour  =  'thistle2'))+
    theme(axis.title = element_text(colour  =  'blue'))+
    theme(plot.title = element_text(colour  =  'brown3', vjust  =  1))+
    theme(panel.background = element_rect(fill  =  '#F2F2F9'))+
    theme(plot.background = element_rect(fill  =  '#E6E6FA', colour  =  'aquamarine4', linetype  =  'dashed'))+
    labs(title  =  'Short Call Condor Strategy', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility / Neutral Outlook', caption  =  'volatilityTrader / MaheshP Kumar')
}

