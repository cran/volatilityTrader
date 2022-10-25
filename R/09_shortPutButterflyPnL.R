#'Calculates per share Profit and Loss (PnL) at expiration for Short Put Butterfly Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.
#'@description
#'This is a volatility strategy consisting of a short position in an ITM put option (in the money put; strike price greater than spot price)  with a strike price X1H, a long position in two ATM (at the money) put options with a strike price X2, and a short position in an OTM put option (out of the money :  strike price less than spot price)  with a strike price X3L . The strikes are equidistant: X2 minus X3L equals to X1H minus X2. This is a net credit trade. In this sense, this is an income strategy However, the potential reward is sizably smaller than with a short straddle or a short strangle (albeit with a lower risk). The trader or investor has a neutral outlook (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Short Put Butterfly Option Strategy and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X1H Higher Strike Price or eXercise price for one ITM shorted Put.
#'@param X2 Strike Price or eXercise price for two ATM bought Puts.
#'@param X3L Higher Strike Price or eXercise price for one OTM shorted Put.
#'@param P1H Put Premium or Put Price received for the first ITM shorted Put.
#'@param P2 Put Premium or Put Price paid for the two ATM bought Puts.
#'@param P3L Put Premium or Put Price received for the one OTM shorted Put.
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
#'shortPutButterflyPnL(50,50,55,45,6,9,5)
#'shortPutButterflyPnL(400,400,420,380,14,19,15,hl=0.9,hu=1.1)
#'@export
shortPutButterflyPnL <- function (ST,X2,X1H,X3L,P2,P1H,P3L,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Cr= P1H + P3L - (2*P2)
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (2* (pmax((X2-myData$spot),0))) - (pmax((X1H-myData$spot),0))- (pmax((X3L-myData$spot),0)) + V0Cr
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
    labs(title  =  'Short Put Butterfly Strategy', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility / Neutral Outlook', caption  =  'volatilityTrader / MaheshP Kumar')
}

