#'Calculates per share Profit and Loss (PnL) at expiration for Long Call Synthetic Straddle Option Strategy and draws its Bar Plot displaying PnL in the plots tab.
#'@description
#'This volatility strategy (which is the same as a long straddle with the put replaced by a synthetic put) amounts to shorting stock and buying two ATM (or the nearest ITM call options with a strike price X. The trader’s outlook is neutral. This is a capital gain strategy (assuming S0 is greater than or equal to X and V0 is greater than (S0 minus X)) (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Long Call Synthetic Straddle Option Strategy and draw its graph in the plots tab.
#'@param ST Spot Price at time T.
#'@param X Strike Price or eXercise price.
#'@param C1 Call Premium or Call Price paid for the first bought Call.
#'@param C2 Call Premium or Call Price paid for the second bought Call.
#'@param S0 Stock Price at which the stock is shorted.
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
#'longCallSyntheticStraddlePnL(25,25,2,2,25.10)
#'longCallSyntheticStraddlePnL(40,40,7,7,41,hl=0.4,hu=1.7)
#'@export
longCallSyntheticStraddlePnL <- function (ST,X,C1,C2,S0,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Cr=(S0- C1-C2)
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (V0Cr-myData$spot)+ (2* (pmax((myData$spot-X),0)))
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
    labs(title  =  'Long Call Synthetic Straddle Strategy', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility / Neutral Outlook', caption  =  'volatilityTrader / MaheshP Kumar')
}

