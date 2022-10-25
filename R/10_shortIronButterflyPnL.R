#'Calculates per share Profit and Loss (PnL) at expiration for Short Iron Butterfly Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.
#'@description
#'This volatility strategy is a combination of a bear put spread and a bull call spread and consists of a short position in an OTM put option (out of the money put : put Strike price is lower than spot price )  with a strike price X1L, a long position in an ATM (at the money)  put option and an ATM (at the money)  call option with a strike price X2M, and a short position in an OTM call option (out of the money call : call Strike price is higher than spot price ) with a strike price X3H. The strikes are equidistant: X2M minus X1L equals to X3H minus X2M . This is a net debit trade. The trader or investor has an outlook that is  neutral (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Short Iron Butterfly Option Strategy and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price for one ITM shorted Put.
#'@param X2M Medium trike Price or eXercise price for  one bought Put and one bought Call.
#'@param X3H Higher Strike Price or eXercise price for one OTM shorted Call.
#'@param P1L Put Premium or Put Price received for the first ITM shorted Put.
#'@param P2 Put Premium or Put Price paid for the bought Put.
#'@param C2 Put Premium or Put Price paid for the bought Call.
#'@param C3H Call Premium or Put Price received for the one OTM shorted Call.
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
#'shortIronButterflyPnL(52,45,50,55,2,4,7,5)
#'shortIronButterflyPnL(405,400,410,420,8,12,14.5,9,hl=0.9,hu=1.1)
#'@export
shortIronButterflyPnL <- function (ST,X1L,X2M,X3H,P1L,P2,C2,C3H,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Dr= P2+C2 -P1L -C3H
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (pmax((X2M-myData$spot),0)) + (pmax((myData$spot-X2M),0)) -   (pmax((X1L-myData$spot),0))- (pmax((myData$spot-X3H),0)) - V0Dr
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
    labs(title  =  'Short Iron Butterfly Strategy', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility / Neutral Outlook', caption  =  'volatilityTrader / MaheshP Kumar')
}

