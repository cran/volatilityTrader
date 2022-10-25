#'Calculates per share Profit and Loss (PnL) at expiration for Long Box Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.
#'@description
#'The Long Box is a complex strategy that can (in some jurisdictions) have beneficial effects for tax planning from year to year. If your incentive for this strategy is a tax play, you should consult with your tax advisor beforehand to evaluate whether or not it is valid where you live and trade to invest (Cohen, 2015).\cr
#'This volatility strategy can be viewed as a combination of a bull call spread and a bear put spread, and consists of a long position in an ITM put option with a strike price X1H, a short position in an OTM put option with a lower strike price X2, a long position in an ITM call option with the strike price X2, and a short position in an OTM call option with the strike price X1H. The trader or investor has an outlook that is  neutral (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Long Box Option Strategy and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X1H Higher Strike Price or eXercise price for one ITM long Put and one OTM shorted call .
#'@param X2 Strike Price or eXercise price for one shorted Put and one long call.
#'@param P1 Put Premium or Put Price received for the shorted Put.
#'@param C2 Put Premium or Put Price paid for the bought Call.
#'@param P3 Put Premium or Put Price paid for the bought Put.
#'@param C4 Call Premium or Put Price received for the shorted Call.
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
#'longBoxPnL(34,40,30,1,6,7,2)
#'@export
longBoxPnL <- function (ST,X1H,X2,P1,C2,P3,C4,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Dr= C2+P3 -P1 -C4
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (pmax((X1H-myData$spot),0))- (pmax((X2-myData$spot),0))+ (pmax((myData$spot-X2),0))- (pmax((myData$spot- X1H),0)) -V0Dr
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  myTibble <- as_tibble(myData)
  myTbColored <- myTibble %>% mutate(PnL = pl >= 0)
  ggplot(myTbColored,aes(x=spot,y=pl,fill=PnL,label=pl)) +
    geom_col(position = "identity") +
    scale_fill_manual(values = c("#D47188","#1B7979"), guide= "none" ) +
    geom_point(aes(color=PnL))+
    scale_color_manual(values =  c("red","chartreuse"), guide= "none" ) +
    geom_text(nudge_y = 0.000,size= 3, color="navyblue")+
    theme(plot.caption = element_text(colour  =  'lightsteelblue3'))+
    theme(axis.line = element_line(linetype  =  'solid',colour ="darkmagenta" ))+
    theme(axis.ticks = element_line(size  =  1,colour = "deeppink1"))+
    theme(panel.grid.major = element_line(colour  =  'lightsteelblue1'))+
    theme(panel.grid.minor = element_line(colour  =  'thistle2'))+
    theme(axis.title = element_text(colour  =  'blue'))+
    theme(plot.title = element_text(colour  =  'brown3', vjust  =  1))+
    theme(panel.background = element_rect(fill  =  '#F2F2F9'))+
    theme(plot.background = element_rect(fill  =  '#E6E6FA', colour  =  'aquamarine4', linetype  =  'dashed'))+
    labs(title  =  'Long Box Strategy', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility/ Tax Planning Purpose (in some jurisdictions only)', caption  =  'volatilityTrader / MaheshP Kumar')
}

