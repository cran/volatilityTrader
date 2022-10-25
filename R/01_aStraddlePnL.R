#'Calculates per share Profit and Loss (PnL) at expiration for Straddle Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.
#'@description
#'According to Kakushadze and Serur (2018), non directional strategies can be divided into two subgroups: (a) volatility strategies that profit if the stock has large price movements (high volatility environment); and (b) sideways strategies that profit if the stock price remains stable (low volatility environment). Here, in this package only high volatility option strategies are discussed and represented through their graphs.\cr
#'This is a volatility strategy consisting of a long position in an ATM (at the money) call option, and a long position in an ATM (at the money) put option with a strike price X. This is a net debit trade. The trader or investor has a neutral outlook. This is a capital gain strategy (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Straddle Option Strategy and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X Strike Price or eXercise price.
#'@param C Call Premium or Call Price paid for the bought Call.
#'@param P Put Premium or Put Price paid for the bought put.
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
#'aStraddlePnL(25,25,2.40,1.70)
#'aStraddlePnL(40,40,3,2,hl=0.7,hu=1.2)
#'aStraddlePnL(1000,1010,18,10,hl=0.955,hu=1.055)
#'@export
aStraddlePnL <- function (ST,X,C,P,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Dr=C+P
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (pmax((myData$spot-X),0)+(pmax((X-myData$spot),0))-V0Dr)
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
       labs(title  =  'Straddle Strategy', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility / Neutral Outlook', caption  =  'volatilityTrader / MaheshP Kumar')
}

