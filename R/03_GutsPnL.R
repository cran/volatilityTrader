#'Calculates per share Profit and Loss (PnL) at expiration for Guts Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.
#'@description
#'This is a volatility strategy consisting of a long position in an ITM (in the money call : ST is greater than call strike price of X1LC ) call option with a strike price X1LC, and a long position in an ITM (in the money put : ST is less than put strike of X2HP) put option with a strike price X2HP. This is a net debit trade. Since both call and put options are ITM, this strategy is more costly to establish than a long straddle position. The trader or investor has neutral outlook. This is a capital gain strategy (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Guts Option Strategy and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X2HP Higher Strike Price or eXercise price bought Put.
#'@param X1LC Lower Strike Price or eXercise price bought Call.
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
#'gutsPnL(25,27,24.75,2.5,3)
#'gutsPnL(46,48,44,2,4,hl=0.6,hu=1.6)
#'gutsPnL(1020,1025,1015,10,18,hl=0.95,hu=1.045)
#'@export
gutsPnL <- function (ST,X2HP,X1LC,P,C,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Dr=C+P
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (pmax((myData$spot-X1LC),0)+(pmax((X2HP-myData$spot),0))-V0Dr)
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  myTibble <- as_tibble (myData)
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
    labs(title  =  ' Long Guts ', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility / Neutral Outlook', caption  =  'volatilityTrader / MaheshP Kumar')
}

