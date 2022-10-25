#'Calculates per share Profit and Loss (PnL) at expiration for Strip Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.
#'@description
#'This Strategy consists of a long call position (in an at the money call option) and a long position in two put options (at the money) with a strike price X. The Strip is a simple adjustment to the Straddle to make it more biased toward the downside. In buying a second put, the strategy retains its preference for high volatility but now with a more bearish slant (Cohen, 2016).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Strip Option Strategy and draw its graph in the Plots tab. EXAMPLE, Buy HypoVola December 9 call at $1.40 (outflow) and Buy two HypoVola December 9 Puts at $0.80 (outflow). This is a net debit trade and involves three cash outflows. The Bar Plot gets displayed in Plots tab.
#'@param ST Spot Price at time T.
#'@param X Strike Price or eXercise price.
#'@param C Call Premium or Call Price paid for bought Call.
#'@param PnL Profit and Loss
#'@param P1 Put Premium paid for the first bought put.
#'@param P2 Put Premium paid for the second bought put.
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
#'stripPnL(9,9,1.4,0.80,0.80)
#'stripPnL(40,40,2.00,1.25,1.25,hl=0.85,hu=1.25)
#'stripPnL(1000,1000,8,5.50,6.50,hl=0.985,hu=1.035)
#'@export
stripPnL <- function (ST,X,C,P1,P2,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Dr=C+P1+P2
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (pmax((myData$spot-X),0)+2*pmax((X-myData$spot),0)-V0Dr)
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
    labs(title  =  'Strip Strategy', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility / Bearish Outlook', caption  =  'volatilityTrader / MaheshP Kumar')
}

