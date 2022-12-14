% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/09_shortPutButterflyPnL.R
\name{shortPutButterflyPnL}
\alias{shortPutButterflyPnL}
\title{Calculates per share Profit and Loss (PnL) at expiration for Short Put Butterfly Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.}
\usage{
shortPutButterflyPnL(
  ST,
  X2,
  X1H,
  X3L,
  P2,
  P1H,
  P3L,
  hl = 0,
  hu = 2,
  spot = spot,
  pl = pl,
  myData = myData,
  myTibble = myTibble,
  PnL = PnL
)
}
\arguments{
\item{ST}{Spot Price at time T.}

\item{X2}{Strike Price or eXercise price for two ATM bought Puts.}

\item{X1H}{Higher Strike Price or eXercise price for one ITM shorted Put.}

\item{X3L}{Higher Strike Price or eXercise price for one OTM shorted Put.}

\item{P2}{Put Premium or Put Price paid for the two ATM bought Puts.}

\item{P1H}{Put Premium or Put Price received for the first ITM shorted Put.}

\item{P3L}{Put Premium or Put Price received for the one OTM shorted Put.}

\item{hl}{lower bound value for setting lower-limit of x-axis displaying spot price.}

\item{hu}{upper bound value for setting upper-limit of x-axis displaying spot price.}

\item{spot}{Spot Price}

\item{pl}{Profit and Loss}

\item{myData}{Data frame}

\item{myTibble}{tibble}

\item{PnL}{Profit and Loss}
}
\value{
graph of the strategy
}
\description{
This is a volatility strategy consisting of a short position in an ITM put option (in the money put; strike price greater than spot price)  with a strike price X1H, a long position in two ATM (at the money) put options with a strike price X2, and a short position in an OTM put option (out of the money :  strike price less than spot price)  with a strike price X3L . The strikes are equidistant: X2 minus X3L equals to X1H minus X2. This is a net credit trade. In this sense, this is an income strategy However, the potential reward is sizably smaller than with a short straddle or a short strangle (albeit with a lower risk). The trader or investor has a neutral outlook (Kakushadze & Serur, 2018).
}
\details{
According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Short Put Butterfly Option Strategy and draw its graph in the Plots tab.
}
\examples{
shortPutButterflyPnL(50,50,55,45,6,9,5)
shortPutButterflyPnL(400,400,420,380,14,19,15,hl=0.9,hu=1.1)
}
