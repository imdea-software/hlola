![HLola logo](hlola.png "HLola")
# HLola

**HLola** is a Haskell implementation of the Stream Runtime Verification language [Lola](http://software.imdea.org/~cesar/papers/2005/time05/time05.pdf).
In this repository, you will find the source code of **HLola**, example specifications, and the steps to use the engine and define new specifications.
For more information, visit the [HLola official website](https://software.imdea.org/hlola).

## Docker image

We provide a [Docker image](https://hub.docker.com/r/imdeasoftware/hlola) ready to build and execute **HLola** specifications out of the box.

You can run the image with no arguments to execute a batch of specifications:
```bash
$> docker run imdeasoftware/hlola:tacas
```
or you can run a set of specifications passing [their ids](#list-of-example-specifications) as arguments. You can also pass the argument `--live` to see the monitors work online. For example:
```bash
$> docker run imdeasoftware/hlola:tacas --live PLTLCSV # or
$> docker run imdeasoftware/hlola:tacas MTLJSON # or
$> docker run imdeasoftware/hlola:tacas --live UAV2
```
See the following section to find out the specification ids.

## List of example specifications
- [PLTLCSV](https://software.imdea.org/hlola/specs.html#PLTLCSV): PLTL example in CSV format
- [PLTLJSON](https://software.imdea.org/hlola/specs.html#PLTLJSON): PLTL example in JSON format
- [MTLCSV](https://software.imdea.org/hlola/specs.html#MTLCSV): MTL example in CSV format
- [MTLJSON](https://software.imdea.org/hlola/specs.html#MTLJSON): MTL example in JSON format
- [PinescriptCSV](https://software.imdea.org/hlola/specs.html#PinescriptCSV): Pinescript example in CSV format
- [PinescriptJSON](https://software.imdea.org/hlola/specs.html#PinescriptJSON): Pinescript example in JSON format
- [UAV1](https://software.imdea.org/hlola/specs.html#UAV1): UAV monitor example 1
- [UAV2](https://software.imdea.org/hlola/specs.html#UAV2): UAV monitor example 2
- [Libraries](https://software.imdea.org/hlola/specs.html#Libraries)

## Description of example specifications
- [PLTL example](https://software.imdea.org/hlola/specs.html#PLTLCSV): a Past-Linear Temporal Logic (PLTL) property for a sender/receiver model taken from [[1]](#references):
  ```
    G (sender.state = waitForAck => Y (H sender.state != waitForAck))
  ```
  which states that, it **G**lobally holds that if the _sender_ is *wait*ing*ForAck*, then **Y**esterday (i.e. at the previous instant), **H**istorically it held that the _sender state_ was not *wait*ing*ForAck*.
  In other words, the sender waits for acknowledgement at most once during the execution.
  The only input stream is the state of the sender at each instant.
  The only output stream is the value of the property at each instant. (TOFIX)

- [MTL example](https://software.imdea.org/hlola/specs.html#MTLCSV): a Metric Temporal Logic (MTL) property to establish deadlines between environment events and the corresponding system responses taken from [[2]](#references):
  ```
  G (alarm => (F[0,10] allclear || F[10,10] shutdown))
  ```
  The property assesses that an _alarm_ is followed by a _shutdown_ event in exactly 10 time units unless _all clear_ is sounded first.
  The only input stream is the event happening at each instant.
  The only output stream is the value of the property at each instant.

- [Pinescript example](https://software.imdea.org/hlola/specs.html#PinescriptCSV): [TradingView](https://www.tradingview.com/) is an online charting platform for stock exchange, which offers a series-oriented language to create customized studies and signals (called Pinescript) and run them in the company servers.
  We have implemented the indicators of Pinescript in **HLola** as a [library](https://software.imdea.org/hlola/specs.html#Libraries), and we show the implementation of [a trading strategy](https://www.tradingview.com/script/DushajXt-MACD-Strategy/) using the **HLola** Pinescript library.
  The input streams are the _close_ and _high_ values of a stock at each day.
  The output streams indicate how much stock to _buy_ or _sell_ every day.

- [UAV specification 1](https://software.imdea.org/hlola/specs.html#UAV1): This specification is an online monitor to assess two properties over the flight of an UAV in real time:
  1. That the UAV does not fly over forbidden regions, and
  2. That the UAV is in good position when it takes a picture.

  The input streams of this specification consist of the state of the UAV at every instant and the onboard camera events to detect when a picture is being captured.
  The output streams are the values of properties 1. and 2.

- [UAV specification 2](https://software.imdea.org/hlola/specs.html#UAV2): This specification estimates the trajectory of a flying UAV to assess if it will reach its target destination.
  ![Fly gif](fly.gif "Fly gif")

  The input streams are data on the state of the vehicle at every instant and its target destination.
  The output stream shows how close to the target destination will the vehicle fly.

- [Libraries](https://software.imdea.org/hlola/specs.html#Libraries): These files encompass the implementations of the libraries PLTL, MTL, Quantitative Metric Temporal Logic (QMTL) and Pinescript.

## References
[1]: Alessandro Cimatti, Marco Roveri and Daniel Sheridan "[Bounded Verification of Past LTL](https://link.springer.com/chapter/10.1007/978-3-540-30494-4_18)".  In Proc. of the 5th Int'l  Conf on Formal Methods in Computer-Aided Design
(FMCAD'04)', pp 245-259, vol 3312 of LNCS, Springer, 2004.

[2]: [Some Recent Results in Metric Temporal Logic](https://link.springer.com/chapter/10.1007/978-3-540-85778-5_1)