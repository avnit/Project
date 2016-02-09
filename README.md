# SG 40 Sugar Trading Project for class FINC 621 
##### By Avnit Evan Hetal and Preedee
=====================================================

### Directory Structure
=======================

|Folder |File Name |Description |
------------:|------------:|------------:|
|R   |      | | 
|    | InSampleTesting.R|The File is starting point to run in the sample testing|
|      |OutOfSampleTesting.R|The File is starting point to run Out of the sample testing|
|      |Initialize.R| Intialize will download the stocks and add indicators for trades|
|      |ProjectStart.R| Creates the rules , indicators and generates trades |
|      |Functions.R| All functions |
|      |MonteCarlo.R| This runs monte carlo simulartor|
|Src    |           | |
|      |rccp_modifyDataFrameWithSugarPrices.cpp|The Rccp code to merge sugar prices and stocks |
|Data    |           |  |
|    |Holdings-xlk.csv | The stocks that are needed for the run |
|    | BloomnbergSugarPrice.csvWithChange| bloomberg sugar prices |

======================
##### Project Description

Sugar Quant 40 has identified a short-term trading strategy. Our strategy involves the prices of sugar and food and beverage companies. We believe that sugar prices and the stock prices of food and beverage companies are inversely related. That is why, when the price of sugar goes up, we will short food and beverage companies and when the price of sugar goes down, we will take a long position on food and beverage companies. We have three technical tools that we will use. They are Relative Strength Index (RSI), Commodity Channel Index (CCI) and Bollinger Bands. The position that we take are best classified as short term. Exit triggers are determined in conjunction with entrance into the trade.

RSI is a momentum oscillator that measures the speed and change of price movements. RSI oscillates between zero and 100. RSI is considered overbought when above 70 and oversold when below 30. CCI measures the current price level relative to an average price level over a given period of time. CCI is relatively high when prices are fat above their average and relatively low when prices are far below their average. Bollinger Bands are place above or below the moving average calculated through volatility. 

Sugar Quant 40 uses Bollinger Bands to judge volatility and was given both a buy and sell threshold and we also use RSI and CCI as indicators determine if a stock is being under or overvalued.  If the daily price of sugar change is more than 0.1, a buy indicator will be satisfied and sell indicator will be satisfied if the price decreased by more than 0.2. We therefore believed that with these strategies, Sugar Quant 40 can maximize our returns and minimize the risk that may occur.

========================





-------------------------












