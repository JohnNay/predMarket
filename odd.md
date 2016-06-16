---
title: "Climate Prediction Market Model"
author: "John J. Nay, Martin Van der Linden, and Jonathan M. Gilligan"
output:
  html_document: null
  pdf_document: null
bibliography: odd.bib
---
## Purpose

The purpose of this model is to simulate a prediction market for future temperatures in order to study the dynamics by which participation in a market may affect the participants' beliefs about the causes of climate change.
The model is inspired by [@Vandenbergh2013f], which suggests that people who doubt scientific assessments of climate change for ideological reasons, but place considerable trust in free markets may change their beliefs about the causes of climate change if a prediction market for future climates places high values on predictions based on the scientific consensus view.

## Entities, State Variables, and Scales

The *entities* are traders in a prediction market. Traders are characterized by the following *state variables*:

* The trader's risk tolerance for bidding in the market. High risk tolerance corresponds to demanding more aggressive prices for buying (the trader sets the price she is willing to pay to buy a security well below the utility she assigns to owning the security) and selling (the trader sets the price she is willing to accept to sell a security well below the utility she assigns to owning the security), and consequently experiencing a higher risk of not completing a trade. Lower risk tolerance corresponds to setting both willing-to-accept and willing-to-pay prices closer to the utility the trader assigns to owning the security.

* Ideology: More ideological traders are more resistant to changing their beliefs when confronted with evidence.

* Belief: The trader's belief about the true cause of climate change. In this model, each trader either believes that global temperature is proportional to the logarithm of the atmospheric carbon dioxide concentration ($\log \mathrm{CO}_2$) or that it is proportional to the total solar irradiance (TSI). A trader's belief may change during the simulation.

    * Corresponding to the belief are parameters characterizing a linear model of temperature (see Submodels for details).

* Wealth: Traders buy and sell securities that represent bets on future global temperature. Traders' wealth changes as a result of buying, selling, and owning winning securities when they come due. Wealth is measured in ECU's (experimental currency units).

The model is characterized by the following global variables:

* True model: the actual cause of climate change. Global temperature in the model may be proportional to $\log \mathrm{CO}_2$ (a simplification of the scientific consensus view) or TSI (a common alternative hypothesis advanced by many people who doubt the scientific consensus). 

* The number of traders

* The number of edges that connect the traders in social networks. Each trader has a minimum of two edges connecting her to other traders.

* The segmentation of the market: This represents the traders' homophily (preference to form social networks with like-minded traders who share their belief in the cause of climate change). A segmentation of zero corresponds to traders being equally likely to make network connections with those who agree and those who disagree, and thus they are exposed to a variety of beliefs. A segmentation of one corresponds to traders making connections only with those who agree with them (an echo chamber).

The market is not explicitly spatial. Each time step represents one year and the model typically runs for several decades to a century. The maximum run length is constrained by the availability of projected vaues for climate forcing terms ($\mathrm{CO}_2$ and TSI).

## Process Overview and Scheduling

Scheduling nests two time scales: A six-year trading sequence, and activities that traders perform every year (time-step) within the trading sequence.

1. At the beginning of each trading sequence:

    * Securities are issued. Each trader receives one of each type of security.

       Each security represents a range of temperatures, and purchasing that security represents a bet that the global average temperature at the end of the trading sequence will lie within the range corresponding to that security. The possible range of temperatures is divided into 10 equal segments and 12 kinds of securities are issued corresponding to those segments plus one for temperatures above the highest segment and one for temperatures below the lowest segment. 

1. At each time-step within a trading sequence, traders:

    a. Re-calibrate their approximate model of the climate, based on newly available global temperature data from the previous year.

    a. Use the updated model to predict a probability distribution for what the temperature will be at the end of the current trading sequence.

    a. Use the updated probability distribution to assign values for the various climate securities being traded.

    a. Buy and sell securities on the market. Traders enter the market in random order. When a trader enters the market, she:
        * Chooses a random security $s_B$, which she will attempt to buy. She assigns a willing-to-pay price, $p_B$, which represents the highest price she will offer for that security. This price will be less than or equal to the actual value she places on the security.
        * Chooses a random security, $s_S$ out of those she owns, which she will try to sell, and assigns a willing-to-accept price, $p_S$, which represents the lowest offer she will accept to sell it.
        * Places orders to buy and sell in the order book in the market.
        * The market-maker attempts to match the new offers with offers previously entered:
            * If there are any outstanding orders to sell the security $s_B$ for a price $p \le p_B$, then the trader will buy one share of $s_B$ from the trader offering it at the lowest price, and both orders are removed from the trading book.
       	    * If there are any outstanding orders to buy the security $s_S$ for a price $p \ge p_S$, then the trader will sell one share of $s_S$ to the trader offering the highest price, and both orders are removed from the trading book.
        * After all traders have come to the market, any outstanding (unconsummated) offers are removed from the trading book and the turn is over.

1. At the end of the six-year trading sequence:
    * Out of the 12 types of security, one will contain the actual global temperature within its temperature range. This security is now worth 1 ECU, and every trader who owns this security will receive 1 ECU per share. All the other securities are worthless.
    * Each trader compares her own wealth to the wealth of her immediate neighbors in her social network and considers changing her belief about climate to the belief of the wealthiest neighbor.

## Design Concepts

### Basic principles

### Emergence

Traders begin with different beliefs about climate change. We look for emergence through interactions in the market causing traders' beliefs about climate to spontaneously converge to a consensus view.

### Adaptation

Traders adapt to changing conditions by buying and selling securities. At each time step, each trader uses her updated approximate probabilistic model of future climate to determine the value she places on each security. Then she uses her risk tolerance to establish her willing-to-pay and willing-to-accept prices for buying and selling, respectively.

### Objectives

Traders seek to maximize their wealth by buying securities they believe are likely to pay off and selling securities they believe are unlikely to pay off.

### Learning

Traders exhibit learning when they use Bayesian updating to adjust the parameters of their approximate model of climate in response to receiving new global temperature data and when they change their beliefs in response to other traders' success in the market.

Specifically, at the end of each trading sequence, if her wealthiest immediate neighbor in her social network (including herself) disagrees with her about the cause of climate change, then with probability $(1 - \text{ideology})$, she changes her belief about climate change to match that neighbor's.

### Prediction

Traders use their approximate models of climate to predict the temperature at the end of the current trading sequence.

### Sensing

Traders sense past and current global average temperature, and know both the past and future values of $\mathrm{CO}_2$  concentration and TSI. Traders also sense the wealth and beliefs about climate change of their immediate neighbors in their social network.

### Interaction

Traders interact by making offers to buy and sell securities in the market. They also interact by examining the beliefs of their neighbors in the social network and possibly changing their beliefs to match their most  successful neigbhor.

### Stochasticity

Global temperature is represented as a linear fucnction of the climate forcing plus stochastic noise that follows an $\text{ARMA}(p,q)$ model. The values of $p$ and $q$ are obtained by fitting models to historical data. Currently, for both $\log \mathrm{CO}_2$ and TSI models, the $\text{AR}(1)$ model fits the data best.

Traders enter the market in random order, in order to assure that no trader has an advantage by entering at a particular time at every step.

Traders' state variables for risk tolerance, ideology, and belief about climate are initialized randomly.

The social network is constructed by randomly making connections between traders, with the provision that each trader has a minimum of two connections.

Traders update their beliefs randomly (see above, under Learning).

### Collectives

There is no collective action, but each trader is a member of a social network.

### Observation

We observe the beliefs of the traders and look for whether those beliefs converge toward the true cause of climate change.

## Initialization

The "true" model of climate change is prescribed (either global temperature is proportional to $\log \mathrm{CO}_2$ or to TSI), and the parameters of the model (intercept, slope, and noise parameters) are fit to historical data.

Future temperatures are simulated, using the parameters from the fit.

Traders are initialized with ideology and risk tolerance set to random values between 0 and 1. Each trader's belief about climate change is randomly initialized to believing that global temperature is proportional either to $\log \mathrm{CO}_2$ or to TSI.

Traders' approximate models are initialized by fitting a linear  model corresponding to the agent's belief about the true driver of global temperature.

The segmentation of the social network is assigned a random value between 0 and 1.

The social network is initialized by assigning each agent two connections, where the probability of connecting to another agent with matching belief is $(1 - \text{segmentation}) / 2$. After each agent has two connections, additional edges are added to the social network until the network has the specified number of edges.

## Input data

Historical values for atmospheric $\mathrm{CO}_2$ concentrations, TSI, and global average temperature and projected values of future $\mathrm{CO}_2$ concentrations and TSI are read in from files.

## Submodels

### Global temperature:

Global temperature at time $t$ is modeled as
$$ T(t) = a + b F(t) + \varepsilon$$,
where $F$ is the climate forcing (either $\log \mathrm{CO}_2$ or TSI), and  $\varepsilon$ is a random deviate drawn from an $\text{ARMA}(p,q)$ process. $a$, $b$, and the parameters characterizing $\varepsilon$ are fit to historical data.

# References