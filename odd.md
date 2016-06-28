---
title: "Climate Prediction Market Model"
author: "John J. Nay, Martin Van der Linden, and Jonathan M. Gilligan"
date: "June 27, 2016"
output:
  pdf_document: null
  html_document: null
subtitle: Overview, Design concepts and Details (ODD)
bibliography: odd.bib
---
## Purpose

The purpose of this model is to simulate a prediction market for future temperatures in order to study the dynamics by which participation in a market may affect the participants' beliefs about the causes of climate change.
The model is inspired by @Vandenbergh2013f, which suggests that people who doubt scientific assessments of climate change for ideological reasons, but place considerable trust in free markets may change their beliefs about the causes of climate change if a prediction market for future climates places high values on predictions based on the scientific consensus view.

## Entities, State Variables, and Scales

The *entities* are traders in a prediction market. Traders are characterized by the following *state variables*:

* Risk taking (real, $\in [0,1]$): Higher values of risk-taking mean the trader will bargain more aggressively in the market, offering less to buy a share and demanding more to sell one. This  increases the potential profit of a trade, but incurs a greater risk of not finding a partner willing to execute the trade (see Submodels for details).

* Ideology (real, $\in [0,1]$): The degree to which the trader's beliefs are influenced by ideology. More ideological traders are more resistant to changing their beliefs when confronted with evidence.

* Belief (categorical): The trader's belief about the true cause of climate change. Each trader believes that global temperature is proportional either to the logarithm of the atmospheric carbon dioxide concentration ($\log \mathrm{CO}_2$) or to the total solar irradiance (TSI). Trader's beliefs may change during the simulation.

    * Corresponding to the belief are parameters characterizing a linear model of temperature (see Submodels for details).

* Wealth (real, $\ge 0$): Traders buy and sell securities that represent bets on future global temperature. Traders' wealth changes as a result of buying, selling, and owning winning securities when they come due. Wealth is measured in ECU's (experimental currency units).

* Securities (list): A list of securities the trader owns during the current trading sequence.

The model is characterized by the following global variables:

* True model (categorical $\in \lbrace \mathrm{CO}_2, \text{TSI} \rbrace$): the actual cause of climate change. Global temperature in the model may be proportional to $\log \mathrm{CO}_2$ (a simplification of the scientific consensus view) or TSI (a common alternative hypothesis advanced by many people who doubt the scientific consensus). 

* The number of traders (integer $> 0$).

* The number of security intervals (integer $>0$). This corresponds to the kinds of securities issued at the beginning of each trading sequence. The larger the number of intervals, the more fine-grained the bets traders place on global temperatures. See below, under process overview and scheduling for more details.

* The number of edges that connect the traders in social networks (integer $> 2 \times \text{number of traders}$). Each trader has a minimum of two edges connecting her to other traders.

* The segmentation of the market (real $\in [0,1]$): This represents the traders' homophily (preference to form social networks with like-minded traders who share their belief in the cause of climate change). A segmentation of zero corresponds to traders being equally likely to make network connections with those who agree and those who disagree, and thus they are exposed to a variety of beliefs. A segmentation of one corresponds to traders making connections only with those who agree with them (an echo chamber).

The market is not explicitly spatial. Each time step represents one year and the model typically runs for several decades to a century. The maximum run length is constrained by the availability of projected vaues for climate forcing terms ($\mathrm{CO}_2$ and TSI).

## Process Overview and Scheduling

Scheduling nests two time scales: A six-year trading sequence, and activities that traders perform every year (time-step) within the trading sequence.

1. At the beginning of each trading sequence:

    * Securities are issued. Each trader receives one of each type of security.

       Each security represents a range of temperatures, and purchasing that security represents a bet that the global average temperature at the end of the trading sequence will lie within the range corresponding to that security. If $N$ is the number of security intervals, then the range of likely temperatures is divided into $N$ equal segments and $N + 2$ kinds of securities are issued corresponding to those segments plus one for temperatures above the highest segment and one for temperatures below the lowest segment.

1. At each time-step within a trading sequence, traders:

    a. Re-calibrate the parameters for their approximate model of the climate, based on newly available global temperature data from the previous year.

    a. Use the updated model to predict a probability distribution for what the temperature will be at the end of the current trading sequence.

    a. Use the updated probability distribution to assign values for the various climate securities being traded.

    a. Buy and sell securities on the market. Traders enter the market in random order. When a trader enters the market, she:
        * Chooses a random security $s_B$, which she will attempt to buy. She assigns a willing-to-pay price, $p_B$, which represents the highest price she will offer for that security. This price will be less than or equal to the actual value she places on the security (see Submodels for details on setting prices for buying and selling).
        * Chooses a random security, $s_S$ out of those she owns, which she will try to sell, and assigns a willing-to-accept price, $p_S$, which represents the lowest offer she will accept to sell it.
        * Places orders to buy and sell in the order book in the market.
        * Each time a new trader places buy and sell orders, the market-maker attempts to match the new offers with offers previously entered:
            * If there are any outstanding orders to sell the security $s_B$ for a price $p \le p_B$, then the trader will buy one share of $s_B$ from the trader offering it at the lowest price, and both orders are removed from the trading book. The price is subtracted from the buyer's wealth and added to the seller's. One share of $s_B$ is removed from the seller's holdings and added to the buyer's.
       	    * If there are any outstanding orders to buy the security $s_S$ for a price $p \ge p_S$, then the trader will sell one share of $s_S$ to the trader offering the highest price, and both orders are removed from the trading book.
    a. After all traders have come to the market, any outstanding (unconsummated) offers are removed from the trading book and the turn is over.

1. At the end of the six-year trading sequence:
    * Out of the $N + 2$ types of security, one will contain the actual global temperature within its temperature range. This security is now worth 1 ECU, and every trader who owns this security will receive 1 ECU per share. All the other securities are worthless.
    * Each trader compares her own wealth to the wealth of her immediate neighbors in her social network and considers changing her belief about climate to the belief of the wealthiest neighbor.

## Design Concepts

### Basic principles

Global temperature: We model the global temperature as a linear function of the forcing term (which may be either $\log \mathrm{CO}_2$ or TSI, depending on the global "true model" variable) plus an $\text{ARMA}(p,q)$ noise process.

Traders estimate the probability distribution for future global temperatures by fitting a linear model correesponding to their beliefs about the climate system to past temperatures and the forcing term corresponding to their belief.  Traders use Bayesian methods to generate posterior probability distributions for all of the model parameters. Traders use these probability distributions of parameters together with known projections of future driving terms to estimate probability distributions for future global temperatures.

Traders use their calculated probability distributions of future global temperatures to assign expected values to different climate securities. They then offer to buy or sell securities for prices that would increase their expected wealth.

Traders may change their beliefs about what is really driving climate change ($\mathrm{CO}_2$ or TSI) if their wealthiest neighbor in the social network has a different belief.

### Emergence

Traders begin with different beliefs about climate change. We look for emergence through interactions in the market causing traders' beliefs about climate to spontaneously converge to a consensus view.

### Adaptation

Traders adapt to changing conditions by updating the parameters of their probabilistic models of future global temperature and using those models to update the expected values they assign securities. The expected values then affect the prices they offer in their buy and sell orders.

### Objectives

Traders seek to maximize their wealth by buying and selling securities. They offer to buy securities for lower prices than the expected value they assign the security and offer to sell for higher prices.

### Learning

Traders exhibit learning when they use Bayesian updating to adjust the parameters of their approximate model of climate in response to receiving new global temperature data and when they change their beliefs in response to other traders' success in the market.

Specifically, at the end of each trading sequence, if her wealthiest immediate neighbor in her social network (including herself) disagrees with her about the cause of climate change, then with probability $(1 - \text{ideology})$, she changes her belief about climate change to match that neighbor's.

### Prediction

Traders use their approximate models of climate to predict the temperature at the end of the current trading sequence.

### Sensing

Traders sense past and current global average temperature, and know both the past and future values of $\mathrm{CO}_2$  concentration and TSI. Note that whereas in the real world, there is considerable uncertainty about future $\mathrm{CO}_2$ concentrations and TSI, in this model, traders have perfect knowledge of the future values of these terms. Traders also sense the wealth and beliefs about climate change of their immediate neighbors in their social network.

### Interaction

Traders interact by making offers to buy and sell securities in the market. They also interact by examining the beliefs of their neighbors in the social network and possibly changing their beliefs to match their most  successful neigbhor.

### Stochasticity

Global temperature is represented as a linear fucnction of the climate forcing plus stochastic noise that follows an $\text{ARMA}(p,q)$ model (see Submodels for details).

Traders enter the market in random order, in order to assure that no trader has an advantage by entering at a particular time at every step.

Traders' state variables for risk tolerance, ideology, and belief about climate are initialized randomly.

The social network is constructed by randomly making connections between traders, with the provision that each trader has a minimum of two connections.

There is a stochastic component to the traders' decisions whether to update their beliefs about what drives changes in the global temperature (see above, under Learning).

### Collectives

There is no explicit collective action, but each trader is a member of a social network.

### Observation

We observe the beliefs of the traders and look for whether those beliefs converge toward a consensus about the true cause of climate change.

## Initialization

The "true" model of climate change is prescribed (either global temperature is proportional to $\log \mathrm{CO}_2$ or to TSI), and the parameters of the model (intercept, slope, and noise parameters) are fit to historical data.

Future temperatures are simulated, using the parameters from the fit.

Global parameters (real, $\in [0,1]$) control the maximum ideology and risk taking. Each trader's ideology and risk-taking are initialized to random values drawn from uniform distribution over $[0, \text{ideology}_{\text{max}}]$ and $[0, \text{risk-taking}_{\text{max}}]$, respectively. Each trader's belief about what causes climate change is randomly initialized to either $\mathrm{CO}_2$ or to TSI.

Traders' approximate models are initialized by fitting a linear model corresponding to the agent's belief about the true driver of global temperature.

The segmentation of the social network is drawn from a uniform distribution on $[0,1]$.

The social network is initialized by assigning each agent two connections, where the probability of connecting to another agent with matching belief is $(1 - \text{segmentation}) / 2$. After each agent has two connections, additional edges are added to the social network until the network has the specified number of edges.

The number of closed security intervals $N$, above) is set to 10, so there are 12 types of security (10 closed intervals plus two open intervals).

## Input data

Historical values for atmospheric $\mathrm{CO}_2$ concentrations, TSI, and global average temperature and projected values of future $\mathrm{CO}_2$ concentrations and TSI are read in from files. We take future $\mathrm{CO}_2$ from the RCP 8.5 pathway [@Riahi_2011,@rcp_database_2009] and TSI from @Velasco_Herrera_2015.

## Submodels

### Global temperature:

Global temperature at time $t$ is modeled as
$$T(t) = a + b F(t) + \varepsilon,$$
where $F$ is the climate forcing (either $\log \mathrm{CO}_2$ or TSI), and  $\varepsilon$ is a random deviate drawn from an $\text{ARMA}(p,q)$ process. Bayesian linear regression on historical data is used to obtain posterior probability distributions for $a$, $b$, and the parameters characterizing the ARMA noise process $\varepsilon$. The ARMA order $(p,q)$ is determined by performing regressions for $p, q \in \lbrace 0, 2, 1 \rbrace$ and choosing the set of $(p,q)$ that minimizes the Widely Applicable Information Criterion [@Watanabe_2013,@Gelman_2014]. For historical data sampled at one-year intervals, this procedure chose $\text{AR}(1)$ for both $\log \mathrm{CO}_2$ and TSI models.

The model takes historical temperatures for past dates, and simulates the temperature at a future time $t'$ by drawing values for $a$, $b$ and the parameters for the ARMA noise process from the posterior probability distributions and then calculating the expected value of the temperature $a + b F(t')$ and adding noise drawn from a stochastic $\text{ARMA}(p,q)$ time-series.

The global temperature time-series for the entire simulation is calculated during initialization.

### Traders' models of future global temperature:

As the simulation progresses, at each time step, traders fit their own models to the equation above, based on the temperature to date (this is the model's time series, which was created during initialization, so if the model is initialized with historical temperatures through 2015, then in simulating the year 2030, traders would know temperatures through the year 2029, which would correspond to the actual historical temperature record through 2015 plus the model's simulated time series for 2016--2029 and the traders would fit parameters $a$, $b$, etc. to this time series). 

At each time step, after the traders fit model parameters to the temperature time-series, they draw 1600 random sets of parameters from the posterior probability distributions and for each set of parameters, they simulate 7 time series from the current time-step through the end of the current trading sequence (1--6 years) to produce an ensemble of 11,200 simulated time-series. The set of temperatures at the final time-step of each simulation in the ensemble form a sample of possible temperatures at the end of the current trading sequence. Traders assign probabilities to each security based on the fraction of temperatures in this sample that fall within that security's temperature interval.

### Generating buy and sell offers

Each trader has a risk-taking preference $r \in [0,1]$. If the trader assigns $v_i$ as the expected value of security $S_i$ at the current time step, then she randomly selects a buying price  $p_b \in [(1 - r) v_i, v_i]$ and a selling price $p_s \in [v_i, (1 + r) v_i]$. The trader will then buy one share of the security if a counterparty offers to sell it for a price $p \le p_b$ and will sell one share if she owns one and a counterparty offers to buy it for a price $p \ge p_s$.

Smaller values of $r$ keep the ask and offer prices close to the expected value of the security, which means that a larger fraction of counterparty offers will be acceptable. Larger values of $r$ mean that the trader may assign smaller buy offers and larger sell offers. These would increase the trader's profits if trades occur, but also increase the probability that no counterparty's offer will be acceptable.

# References
