# Derivatives Analysis

[![R](https://img.shields.io/badge/R-4.x-blue.svg)](https://www.r-project.org/)
![GitHub last commit](https://img.shields.io/github/last-commit/vladislavpyatnitskiy/deRivatives.svg)

## Overview
This repository focuses on financial derivatives, covering fundamental pricing models, trading strategies, and risk management techniques. The goal is to provide well-documented implementations in R for analysing and understanding derivative instruments.

## Topics Covered

### 1. Binomial Trees
- Implementation of binomial tree models for pricing options.

### 2. Trading Strategies
A collection of options trading strategies used for hedging, speculation, and arbitrage:
- **Bull Spread** – A strategy using call options to profit from moderate price increases.
- **Bear Spread** – A strategy using put options to profit from moderate price decreases.
- **Butterfly Spread** – A neutral strategy involving multiple strike prices.
- **Straddle** – A volatility-based strategy using both call and put options.
- **Strangle** – A variation of the straddle with different strike prices.
- **Strip** – A strategy betting on higher volatility with more puts than calls.
- **Strap** – Similar to the strip but with more calls than puts.

### 3. Confidence Intervals
- Estimating confidence intervals for derivative pricing models.

### 4. Greeks
- **Delta** – Measures the sensitivity of an option's price to changes in the underlying asset's price.
- **Gamma** – Measures the rate of change of Delta with respect to the underlying asset's price.
- **Theta** – Represents the time decay of an option's price.
- **Vega** – Measures sensitivity to volatility changes in the underlying asset.
- **Rho** – Measures sensitivity to interest rate changes.
- Applications of Greeks in risk management and portfolio hedging.
