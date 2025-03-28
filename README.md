# Derivatives Analysis

[![R](https://img.shields.io/badge/R-4.x-blue.svg)](https://www.r-project.org/)
![GitHub last commit](https://img.shields.io/github/last-commit/vladislavpyatnitskiy/deRivatives.svg)

## Overview
This repository focuses on financial derivatives, covering fundamental pricing models, trading strategies and risk management techniques. The goal is to provide well-documented implementations in R for analysing and understanding derivative instruments.

![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Stock%20vs%20Options%20PNL.png?raw=true)
**Fig. 1. Comparison of pay-offs between buying stocks and options**

## Topics Covered

### 1. Binomial Trees
- Implementation of binomial tree models for pricing options.

### 2. Trading Strategies
A collection of options trading strategies used for hedging, speculation and arbitrage:

#### Spreads
##### Bull
**a strategy using call options to profit from moderate price increases.**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Spreads/Bull%20Spread%20Using%20Calls.png?raw=true)
   **Fig. 2. Bull Spread using Calls**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Spreads/Bull%20Spread%20Using%20Puts.png?raw=true)
  **Fig. 3. Bull Spread using Puts**
  
##### Bear
**a strategy using put options to profit from moderate price decreases.**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Spreads/Bear%20Spread%20Using%20Calls.png?raw=true)
   **Fig. 4. Bear Spread using Calls**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Spreads/Bear%20Spread%20Using%20Puts.png?raw=true)
  **Fig. 5. Bear Spread using Puts**

##### Butterfly
**a neutral strategy involving multiple strike prices.**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Spreads/Butterfly%20Spread%20Using%20Calls.png?raw=true)
   **Fig. 6. Butterfly Spread using Calls**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Spreads/Butterfly%20Spread%20Using%20Puts.png?raw=true)
  **Fig. 7. Butterfly Spread using Puts**

#### Combinations
##### Straddle
**a volatility-based strategy using both call and put options.**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Combos/Straddle%20Combination.png?raw=true)
   **Fig. 8. Straddle Combination**

##### Strangle
**a variation of the straddle with different strike prices.**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Combos/Strangle%20Combination.png?raw=true)
   **Fig. 9. Strangle Combination**

##### Strip
**a strategy betting on higher volatility with more puts than calls.**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Combos/Strip.png?raw=true)
   **Fig. 10. Strip Combination**

##### Strap
**a similar to the strip but with more calls than puts.**

  ![](https://github.com/vladislavpyatnitskiy/deRivatives/blob/main/Trading%20Strategies%20Visualisation/Plots/Combos/Strap.png?raw=true)
   **Fig. 11. Strap Combination**

### 3. Confidence Intervals
- Estimating confidence intervals for derivative pricing models.

### 4. Greeks
- **Delta** – Measures the sensitivity of an option's price to changes in the underlying asset's price.
- **Gamma** – Measures the rate of change of Delta with respect to the underlying asset's price.
- **Theta** – Represents the time decay of an option's price.
- **Vega** – Measures sensitivity to volatility changes in the underlying asset.
- **Rho** – Measures sensitivity to interest rate changes.
- Applications of Greeks in risk management and portfolio hedging.

```
             Call         Put
Delta  0.05417375 -0.06985850
Theta -4.30538996 -1.85300567
Gamma  0.06554538  0.06554538
Vega  12.10524275 12.10524275
Rho    8.90657410 -9.95716588
```

## References

Hull, J.C. (2015) Options, Futures, and other derivatives. 9th edn. Pearson Education.
