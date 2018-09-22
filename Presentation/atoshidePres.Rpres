AToShiDe

========================================================
title: false
author: Pascal
date: 22-09-2018
transition: rotate
incremental: true
navigation: slide 
autosize: true
width: 1200
height: 1024

Another Toy Shiny Demonstration [あとしで]

<small>Coursera JHU, Developing Data Product</small>


Purpose
========================================================
type: section

<small>
- Apply different algorithms on same dataset and compare their results according to metric criteria such as `accuracy` or `kappa`,
- Extend this process to several datasets (for `classification`) obtained from UCI repository,
- Split dataset into two subsets: `training` and `test`,
- Choose type of `cross validation` and `metric`,
- Train model with given algorithm and use result to calculate `accuracy`, `kappa` (metrics) and out-of sample error (oose) on test set,
- Report `metrics`, `oose` and `user time` taken to execute algorithm,
- Report best results according to chosen metric,
- Display basic plots about metrics.
</small>

In practice, as a proof of concept
========================================================
type: section

<small>

- Two datasets available: 
  - `ionosphere` [https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/]
  - `phishing data` [https://archive.ics.uci.edu/ml/machine-learning-databases/00379/]
- Split of dataset selectable (default 70% for train dataset)
- List of algorithms limited to four:
  - `C5.0` [Decision trees], `SGB` [Stochastic Gradent Boosting],
  - `Bagged CART` [Classification and Regression Trees] and `Bagged RF` [Random Forest].
- Two type of `k-fold cross validation` (k=10, fixed):
  - Simple `cv` and Repeated `cv` (`3 k-fold`)
- Two metrics: 
  - `Accuracy` and `Kappa`

</small>

About tradeoffs
========================================================
type: section

The tradeoffs are between fluidity/responsiveness and long computation

<small>
- As the size of dataset increases and depending on selected parameters and algorithm, a result can take a while to be  computed.
- So, in order to keep the fluidity, I relied on caching by pre-computing a subset of the possible results (using a fixed seed).
- However, not all the subsets were pre-computed, so you can see for yourself (for example with a 60% split, ...).
</small>


Snapshot
========================================================
<img src='./atoshidePres.R-figure/AToShiDe.png' alt="AToShiDe" style="width:90%;height:90%;border:0;float:right;">
