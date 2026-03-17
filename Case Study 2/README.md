# Expected Goals (xG) Model — Women's Soccer

**Logistic regression model predicting shot conversion probability in the 2018 NWSL season.**

Built for the Applied Data Analytics in Sports Management course at FSU. The goal was to build an xG model from scratch using shot-level tracking data, then use it to evaluate individual player and team finishing performance.

---

## What it does

Uses shot-level data from the 2018 NWSL season to predict the probability of any given shot resulting in a goal. The model accounts for shooting position, goalkeeper placement, defensive pressure, and possession context.

**Brier Score: 0.0793** — indicating strong probability calibration across all shot types.

---

## Key findings

- **Goalkeeper position is the strongest predictor** — a keeper in the shooter's direct path reduces goal probability by ~2.7%
- **Defensive congestion matters** — each additional defender in the shooting cone drops conversion rate by ~2.3%
- **Distance to goal** reduces probability by ~0.98% per unit — but keeper distance and defensive pressure add meaningful context beyond raw distance alone
- **Best finisher:** Christen Press — scored 0.737 goals above expected in a single instance
- **Team overperformers:** Portland Thorns (+1.8 goals above xG) and Utah Royals (+0.6)
- **Biggest underperformer:** Washington Spirit — projected 9.71 goals, scored only 3 (–6.71 vs expected)

---

## Model variables

| Variable | Effect on goal probability |
|---|---|
| Distance to goal | −0.98% per unit increase |
| Distance to keeper | +0.56% per unit increase |
| Defenders in cone | −2.3% per additional defender |
| Goalkeeper in cone | −2.7% if keeper in path |
| Defenders behind ball | −0.82% per additional defender |
| Distance to nearest defender | +0.6% per unit increase |
| Possession | −0.01% per possession |

---

## Tech

- **Language:** R
- **Methods:** Logistic regression, marginal effects, Brier score evaluation
- **Data:** 2018 NWSL shot-level tracking data

---

*Julia Sveen · Florida State University · Completed in 2024*
