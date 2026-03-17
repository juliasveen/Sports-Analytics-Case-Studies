# Dodgers Stadium Attendance Analysis

**Regression analysis predicting percent capacity at Dodgers Stadium from 2014–2022.**

Built for the Applied Data Analytics in Sports Management course at FSU. The goal was to figure out what actually drives attendance — opponent, month, team performance — and build a model that could predict it.

---

## What it does

Uses MLB attendance and venue capacity data to predict what percentage of Dodgers Stadium fills up on any given game day. Three regression models were built and compared using R-squared, MAE, and RMSE.

**Best model (Model 3) explained ~22% of variance in percent capacity** using a combination of:
- Runs scored / runs allowed going into the game
- Opponent win percentage
- Specific high-draw opponents (Giants, Angels)
- Month (July–October games draw more)
- Saturday games

---

## Key findings

- **Opponent matters** — the Giants, Angels, and Astros consistently pull higher attendance
- **Summer and postseason months** (July–October) are the strongest seasonal predictors
- **Team and opponent stats** (runs scored/allowed) have more predictive power than raw win/loss record
- Results could inform the Dodgers' marketing strategy and ticket pricing by game

---

## Models

| Model | Key variables | R² | MAE |
|---|---|---|---|
| Model 1 | Win %, games back, day of week, month | 0.1655 | 0.090 |
| Model 2 | Runs scored/allowed, specific opponents | 0.1016 | 0.093 |
| Model 3 | Combined best variables from 1 & 2 | **0.2186** | 0.094 |

---

## Tech

- **Language:** R
- **Methods:** Multiple linear regression, dummy coding, dataset merging
- **Data:** MLB_Attendance, MLB_Venue_Capacity

---

*Julia Sveen · Florida State University · Completed in 2023*
