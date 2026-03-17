# NFL Wide Receiver Clustering — Draft Prospect Analysis

**K-means clustering model grouping NFL wide receivers by performance profile to evaluate 2024 draft prospects.**

Built for the Applied Data Analytics in Sports Management course at FSU. The goal was to identify player archetypes among historical WR data and see where 2024 draft prospects fit relative to established NFL players.

---

## What it does

Uses combine metrics and college performance stats to cluster wide receivers into similarity groups, then maps 2024 NFL draft prospects onto those clusters to identify comparable players already in the league.

**Optimal clusters: 5**, determined via elbow plot analysis.

---

## Key findings

- **Cluster 1** — tall, agile receivers with strong physical profiles but below-average production metrics (receptions, receiving yards). Similar players: Seth Williams, Kadarius Toney. 2024 prospects: Johnny Wilson, Adonai Mitchell
- **Cluster 2** — solid reception numbers and per-game production, but limited in contested catch situations
- **Cluster 3** — projected underperformers, well below average across receptions, receiving yards, and receptions per game
- **Cluster 4** — best draft position on average (~95th pick / 3rd round). Similar players: Jaxon Smith-Njigba, Zay Flowers. 2024 prospects: Xavier Worthy, Ladd McConkey
- **Cluster 5** — underrated profile — not elite athletically, but strong production numbers across the board

**Draft trend:** Cluster 4 gets drafted earliest on average, though differences between groups are relatively small — suggesting draft position doesn't always align neatly with player archetype.

---

## Tech

- **Language:** R
- **Methods:** K-means clustering, elbow plot, cluster visualization, z-score standardization
- **Data:** WR_Clustering dataset (historical NFL WR stats + 2024 draft prospects)

---

*Julia Sveen · Florida State University · Completed in 2024*
