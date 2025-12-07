# Downloadable pipeline

This folder contains a copy of the full `hefml_cbb_pipeline.R` script so it can be downloaded directly from the repository without digging through diffs.

- `hefml_cbb_pipeline.R`: End-to-end college basketball modeling pipeline with API data fetch, feature engineering, training (CatBoost moneyline + quantile RF spread), validation helpers, and export of predictions/metrics.

## Direct HTTP link (once pushed to GitHub)
If this repository is hosted on GitHub, you can grab the script via the raw-file endpoint:

```
https://raw.githubusercontent.com/<YOUR_ORG_OR_USER>/HEFML-CBB/main/downloads/hefml_cbb_pipeline.R
```

Replace `<YOUR_ORG_OR_USER>` with your GitHub namespace. You can also download from the command line:

```
wget https://raw.githubusercontent.com/<YOUR_ORG_OR_USER>/HEFML-CBB/main/downloads/hefml_cbb_pipeline.R
```

If you are browsing in GitHub’s UI, open `downloads/hefml_cbb_pipeline.R` and click **Download raw file** to save it locally.

## Reading the outputs
- **10th/50th/90th percentiles (spread intervals):** These correspond to the lower-tail, median, and upper-tail predicted spread (home minus away) from the quantile regression forest. A negative number means the model expects the home team to lose by that many points (away favored). A positive number means the home team is favored by that many points. The 10th percentile is a “best-case for the away team” scenario, the 50th is the central estimate, and the 90th is a “best-case for the home team” scenario.
- **Who is favored in the UI:** Anywhere you see spreads/edges as numbers, **positive = home favored**, **negative = away favored**. In the cards/tables, chips with green shading indicate the side the model prefers; red indicates the other side. If you only see the numeric spread, just read the sign: positive values favor the listed home team; negative values favor the away team.
