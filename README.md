# wr-applications

Shiny app mapping **active New and Change water right applications** from WA Ecology's
Water Rights Tracking System (WRTS). Data refreshes every Monday morning via GitHub
Actions; Posit Connect Cloud republishes from each new commit.

## Folder layout

```
wr-applications/                      <- RStudio project folder = GitHub repo
├── app.R                             the Shiny app                    (deploy)
├── manifest.json                     made by rsconnect, step 4         (deploy)
├── data/
│   ├── applications.rds              made by the pipeline             (deploy)
│   └── boundaries.rds                made by the pipeline             (deploy)
├── pipeline/
│   ├── run_pipeline.R                runs the three steps below in order
│   ├── fetch_wrts_export.R           1. query WRTS + download .xlsx  -> pipeline/raw/
│   ├── geocode_records.R             2. PLSS -> lon/lat (cached)     -> pipeline/work/
│   ├── prepare_app_data.R            3. build data/*.rds
│   └── cache/section_bbox_cache.csv  PLSS section cache (copied from permit-events)
├── .github/workflows/refresh-data.yml   weekly schedule (Mon 14:47 UTC)
├── .gitignore                        keeps raw/, work/, .Renviron out of GitHub
└── wr-applications.Rproj
```

## One-time setup

1. **Create the repo** `jeffmarti/wr-applications` on GitHub (empty, no README),
   then clone it in RStudio (File > New Project > Version Control > Git).
2. **Copy these files** into the project folder, keeping the folder layout above.
   `.github` and `.gitignore` start with a dot, so they are hidden in Finder/Explorer;
   check they copied.
3. **Build the data once locally** (RStudio console, project open):
   ```r
   source("pipeline/run_pipeline.R")
   ```
   Read the summaries it prints, especially the **APP DATA SUMMARY**. Then run the app
   with the **Run App** button and look it over.
4. **Write the manifest** Connect Cloud needs (same as permit-events):
   ```r
   rsconnect::writeManifest(appFiles = c("app.R", "data/applications.rds",
                                         "data/boundaries.rds"))
   ```
5. **Commit and push** everything (Git pane > Commit > Push).
6. **Publish on Posit Connect Cloud** from the GitHub repo (branch `main`,
   primary file `app.R`) with "republish on push" turned on. Add `CARTO_API_KEY` in the
   **Variables** section of the app's settings.
7. **Test the workflow**: GitHub > Actions > "Refresh application data" > Run workflow.

## Weekly routine

Nothing, unless the Monday run fails (GitHub emails you). Before editing locally,
always `git pull` first, because the workflow commits every week.
