Place screenshots or exported example plots in this directory using these filenames:

- `app-gui.png`
- `weight-plot.png`
- `survival-plot.png`

The plot images can be rendered from synthetic example data with:

```bash
Rscript scripts/render_public_assets.R
```

`app-gui.png` still needs to be captured manually from a running app session, and is
currently absent. When recapturing it, load the bundled example dataset first (Import tab
-> "Load example data", backed by `inst/templates/example_weights.csv`). Only the bundled
example data may appear in committed images -- never real study data.
