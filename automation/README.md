# Automation Helpers

This directory contains optional non-interactive reporting helpers that reuse the app import, transformation, plotting, and summary code. It is kept in the public repo as secondary material rather than the main entry point.

Institution-specific scheduling, hosting, data-source, and notification setup are intentionally omitted from the public release. The included example configuration and preview commands are meant for local inspection only.

## Quick preview

```bash
Rscript --vanilla automation/preview_report.R --format preview --verbose
```

This writes a local HTML preview using the synthetic example data in `inst/templates/`.

## Included files

- `automation/run_reports.R`: batch entry point
- `automation/preview_report.R`: local preview helper
- `automation/config/`: example study, schedule, and style settings
- `automation/reports/study_report.Rmd`: report template
