CliMPA refined website files

Files included
- index.html
- styles.css
- nav.js
- factsheets.html
- factsheet-mimp.html
- data-source.html
- team.html

What was changed
- Reworked the home page into a centered, full-width hero.
- Added three existing-site links as clear home-page actions.
- Standardized centered page introductions across Fact Sheets, MIMP Fact Sheet,
  Data Source, and Team pages.
- Removed the default-open state from the Fact Sheets dropdown.
- Simplified repeated footer text.
- Improved responsive navigation, cards, tables, page spacing, and typography.
- Kept existing URLs and asset paths intact where possible.
- Kept climate-dashboard.html links intact. The embedded dashboard itself was
  not modified and remains separately managed.
- Retained CSS support for .embed-wrap, iframe, toolbar, and external-open link.

Deployment
1. Back up the current site files.
2. Replace the corresponding HTML/CSS/JS files with these versions.
3. Keep the existing assets/ directory and climate-dashboard.html in place.
4. Test index.html, the Fact Sheets dropdown, the MIMP PDF download, Team
   images, Data Source external links, and the dashboard embed after deployment.

Final update
- Top navigation/header is sticky on all pages and remains visible while content scrolls.
- Removed the three redundant home hero buttons because those destinations are already in the main navigation.
