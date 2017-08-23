# R-ShinyCheezyDemo
Shiny App demo using tickets data for novelty detection analytics using R. Using IsolationTrees Novelty Scoring Models from IsolationForest package to identify anomalies in tickets count.


### Installing:

1. Save "tickets.RData", "server.R", "ui.R", "Messages.csv" file and "www" in the same folder to your local drive on pc

2. Open RStudio and load in the tickets data "tickets.RData" (code snippet example below). Run below code only once

3. Open "server.R" and "ui.R" files in Rstudio


```
# Load Tickets data (ticket.data)

load("tickets.RData")

attach(ticket.data)

```
