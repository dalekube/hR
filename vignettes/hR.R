## ----setup, include = FALSE---------------------------------------------------
library(hR)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----workforceHistory---------------------------------------------------------
data("workforceHistory")

# Reduce to DATE <= today to exclude future-dated records
dt = workforceHistory[DATE<=Sys.Date()]

# Reduce to max DATE and SEQ per person
dt = dt[dt[,.I[which.max(DATE)],by=.(EMPLID)]$V1]
dt = dt[dt[,.I[which.max(SEQ)],by=.(EMPLID,DATE)]$V1]

# Only consider workers who are currently active
# This provides a reliable 'headcount' data set that reflects today's active workforce
dt = dt[STATUS=="Active"]

# Exclude the CEO because she does not have a supervisor
CEO = dt[TITLE=="CEO",EMPLID]
dt = dt[EMPLID!=CEO]

# Show the prepared table
# This represents an example, active workforce
print(dt[,.(EMPLID,NAME,TITLE,SUPVID)])

## ----hierarchyLong------------------------------------------------------------
hLong = hierarchy(dt$EMPLID,dt$SUPVID,format="long")
print(hLong)

# Who reports up through Susan? (direct and indirect reports)
print(hLong[Supervisor==CEO])

## ----hierarchyWide------------------------------------------------------------
hWide = hierarchy(dt$EMPLID,dt$SUPVID,format="wide")
print(hWide)

# Who reports up through Pablo? (direct and indirect reports)
print(hWide[Supv2==199827])

## ----hierarchyStats-----------------------------------------------------------
hStats = hierarchyStats(dt$EMPLID,dt$SUPVID)

# Total Levels:
print(hStats$levelsCount$value)

# Total Individual Contributors:
print(hStats$individualContributorsCount$value)

# Total People Managers:
print(hStats$peopleManagersCount$value)

# Median Direct Reports:
print(hStats$medianDirectReports$value)

# Median Span of Control (Direct and Indirect Reports):
print(hStats$medianSpanOfControl$value)

# Span of Control Table
print(hStats$spanOfControlTable)

