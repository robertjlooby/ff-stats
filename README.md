# ff-stats

# Output a new csv with the player's projected scores

`stack exec fetch-projections -- --file /path/to/csv --out /path/to/new-csv --week 2`

# Get the best lineups (from a csv that has projections)

`stack exec best-teams -- --file /path/to/csv --out /path/to/new-csv --pool 100 --salary 50000 --count 5`
