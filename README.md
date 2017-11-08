# ff-stats

# Output a new csv for classic with the players projected scores

`stack exec fetch-classic-projections -- --file /path/to/csv --out /path/to/new-csv --week 2`

# Get the best classic lineup (from a csv that has projections)

`stack exec best-classic -- --file /path/to/csv --pool 100 --salary 50000 --count 5`
