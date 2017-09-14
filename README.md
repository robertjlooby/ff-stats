# ff-stats

# Get a player's projected score for a week

`stack exec fetch-week-projection -- --name 'Player Name' --week 1`

# Get the best pick 'em lineup for a week

`stack exec best-pick-em -- --file /path/to/csv --week 2`

# Output a new csv for pick 'em with the players projected scores

`stack exec fetch-pick-em-projections -- --file /path/to/csv --out /path/to/new-csv --week 2`
