# Get metadata and find potential categories in data
metadata <- findCategories(val2006R)
metadata

# Find clusters in data
election_data <- val2014K %>%
  select(NAMN, KOMMUN_ID, largest_party, largest_party_percent, starts_with("PROCENT_ANDRING")) %>%
  tbl_df
clustered_data <- findClusters(election_data)
clustered_data$cluster

# Visualise clustered data
clusterVis(clustered_data)
