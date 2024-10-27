This project evaluates and prioritizes protected areas across West Africa by 
calculating and mapping key biodiversity and conservation metrics, including species 
richness, threatened species index, phylogenetic diversity, and habitat degradation. 
This project aims to inform conservation planning and improve biodiversity outcomes 
for protected areas across West Africa.

## Project Structure

- **Data Processing**: Gathers data (from GBIF and Laboratory of Applied Ecology) 
on species, IUCN statuses, and land cover across West African protected areas.
- **Index Calculation**: Computes key biodiversity and habitat degradation metrics.
- **Mapping and Visualization**: Generates maps of biodiversity indices.
- **Prioritization**: Ranks protected areas based on calculated indices for conservation priority.

## Data Requirements

This project requires the following datasets:

- **Species Data**: Contains species names, taxon keys, and IUCN Red List categories for each species in West Africa.
- **Protected Area Species Data**: List of species found in specific West African protected areas.
- **Land Cover Data**: Land cover data to calculate habitat degradation over time (2017-2023).
- **Geospatial Data**: Shapefiles for West African boundaries and protected areas.

## Analysis Workflow

1. **Species IUCN Status Retrieval**:
   - Uses the GBIF API to fetch IUCN categories for species within protected areas.
   - Missing values are manually completed where needed.

2. **Biodiversity Indices Calculation**:
   - **Specific Richness**: Counts species per protected area.
   - **Threatened Species Index (TSI)**: Quantifies the risk level by scoring species based on IUCN threat categories.
   - **Phylogenetic Diversity**: Assesses evolutionary richness using species phylogenetic trees.

3. **Habitat Degradation Rate**:
   Calculates habitat loss by comparing natural vs. anthropogenic land use changes between 2017 and 2023.

4. **Prioritization and Mapping**:
   - Combines indices into a unified score and ranks protected areas into priority 
   levels for conservation (Top, High, Midlevel, Low, Least).
   - Generates maps for each metric and priority rankings.

## Outputs

Maps and rankings are saved in the `plots/` folder:
- **Species Richness Map**: Displays species counts across protected areas.
- **Threatened Species Index Map**: Visualizes areas with high concentrations of threatened species.
- **Phylogenetic Diversity Map**: Shows evolutionary richness.
- **Habitat Loss Map**: Highlights protected areas with significant habitat degradation.
- **Priority Ranking Map**: Prioritizes protected areas based on cumulative biodiversity indices.