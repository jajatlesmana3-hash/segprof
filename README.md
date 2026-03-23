# segprof

Customer Segment Profiler with Automatic Segmentation

## Features

- Automatic RFM + K-means segmentation from transaction data
- Profiling of existing segments if provided
- Handles mixed data types (numeric, categorical)
- Produces summary table and professional visualizations:
  - Segment size bar chart
  - Radar chart (normalized means)
  - 2D scatter with ellipses
  - 3D interactive scatter (optional)

## Installation

```r
devtools::install_github("jajat/segprof")
