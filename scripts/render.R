library(rmarkdown)
code_dir <- "notebooks"
report_filename <- "5.Appendix-Updated-January2023.Rmd"
report_filename <- file.path(code_dir, report_filename)
output_dir <- "/Users/jodydaniel/OneDrive/Coding Projects/Trait-Performance-Relationships/notebooks"
output <- file.path("..", output_dir)
render(report_filename, output_dir = output_dir, params  = list(output_dir, output))



library(rmarkdown)
code_dir <- "notebooks"
report_filename <- "5.Appendix.Rmd"
report_filename <- file.path(code_dir, report_filename)
output_dir <- "/Users/jodydaniel/OneDrive/Coding Projects/Trait-Performance-Relationships/notebooks"
output <- file.path("..", output_dir)
render(report_filename, output_dir = output_dir, params  = list(output_dir, output))


library(rmarkdown)
code_dir <- "notebooks"
report_filename <- "4.GBMGrowthRatesExploration.Rmd"
report_filename <- file.path(code_dir, report_filename)
output_dir <- "/Users/jodydaniel/OneDrive/Coding Projects/Trait-Performance-Relationships/notebooks"
output <- file.path("..", output_dir)
render(report_filename, output_dir = output_dir, params  = list(output_dir, output))

library(rmarkdown)
code_dir <- "notebooks"
report_filename <- "3.RemoveInfluenceSamplingDate.Rmd"
report_filename <- file.path(code_dir, report_filename)
output_dir <- "/Users/jodydaniel/OneDrive/Coding Projects/Trait-Performance-Relationships/notebooks"
output <- file.path("..", output_dir)
render(report_filename, output_dir = output_dir, params  = list(output_dir, output))


library(rmarkdown)
code_dir <- "notebooks"
report_filename <- "2.PCAEnvironmentalTraits.Rmd"
report_filename <- file.path(code_dir, report_filename)
output_dir <- "/Users/jodydaniel/OneDrive/Coding Projects/Trait-Performance-Relationships/notebooks"
output <- file.path("..", output_dir)
render(report_filename, output_dir = output_dir, params  = list(output_dir, output))



library(rmarkdown)
code_dir <- "notebooks"
report_filename <- "1.DataExplorationCleaning.Rmd"
report_filename <- file.path(code_dir, report_filename)
output_dir <- "/Users/jodydaniel/OneDrive/Coding Projects/Trait-Performance-Relationships/notebooks"
output <- file.path("..", output_dir)
render(report_filename, output_dir = output_dir, params  = list(output_dir, output))

