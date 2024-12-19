# RetractionFindR
Academic journals sometimes retract articles they've published, usually because there were significant flaws in how the research was conducted, such that the results cannot be trusted. A recent study found that 90% of retracted articles continue to be cited after they've been retracted. When retracted articles are included in literature reviews (particularly systematic reviews), they can distort the reviews' conclusions about the current state of scientific knowledge.

RetractionFindR provides an easy way to check whether a set of references contains any retracted studies. The package reads RIS files and compares the references with the RetractionWatch database, the most comprehensive source of data about journal retractions.

RetractionFindR is particularly useful for checking whether any studies you plan to screen for or include in a systematic review have been retracted. But it can be used to check any set of references.

To use RetractionFindR, for now you can download/install a local version of the package from this GitHub repository (e.g., using `devtools::install_github`). But there are plans for a Shiny app version in the near future. Stay tuned!