## Reason for update
Submitting an immediate patch to hide the sub-functions from user space. One of these sub-functions specifically "plot_violin_1x.R" conflicts with the main function-defined  graphical parameters on exit of the function which renders the resulting figure malformed.

## Test environments
R version 3.6.2 (2017-01-27)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 16.04.6 LTS

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.

## Spell checking
Hex color codes are being identified as misspelled words.
github, Github, https, jeffersonfparil, md, README, and YYYYMMDD are not mispelled.

## Unexpected files and folders
The "misc/" directory is storing a README_for_github.md as well as the two png images used in this file.
