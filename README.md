## `aae.data`: methods to perform QA/QC on AAE data 

The `aae.data` package supports QA/QC of data sets to be uploaded to the database. Future extensions will include tools to reformat and analyse data from the AAEDB.

## Usage

The package is currently available on GitHub and can be installed with the `remotes` package:

```
remotes::install_github("aae-stats/aae.data")
```

The main function in the `aae.data` package is `generate_report`, which checks a provided data set for consistency with AAEDB requirements. The default output of this function is a HTML document providing a full report on the input data set. This function can be used with any matrix, data.frame, or tibble:

```
# load some data
data <- read.csv("data/to/load")

# generate a report based on this data set
generate_report(data)
```

## Contact

Please leave feedback, bug reports, or feature requests at the GitHub [issues page](https://github.com/aae-stats/aae.data/issues).

Last updated: 7 December 2022

