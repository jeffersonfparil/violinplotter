# R package submission to CRAN 

## Install dependencies
```
sudo apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
install.packages("devtools") ### for packaging and testing
install.packages("roxygen2") ### for documentation
install.packages("spelling") ### for spell_check
install.packages("rhub") ### for R-hub check
```

### Spell check
```
devtools::spell_check()
```

### Documentation
```
devtools::document()
```

### Check R-hub
```
devtools::check_rhub()
# rhub::validate_email(email = "jeffersonparil@gmail.com", token = "2ea6e47d16274a32a4cca5f744e5f20") ### if token is unavailable
```

### Check win-builder
```
devtools::check_win_devel()
```

### Build and test
```
devtools::release()
```
