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
rhub::validate_email(email="jeffersonparil@gmail.com", token="6e4f2f593e0d4f328ec479648e23cee1")
rhub::validate_email()
# then select the email
# wait for the email with the new token and paste the new token
devtools::check_rhub(platforms="solaris-x86-patched")
```

### Check win-builder
```
devtools::check_win_devel()
```

### Build and test
```
devtools::release()
```
