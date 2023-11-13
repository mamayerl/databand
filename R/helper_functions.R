
## Check if varnames are identical. Future: Add suffix to identical variablesdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==
check_varnames <- function(x, y){
  if(isTRUE(any(x %in% y))){
    stop("At least one input variables (var vs. byvar) are identical. Check if variable names are correct.")
    #print("At least one input variables (var vs. byvar) are identical. Check if variable names are correct.")
  }
}

