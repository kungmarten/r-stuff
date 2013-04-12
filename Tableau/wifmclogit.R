# Ugly script to create a simulation worksheet for Tableau
# Only works on logistic regression with dummy variables.
# Tableau is buggy: the first time you open the worksheet you have to put a space in the [Predicted] field and save.

wifmclogit <- function(w, Z, f, drivername, dbname, driverversion, dbport, dbserver, dbuser) {
# w = glm() regression output
# Z = dataset
# f = desired filename
# The variables below can be acquired by opening an existing .tbw-file in notepad
# drivername = what DB driver to use 
# dbname = database name
# driverversion = version of Tableau driver
# dbport = database port
# dbserver = database server address
# dbuser = database user name
require(XML)
# Only use columns in model
k <- Z[, names(Z) %in% names(w$coefficients)]

# Create the regression list of independent variables to calculate on
wm <- as.matrix(w$coefficients)
cmu <- colMeans(k)

# Separate Intercept
wmi <- wm[1, ]
wmu <- wm[-1, ]
cl <- length(wmu)

wmu <- wmu[order(names(wmu))]
cmu <- cmu[order(names(cmu))]

# Bug in xmlTree, suppress nonsense warning
options(warn=-1)
tr = xmlTree("workbook", attrs = c("version" = '8.0'), namespace = list(user='http://www.tableausoftware.com/xml/user'))
# Reinstate warnings
options(warn=0)

  tr$addNode("preferences")
    tr$addNode("preference", attrs = c("name"='ui.encoding.shelf.height', "value" = '24'))
    tr$addNode("preference", attrs = c("name"='ui.shelf.height', "value"='26'))
  tr$closeNode()

  tr$addNode("datasources", close = FALSE)
    # Add parameters from the logit model
	tr$addNode("datasource", attrs = c("hasconnection"='false', "inline"='true', "name"='Parameters', "version"='8.0'), close = FALSE)
      tr$addNode("aliases", attrs = c("enabled"='yes'))
	  
	  # Add the intercept
	  tr$addNode("column", attrs = c("caption" = "Intercept", "datatype" = 'real', "name" = paste("[Intercept]", sep = ""), "param-domain-type" = "any", "role" = 'measure', "type" = 'quantitative', "value" = wmi[[1]]), close = FALSE)
	    tr$addNode("calculation", attrs = c("class" = 'tableau', "formula" = wmi[[1]]))
	  tr$closeNode()
	  
	  counter <- length(wmu)
	  
	  while (counter > 0) {
	    # Fill those beta values"
		bname <- paste("Beta", names(wmu[counter]))
		bvalue <- wmu[[counter]]
		
		# And them prob values
		pname <- paste("Prob", names(wmu[counter]))
		pvalue <- cmu[[counter]]
		
		# And building blocks
		bbname <- paste("Is", names(wmu[counter]))
		
		# And now populate the parameternodes with some beta values
	    tr$addNode("column", attrs = c("caption" = bname, "datatype" = 'real', "name" = paste("[", bname, "]", sep = ""), "param-domain-type" = "any", "role" = 'measure', "type" = 'quantitative', "value" = bvalue), close = FALSE)
	      tr$addNode("calculation", attrs = c("class" = 'tableau', "formula" = bvalue))
		tr$closeNode()
		
		# Keep producing the prob values but not for the intercept oc.
		# tr$addNode("column", attrs = c("caption" = pname, "datatype" = "real", "name" = paste("[", pname, "]", sep = ""), "param-domain-type" = "any", "role" = 'measure', "type" = 'quantitative', "value" = pvalue), close = FALSE)
	      # tr$addNode("calculation", attrs = c("class" = 'tableau', "formula" = pvalue))
		  # tr$addNode("range", attrs = c("max" = "1", "min" = "0"))
		# tr$closeNode()		
		
		# Keep producing the prob values but not for the intercept oc.
		tr$addNode("column", attrs = c("caption" = bbname, "datatype" = "integer", "name" = paste("[", bbname, "]", sep = ""), "param-domain-type" = "any", "role" = "measure", "type" = "nominal", "value" = "0"), close = FALSE)
	      tr$addNode("calculation", attrs = c("class" = 'tableau', "formula" = "0"))
		  # tr$addNode("aliases", close = FALSE)
		    # tr$addNode("alias", attrs = c("key" = "0", "value" = "No"))
			# tr$addNode("alias", attrs = c("key" = "1", "value" = "Yes"))
		  # tr$closeNode()
		  # tr$addNode("members", close = FALSE)
		    # tr$addNode("member", attrs = c("alias" = "No", "value" = "0"))
			# tr$addNode("member", attrs = c("alias" = "Yes", "value" = "1"))
		  # tr$closeNode()
		tr$closeNode()
		
		# Finish the loop!
		counter <- counter-1
		}
	tr$closeNode()
		
	# Add the data source
	tr$addNode("datasource", attrs = c("caption" = "Random data", "inline" = "true", "name" = "Random Numbers", "version" = "8.0"), close = FALSE)
	  tr$addNode("connection", attrs = c("class" = drivername, "dbname" = dbname, "expected-driver-version" = driverversion, "one-time-sql" = "", "port" = dbport, "server" = dbserver, "username" = dbuser), close = FALSE)
	    # Create the query
        usqlq <- "select * from ("
		sqlq <- paste("select 'Normal' as \"Boost\", ", wmi[[1]], " as \"Propensity\"", sep = "")
		counter <- length(wmu)
	  
	    while (counter > 0) {
		  nsqlentry <- paste("MC ", names(cmu[counter]), sep = "")
		  sqlq <- paste(sqlq, " , case when random() <= ", cmu[[counter]], " then 1 else 0 end as \"", nsqlentry, "\"", sep = "")
	      counter <- counter-1
		  }

		usqlq <- paste(usqlq, sqlq, " from (select * from tblUsers limit 100000) a", sep = "")
		
		for (i in 1:length(wmu)) {
		  sqlq <- paste(" union all select '", names(cmu[i]), "' as \"Boost\", ", wmu[[i]], " as \"Propensity\"", sep = "")
		  cmub <- cmu
		  cmub[i] <- cmub[i]*1.2
		  counter <- length(wmu)
	  
	      while (counter > 0) {
		    nsqlentry <- paste("MC ", names(cmub[counter]), sep = "")
		    sqlq <- paste(sqlq, " , case when random() <= ", cmub[[counter]], " then 1 else 0 end as \"", nsqlentry, "\"", sep = "")
	        counter <- counter-1
		    }
		  usqlq <- paste(usqlq, sqlq, " from (select * from tblUsers limit 100000) a", i,  sep = "")
		}		
	    
		usqlq <- paste(usqlq, ") as a")
	    tr$addNode("relation", attrs = c("name" = "TableauSQL", "type" = "text"), c(usqlq))
      tr$closeNode()
	  tr$addNode("aliases", attrs = c("enabled" = "yes"))
	
	  # Calculate the reponse
	  rcalc <- "[Intercept]"
	  counter <- length(wmu)
	  
	  while (counter > 0) {
	    
		bname <- paste("Beta", names(wmu[counter]))
		mcname <- paste("MC", names(cmu[counter]))
		rcalc <- paste(rcalc, " + [", bname, "]*[", mcname,"]", sep = "")
		counter <- counter-1
	    }
		
	  # Predicted score. Note that this is for logistic regression only
	  tr$addNode("column", attrs = c("aggregation" = "Avg", "default-format" = "p0.00%", "caption" = "Predicted", "datatype" = "real", "name" = "[Predicted]", "role" = "measure", "type" = "quantitative"), close = FALSE)
	    tr$addNode("calculation", attrs = c("class" = "tableau", "formula" = paste("1/(1+EXP(-1*(", rcalc, ")))", sep = "")))
	  tr$closeNode()
	  
	  # Calculate the interactive
	  rcalc <- "[Intercept]"
	  counter <- length(wmu)
	  
	  while (counter > 0) {
	    
		bname <- paste("Beta", names(wmu[counter]))
		mcname <- paste("Is", names(cmu[counter]))
		rcalc <- paste(rcalc, " + [", bname, "]*[", mcname,"]", sep = "")
		counter <- counter-1
	    }
		
	  # Predicted score of the interactive. Note that this is for logistic regression only
	  tr$addNode("column", attrs = c("aggregation" = "Avg", "default-format" = "p0.00%", "caption" = "Interactive Prediction", "datatype" = "real", "name" = "[Interactive Prediction]", "role" = "measure", "type" = "quantitative"), close = FALSE)
	    tr$addNode("calculation", attrs = c("class" = "tableau", "formula" = paste("1/(1+EXP(-1*(", rcalc, ")))", sep = "")))
	  tr$closeNode()
	  
	  
	  # Bin the predicted score to create a histogram
	  tr$addNode("column", attrs = c("caption" = "Predicted (bin)", "datatype" = "integer", "name" = "[Predicted (bin)]", "role" = "dimension", "type" = "ordinal"), close = FALSE)
	    tr$addNode("calculation", attrs = c("class" = "bin", "decimals" = "-2", "formula" = "[Predicted]", "peg" = "0", "size" = "0.01"))
      tr$closeNode()

      # Classify response as "survived" or "died"
      tr$addNode("column", attrs = c("caption" = "Survival", "datatype" = "string", "name" = "[Survival]", "role" = "dimension", "type" = "nominal"), close = FALSE)
	    tr$addNode("calculation", attrs = c("class" = "tableau", "formula" = "IIF([Predicted]>=0.5, \"Survived\", \"Died\")"))
	  tr$closeNode()
	  
	  # Root prediction of no stimulation
	  tr$addNode("column", attrs = c("aggregation" = "Avg", "default-format" = "p0.00%", "caption" = "Root Prediction", "datatype" = "real", "name" = "[Root Prediction]", "role" = "measure", "type" = "quantitative"), close = FALSE)
	    tr$addNode("calculation", attrs = c("class" = "tableau", "formula" = paste("1/(1+EXP(-1*(", wmi[[1]], ")))", sep = "")))
	  tr$closeNode()
	  
	  	  	  
	  tr$addNode("column", attrs = c("caption" = "Number of Records", "datatype" = "integer", "name" = "[Number of Records]", "role" = "measure", "type" = "quantitative", "user:auto-column" = "numrec"), close = FALSE)
	    tr$addNode("calculation", attrs = c("class" = "tableau", "formula" = "1"))
	  tr$closeNode()
	tr$closeNode()
  tr$closeNode()
tr$closeNode()
   
  # Create the worksheet 
  # tr$addNode("worksheets", close = FALSE)
    # tr$addNode("worksheet", attrs = c("name" = "Distribution Graph"), close = FALSE)
	  # tr$addNode("table", close = FALSE)
	  
	    # tr$addNode("view", close = FALSE)
          # tr$addNode("datasources", close = FALSE)
		    # tr$addNode("datasource", attrs = c("caption" = "Random data", "name" = "Random Numbers"))
		    # tr$addNode("datasource", attrs = c("name" = "Parameters"))
	      # tr$closeNode()
	      # tr$addNode("datasource-dependencies", attrs = c("datasource" = "Random Numbers"), close = FALSE)
	        # tr$addNode("column-instance", attrs = c("column" = "[Boost]", "derivation" = "None", "name" = "[none:Boost:nk]", "pivot" = "key", "type" = "nominal"))
	      # tr$closeNode()
	      # tr$addNode("filter", attrs = c("class" = "categorical", "column" = "[Random Numbers].[:Measure Names]"), close = FALSE)
	        # tr$addNode("groupfilter", attrs = c("function" = "level-members", "level" = "[:Measure Names]"))
	      # tr$closeNode()
	      # tr$addNode("filter", attrs = c("class" = "categorical", "column" = "[Random Numbers].[none:Boost:nk]"), close = FALSE)
	        # tr$addNode("groupfilter", attrs = c("function" = "member", "level" = "[none:Boost:nk]", "member" = "\"Normal\"", "user:ui-domain" = "database", "user:ui-enumeration" = "inclusive", "user:ui-marker" = "enumerate"))
	      # tr$closeNode()
	      # tr$addNode("slices", close = FALSE)
		    # tr$addNode("column", "[Random Numbers].[none:Boost:nk]")
		    # tr$addNode("column", "[Random Numbers].[:Measure Names]")
		  # tr$closeNode()
	      # tr$addNode("aggregation", attrs = c("value" = "true"))
		# tr$closeNode()
		
		# tr$addNode("style", close = FALSE)
		  # tr$addNode("style-rule", attrs = c("element" = "axis"), close = FALSE)
		    # tr$addNode("encoding", attrs = c("attr" = "space", "class" = "0", "field" = "[Random Numbers].[pcto:cum:sum:Number of Records:qk]", "field-type" = "quantitative", "fold" = "true", "scope" = "rows", "type" = "space"))
		  # tr$closeNode()
		# tr$closeNode()
		
	    # tr$addNode("panes", close = FALSE)
		  # tr$addNode("pane", close = FALSE)
		    # tr$addNode("view", close = FALSE)
			  # tr$addNode("breakdown", attrs = c("value" = "auto"))
			# tr$closeNode()
		    # tr$addNode("mark", attrs = c("class" = "Automatic"), close = FALSE)
              # tr$addNode("color", attrs = c("column" = "[Random Numbers].[:Measure Names]"))
            # tr$closeNode()
		  # tr$closeNode()
		  # tr$addNode("pane", attrs = c("id" = "1", "y-axis-name" = "[Random Numbers].[pcto:sum:Number of Records:qk]"), close = FALSE)
            # tr$addNode("view", close = FALSE)
              # tr$addNode("breakdown", attrs = c("value" = "auto"))
            # tr$closeNode()
            # tr$addNode("mark", attrs = c("class" = "Bar"), close = FALSE)
              # tr$addNode("color", attrs = c("column" = "[Random Numbers].[:Measure Names]"))
            # tr$closeNode()
          # tr$closeNode()
          # tr$addNode("pane", attrs = c("id" = "2", "y-axis-name" = "[Random Numbers].[pcto:cum:sum:Number of Records:qk]"), close = FALSE)
            # tr$addNode("view", close = FALSE)
              # tr$addNode("breakdown", attrs = c("value" = "auto"))
            # tr$closeNode()
            # tr$addNode("mark", attrs = c("class" = "Line"), close = FALSE)
              # tr$addNode("color", attrs = c("column" = "[Random Numbers].[:Measure Names]"))
            # tr$closeNode()
          # tr$closeNode()
        # tr$closeNode()
        # tr$addNode("rows", "([Random Numbers].[pcto:sum:Number of Records:qk] + [Random Numbers].[pcto:cum:sum:Number of Records:qk])")
        # tr$addNode("cols", "[Random Numbers].[Predicted (bin)]")
      # tr$closeNode()
    # tr$closeNode()
  # tr$closeNode()
  
  saveXML(xmlRoot(tr), file = c(f), indent = TRUE, prefix = '<?xml version="1.0" encoding="utf-8" ?>')
}