library(tidyverse)

# quick report ------------------------------------------------------------
library(DataExplorer)
create_report(df[,c(3:19)], #select variables
              output_file = "clinical_data_quality_report.html")
table1(~.,df)


# summary of NA -----------------------------------------------------------
####mice####
md.pattern(df[,(3:19)],
           rotate.names = T)

####VIM####
df_c <- df[,(3:19)]
mice_plot <- aggr(df_c,col = c("lightblue","lightpink"),
                  numbers = T, 
                  sortVars = T,
                  labels = names(df_c),
                  cex.axis=0.5,
                  #gap=5,
                  ylab=c("Missing data","Pattern")
)

# summary of outliers -----------------------------------------------------
####extremevalues####
library(extremevalues)

test <- (df[3:19]) #clean NA before using 'getOutliers'

#check single variable
K <- getOutliers(test$bmi, method ="I", distribution ="lognormal")
print(K)
outlierPlot(test$bmi, K, mode ="qq")


L <- getOutliers(test$bmi, method ="II", distribution ="lognormal")
print(L)
outlierPlot(test$bmi, L, mode ="residual")


#check multiple varibales
# create directory for plots
plot_dir <- "outlier_plots"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
  cat("created directory:", plot_dir, "\n")
}


results <- list()
plot_files <- list()


for (col_name in names(test)) {
  # only process numeric variables
  if (is.numeric(test[[col_name]])) {
    tryCatch({
      # Extract the column and remove NAs specifically for this variable
      col_data <- test[[col_name]]
      na_count <- sum(is.na(col_data))
      non_na_data <- col_data[!is.na(col_data)]
      non_na_count <- length(non_na_data)
      
      
      # Skip if no non-NA values
      if (length(non_na_data) == 0) {
        cat("Skipping column", col_name, "due to all NA values\n")
        next
      }
      
      # Detect outliers on non-NA data
      K <- getOutliers(non_na_data, method = "I", distribution = "lognormal")
      
      results[[col_name]] <- K
      
      # create plot
      file_name <- file.path(plot_dir, paste0("outlier_plot_", col_name, ".png"))
      png(file_name, width = 800, height = 600)
      outlierPlot(non_na_data, K, mode = "qq",fat = FALSE, 
                  title = paste(col_name, 
                  "\nNon-NA values:", non_na_count, 
                  "| Missing:", na_count,
                  "\nOutliers:", length(K$iLeft) + length(K$iRight)))
      dev.off()
      
      plot_files[[col_name]] <- file_name
      
      # print results
      cat("\n=====", col_name, "outliers =====\n")
      print(K)
      cat("plot saved in:", file_name, "\n")
      
    }, error = function(e) {
      warning(paste("column", col_name, "error:", e$message))
    })
  } else {
    cat("skip non-numeric column:", col_name, "\n")
  }
}


# results$your_variable
# results
