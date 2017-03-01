library("datadoctor")
library("moments")
library("splines")
library("AlteryxPredictive")

trimNewlines <- function (x) {
  if (!is.character(x)) {
    stop("The argument \"x\" must be a character vector.")
  }
  AlteryxPredictive:::trim.blanks(sub("\\n", "", x))
}


## The initial data prep
# Read in the data
the_data <- read.Alteryx2("#1", default = eggs)
# Make sure that the target isn't in the set of predictors
the_vars <- names(the_data)
duplicated_var <- paste0("Right_", the_vars[1])
if (duplicated_var %in% the_vars[2:length(the_vars)]) {
  the_preds <- the_vars[2:length(the_vars)]
  the_preds <- the_preds[!(the_preds %in% duplicated_var)]
  the_vars <- c(the_vars[1], the_preds)
  the_data <- the_data[, the_vars]
}
# Make sure not everything is a factor. This corresponds to the case where a
# CSV is brought in and not been run through the Auto Field tool or not "typed"
# in any other way
all_are_factors <- all(sapply(the_data, class) == "factor")
# Address white space only values in the variables
the_data <- change_missing_to_na(the_data)
# Remove records that have missing values in the target variable
num_target_missing <- sum(as.numeric(is.na(the_data[[1]])))
the_data <- the_data[!is.na(the_data[[1]]),]
# Remove the single value columns
single_value_columns <- cols_with_unique_val(the_data, names(the_data))
target_name <- names(the_data)[1]
if (target_name %in% single_value_columns) {
  stop.Alteryx("The selected target variable contains only a single value, so cannot be used to create a predictive model.")
}
the_data <- the_data[,!(names(the_data) %in% single_value_columns)]

## The report text for variables with only a single value
unary_report_text <- character(0)
if (length(single_value_columns) == 1) {
  unary_report_text <- paste("The column", single_value_columns, "contains only a single value, and is dropped from the analysis.")
}
if (length(single_value_columns) > 1) {
  unary_report_text <- paste("The columns:", formattedTextList(single_value_columns), "each contain only a single value, and are dropped from the analysis.")
}

## Target variable analysis
the_target <- the_data[, 1, drop = FALSE]
# Conversion status indicators for the target
target_converted_to_int <- FALSE
target_converted_to_factor <- FALSE
target_converted_to_number <- FALSE
target_may_be_count <- FALSE
# Make sure a numeric target isn't an integer in disguise and convert if it is
if (class(the_target[[1]]) == "numeric") {
  target_is_int <- unlist(cols_numeric_is_integer(the_target, target_name))
  if (length(target_is_int) > 0) {
    the_target[[1]] <- as.integer(the_target[[1]])
    target_converted_to_int <- TRUE
  }
}
# See if an integer is likely to be a factor, and convert it if it is
if (class(the_target[[1]]) == "integer") {
  target_is_factor <- FALSE
  target_num_levels <- length(unique(the_target[, 1]))
  target_vals_sqntl <- unlist(cols_int_sequential(the_target, target_name))
  if (target_num_levels == 2) {
    target_is_factor <- TRUE
  }
  if (target_num_levels > 2 && target_num_levels < 11 && length(target_vals_sqntl) > 0) {
    target_is_factor <- TRUE
  }
  if (target_is_factor) {
    the_target[[1]] <- as.factor(the_target[[1]])
    target_converted_to_factor <- TRUE
  }
}
# See if a factor variable is actually a number and covert it if it is
if (class(the_target[[1]]) == "factor") {
  target_converted_to_number <- cols_factor_is_number(the_target, target_name)
  if (target_converted_to_number) {
    the_target[[1]] <- as.numeric(the_target[[1]])
  }
}
# Examine other potential issues with the target
if (class(the_target[[1]]) == "factor") {
  target_num_levels <- length(unique(the_target[, 1]))
  target_level_counts <- summary(the_target[, 1], maxsum = length(unique(the_target[, 1])))
}
if (class(the_target[[1]]) != "factor") {
  target_skew <- skewness(the_target[[1]])
  target_outliers <- if (target_skew > 1.5 && min(the_target[, 1]) > 0) {
    detect_outlier_single(log(the_target[, 1]), strategy = "3sigma")
  } else {
    detect_outlier_single(the_target[, 1], strategy = "3sigma")
  }
}
if (class(the_target[[1]]) == "integer" && min(the_target[[1]]) < 51 && max(the_target[[1]]) < 2000) {
  target_may_be_count <- TRUE
}

## Create the target variable analysis report elements
# Report the target variable name
target_report_text <- paste0("The target column used in the analysis is ", target_name, ",")
# Report the type of the target variable, and whether the type was changed
if (!target_converted_to_factor) {
  if (class(the_target[[1]]) == "factor") {
    target_type <- "a categorical column."
  }
  if (class(the_target[[1]]) == "integer") {
    target_type <- "an integer column."
  }
  if (class(the_target[[1]]) == "numeric") {
    target_type <- "a non-integer numeric column."
  }
  target_type_text <- paste0("which is ", target_type)
}
if (target_converted_to_int && target_converted_to_factor) {
  target_type_text <- "which is a non-integer numeric column that appears to actually be a categorical column. It is treated as a categorical column for this analysis."
}
if (!target_converted_to_int && target_converted_to_factor) {
  target_type_text <- "which is an integer numeric column that appears to actually be a categorical variable. It is treated as a categorical variable for this analysis."
}
if (target_converted_to_number) {
  target_type_text <- "which is a categorical column that appears to actually be a numeric variable. It is treated as a numeric variable for this analysis."
}
if (class(the_target[[1]]) == "factor") {
  target_num_levels <- length(levels(the_target[[1]]))
  if (target_num_levels == 2) {
    target_type_text <- paste(target_type_text, "The target column has two levels, so it is what is known as a binomial variable.")
  } else {
    target_type_text <- paste(target_type_text, paste0("The target column has", target_num_levels, "levels, so it is what is known as multinomial in nature."))
  }
}

target_report_text <- paste(target_report_text, target_type_text)
if (class(the_target[[1]]) != "factor") {
  target_report_text <- paste(target_report_text, paste0("The skewness of the target is ", round(target_skew, 2), "."))
  if (target_may_be_count) {
    target_report_text <- paste(target_report_text, "The target may also represent count data.")
  }
}
# Report the number of rows with missing values of the target have been removed
if (num_target_missing == 1) {
  target_report_text <- paste(target_report_text, "There is one record with a missing value for this column, and that record has been removed from the analysis.")
}
if (num_target_missing > 1) {
  target_report_text <- paste(target_report_text, "There are", num_target_missing, "records with missing values for this column, and those records have been removed from the analysis.")
}
# Report other potential issues associated with the target
target_other_text <- NULL
if (class(the_target[[1]]) == "factor") {
  target_level_counts <- target_level_counts[order(target_level_counts, decreasing = TRUE)]
  ratio_1_2 <- target_level_counts[1]/target_level_counts[2]
  ratio_value <- round(ratio_1_2, 1)
  # The case of low counts in at least one of the levels
  if (min(target_level_counts) < 10) {
    target_other_text <- " In addition, the target column has categories with low count levels"
    if (ratio_1_2 < 1.5) {
      target_other_text <- paste0(target_other_text, ".")
    }
    if (ratio_1_2 >= 1.5 && ratio_1_2 < 3) {
      target_other_text <- paste0(target_other_text, ", and represents an unbalanced sample, with the most common level being ", ratio_value, " times more numerious than the next most common level.")
    }
    if (ratio_1_2 >= 3 && ratio_1_2 < 6) {
      target_other_text <- paste0(target_other_text, ", and represents a very unbalanced sample, with the most common level being ", ratio_value, " times more numerious than the next most common level. This imbalance in the target column may cause some modeling methods, such as a Decision Tree model, to perform poorly.")
      if (length(target_level_counts) == 2) {
        target_other_text <- paste(target_other_text,  "Consider the use of the Oversample Field tool to provide an analysis data set with a more balanced target.")
      }
   }
    if (ratio_1_2 >= 6) {
      target_other_text <- paste0(target_other_text, ", and represents an extremely unbalanced sample, with the most common level being ", ratio_value, " times more numerious than the next most common level. This imbalance in the target column is likely to cause some modeling methods, such as a Decision Tree model, to perform poorly. Strongly consider the use of the Oversample Field tool to provide an analysis data set with a more balanced target.")
      if (length(target_level_counts) == 2) {
        target_other_text <- paste(target_other_text,  "Strongly consider the use of the Oversample Field tool to provide an analysis data set with a more balanced target.")
      }
    }
  }
  if (min(target_level_counts) >= 10) {
    if (ratio_1_2 < 1.5) {
      target_other_text <- NULL
    }
    if (ratio_1_2 >= 1.5 && ratio_1_2 < 3) {
      target_other_text <- paste0(" The target represents an unbalanced sample, with the most common level being ", ratio_value, " times more numerious than the next most common level.")
    }
    if (ratio_1_2 >= 3 && ratio_1_2 < 6) {
      target_other_text <- paste0(" The target represents a very unbalanced sample, with the most common level being ", ratio_value, " times more numerious than the next most common level. This imbalance in the target column may cause some modeling methods, such as a Decision Tree model, to perform poorly.")
      if (length(target_level_counts) == 2) {
        target_other_text <- paste(target_other_text,  "Consider the use of the Oversample Field tool to provide an analysis data set with a more balanced target.")
      }
    }
    if (ratio_1_2 >= 6) {
      target_other_text <- paste0(" The target represents an extremely unbalanced sample, with the most common level being ", ratio_value, " times more numerious than the next most common level. This imbalance in the target column is likely to cause some modeling methods, such as a Decision Tree model, to perform poorly. Stongly consider the use of the Oversample Field tool to provide an analysis data set with a more balanced target.")
      if (length(target_level_counts) == 2) {
        target_other_text <- paste(target_other_text,  "Strongly consider the use of the Oversample Field tool to provide an analysis data set with a more balanced target.")
      }
    }
  }
}
if (class(the_target[[1]]) != "factor") {
  if (length(target_outliers) > 0) {
    target_other_text <- " In addition, the target variable contains outlier values."
  }
}
target_report_text <- paste0(target_report_text, target_other_text)
grp_out <- if (length(unary_report_text) > 0) {
  data.frame(grp = c("Target text", "Single value predictor"), out = c(target_report_text, unary_report_text))
} else {
  data.frame(grp = "Target text", out = target_report_text)
}

## Predictor field types analysis and possible reassignment
# Get the initial field types
if (!all_are_factors) {
  pred_types <- get_col_types(the_data[, -1])
  orig_factors <- pred_types$factor_cols
  orig_integers <- pred_types$integer_cols
  orig_numerics <- pred_types$numeric_cols
} else {
  orig_factors <- names(the_data)[-1]
  orig_integers <- character(0)
  orig_numerics <- character(0)
}
# Initialize current variables
current_factors <- orig_factors
current_integers <- orig_integers
current_numerics <- orig_numerics
# Look at field names for clues surrounding numeric codes. This is currently
# US centric, so will need to be expanded to implement i18n
lc_factors <- tolower(orig_factors)
zip_vars <- orig_factors[grepl("zip", lc_factors)]
post_vars <- orig_factors[grepl("post", lc_factors)]
fips_vars <- orig_factors[grepl("fips", lc_factors)]
code_vars <- orig_factors[grepl("code", lc_factors)]
special_names <- unique(c(zip_vars, post_vars, fips_vars, code_vars))
# There are two cases. The first is when not all fields are factors, the second
# is when all fields are factors. The difference is that a reduced amount of
# checking and conversion is needed when are fields are factors.
# Start with data types that are only relevant when not all fields are factors.
if (!all_are_factors) {
  # Check to see if there are integers disguised as numeric fields and convert
  numerics_are_integers <- cols_int_sequential(the_data, orig_numerics)
  if (length(numerics_are_integers) > 0) {
    the_data <- change_numeric_to_int(the_data, numerics_are_integers)
    current_integers <- c(orig_integers, numerics_are_integers)
    current_numerics <- current_numerics[!(current_numerics %in% numerics_are_integers)]
  }
  # Check to see if there are categoricals disguised as integers and convert.
  integers_are_categoricals <- cols_int_is_categorical(the_data, current_integers)
  if (length(integers_are_categoricals) > 0) {
    the_data <- change_int_to_factor(the_data, integers_are_categoricals)
    current_integers <- current_integers[!(current_integers %in% integers_are_categoricals)]
    current_factors <- unique(c(orig_factors, integers_are_categoricals))
  }
  # Check to see if there are numeric codes disguised as integers and convert.
  # Possible ZIP codes
  if (length(zip_vars) > 0) {
    # The variables that contain "zip" in the name and are integers
    int_zip_vars <- zip_vars[zip_vars %in% current_integers]
    test_int_zip_vars <- logical(0)
    for (i in int_zip_vars) {
      this_vector <- eval(parse(text = paste0("the_data[[", i, "]]")))
      this_no_na <- this_vector[!is.na(this_vector)]
      this_test <- if (max(this_no_na) < 99951 && min(this_no_na) > 500) {
        TRUE
      } else {
        FALSE
      }
      test_int_zip_vars <- c(test_int_zip_vars, this_test)
    }
    # Convert apparent ZIP code integer fields to categoricals
    if (any(test_int_zip_vars)) {
      the_data <- change_int_to_factor(the_data, int_zip_vars[test_int_zip_vars])
      current_factors <- c(current_factors, int_zip_vars[test_int_zip_vars])
      current_integers <- current_integers[!(current_integers %in% int_zip_vars[test_int_zip_vars])]
    }
  }
  # Possible postal codes
  if (length(post_vars[!(post_vars %in% zip_vars)]) > 0) {
    post_vars <- post_vars[!(post_vars %in% zip_vars)]
    # The variables that contain "post" in the name and are integers
    int_post_vars <- post_vars[post_vars %in% current_integers]
    test_int_post_vars <- logical(0)
    for (i in int_post_vars) {
      this_vector <- eval(parse(text = paste0("the_data[[", i, "]]")))
      this_no_na <- this_vector[!is.na(this_vector)]
      this_test <- if (max(this_no_na) < 99951 && min(this_no_na) > 500) {
        TRUE
      } else {
        FALSE
      }
      test_int_post_vars <- c(test_int_post_vars, this_test)
    }
    # Convert apparent post code integer fields to categoricals
    if (any(test_int_post_vars)) {
      the_data <- change_int_to_factor(the_data, int_post_vars[test_int_post_vars])
      current_factors <- c(current_factors, int_post_vars[test_int_post_vars])
      current_integers <- current_integers[!(current_integers %in% int_post_vars[test_int_post_vars])]
    }
  }
  # Possible state FIPS codes (other geographies are not addressed)
  if (length(fips_vars) > 0) {
    num_st_fips <- c(1, 2, 60, 4, 5, 81, 6, 8, 9, 10, 11, 64, 12, 13, 66, 15, 84, 16, 17, 18, 19, 86, 67, 20, 21, 89, 22, 23, 68, 24, 25, 26, 71, 27, 28, 29, 30, 76, 31, 32, 33, 34, 35, 36, 37, 38, 69, 39, 40, 41, 70, 95, 42, 72, 44, 45, 46, 47, 48, 74, 49, 50, 78, 51, 79, 53, 54, 55, 56)
    # The variables that contain "fips" in the name and are integers
    int_fips_vars <- fips_vars[fips_vars %in% current_integers]
    test_int_fips_vars <- logical(0)
    for (i in int_fips_vars) {
      this_vector <- eval(parse(text = paste0("the_data[[", i, "]]")))
      this_no_na <- this_vector[!is.na(this_vector)]
      this_test <- if (all(this_no_na %in% num_st_fips)) {
        TRUE
      } else {
        FALSE
      }
      test_int_fips_vars <- c(test_int_fips_vars, this_test)
    }
    # Convert apparent state FIPS code integer fields to categoricals
    if (any(test_int_fips_vars)) {
      the_data <- change_int_to_factor(the_data, int_fips_vars[test_int_fips_vars])
      current_factors <- c(current_factors, int_fips_vars[test_int_fips_vars])
      current_integers <- current_integers[!(current_integers %in% int_fips_vars[test_int_fips_vars])]
    }
  }
  # Variables with the word "code" in their name that have not already been addressed
  if (length(code_vars[!(code_vars %in% unique(c(zip_vars, post_vars, fips_vars)))]) > 0) {
    code_vars <- code_vars[!(code_vars %in% unique(c(zip_vars, post_vars, fips_vars)))]
    # The variables that contain "code" in the name and are integers
    int_code_vars <- code_vars[code_vars %in% current_integers]
    # Convert all the fields that contain the word code in their name to factors
    the_data <- change_int_to_factor(the_data, int_code_vars)
    current_factors <- c(current_factors, int_code_vars)
    current_integers <- current_integers[!(current_integers %in% int_code_vars)]
  }
}
# Check to see if there are numbers disguised as categoricals and convert
if (length(orig_factors) > 0) {
  # Start with variables that do not appear to be numeric codes
  relevant_factors <- orig_factors[!(orig_factors %in% special_names)]
  test_factors_are_numbers <- cols_factor_is_number(the_data, relevant_factors)
  factors_are_numbers <- relevant_factors[test_factors_are_numbers]
  if (length(factors_are_numbers) > 0) {
    the_data <- change_factor_to_number(the_data, factors_are_numbers)
  }
  current_factors <- current_factors[!(current_factors %in% factors_are_numbers)]
  current_numerics <- c(current_numerics, factors_are_numbers)
  # Address the fields that are categoricals and have names that suggest that
  # they may be numerical codes. This will need to be sent through an i18n process
  # Possible ZIP codes
  if (length(zip_vars) > 0) {
    # The variables that contain "zip" in the name and are factors
    fac_zip_vars <- zip_vars[zip_vars %in% current_factors]
    test_fac_zip_vars <- logical(0)
    for (i in fac_zip_vars) {
      this_vector <- eval(parse(text = paste0('the_data[["', i, '"]]')))
      this_no_na <- this_vector[!is.na(this_vector)]
      ## THIS CAN BE IMPROVED, BUT IT REQUIRES A TEST TO SEE IF THE FIELD CAN BE AN INTEGER
      this_test <- if (max(nchar(as.character(this_no_na))) < 6 && min(nchar(as.character(this_no_na))) > 2) {
        TRUE
      } else {
        FALSE
      }
      test_fac_zip_vars <- c(test_fac_zip_vars, this_test)
    }
    # Convert fields with "zip" in the name that do not appear to be zip codes,
    # but which do appear to be numeric
    if (any(!test_fac_zip_vars)) {
      factors_are_numbers <- cols_factor_is_number(the_data, fac_zip_vars[!test_fac_zip_vars])
      if (length(factors_are_numbers) > 0) {
        the_data <- change_factor_to_number(the_data, factors_are_numbers)
        current_numerics <- c(current_numerics, factors_are_numbers)
        current_factors <- current_factors[!(current_factors %in% factors_are_numbers)]
      }
    }
  }
  # Possible postal codes
  if (length(post_vars[!(post_vars %in% zip_vars)]) > 0) {
    post_vars <- post_vars[!(post_vars %in% zip_vars)]
    # The variables that contain "post" in the name and are factors
    fac_post_vars <- post_vars[post_vars %in% current_factors]
    test_fac_post_vars <- logical(0)
    for (i in fac_post_vars) {
      this_vector <- eval(parse(text = paste0('the_data[["', i, '"]]')))
      this_no_na <- this_vector[!is.na(this_vector)]
      this_test <- if (all(nchar(as.character(this_no_na)) <= 5)) {
        TRUE
      } else {
        FALSE
      }
      test_fac_post_vars <- c(test_fac_post_vars, this_test)
    }
    # Convert fields with "post" in the name that do not appear to be postal codes,
    # but which do appear to be numeric
    if (any(!test_fac_post_vars)) {
      factors_are_numbers <- cols_factor_is_number(the_data, fac_post_vars[!test_fac_post_vars])
      if (length(factors_are_numbers) > 0) {
        the_data <- change_factor_to_number(the_data, factors_are_numbers)
        current_numerics <- c(current_numerics, factors_are_numbers)
        current_factors <- current_factors[!(current_factors %in% factors_are_numbers)]
      }
    }
  }
  # Possible state FIPS codes. Note: Other geographies are not examined.
  if (length(fips_vars) > 0) {
    chr_st_fips <- c("1", "01", "2", "02", "60", "4", "04", "5", "05", "81", "6", "06", "8", "08", "9", "09", "10", "11", "64", "12", "13", "66", "15", "84", "16", "17", "18", "19", "86", "67", "20", "21", "89", "22", "23", "68", "24", "25", "26", "71", "27", "28", "29", "30", "76", "31", "32", "33", "34", "35", "36", "37", "38", "69", "39", "40", "41", "70", "95", "42", "72", "44", "45", "46", "47", "48", "74", "49", "50", "78", "51", "79", "53", "54", "55", "56")
    # The variables that contain "fips" in the name and are factors
    fac_fips_vars <- fips_vars[fips_vars %in% current_factors]
    test_fac_fips_vars <- logical(0)
    for (i in fac_fips_vars) {
      this_vector <- eval(parse(text = paste0('the_data[["', i, '"]]')))
      this_no_na <- this_vector[!is.na(this_vector)]
      this_test <- if (all(as.character(this_no_na)) %in% chr_st_fips) {
        TRUE
      } else {
        FALSE
      }
      test_fac_fips_vars <- c(test_fac_fips_vars, this_test)
    }
    # Convert fields with "fips" in the name that do not appear to be state FIPS
    # codes, but which do appear to be numeric
    if (any(!test_fac_fips_vars)) {
      factors_are_numbers <- cols_factor_is_number(the_data, fac_fips_vars[!test_fac_fips_vars])
      if (length(factors_are_numbers) > 0) {
        the_data <- change_factor_to_number(the_data, factors_are_numbers)
        current_numerics <- c(current_numerics, factors_are_numbers)
        current_factors <- current_factors[!(current_factors %in% factors_are_numbers)]
      }
    }
  }
}

## Predictor field types reporting
# Determine how the field types have been changed
orig_numbers <- c(orig_integers, orig_numerics)
current_numbers <- c(current_integers, current_numerics)
changed_to_factor <- orig_numbers[!(orig_numbers %in% current_numbers)]
changed_to_number <- orig_factors[!(orig_factors %in% current_factors)]
all_changed_type <- c(changed_to_factor, changed_to_number)
type_all_factors <- character(0)
# If any fields have changed type, then prepare the table and text elements
if (length(all_changed_type) > 0) {
  # The table elements of the report
  change_status <- all_changed_type %in% changed_to_factor
  from_number <- from_factor <- rep("X", length(change_status))
  from_number[!change_status] <- " "
  from_factor[change_status] <- " "
  type_table <- paste(all_changed_type, from_number, from_factor, sep = "|")
  # Let the user know that they did not appear to use an Auto Field or similar
  # tool to correct the data. This is most likely to occur when the input data
  # is a text file.
  if (all_are_factors && length(all_changed_type) > 0) {
    type_all_factors <- "All of the incoming columns are string types, but they do not all appear to be strings. Consider using an Auto Field tool on the data prior to connecting the stream to the Predictive Adviser tool."
  }
  # The text elements of the report
  type_text <- if (length(all_changed_type) == 1) {
    "There appears to be a column where changing the data type may be warranted."
  } else {
    "There appear to be several columns where changing their data type may be warranted."
  }
  # The case where there are issues with both categorical and continuous fields
  if (length(changed_to_factor) > 0 && length(changed_to_number) > 0) {
    type_text1 <- if (length(changed_to_factor) == 1) {
      paste("The column", changed_to_factor, "is likely a categorical. Consider changing the data type of this column to a V_WString using a Select Tool. For this analysis only, the columns that appear to be categories is converted to categorical.")
    } else {
      paste("The columns:", formattedTextList(changed_to_factor), "are likely to be categorical. Consider changing the data types of these columns to V_WString using a Select Tool. For this analysis only, the columns that appear to be categories are converted to categoricals.")
    }
    type_text2 <- if (length(changed_to_number) == 1) {
      paste("Similarly, the column", changed_to_number, "is likely to be a numeric. Consider changing the data type of this column to a Double using a Select Tool. For this analysis only, the column that appears to be numeric is converted to numeric.")
    } else {
      paste("Similarly, the columns:", formattedTextList(changed_to_number), "are likely to be numeric. Consider changing the data types of these columns to Doubles using a Select tool. For this analysis only, the columns that appear to be numbers are converted to numbers.")
    }
    type_text1 <- paste(type_text1, type_text2)
  }
  # The case where there are issues only with the categorical fields
  if (length(changed_to_factor) > 0 && length(changed_to_number) == 0) {
    type_text1 <- if (length(changed_to_factor) == 1) {
      paste("The column", changed_to_factor, "is likely a categorical. Consider changing the data type of this column to a V_WString using a Select Tool.")
    } else {
      paste("The columns:", formattedTextList(changed_to_factor), "are likely to be categorical. Consider changing the data types of these columns to V_WString using a Select Tool.")
    }
  }
  # The case where there are issues only with the continuous fields
  if (length(changed_to_factor) == 0 && length(changed_to_number) > 0) {
    type_text1 <- if (length(changed_to_number) == 1) {
      paste("The column", changed_to_number, "is likely numeric. Consider changing the data type of this column to a Double using a Select Tool.")
    } else {
      paste("The columns:", formattedTextList(changed_to_number), "are likely to be numeric. Consider changing the data types of these columns to Doubles using a Select Tool.")
    }
  }
  type_text <- paste(type_text, type_text1)
  # Prepare the type analysis elements for output
  type_report <- data.frame(grp = c(rep("Type table", length(type_table)), "Type text"), out = c(type_table, type_text))
} else {
  type_text <- if (length(unary_report_text) > 0) {
    "All of the columns that have more than a single value appear to be of the appropriate data type."
  } else {
    "All of the columns appear to be of the appropriate data type."
  }
  type_report <- data.frame(grp = "Type text", out = type_text)
}
if (length(type_all_factors) > 0) {
  type_report <- rbind(data.frame(grp = "Type all factors", out = type_all_factors), type_report)
}
grp_out <- rbind(grp_out, type_report)

### ANALYSIS OF PREDICTORS WITH MULTIPLE POSSIBLE TYPES
## Missing variable analysis
all_missing <- cols_missing_blank(the_data)
row.names(all_missing) <- NULL
# Split out the factors and numerics/integers
factors_missing <- all_missing[as.character(all_missing$Column) %in% current_factors, , drop = FALSE]
numbers_missing <- all_missing[as.character(all_missing$Column) %in% current_numbers, , drop = FALSE]
## Possible unique key fields
# Deal with columns that came in as factors, but were coverted to numbers
new_classes <- sapply(the_data, class)
new_numerics <- names(new_classes)[new_classes == "numeric"]
numbers_from_factors_are_integer <- cols_numeric_is_integer(the_data, new_numerics)
numbers_from_factors_are_integer <- numbers_from_factors_are_integer[!is.na(numbers_from_factors_are_integer)]
possible_unique_key_cols <- unique(c(current_factors, current_integers, numbers_from_factors_are_integer))
possible_unique_keys <- sapply(the_data[, possible_unique_key_cols], function(x) length(unique(x)))
unique_keys <- names(possible_unique_keys)[possible_unique_keys == nrow(the_data)]
## Reporting unique keys
if (length(unique_keys) > 0) {
  unique_keys_list <- formattedTextList(unique_keys)
  if (length(unique_keys) == 1) {
    unique_keys_text <- paste("The column", unique_keys_list, "is potentially a unique key ID, and should not be used as a predictor column if this is the case.")
  } else {
    unique_keys_text <- paste("The columns:", unique_keys_list, "are potentially unique key IDs, and the ones that are should not be used as predictor columns.")
  }
  unique_df <- data.frame(grp = "Unique keys", out = unique_keys_text)
  grp_out <- rbind(grp_out, unique_df)
}

### CATEGORICAL PREDICTORS
if (length(current_factors) > 0) {
  ## Analysis
  # Sparse levels
  sparse_levels <- cols_num_sparse_levels(the_data, current_factors)
  # The number of levels
  num_levels <- sapply(the_data[, current_factors, drop = FALSE], function(x) length(levels(x)))
  # Level balance: normalized Herfindahl-Hirschman index
  balance <- 1 - sapply(the_data[, current_factors, drop = FALSE], datadoctor:::get_HHI)/10000

  ## Reporting
  # Combine all categorical variable specific metrics into a single data frame
  sparse_levels$num_levels <- num_levels
  sparse_levels$balance <- balance
  # Add in the missing data analysis for the categoricals
  factor_metrics <- cbind(factors_missing, sparse_levels[, -1])
  fitness <- cols_factor_fitness(factor_metrics)
  factor_metrics <- cbind(factor_metrics, data.frame(Fitness = fitness))
  factor_metrics <- factor_metrics[order(factor_metrics$Fitness),]
  factor_metrics_string <- paste(factor_metrics$Column, factor_metrics$Number_Missing_Blank, factor_metrics$Percent_Missing_Blank, factor_metrics$five, factor_metrics$ten, factor_metrics$twenty, factor_metrics$num_levels, round(factor_metrics$balance, 4),factor_metrics$Fitness, sep = "|")
  grp_out <- rbind(grp_out, data.frame(grp = rep("Factor metrics", nrow(factor_metrics)), out = factor_metrics_string))
}

### CONTINUOUS PREDICTORS
if (length(current_numbers) > 0) {
  ## Analysis
  # Outliers
  num_outliers <- cols_num_outliers(the_data, current_numbers, "3sigma")
  pct_outliers <- round((100*num_outliers)/sapply(the_data[, current_numbers, drop = FALSE], function(x) length(x[!is.na(x)])), 2)
  # Distribution shape
  skew <- round(sapply(the_data[, current_numbers, drop = FALSE], function(x) skewness(x[!is.na(x)])), 4)
  kurt <- round(sapply(the_data[, current_numbers, drop = FALSE], function(x) kurtosis(x[!is.na(x)])), 4)
  number_metrics <- cbind(numbers_missing, data.frame(Num_Outliers = num_outliers, Pct_Outliers = pct_outliers, Skewness = skew, Kurtosis = kurt))
  continuous_fitness <- cols_continuous_fitness(number_metrics)
  number_metrics$Fitness <- continuous_fitness

  ## Reporting
  number_metrics <- number_metrics[order(number_metrics$Fitness),]
  continuous_metrics_string <- paste(number_metrics$Column, number_metrics$Number_Missing_Blank, number_metrics$Percent_Missing_Blank, number_metrics$Num_Outliers, number_metrics$Pct_Outliers, number_metrics$Skewness, number_metrics$Kurtosis, number_metrics$Fitness, sep = "|")
  grp_out <- rbind(grp_out, data.frame(grp = rep("Numeric metrics", length(continuous_metrics_string)), out = continuous_metrics_string))
}

### VARIABLE IMPORTANTANCE WEIGHTS
## Analysis
# Remove categorical unique keys
unique_key_factors <- current_factors[current_factors %in% unique_keys]
if (length(unique_key_factors) > 0) {
  the_data <- the_data[, !(names(the_data) %in% unique_key_factors)]
}
# Missing value imputation
the_data <- cols_imputation(the_data)
# Put the target and the predictors back together
#the_data <- cbind(the_target, the_data)
# Create the formula for the analysis
the_formula <- eval(parse(text = paste(names(the_data)[1], "~", paste(names(the_data)[-1], collapse = " + "))))
# Perform the importance weight analysis
importance_weights <- if (class(the_target[[1]]) == "factor") {
                        chiSquaredImportance(the_formula, the_data)
                      } else {
                        bsplineImportance(the_formula, the_data)
                      }
believability <- rep(1, nrow(importance_weights))
if (any(importance_weights$Importance > 0.5)) {
  believability[importance_weights$Importance > 0.5] <- 1 - 2*(importance_weights$Importance[importance_weights$Importance > 0.5] - 0.5)
}
importance_weights$Believability <- believability
if (length(current_factors) > 0) {
  factor_fitness_df <- factor_metrics[, c("Column", "Fitness")]
}
if (length(current_numbers) > 0) {
  number_fitness_df <- number_metrics[, c("Column", "Fitness")]
}
if (length(current_factors) > 0 && length(current_numbers) > 0) {
  fitness_df <- rbind(factor_fitness_df, number_fitness_df)
}
if (length(current_factors) > 0 && length(current_numbers) == 0) {
  fitness_df <- factor_fitness_df
}
if (length(current_factors) == 0 && length(current_numbers) > 0) {
  fitness_df <- number_fitness_df
}
believe_df <- importance_weights[, c(1, 4)]
names(believe_df) <- c("Column", "Believability")
summary_df <- merge(fitness_df, believe_df, by = "Column")

## Reporting
# Convert the linearity measure to a character field and get rid of the NAs
linearity_metric <- as.character(round(importance_weights$Linearity, 4))
linearity_metric[is.na(linearity_metric)] <- " "
importance_metrics <- paste(importance_weights$Variable,
 round(importance_weights$Importance, 4), linearity_metric, sep = "|")
grp_out <- rbind(grp_out, data.frame(grp = rep("Importance weights", length(importance_metrics)), out = importance_metrics))
summary_string <- paste(summary_df$Column, summary_df$Fitness, round(summary_df$Believability, 4), sep = "|")
grp_out <- rbind(grp_out, data.frame(grp = rep("Summary", length(summary_string)), out = summary_string))
### WRITE OUT THE grp_out TABLE
write.Alteryx2(grp_out)

### INTERACTIVE DASHBOARDS
