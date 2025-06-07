# Function Overview Generator for R Scripts
# This script extracts function signatures and documentation from R files

library(stringr)

# Helper function for repeating strings (since stringr uses str_dup, not str_repeat)
str_repeat <- function(string, times) str_dup(string, times)

#' Extract function information from an R script
#' 
#' @param file_path Path to the R script
#' @param include_roxygen Whether to include roxygen2 documentation
#' @param include_body_preview Whether to include first few lines of function body
#' @return Data frame with function information
extract_function_info <- function(file_path, 
                                  include_roxygen = TRUE, 
                                  include_body_preview = FALSE,
                                  preview_lines = 3) {
  
  # Read the file
  lines <- readLines(file_path, warn = FALSE)
  
  # Initialize storage
  functions_info <- list()
  current_roxygen <- ""
  
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Collect roxygen comments if requested
    if (include_roxygen && str_detect(line, "^\\s*#'")) {
      current_roxygen <- paste(current_roxygen, str_trim(str_remove(line, "^\\s*#'")), sep = "\n")
    }
    
    # Detect function definitions
    # Matches: function_name <- function(params) or function_name = function(params)
    if (str_detect(line, "^[a-zA-Z_][a-zA-Z0-9_.]*\\s*(<-|=)\\s*function\\s*\\(")) {
      
      # Extract function name
      func_name <- str_extract(line, "^[a-zA-Z_][a-zA-Z0-9_.]*")
      
      # Extract parameters (might span multiple lines)
      param_text <- str_extract(line, "function\\s*\\([^)]*")
      
      # Handle multi-line parameters
      j <- i
      bracket_count <- str_count(param_text, "\\(") - str_count(param_text, "\\)")
      
      while (bracket_count > 0 && j < length(lines)) {
        j <- j + 1
        next_line <- lines[j]
        param_text <- paste(param_text, next_line)
        bracket_count <- bracket_count + str_count(next_line, "\\(") - str_count(next_line, "\\)")
      }
      
      # Clean up parameters
      params <- str_extract(param_text, "\\([^)]*\\)")
      if (is.na(params)) params <- "()"
      
      # Get function body preview if requested
      body_preview <- ""
      if (include_body_preview) {
        start_line <- j + 1
        end_line <- min(start_line + preview_lines - 1, length(lines))
        if (start_line <= length(lines)) {
          body_lines <- lines[start_line:end_line]
          # Remove leading whitespace and filter out just braces/empty lines
          body_lines <- str_trim(body_lines)
          body_lines <- body_lines[!body_lines %in% c("", "{", "}")]
          body_preview <- paste(head(body_lines, preview_lines), collapse = "\n")
        }
      }
      
      # Store function info
      functions_info[[length(functions_info) + 1]] <- list(
        name = func_name,
        signature = paste0(func_name, params),
        parameters = params,
        roxygen = if(include_roxygen) str_trim(current_roxygen) else "",
        body_preview = body_preview,
        line_number = i
      )
      
      # Reset roxygen for next function
      current_roxygen <- ""
    }
    
    # Reset roxygen if we hit a non-comment, non-function line
    if (!str_detect(line, "^\\s*#'") && !str_detect(line, "^\\s*$") && 
        !str_detect(line, "^[a-zA-Z_][a-zA-Z0-9_.]*\\s*(<-|=)\\s*function")) {
      current_roxygen <- ""
    }
  }
  
  # Convert to data frame
  if (length(functions_info) == 0) {
    return(data.frame(name = character(), signature = character(), 
                      parameters = character(), roxygen = character(),
                      body_preview = character(), line_number = integer()))
  }
  
  df <- data.frame(
    name = sapply(functions_info, function(x) x$name),
    signature = sapply(functions_info, function(x) x$signature),
    parameters = sapply(functions_info, function(x) x$parameters),
    roxygen = sapply(functions_info, function(x) x$roxygen),
    body_preview = sapply(functions_info, function(x) x$body_preview),
    line_number = sapply(functions_info, function(x) x$line_number),
    stringsAsFactors = FALSE
  )
  
  return(df)
}

#' Create a formatted overview document
#' 
#' @param file_path Path to the R script
#' @param output_format Output format: "html", "pdf", "markdown", or "console"
#' @param output_file Output file name (without extension)
#' @param include_roxygen Whether to include roxygen2 documentation
#' @param include_body_preview Whether to include function body preview
#' @return Path to generated file (if applicable)
create_function_overview <- function(file_path, 
                                     output_format = "html",
                                     output_file = NULL,
                                     include_roxygen = TRUE,
                                     include_body_preview = FALSE) {
  
  # Extract function information
  func_info <- extract_function_info(file_path, include_roxygen, include_body_preview)
  
  if (nrow(func_info) == 0) {
    message("No functions found in the script.")
    return(NULL)
  }
  
  # Generate output file name if not provided
  if (is.null(output_file)) {
    base_name <- tools::file_path_sans_ext(basename(file_path))
    output_file <- paste0(base_name, "_function_overview")
  }
  
  # Create content based on format
  if (output_format == "console") {
    # Print to console
    cat("FUNCTION OVERVIEW:", basename(file_path), "\n")
    cat(str_repeat("=", 60), "\n\n")
    
    for (i in seq_len(nrow(func_info))) {
      cat("Function:", func_info$name[i], "\n")
      cat("Line:", func_info$line_number[i], "\n")
      cat("Signature:", func_info$signature[i], "\n")
      
      if (include_roxygen && nchar(func_info$roxygen[i]) > 0) {
        cat("Documentation:\n")
        cat(str_wrap(func_info$roxygen[i], width = 70, indent = 2), "\n")
      }
      
      if (include_body_preview && nchar(func_info$body_preview[i]) > 0) {
        cat("Body preview:\n")
        cat(str_wrap(func_info$body_preview[i], width = 70, indent = 2), "\n")
      }
      
      cat(str_repeat("-", 40), "\n\n")
    }
    
    return(invisible(NULL))
    
  } else if (output_format == "markdown") {
    # Create markdown content
    md_content <- c(
      paste("# Function Overview:", basename(file_path)),
      "",
      paste("Generated on:", Sys.Date()),
      paste("Total functions found:", nrow(func_info)),
      ""
    )
    
    for (i in seq_len(nrow(func_info))) {
      md_content <- c(md_content,
                      paste("##", func_info$name[i]),
                      "",
                      paste("**Line:**", func_info$line_number[i]),
                      "",
                      paste("**Signature:**"),
                      paste("```r"),
                      func_info$signature[i],
                      paste("```"),
                      ""
      )
      
      if (include_roxygen && nchar(func_info$roxygen[i]) > 0) {
        md_content <- c(md_content,
                        "**Documentation:**",
                        "",
                        func_info$roxygen[i],
                        ""
        )
      }
      
      if (include_body_preview && nchar(func_info$body_preview[i]) > 0) {
        md_content <- c(md_content,
                        "**Body preview:**",
                        "```r",
                        func_info$body_preview[i],
                        "```",
                        ""
        )
      }
      
      md_content <- c(md_content, "---", "")
    }
    
    # Write markdown file
    output_path <- paste0(output_file, ".md")
    writeLines(md_content, output_path)
    message("Markdown overview saved to:", output_path)
    return(output_path)
    
  } else if (output_format %in% c("html", "pdf")) {
    # Create temporary markdown file and render
    temp_md <- tempfile(fileext = ".md")
    
    # Generate markdown content
    md_content <- c(
      "---",
      paste("title: 'Function Overview:", basename(file_path), "'"),
      paste("date:", Sys.Date()),
      "output:",
      if (output_format == "html") "  html_document:" else "  pdf_document:",
      "    toc: true",
      "    toc_depth: 2",
      "---",
      "",
      paste("Total functions found:", nrow(func_info)),
      ""
    )
    
    for (i in seq_len(nrow(func_info))) {
      md_content <- c(md_content,
                      paste("##", func_info$name[i]),
                      "",
                      paste("**Line:**", func_info$line_number[i]),
                      "",
                      paste("**Signature:**"),
                      "```r",
                      func_info$signature[i],
                      "```",
                      ""
      )
      
      if (include_roxygen && nchar(func_info$roxygen[i]) > 0) {
        md_content <- c(md_content,
                        "**Documentation:**",
                        "",
                        func_info$roxygen[i],
                        ""
        )
      }
      
      if (include_body_preview && nchar(func_info$body_preview[i]) > 0) {
        md_content <- c(md_content,
                        "**Body preview:**",
                        "```r",
                        func_info$body_preview[i],
                        "```",
                        ""
        )
      }
    }
    
    writeLines(md_content, temp_md)
    
    # Render to desired format
    output_path <- paste0(output_file, ".", output_format)
    
    tryCatch({
      rmarkdown::render(temp_md, 
                        output_format = if (output_format == "html") "html_document" else "pdf_document",
                        output_file = output_path,
                        quiet = TRUE)
      message(paste(str_to_title(output_format), "overview saved to:", output_path))
    }, error = function(e) {
      message("Error rendering ", output_format, ": ", e$message)
      message("Markdown file saved instead to: ", gsub("\\.pdf|\\.html", ".md", output_path))
      file.copy(temp_md, gsub("\\.pdf|\\.html", ".md", output_path))
    })
    
    # Clean up
    unlink(temp_md)
    return(output_path)
  }
}

#' Quick function overview for console display
#' 
#' @param file_path Path to the R script
quick_overview <- function(file_path) {
  func_info <- extract_function_info(file_path, include_roxygen = FALSE, include_body_preview = FALSE)
  
  if (nrow(func_info) == 0) {
    message("No functions found in the script.")
    return(invisible(NULL))
  }
  
  cat("QUICK FUNCTION LIST:", basename(file_path), "\n")
  cat(str_repeat("=", 50), "\n")
  
  # Create a simple table
  for (i in seq_len(nrow(func_info))) {
    cat(sprintf("%-3d %-25s %s\n", 
                func_info$line_number[i],
                func_info$name[i], 
                func_info$parameters[i]))
  }
  
  cat(str_repeat("=", 50), "\n")
  cat("Format: Line# Function_Name Parameters\n")
}

# Example usage:

# Basic console overview
# quick_overview("second_pass/welfare_analysis_framework.R")

# Detailed console output with documentation
# create_function_overview("second_pass/welfare_analysis_framework.R", 
#                          output_format = "console", 
#                          include_roxygen = TRUE)

# Generate HTML overview
# create_function_overview("second_pass/welfare_analysis_framework.R", 
#                          output_format = "html", 
#                          output_file = "welfare_framework_overview")

# Generate PDF overview (requires LaTeX)
# create_function_overview("second_pass/welfare_analysis_framework.R", 
#                          output_format = "pdf", 
#                          output_file = "welfare_framework_overview")

# Generate markdown overview
# create_function_overview("second_pass/welfare_analysis_framework.R", 
#                          output_format = "markdown", 
#                          output_file = "welfare_framework_overview")