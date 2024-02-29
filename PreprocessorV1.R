# Check if the package is installed, if not install it
if (!requireNamespace("vcfR", quietly = TRUE)) {
  install.packages("vcfR")
}

# Load the library
library(vcfR)

# Function to extract SNV positions from a VCF file
extract_snv_positions <- function(vcf_file) {
  # Error handling for file existence
  if (!file.exists(vcf_file)) {
    stop("VCF file does not exist: ", vcf_file)
  }
  
  vcf_data <- read.vcfR(vcf_file)
  # Filter for SNVs: non-missing REF and ALT, and single nucleotide variants
  snv_data <- vcf_data[vcf_data@fix[, "REF"] != "." & vcf_data@fix[, "ALT"] != "." & 
                         (nchar(as.character(vcf_data@fix[, "REF"])) == 1 |
                            grepl(",", vcf_data@fix[, "REF"])) &
                         (nchar(as.character(vcf_data@fix[, "ALT"])) == 1 |
                            grepl(",", vcf_data@fix[, "ALT"])), ]
  return(snv_data@fix[, c("CHROM", "POS")])
}

# Function to combine chromosome names with positions
extract_chrom_positions <- function(snv_positions) {
  chrom_and_pos <- snv_positions[, c("CHROM", "POS")]
  # Filter for valid chromosome names
  valid_chromosomes <- grepl("^(chr|CHR|[0-9])", chrom_and_pos[, "CHROM"])
  valid_chrom_and_pos <- chrom_and_pos[valid_chromosomes, ]
  # Combine chromosome and position with a colon separator
  combined_chrom_pos <- apply(valid_chrom_and_pos, 1, function(x) paste(x[1], x[2], sep = ":"))
  return(combined_chrom_pos)
}

# Replace placeholders with actual VCF or VCF.gz file paths
vcf_file_paths <- c("bcftool_snp_filter_28merge.vcf", "deepvariant_snpf_28merge.vcf", "freebayes_snpnf_28merge.vcf", "gatk_snpf_28merge.vcf")

# Process each VCF file
all_results <- list()
for (file_path in vcf_file_paths) {
  method_name <- gsub(".vcf.gz|.vcf", "", basename(file_path))
  snv_positions <- extract_snv_positions(file_path)
  chrom_positions <- extract_chrom_positions(snv_positions)
  all_results[[method_name]] <- chrom_positions
  # Memory management
  rm(snv_positions)
  gc()
}

# Specify the path for the output CSV file
output_csv_path <- "example4.csv"

# Determine the maximum length and pad other lists to this length
max_length <- max(sapply(all_results, length))
all_results_padded <- lapply(all_results, function(x) {
  length(x) <- max_length  # Pad with NA to the maximum length
  x
})

# Convert to data frame and name columns
results_df <- as.data.frame(all_results_padded)
colnames(results_df) <- names(all_results)

# Save results to CSV
write.csv(results_df, output_csv_path, row.names = FALSE, na = "")
