# Read in .csv mapping figure names to order in manuscript

if (!dir.exists(file.path("figures", "renamed_for_journal"))) {
  dir.create(file.path("figures", "renamed_for_journal"))
}

# delete all files in figures/renamed_for_journal if any exist
mapping_file <- read.csv(file.path("figure_inputs", "figure_rename_for_journal.csv"))
for (i in 1:nrow(mapping_file)) {
  old_name <- mapping_file$filename[i]
  new_name <- file.path("renamed_for_journal", mapping_file$pub_filename[i])
  if (file.exists(file.path("figures", new_name))) {
    stop("File already exists: ", new_name)
  }
  file.copy(
    file.path("figures", old_name),
    file.path("figures", new_name)
  )
}
