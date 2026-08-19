# ============================================================
# Repeatable CLD analysis framework
# Mafia Island Marine Park / general CLD workflow
# Joseph Maina
#
# Workflow:
#   raw CLDs
#     -> raw_nodes.csv
#     -> raw_links.csv
#     -> node_harmonisation.csv
#     -> candidate harmonised links
#     -> manual/evidence-based link curation
#     -> final integrated node/link registers
#     -> provenance summaries
#     -> directed signed network
#     -> feedback-cycle screening
#     -> module-level figure inputs
#     -> supplementary tables/figures
#
# IMPORTANT:
# The first run creates data_processed/link_curation.csv and stops.
# Review that file against the original CLDs, then rerun the script.
# The final 52-link network is created only from links explicitly
# marked "retain" in the curation file.
# ============================================================

library(tidyverse)
library(igraph)
library(openxlsx)
library(glue)

# ------------------------------------------------------------
# 0. Project configuration
# ------------------------------------------------------------

# Either:
#   (a) set CLD_PROJECT_DIR in your environment, or
#   (b) edit the fallback path below.
project_dir <- Sys.getenv(
  "CLD_PROJECT_DIR",
  unset = "/Users/maina/Documents/CLIMPA/Analyses"
)

setwd(project_dir)

# Study-specific expected counts.
# These are quality-control checks, not values used to construct the network.
expected_raw_nodes       <- 142L
expected_raw_links       <- 161L
expected_harmonised_nodes <- 32L
expected_final_links      <- 52L

# Maximum number of nodes in computationally screened cycles.
max_cycle_length <- 8L

source_order <- c("EXP", "PM", "COM")

# ------------------------------------------------------------
# 1. Paths and output folders
# ------------------------------------------------------------

dir.create("data_processed", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/network", showWarnings = FALSE, recursive = TRUE)

raw_nodes_path <- "data_raw/raw_nodes.csv"
raw_links_path <- "data_raw/raw_links.csv"
harmonisation_path <- "data_raw/node_harmonisation.csv"
curation_path <- "data_processed/link_curation.csv"

# ------------------------------------------------------------
# 2. Helper functions
# ------------------------------------------------------------

check_cols <- function(df, required, name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      glue(
        "{name} is missing required columns: ",
        "{paste(missing, collapse = ', ')}"
      )
    )
  }
}

collapse_sources <- function(x) {
  x <- unique(na.omit(x))
  ordered <- c(
    source_order[source_order %in% x],
    sort(setdiff(x, source_order))
  )
  paste(ordered, collapse = "; ")
}

source_scope <- function(n_sources) {
  case_when(
    n_sources == 3 ~ "all three sources",
    n_sources == 2 ~ "two sources",
    n_sources == 1 ~ "one source",
    TRUE ~ paste0(n_sources, " sources")
  )
}

warn_if_count_differs <- function(observed, expected, label) {
  if (!is.na(expected) && observed != expected) {
    warning(
      glue(
        "{label}: observed {observed}, expected {expected}. ",
        "Check the current input/curation files."
      )
    )
  }
}

normalise_polarity <- function(x) {
  x <- str_squish(as.character(x))
  case_when(
    x %in% c("+", "positive", "Positive", "POS", "pos", "1", "+1") ~ "+",
    x %in% c("-", "negative", "Negative", "NEG", "neg", "-1", "−") ~ "-",
    TRUE ~ x
  )
}

# ------------------------------------------------------------
# 3. Import raw data
# ------------------------------------------------------------

raw_nodes <- read_csv(raw_nodes_path, show_col_types = FALSE)
raw_links <- read_csv(raw_links_path, show_col_types = FALSE)
node_harmonisation <- read_csv(harmonisation_path, show_col_types = FALSE)

required_raw_nodes <- c(
  "source",
  "raw_node_id",
  "raw_label"
)

required_raw_links <- c(
  "source",
  "raw_link_id",
  "from_raw_id",
  "from_raw_label",
  "to_raw_id",
  "to_raw_label",
  "polarity",
  "transcription_status"
)

required_harmonisation <- c(
  "source",
  "raw_node_id",
  "raw_label",
  "harmonised_node_id",
  "harmonised_node",
  "integrated_domain",
  "mapping_status"
)

check_cols(raw_nodes, required_raw_nodes, "raw_nodes")
check_cols(raw_links, required_raw_links, "raw_links")
check_cols(
  node_harmonisation,
  required_harmonisation,
  "node_harmonisation"
)

# ------------------------------------------------------------
# 4. Standardise fields
# ------------------------------------------------------------

raw_nodes <- raw_nodes %>%
  mutate(
    source = toupper(str_squish(source)),
    raw_node_id = str_squish(raw_node_id),
    raw_label = str_squish(raw_label)
  )

raw_links <- raw_links %>%
  mutate(
    source = toupper(str_squish(source)),
    raw_link_id = str_squish(raw_link_id),
    from_raw_id = str_squish(from_raw_id),
    from_raw_label = str_squish(from_raw_label),
    to_raw_id = str_squish(to_raw_id),
    to_raw_label = str_squish(to_raw_label),
    polarity = normalise_polarity(polarity),
    transcription_status = tolower(str_squish(transcription_status))
  )

node_harmonisation <- node_harmonisation %>%
  mutate(
    source = toupper(str_squish(source)),
    raw_node_id = str_squish(raw_node_id),
    raw_label = str_squish(raw_label),
    harmonised_node_id = str_squish(harmonised_node_id),
    harmonised_node = str_squish(harmonised_node),
    integrated_domain = str_squish(integrated_domain),
    mapping_status = tolower(str_squish(mapping_status))
  )

# Polarity quality control
bad_polarity <- raw_links %>%
  filter(!polarity %in% c("+", "-"))

if (nrow(bad_polarity) > 0) {
  write_csv(
    bad_polarity,
    "data_processed/raw_links_invalid_polarity.csv"
  )
  stop(
    "Some raw links have polarity values other than '+' or '-'. ",
    "See data_processed/raw_links_invalid_polarity.csv"
  )
}

# ------------------------------------------------------------
# 5. Input integrity checks
# ------------------------------------------------------------

duplicate_raw_nodes <- raw_nodes %>%
  count(source, raw_node_id, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_raw_nodes) > 0) {
  write_csv(
    duplicate_raw_nodes,
    "data_processed/duplicate_raw_node_ids.csv"
  )
  stop("Duplicate raw node IDs detected within source CLDs.")
}

duplicate_raw_links <- raw_links %>%
  count(raw_link_id, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_raw_links) > 0) {
  write_csv(
    duplicate_raw_links,
    "data_processed/duplicate_raw_link_ids.csv"
  )
  stop("Duplicate raw link IDs detected.")
}

duplicate_harmonisation <- node_harmonisation %>%
  count(source, raw_node_id, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_harmonisation) > 0) {
  write_csv(
    duplicate_harmonisation,
    "data_processed/duplicate_harmonisation_records.csv"
  )
  stop("Duplicate source/raw-node mappings detected in node_harmonisation.")
}

# Check that harmonisation records refer to raw nodes
harmonisation_without_raw_node <- node_harmonisation %>%
  anti_join(
    raw_nodes %>% select(source, raw_node_id),
    by = c("source", "raw_node_id")
  )

raw_nodes_without_harmonisation <- raw_nodes %>%
  anti_join(
    node_harmonisation %>% select(source, raw_node_id),
    by = c("source", "raw_node_id")
  )

write_csv(
  harmonisation_without_raw_node,
  "data_processed/harmonisation_without_raw_node.csv"
)

write_csv(
  raw_nodes_without_harmonisation,
  "data_processed/raw_nodes_without_harmonisation.csv"
)

# Check raw link endpoints against raw node IDs
raw_link_endpoint_check <- bind_rows(
  raw_links %>%
    transmute(
      raw_link_id,
      source,
      endpoint = "from",
      raw_node_id = from_raw_id
    ),
  raw_links %>%
    transmute(
      raw_link_id,
      source,
      endpoint = "to",
      raw_node_id = to_raw_id
    )
) %>%
  anti_join(
    raw_nodes %>% select(source, raw_node_id),
    by = c("source", "raw_node_id")
  )

write_csv(
  raw_link_endpoint_check,
  "data_processed/raw_link_endpoints_not_in_raw_nodes.csv"
)

warn_if_count_differs(
  nrow(raw_nodes),
  expected_raw_nodes,
  "Raw node count"
)

warn_if_count_differs(
  nrow(raw_links),
  expected_raw_links,
  "Raw link count"
)

# ------------------------------------------------------------
# 6. Retain harmonisation records used in integrated analysis
# ------------------------------------------------------------

harmonisation_retained <- node_harmonisation %>%
  filter(mapping_status == "retained_in_integrated_register")

harmonisation_not_retained <- node_harmonisation %>%
  filter(mapping_status != "retained_in_integrated_register")

write_csv(
  harmonisation_not_retained,
  "data_processed/harmonisation_not_retained_or_for_review.csv"
)

# ------------------------------------------------------------
# 7. Generate integrated node register
# ------------------------------------------------------------

integrated_nodes <- harmonisation_retained %>%
  group_by(
    harmonised_node_id,
    harmonised_node,
    integrated_domain
  ) %>%
  summarise(
    sources = collapse_sources(source),
    n_sources = n_distinct(source),
    source_scope = source_scope(n_sources),
    n_raw_terms = n_distinct(raw_node_id),
    example_raw_terms = paste(
      sort(unique(raw_label)),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  arrange(harmonised_node_id) %>%
  transmute(
    node_id = harmonised_node_id,
    harmonised_node,
    domain = integrated_domain,
    sources,
    n_sources,
    source_scope,
    n_raw_terms,
    example_raw_terms
  )

warn_if_count_differs(
  nrow(integrated_nodes),
  expected_harmonised_nodes,
  "Harmonised node count"
)

write_csv(
  integrated_nodes,
  "data_processed/integrated_nodes.csv"
)

# ------------------------------------------------------------
# 8. Map raw links to harmonised source and target nodes
# ------------------------------------------------------------

from_map <- harmonisation_retained %>%
  select(
    source,
    from_raw_id = raw_node_id,
    from_node_id = harmonised_node_id,
    from_node = harmonised_node,
    from_domain = integrated_domain
  )

to_map <- harmonisation_retained %>%
  select(
    source,
    to_raw_id = raw_node_id,
    to_node_id = harmonised_node_id,
    to_node = harmonised_node,
    to_domain = integrated_domain
  )

harmonised_links_raw <- raw_links %>%
  left_join(
    from_map,
    by = c("source", "from_raw_id")
  ) %>%
  left_join(
    to_map,
    by = c("source", "to_raw_id")
  ) %>%
  mutate(
    mapping_result = case_when(
      !is.na(from_node_id) & !is.na(to_node_id) ~ "both_endpoints_retained",
      is.na(from_node_id) & !is.na(to_node_id) ~ "source_not_retained_or_unmapped",
      !is.na(from_node_id) & is.na(to_node_id) ~ "target_not_retained_or_unmapped",
      TRUE ~ "both_endpoints_not_retained_or_unmapped"
    )
  )

write_csv(
  harmonised_links_raw,
  "data_processed/raw_links_harmonised_audit.csv"
)

unmapped_or_nonretained_links <- harmonised_links_raw %>%
  filter(mapping_result != "both_endpoints_retained")

write_csv(
  unmapped_or_nonretained_links,
  "data_processed/unmapped_or_nonretained_links.csv"
)

# ------------------------------------------------------------
# 9. Separate within-node and between-node relationships
# ------------------------------------------------------------

mapped_links <- harmonised_links_raw %>%
  filter(mapping_result == "both_endpoints_retained")

within_node_links <- mapped_links %>%
  filter(from_node_id == to_node_id) %>%
  mutate(
    exclusion_reason =
      "source and target collapse to the same harmonised node"
  )

between_node_links <- mapped_links %>%
  filter(from_node_id != to_node_id)

write_csv(
  within_node_links,
  "data_processed/within_node_raw_relationships.csv"
)

write_csv(
  between_node_links,
  "data_processed/between_node_raw_relationships.csv"
)

# ------------------------------------------------------------
# 10. Create candidate harmonised signed-link register
# ------------------------------------------------------------

candidate_integrated_links <- between_node_links %>%
  group_by(
    from_node_id,
    from_node,
    from_domain,
    to_node_id,
    to_node,
    to_domain,
    polarity
  ) %>%
  summarise(
    sources = collapse_sources(source),
    n_sources = n_distinct(source),
    source_scope = source_scope(n_sources),
    n_raw_links = n(),
    supporting_raw_link_ids = paste(
      sort(unique(raw_link_id)),
      collapse = "; "
    ),
    transcription_status = paste(
      sort(unique(transcription_status)),
      collapse = "; "
    ),
    any_verification_flag = any(
      transcription_status != "supported",
      na.rm = TRUE
    ),
    raw_relationships = paste(
      sort(
        unique(
          paste0(
            source, ": ",
            from_raw_label, " -> ",
            to_raw_label
          )
        )
      ),
      collapse = " | "
    ),
    .groups = "drop"
  ) %>%
  arrange(
    from_node_id,
    to_node_id,
    polarity
  ) %>%
  mutate(
    candidate_link_id = sprintf(
      "CL%03d",
      row_number()
    )
  ) %>%
  relocate(candidate_link_id)

# Identify source-target pairs represented with both polarities
polarity_conflict_pairs <- candidate_integrated_links %>%
  group_by(
    from_node_id,
    from_node,
    to_node_id,
    to_node
  ) %>%
  summarise(
    n_polarities = n_distinct(polarity),
    polarities = paste(
      sort(unique(polarity)),
      collapse = "; "
    ),
    candidate_link_ids = paste(
      candidate_link_id,
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  filter(n_polarities > 1)

candidate_integrated_links <- candidate_integrated_links %>%
  left_join(
    polarity_conflict_pairs %>%
      select(
        from_node_id,
        to_node_id
      ) %>%
      mutate(polarity_conflict = TRUE),
    by = c(
      "from_node_id",
      "to_node_id"
    )
  ) %>%
  mutate(
    polarity_conflict = replace_na(
      polarity_conflict,
      FALSE
    ),
    review_priority = case_when(
      polarity_conflict ~ "high: polarity conflict",
      any_verification_flag ~ "high: source transcription requires verification",
      n_sources == 1 ~ "moderate: single-source relationship",
      TRUE ~ "standard"
    )
  )

write_csv(
  candidate_integrated_links,
  "data_processed/candidate_harmonised_links.csv"
)

write_csv(
  polarity_conflict_pairs,
  "data_processed/polarity_conflicts.csv"
)

# ------------------------------------------------------------
# 11. Study-stage audit summary before curation
# ------------------------------------------------------------

pre_curation_summary <- tibble(
  stage = c(
    "Raw source-specific nodes",
    "Raw signed causal links",
    "Retained source-node harmonisation records",
    "Unique harmonised nodes",
    "Raw links with both endpoints mapped to retained nodes",
    "Raw links with unmapped/non-retained endpoint(s)",
    "Within-node raw relationships",
    "Between-node raw relationships",
    "Candidate harmonised signed links",
    "Harmonised source-target pairs with polarity conflict",
    "Raw links flagged for verification"
  ),
  n = c(
    nrow(raw_nodes),
    nrow(raw_links),
    nrow(harmonisation_retained),
    nrow(integrated_nodes),
    nrow(mapped_links),
    nrow(unmapped_or_nonretained_links),
    nrow(within_node_links),
    nrow(between_node_links),
    nrow(candidate_integrated_links),
    nrow(polarity_conflict_pairs),
    sum(raw_links$transcription_status != "supported", na.rm = TRUE)
  )
)

write_csv(
  pre_curation_summary,
  "data_processed/pre_curation_summary.csv"
)

# ------------------------------------------------------------
# 12. Create or read link-curation register
# ------------------------------------------------------------
#
# This is the explicit evidence-based decision point between
# the candidate harmonised edge set and the final retained network.
#
# Allowed review_status values:
#   retain
#   exclude
#   requires_validation
#
# For retained links, final_polarity must be "+" or "-".
# ------------------------------------------------------------

if (!file.exists(curation_path)) {

  link_curation_template <- candidate_integrated_links %>%
    transmute(
      candidate_link_id,
      from_node_id,
      from_node,
      to_node_id,
      to_node,
      candidate_polarity = polarity,
      sources,
      n_sources,
      n_raw_links,
      supporting_raw_link_ids,
      transcription_status,
      polarity_conflict,
      review_priority,
      review_status = "",
      final_polarity = candidate_polarity,
      review_reason = "",
      evidence_status = if_else(
        any_verification_flag,
        "requires_review",
        "supported_in_transcribed_sources"
      )
    )

  write_csv(
    link_curation_template,
    curation_path
  )

  message(
    "\nA new link-curation template has been created at:\n  ",
    curation_path,
    "\n\nReview each candidate relationship against the original CLDs, ",
    "complete review_status/final_polarity/review_reason/evidence_status, ",
    "then rerun this script.\n",
    "The final integrated network is intentionally not constructed ",
    "until this curation step is completed."
  )

  stop(
    "Curation template created. Review data_processed/link_curation.csv ",
    "against the original CLDs, complete the curation fields, and rerun."
  )
}

# Read curation fields explicitly as CHARACTER columns.
# Blank columns in a newly created CSV are otherwise often guessed by readr
# as logical (all NA), which causes replace_na(..., "") type errors.
link_curation_manual <- read_csv(
  curation_path,
  show_col_types = FALSE,
  col_types = cols(
    candidate_link_id = col_character(),
    review_status = col_character(),
    final_polarity = col_character(),
    review_reason = col_character(),
    evidence_status = col_character(),
    .default = col_guess()
  )
)

required_curation <- c(
  "candidate_link_id",
  "review_status",
  "final_polarity",
  "review_reason",
  "evidence_status"
)

check_cols(
  link_curation_manual,
  required_curation,
  "link_curation"
)

# Force the manually edited fields to character and standardise blanks.
link_curation_manual <- link_curation_manual %>%
  mutate(
    across(
      all_of(required_curation),
      as.character
    ),
    candidate_link_id = str_squish(candidate_link_id),
    review_status = str_to_lower(
      str_squish(coalesce(review_status, ""))
    ),
    final_polarity = normalise_polarity(
      coalesce(final_polarity, "")
    ),
    review_reason = str_squish(
      coalesce(review_reason, "")
    ),
    evidence_status = str_squish(
      coalesce(evidence_status, "")
    )
  )

if (anyDuplicated(link_curation_manual$candidate_link_id) > 0) {
  stop("Duplicate candidate_link_id values detected in link_curation.csv")
}

missing_curation_ids <- setdiff(
  candidate_integrated_links$candidate_link_id,
  link_curation_manual$candidate_link_id
)

stale_curation_ids <- setdiff(
  link_curation_manual$candidate_link_id,
  candidate_integrated_links$candidate_link_id
)

if (length(missing_curation_ids) > 0) {
  stop(
    "link_curation.csv is missing current candidate IDs: ",
    paste(missing_curation_ids, collapse = ", ")
  )
}

if (length(stale_curation_ids) > 0) {
  warning(
    "link_curation.csv contains IDs not present in the current candidate set: ",
    paste(stale_curation_ids, collapse = ", ")
  )
}

allowed_review_status <- c(
  "retain",
  "exclude",
  "requires_validation"
)

# Stop cleanly if the curation table has not yet been completed.
incomplete_curation <- link_curation_manual %>%
  filter(
    review_status == "" |
      !review_status %in% allowed_review_status
  )

if (nrow(incomplete_curation) > 0) {
  write_csv(
    incomplete_curation,
    "data_processed/incomplete_curation_rows.csv"
  )
  stop(
    nrow(incomplete_curation),
    " curation row(s) are still incomplete. ",
    "Open data_processed/link_curation.csv and set review_status to ",
    "'retain', 'exclude', or 'requires_validation'. ",
    "See data_processed/incomplete_curation_rows.csv."
  )
}

curated_candidates <- candidate_integrated_links %>%
  left_join(
    link_curation_manual %>%
      select(
        candidate_link_id,
        review_status,
        final_polarity,
        review_reason,
        evidence_status
      ),
    by = "candidate_link_id"
  ) %>%
  mutate(
    review_status = str_to_lower(
      str_squish(coalesce(as.character(review_status), ""))
    ),
    final_polarity = normalise_polarity(
      coalesce(as.character(final_polarity), "")
    ),
    review_reason = str_squish(
      coalesce(as.character(review_reason), "")
    ),
    evidence_status = str_squish(
      coalesce(as.character(evidence_status), "")
    )
  )

invalid_review_status <- curated_candidates %>%
  filter(!review_status %in% allowed_review_status)

if (nrow(invalid_review_status) > 0) {
  write_csv(
    invalid_review_status,
    "data_processed/invalid_curation_status.csv"
  )
  stop(
    "Every candidate must have review_status = retain, exclude, ",
    "or requires_validation. See data_processed/invalid_curation_status.csv"
  )
}

invalid_retained_polarity <- curated_candidates %>%
  filter(
    review_status == "retain",
    !final_polarity %in% c("+", "-")
  )

if (nrow(invalid_retained_polarity) > 0) {
  write_csv(
    invalid_retained_polarity,
    "data_processed/invalid_retained_polarity.csv"
  )
  stop(
    "Every retained link must have final_polarity '+' or '-'."
  )
}

write_csv(
  curated_candidates,
  "data_processed/candidate_links_with_curation.csv"
)

# ------------------------------------------------------------
# 13. Build final retained signed-link register
# ------------------------------------------------------------

integrated_links <- curated_candidates %>%
  filter(review_status == "retain") %>%
  mutate(
    polarity = final_polarity
  ) %>%
  arrange(
    from_node_id,
    to_node_id,
    polarity
  )

# A final directed pair should not retain both + and - polarity.
final_polarity_conflicts <- integrated_links %>%
  count(
    from_node_id,
    to_node_id,
    name = "n_retained"
  ) %>%
  filter(n_retained > 1)

if (nrow(final_polarity_conflicts) > 0) {
  write_csv(
    final_polarity_conflicts,
    "data_processed/final_retained_pair_conflicts.csv"
  )
  stop(
    "The final retained register still contains more than one ",
    "relationship for at least one directed source-target pair."
  )
}

integrated_links <- integrated_links %>%
  mutate(
    link_id = sprintf(
      "L%02d",
      row_number()
    ),
    link_description = case_when(
      polarity == "+" ~ paste0(
        from_node,
        " has a same-direction effect on ",
        to_node
      ),
      polarity == "-" ~ paste0(
        from_node,
        " has an opposite-direction effect on ",
        to_node
      )
    )
  ) %>%
  select(
    link_id,
    from_node_id,
    from_node,
    from_domain,
    to_node_id,
    to_node,
    to_domain,
    polarity,
    link_description,
    sources,
    n_sources,
    source_scope,
    n_raw_links,
    supporting_raw_link_ids,
    evidence_status,
    review_reason
  )

warn_if_count_differs(
  nrow(integrated_links),
  expected_final_links,
  "Final retained link count"
)

write_csv(
  integrated_links,
  "data_processed/integrated_links.csv"
)

# Keep excluded / validation-needed candidates explicitly
curation_excluded <- curated_candidates %>%
  filter(review_status == "exclude")

curation_requires_validation <- curated_candidates %>%
  filter(review_status == "requires_validation")

write_csv(
  curation_excluded,
  "data_processed/curation_excluded_links.csv"
)

write_csv(
  curation_requires_validation,
  "data_processed/curation_links_requiring_validation.csv"
)

# ------------------------------------------------------------
# 14. Source provenance and overlap summaries
# ------------------------------------------------------------

node_source_summary <- integrated_nodes %>%
  count(
    domain,
    sources,
    n_sources,
    source_scope,
    name = "n_nodes"
  ) %>%
  arrange(
    domain,
    desc(n_sources),
    sources
  )

link_source_summary <- integrated_links %>%
  count(
    sources,
    n_sources,
    source_scope,
    name = "n_links"
  ) %>%
  arrange(
    desc(n_sources),
    sources
  )

node_overlap_summary <- integrated_nodes %>%
  count(
    n_sources,
    source_scope,
    name = "n_nodes"
  ) %>%
  arrange(desc(n_sources))

link_overlap_summary <- integrated_links %>%
  count(
    n_sources,
    source_scope,
    name = "n_links"
  ) %>%
  arrange(desc(n_sources))

source_overlap_summary <- full_join(
  node_overlap_summary,
  link_overlap_summary,
  by = c(
    "n_sources",
    "source_scope"
  )
) %>%
  replace_na(
    list(
      n_nodes = 0L,
      n_links = 0L
    )
  ) %>%
  arrange(desc(n_sources))

domain_source_matrix <- harmonisation_retained %>%
  distinct(
    source,
    harmonised_node_id,
    integrated_domain
  ) %>%
  count(
    integrated_domain,
    source,
    name = "n_nodes"
  ) %>%
  pivot_wider(
    names_from = source,
    values_from = n_nodes,
    values_fill = 0
  ) %>%
  rename(domain = integrated_domain)

# Expanded source-pattern summary
node_source_patterns <- integrated_nodes %>%
  count(
    sources,
    n_sources,
    name = "n_nodes"
  ) %>%
  arrange(desc(n_sources), sources)

link_source_patterns <- integrated_links %>%
  count(
    sources,
    n_sources,
    name = "n_links"
  ) %>%
  arrange(desc(n_sources), sources)

# ------------------------------------------------------------
# 15. Build final directed signed igraph network
# ------------------------------------------------------------

edges_for_graph <- integrated_links %>%
  transmute(
    from = from_node_id,
    to = to_node_id,
    polarity,
    link_id,
    sources,
    n_sources
  )

nodes_for_graph <- integrated_nodes %>%
  transmute(
    name = node_id,
    label = harmonised_node,
    domain,
    sources,
    n_sources
  )

g <- graph_from_data_frame(
  d = edges_for_graph,
  vertices = nodes_for_graph,
  directed = TRUE
)

node_metrics <- tibble(
  node_id = V(g)$name,
  indegree = degree(
    g,
    mode = "in"
  ),
  outdegree = degree(
    g,
    mode = "out"
  ),
  total_degree = degree(
    g,
    mode = "all"
  ),
  betweenness = betweenness(
    g,
    directed = TRUE,
    normalized = TRUE
  )
) %>%
  left_join(
    integrated_nodes,
    by = "node_id"
  ) %>%
  arrange(
    desc(total_degree),
    desc(betweenness)
  )

write_graph(
  g,
  "outputs/network/integrated_cld.graphml",
  format = "graphml"
)

# ------------------------------------------------------------
# 16. Computational screening for simple directed cycles
# ------------------------------------------------------------
#
# This screening identifies candidate cycles.
# It does NOT automatically determine the manuscript's principal loops.
# ------------------------------------------------------------

find_cycles_from_node <- function(
    graph,
    start_node,
    max_length = 8L
) {

  cycles <- list()

  walk <- function(current, path) {

    if (length(path) > max_length) {
      return(invisible(NULL))
    }

    next_nodes <- as_ids(
      neighbors(
        graph,
        current,
        mode = "out"
      )
    )

    for (next_node in next_nodes) {

      if (
        next_node == start_node &&
        length(path) >= 2
      ) {

        cycles[[length(cycles) + 1L]] <<-
          c(path, start_node)

      } else if (
        !(next_node %in% path)
      ) {

        walk(
          next_node,
          c(path, next_node)
        )
      }
    }

    invisible(NULL)
  }

  walk(
    start_node,
    start_node
  )

  cycles
}

canonical_cycle <- function(cycle) {

  cycle_no_end <- cycle[
    -length(cycle)
  ]

  rotations <- map(
    seq_along(cycle_no_end),
    function(i) {

      if (i == 1L) {
        cycle_no_end
      } else {
        c(
          cycle_no_end[
            i:length(cycle_no_end)
          ],
          cycle_no_end[
            1:(i - 1L)
          ]
        )
      }
    }
  )

  rotation_strings <- map_chr(
    rotations,
    paste,
    collapse = " -> "
  )

  sort(rotation_strings)[1]
}

all_cycles <- map(
  V(g)$name,
  ~ find_cycles_from_node(
    g,
    .x,
    max_length = max_cycle_length
  )
) %>%
  flatten()

if (length(all_cycles) == 0) {

  cycles_tbl <- tibble(
    cycle_id = character(),
    canonical = character(),
    n_nodes = integer()
  )

} else {

  cycles_tbl <- tibble(
    raw_cycle = map_chr(
      all_cycles,
      paste,
      collapse = " -> "
    ),
    canonical = map_chr(
      all_cycles,
      canonical_cycle
    ),
    n_nodes = map_int(
      all_cycles,
      ~ length(.x) - 1L
    )
  ) %>%
    distinct(
      canonical,
      .keep_all = TRUE
    ) %>%
    arrange(
      n_nodes,
      canonical
    ) %>%
    mutate(
      cycle_id = sprintf(
        "C%03d",
        row_number()
      )
    ) %>%
    relocate(cycle_id)
}

# ------------------------------------------------------------
# 17. Calculate candidate-cycle polarity
# ------------------------------------------------------------

edge_polarity_lookup <- integrated_links %>%
  select(
    from_node_id,
    to_node_id,
    polarity
  )

get_edge_polarity <- function(
    from_id,
    to_id,
    links_tbl
) {

  pol <- links_tbl %>%
    filter(
      from_node_id == from_id,
      to_node_id == to_id
    ) %>%
    pull(polarity)

  if (length(pol) == 0) {
    return(NA_character_)
  }

  if (length(unique(pol)) > 1) {
    stop(
      glue(
        "More than one polarity retained for ",
        "{from_id} -> {to_id}"
      )
    )
  }

  unique(pol)[1]
}

calculate_loop_polarity <- function(
    cycle_string,
    links_tbl
) {

  nodes <- str_split(
    cycle_string,
    " -> ",
    simplify = TRUE
  ) %>%
    as.character()

  nodes_closed <- c(
    nodes,
    nodes[1]
  )

  polarities <- map2_chr(
    nodes_closed[
      -length(nodes_closed)
    ],
    nodes_closed[-1],
    ~ get_edge_polarity(
      .x,
      .y,
      links_tbl
    )
  )

  n_negative <- sum(
    polarities == "-",
    na.rm = TRUE
  )

  inferred_loop_type <- if_else(
    n_negative %% 2L == 0L,
    "reinforcing",
    "balancing"
  )

  tibble(
    polarity_sequence = paste(
      polarities,
      collapse = " "
    ),
    n_negative = n_negative,
    inferred_loop_type = inferred_loop_type
  )
}

if (nrow(cycles_tbl) > 0) {

  cycle_polarities <- map_dfr(
    cycles_tbl$canonical,
    calculate_loop_polarity,
    links_tbl = edge_polarity_lookup
  )

  cycles_tbl <- bind_cols(
    cycles_tbl,
    cycle_polarities
  )
}

# Add human-readable node labels for screening
node_label_lookup <- integrated_nodes %>%
  select(
    node_id,
    harmonised_node
  )

cycle_id_to_label <- function(
    cycle_string,
    lookup
) {

  ids <- str_split(
    cycle_string,
    " -> ",
    simplify = TRUE
  ) %>%
    as.character()

  labels <- lookup$harmonised_node[
    match(
      ids,
      lookup$node_id
    )
  ]

  paste(
    labels,
    collapse = " -> "
  )
}

if (nrow(cycles_tbl) > 0) {
  cycles_tbl <- cycles_tbl %>%
    mutate(
      labelled_cycle = map_chr(
        canonical,
        cycle_id_to_label,
        lookup = node_label_lookup
      )
    )
}

# ------------------------------------------------------------
# 18. Curated principal feedback structures
# ------------------------------------------------------------
#
# These are manuscript interpretation records, not automatically
# selected from the computational cycle table.
# ------------------------------------------------------------

principal_loops <- tribble(
  ~loop_id,
  ~loop_label,
  ~loop_group,
  ~loop_type,
  ~short_description,
  ~validation_status,

  "i",
  "Ecological degradation–livelihood pressure loop",
  "Related reinforcing degradation pathways",
  "reinforcing",
  "Climate and water-quality stress contributes to habitat decline; reduced ecological and livelihood benefits can increase locally mediated pressure on the stressed system.",
  "curated from retained integrated signed-link structure",

  "ii",
  "Coastal protection–erosion reinforcement loop",
  "Related reinforcing degradation pathways",
  "reinforcing",
  "Decline in mangroves and coastal forests can increase erosion and beach loss, weakening coastal protection and associated livelihood and tourism benefits.",
  "curated from retained integrated signed-link structure",

  "iii",
  "Seaweed–livelihood reinforcement loop",
  "Distinct reinforcing seaweed–livelihood pathway",
  "reinforcing",
  "Environmental stress and salinity-related change can reduce seaweed yield and household income, increasing livelihood vulnerability and dependence on climate-sensitive resources.",
  "curated from retained integrated signed-link structure",

  "iv",
  "Governance/enforcement response pathway",
  "Balancing response pathways",
  "balancing response",
  "Monitoring, compliance, enforcement and awareness can reduce illegal extraction and locally mediated pressure, thereby counteracting degradation.",
  "curated response pathway; closure interpreted at system level",

  "v",
  "Restoration/adaptation response pathway",
  "Balancing response pathways",
  "balancing response",
  "Habitat restoration, protection and livelihood adaptation can improve ecological and livelihood condition and reduce vulnerability.",
  "curated response pathway; closure interpreted at system level"
)

# ------------------------------------------------------------
# 19. Module-level aggregation for publication Figure 1A
# ------------------------------------------------------------
#
# The 32-node analytical network is mapped to nine display modules.
# This is a communication layer only; network analyses above use
# the full harmonised node/link register.
# ------------------------------------------------------------

module_crosswalk <- integrated_nodes %>%
  mutate(
    display_module_id = case_when(
      node_id %in% c(
        "N01", "N02", "N03",
        "N04", "N05", "N06"
      ) ~ "M1",

      node_id %in% c(
        "N07", "N08", "N09"
      ) ~ "M2",

      node_id %in% c(
        "N10", "N11", "N12", "N13"
      ) ~ "M3",

      node_id %in% c(
        "N14", "N15", "N16",
        "N17", "N18"
      ) ~ "M4",

      node_id %in% c(
        "N19", "N20"
      ) ~ "M5",

      node_id %in% c(
        "N21", "N22", "N23",
        "N24", "N25", "N26"
      ) ~ "M6",

      node_id %in% c(
        "N27", "N28", "N29"
      ) ~ "M7",

      node_id %in% c(
        "N30", "N31"
      ) ~ "M8",

      node_id == "N32" ~ "M9",

      TRUE ~ NA_character_
    ),

    display_module = case_when(
      display_module_id == "M1" ~
        "Climate and ocean drivers",

      display_module_id == "M2" ~
        "Coastal hazards and water quality",

      display_module_id == "M3" ~
        "Coastal habitat condition",

      display_module_id == "M4" ~
        "Ecosystem and cultural benefits",

      display_module_id == "M5" ~
        "Livelihoods and well-being",

      display_module_id == "M6" ~
        "Locally mediated pressures",

      display_module_id == "M7" ~
        "Governance and conservation responses",

      display_module_id == "M8" ~
        "Enabling livelihood opportunities",

      display_module_id == "M9" ~
        "Climate resilience",

      TRUE ~ NA_character_
    )
  )

if (any(is.na(module_crosswalk$display_module_id))) {
  warning(
    "At least one harmonised node has no publication module assignment."
  )
}

module_links_all <- integrated_links %>%
  left_join(
    module_crosswalk %>%
      select(
        from_node_id = node_id,
        from_module_id = display_module_id,
        from_module = display_module
      ),
    by = "from_node_id"
  ) %>%
  left_join(
    module_crosswalk %>%
      select(
        to_node_id = node_id,
        to_module_id = display_module_id,
        to_module = display_module
      ),
    by = "to_node_id"
  ) %>%
  filter(
    !is.na(from_module_id),
    !is.na(to_module_id),
    from_module_id != to_module_id
  ) %>%
  group_by(
    from_module_id,
    from_module,
    to_module_id,
    to_module,
    polarity
  ) %>%
  summarise(
    n_integrated_links = n(),
    n_unique_sources = n_distinct(
      unlist(str_split(sources, ";\\s*"))
    ),
    supporting_link_ids = paste(
      link_id,
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  arrange(
    from_module_id,
    to_module_id,
    polarity
  )

# Flag module-pair polarity conflicts for figure curation.
module_pair_conflicts <- module_links_all %>%
  count(
    from_module_id,
    to_module_id,
    name = "n_polarities"
  ) %>%
  filter(n_polarities > 1)

module_links_all <- module_links_all %>%
  left_join(
    module_pair_conflicts %>%
      select(
        from_module_id,
        to_module_id
      ) %>%
      mutate(module_polarity_conflict = TRUE),
    by = c(
      "from_module_id",
      "to_module_id"
    )
  ) %>%
  mutate(
    module_polarity_conflict = replace_na(
      module_polarity_conflict,
      FALSE
    )
  )

# A figure-edge review template is exported but not automatically
# interpreted as the publication figure.
module_figure_edge_template <- module_links_all %>%
  mutate(
    show_in_publication_figure = "",
    figure_note = ""
  )

# ------------------------------------------------------------
# 20. Source-characteristic comparison table
# ------------------------------------------------------------

source_characteristic_comparison <- tribble(
  ~source,
  ~source_label,
  ~primary_emphasis,
  ~characteristic_content,

  "EXP",
  "Expert-derived CLD",
  "Generalisable biophysical mechanisms and resilience",
  "Climate forcing, ecosystem sensitivity, ecological condition, restoration, social well-being and resilience.",

  "PM",
  "Park-manager CLD",
  "Protected-area assets, benefits and management response",
  "Wildlife, beaches and cultural assets, tourism attractiveness, employment, conservation effectiveness, enforcement and park-level management constraints.",

  "COM",
  "Community CLD",
  "Livelihoods, markets and governance constraints",
  "Seaweed yield, fish catch, household income, seaweed prices and market access, illegal fishing, monitoring, enforcement and awareness."
)

# ------------------------------------------------------------
# 21. Candidate management entry-point table
# ------------------------------------------------------------

management_entry_points <- tribble(
  ~entry_id,
  ~vulnerability_pathway,
  ~candidate_management_entry_point,
  ~expected_system_benefit,
  ~interpretation_status,

  "LP1",
  "Habitat degradation",
  "Restoration and protection of coral reefs, mangroves, seagrasses and coastal forests",
  "Improved habitat condition, ecological recovery and adaptive capacity",
  "candidate entry point derived from integrated CLD; not evaluated intervention effectiveness",

  "LP2",
  "Illegal fishing and extraction",
  "Monitoring, compliance, enforcement and awareness",
  "Reduced locally mediated pressure and improved fish and habitat condition",
  "candidate entry point derived from integrated CLD; not evaluated intervention effectiveness",

  "LP3",
  "Livelihood sensitivity",
  "Seaweed market support, improved prices and market access, SMEs and alternative livelihoods",
  "Improved income security and reduced dependence on stressed resources",
  "candidate entry point derived from integrated CLD; not evaluated intervention effectiveness",

  "LP4",
  "Runoff and shoreline loss",
  "Catchment-to-coast and coastal-pressure management",
  "Reduced erosion, improved water quality and protection of coastal assets",
  "candidate entry point derived from integrated CLD; not evaluated intervention effectiveness"
)

# ------------------------------------------------------------
# 22. Final analytical summary
# ------------------------------------------------------------

final_summary <- tibble(
  metric = c(
    "Raw nodes",
    "Raw signed causal links",
    "Retained harmonised nodes",
    "Final retained signed links",
    "Nodes represented in all three sources",
    "Links represented in all three sources",
    "Computationally screened unique directed cycles",
    "Curated principal feedback structures"
  ),
  value = c(
    nrow(raw_nodes),
    nrow(raw_links),
    nrow(integrated_nodes),
    nrow(integrated_links),
    sum(integrated_nodes$n_sources == 3),
    sum(integrated_links$n_sources == 3),
    nrow(cycles_tbl),
    nrow(principal_loops)
  )
)

# ------------------------------------------------------------
# 23. Export processed tables
# ------------------------------------------------------------

write_csv(
  node_source_summary,
  "data_processed/node_source_summary.csv"
)

write_csv(
  link_source_summary,
  "data_processed/link_source_summary.csv"
)

write_csv(
  source_overlap_summary,
  "data_processed/source_overlap_summary.csv"
)

write_csv(
  node_source_patterns,
  "data_processed/node_source_patterns.csv"
)

write_csv(
  link_source_patterns,
  "data_processed/link_source_patterns.csv"
)

write_csv(
  domain_source_matrix,
  "data_processed/domain_source_matrix.csv"
)

write_csv(
  node_metrics,
  "data_processed/node_metrics.csv"
)

write_csv(
  cycles_tbl,
  "data_processed/computational_cycles.csv"
)

write_csv(
  principal_loops,
  "data_processed/principal_feedback_structures.csv"
)

write_csv(
  module_crosswalk,
  "data_processed/module_crosswalk.csv"
)

write_csv(
  module_links_all,
  "data_processed/module_links_all.csv"
)

write_csv(
  module_figure_edge_template,
  "data_processed/module_figure_edge_template.csv"
)

write_csv(
  source_characteristic_comparison,
  "data_processed/source_characteristic_comparison.csv"
)

write_csv(
  management_entry_points,
  "data_processed/management_entry_points.csv"
)

write_csv(
  final_summary,
  "data_processed/final_summary.csv"
)

# ------------------------------------------------------------
# 24. Export supplementary workbook
# ------------------------------------------------------------

wb <- createWorkbook()

# Manuscript-facing sheets
addWorksheet(wb, "S7_nodes")
writeData(
  wb,
  "S7_nodes",
  integrated_nodes
)

addWorksheet(wb, "S8_links")
writeData(
  wb,
  "S8_links",
  integrated_links
)

addWorksheet(wb, "S9_feedback")
writeData(
  wb,
  "S9_feedback",
  principal_loops
)

addWorksheet(wb, "S10_source_comparison")
writeData(
  wb,
  "S10_source_comparison",
  source_characteristic_comparison
)

addWorksheet(wb, "S11_management_entries")
writeData(
  wb,
  "S11_management_entries",
  management_entry_points
)

# Analytical/audit sheets
addWorksheet(wb, "Audit_summary")
writeData(
  wb,
  "Audit_summary",
  pre_curation_summary
)

addWorksheet(wb, "Source_overlap")
writeData(
  wb,
  "Source_overlap",
  source_overlap_summary
)

addWorksheet(wb, "Domain_source_matrix")
writeData(
  wb,
  "Domain_source_matrix",
  domain_source_matrix
)

addWorksheet(wb, "Candidate_links")
writeData(
  wb,
  "Candidate_links",
  candidate_integrated_links
)

addWorksheet(wb, "Polarity_conflicts")
writeData(
  wb,
  "Polarity_conflicts",
  polarity_conflict_pairs
)

addWorksheet(wb, "Within_node_links")
writeData(
  wb,
  "Within_node_links",
  within_node_links
)

addWorksheet(wb, "Unmapped_links")
writeData(
  wb,
  "Unmapped_links",
  unmapped_or_nonretained_links
)

addWorksheet(wb, "Node_metrics")
writeData(
  wb,
  "Node_metrics",
  node_metrics
)

addWorksheet(wb, "Computed_cycles")
writeData(
  wb,
  "Computed_cycles",
  cycles_tbl
)

addWorksheet(wb, "Module_crosswalk")
writeData(
  wb,
  "Module_crosswalk",
  module_crosswalk
)

addWorksheet(wb, "Module_links")
writeData(
  wb,
  "Module_links",
  module_links_all
)

addWorksheet(wb, "Final_summary")
writeData(
  wb,
  "Final_summary",
  final_summary
)

saveWorkbook(
  wb,
  "outputs/tables/CLD_integrated_supplementary_tables.xlsx",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 25. Plot: source contribution by integrated domain
# ------------------------------------------------------------

domain_source_long <- harmonisation_retained %>%
  distinct(
    source,
    harmonised_node_id,
    integrated_domain
  ) %>%
  count(
    integrated_domain,
    source,
    name = "n_nodes"
  )

p_domain_sources <- domain_source_long %>%
  ggplot(
    aes(
      x = integrated_domain,
      y = n_nodes,
      fill = source
    )
  ) +
  geom_col(
    position = "dodge"
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of harmonised nodes",
    fill = "Source",
    title = "Source contribution by integrated CLD domain"
  ) +
  theme_minimal(
    base_size = 11
  )

ggsave(
  "outputs/figures/domain_source_contribution.png",
  p_domain_sources,
  width = 8,
  height = 5.5,
  dpi = 400
)

# ------------------------------------------------------------
# 26. Plot: node/link overlap by number of supporting sources
# ------------------------------------------------------------

overlap_plot_data <- bind_rows(
  integrated_nodes %>%
    count(
      n_sources,
      name = "n"
    ) %>%
    mutate(
      element = "Nodes"
    ),

  integrated_links %>%
    count(
      n_sources,
      name = "n"
    ) %>%
    mutate(
      element = "Links"
    )
)

p_overlap <- overlap_plot_data %>%
  mutate(
    support = factor(
      n_sources,
      levels = c(1, 2, 3),
      labels = c(
        "One source",
        "Two sources",
        "All three sources"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = support,
      y = n,
      fill = element
    )
  ) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    x = NULL,
    y = "Number of retained elements",
    fill = NULL,
    title = "Cross-source support for retained CLD nodes and links"
  ) +
  theme_minimal(
    base_size = 11
  )

ggsave(
  "outputs/figures/source_overlap_nodes_links.png",
  p_overlap,
  width = 7,
  height = 4.5,
  dpi = 400
)

# ------------------------------------------------------------
# 27. Exploratory network plot
# ------------------------------------------------------------
#
# This plot is for analysis/quality control.
# The publication-quality figure remains the curated
# Overleaf/LaTeX representation.
# ------------------------------------------------------------

set.seed(20260228)

layout_xy <- layout_with_fr(g)

png(
  "outputs/figures/integrated_network_exploratory.png",
  width = 2600,
  height = 2200,
  res = 300
)

plot(
  g,
  layout = layout_xy,
  vertex.label = V(g)$label,
  vertex.size = 13,
  vertex.label.cex = 0.55,
  vertex.frame.color = "grey40",
  edge.arrow.size = 0.35,
  edge.curved = 0.08,
  edge.lty = ifelse(
    E(g)$polarity == "+",
    1,
    2
  ),
  main = "Integrated CLD — analytical network"
)

dev.off()

# ------------------------------------------------------------
# 28. Console summary
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("CLD PIPELINE COMPLETE\n")
cat("============================================================\n")

print(pre_curation_summary)
cat("\nFinal network summary:\n")
print(final_summary)

cat("\nKey files written:\n")
cat("  data_processed/integrated_nodes.csv\n")
cat("  data_processed/integrated_links.csv\n")
cat("  data_processed/source_overlap_summary.csv\n")
cat("  data_processed/computational_cycles.csv\n")
cat("  data_processed/module_crosswalk.csv\n")
cat("  data_processed/module_links_all.csv\n")
cat("  outputs/tables/CLD_integrated_supplementary_tables.xlsx\n")
cat("  outputs/network/integrated_cld.graphml\n")
cat("  outputs/figures/domain_source_contribution.png\n")
cat("  outputs/figures/source_overlap_nodes_links.png\n")
cat("  outputs/figures/integrated_network_exploratory.png\n")
cat("============================================================\n")
