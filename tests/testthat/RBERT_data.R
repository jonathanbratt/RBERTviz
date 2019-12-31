# Run this if RBERT::extract_features (etc) change to set up the feats data.

# We need the checkpoint to be available for the other tests, so download it
# here.
BERT_PRETRAINED_DIR <- RBERT::download_BERT_checkpoint(
  model = "bert_base_uncased"
)

# First two examples are single-segment, third is two-segment.
chicken <- list(
  "The chicken didn't cross the road, because it was too tired.",
  "The chicken didn't cross the road, because it was too wide.",
  c("Why did the chicken cross the road?",
    "To get to the other side.")
)
chicken_ex <- RBERT::make_examples_simple(chicken)

feats_chicken <- RBERT::extract_features(examples = chicken_ex,
                                         ckpt_dir = BERT_PRETRAINED_DIR,
                                         layer_indexes = 0:12,
                                         batch_size = 2L,
                                         features = c("output",
                                                      "attention"))
# Save RDS version 2 so tests can run on R < 3.5.
saveRDS(
  feats_chicken,
  here::here("tests", "testthat", "feats_chicken.rds"),
  version = 2
)
