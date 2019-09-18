# Copyright 2019 Bedford Freeman & Worth Pub Grp LLC DBA Macmillan Learning.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

temp_checkpoint_dir <- tempdir()
print(paste0("setting up checkpoint dir: ", temp_checkpoint_dir))

# We need the checkpoint to be available for the other tests, so download it
# here.

BERT_PRETRAINED_DIR <- RBERT::download_BERT_checkpoint(
    model = "bert_base_uncased",
    destination = temp_checkpoint_dir
)
vocab_file <- file.path(BERT_PRETRAINED_DIR, 'vocab.txt')
init_checkpoint <- file.path(BERT_PRETRAINED_DIR, 'bert_model.ckpt')
bert_config_file <- file.path(BERT_PRETRAINED_DIR, 'bert_config.json')
