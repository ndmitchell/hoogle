import os
from pathlib import Path

base_dir =  Path(__file__).parents[2]
corpora_dir = os.path.join(base_dir, 'datasets\\corpora\\without-code-examples')
evaluation_dir = os.path.join(base_dir, 'datasets\\evaluation')

# Corpora
lemmatized_unique_functions_corpus = os.path.join(corpora_dir, 'lem-all-unique-functions.jsonl')
tokenized_unique_functions_corpus = os.path.join(corpora_dir, 'tokenized-all-unique-functions.jsonl')

# Contains not only function but also package, module, etc. documentation 
lemmatized_unique_sentences_corpus = os.path.join(corpora_dir, 'lemmatized-corpus.txt')
tokenized_unique_sentences_corpus = os.path.join(corpora_dir, 'tokenized-corpus.txt')

unique_functions_corpus = os.path.join(corpora_dir, 'all-unique-functions.jsonl')
raw_corpus = os.path.join(corpora_dir, 'no-code-ex-raw.dump.jsonl')

# Evaluation
lemmatized_tfidf_evaluation_set = os.path.join(evaluation_dir, 'lemmatized-eval-plain-hoogle-results.jsonl')
tokenized_tfidf_evaluation_set = os.path.join(evaluation_dir, 'tokenized-eval-plain-hoogle-results.jsonl')
manual_evaluation_set = os.path.join(evaluation_dir, 'ManualEvalSet.txt')