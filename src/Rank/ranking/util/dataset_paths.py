import os

corpora_dir = '..\\..\\datasets\\corpora'
evaluation_dir = '..\\..\\datasets\\evaluation'

# Corpora
lemmatized_unique_functions_corpus = os.path.join(corpora_dir, 'lem-test-all-unique-functions.jsonl')
unique_functions_corpus = os.path.join(corpora_dir, 'test-all-unique-functions.jsonl')
raw_corpus = os.path.join(corpora_dir, 'raw.docs.jsonl')

# Evaluation
hoogle_results = os.path.join(evaluation_dir, 'eval-result-test.jsonl')
unified_hoogle_results = os.path.join(evaluation_dir, 'eval-plain-hoogle-results.jsonl')
tf_idf_evaluation_set = os.path.join(evaluation_dir, 'test-tfidf-evalset.jsonl')
manual_evaluation_set = os.path.join(evaluation_dir, 'ManualEvalSet.txt')