import numpy as np
from gensim.models.fasttext import FastText
from ranking.models.model import Model
from ranking.storage.document_store import DocumentStore
from sklearn.metrics.pairwise import cosine_similarity


class FastTextModel(Model):
    def __init__(self, store: DocumentStore, corpus):
        super().__init__(store)
        self._model = FastText()
        self._model.build_vocab(sentences=corpus)
        self._model.train(sentences=corpus,
                          total_examples=self._model.corpus_count, epochs=10)

    def score(self, query, storage_ids):
        documents = self._store.get_doc_contents_by_storage_ids(storage_ids)
        scores = [(storage_id, self.sim(query, document.split()))
                  for storage_id, document in documents]
        return scores

    def get_embeddings(self, words):
        if len(words) == 0:
            return np.zeros(self._model.vector_size)
        word_embeddings = np.array(
            [self._model.wv.get_vector(word) for word in words])
        result = np.mean(word_embeddings, axis=0)
        return result

    def sim(self, first_words, second_words):
        first_words_embeddings = self.get_embeddings(first_words)
        second_words_embeddings = self.get_embeddings(second_words)
        [[similarity]] = cosine_similarity(first_words_embeddings.reshape(
            1, -1), second_words_embeddings.reshape(1, -1))
        return similarity
