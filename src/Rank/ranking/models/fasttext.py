import numpy as np
from gensim.models.fasttext import FastText
from ranking.models.model import Model
from ranking.storage.document_store import DocumentStore
from sklearn.metrics.pairwise import cosine_similarity
import pandas as pd


class FastTextModel(Model):
    def __init__(self, store: DocumentStore, corpus):
        super().__init__(store)
        self._model = FastText(sg=1)
        self._model.build_vocab(corpus)
        self._model.train(corpus,
                          total_examples=self._model.corpus_count, epochs=10)
        self._document_embeddings = self._get_store_embeddings(store)

    def score(self, query, storage_ids):
        scores = [(storage_id, self.sim(self.get_embeddings(query.split()), self._document_embeddings[storage_id])) 
            for storage_id in storage_ids]
        return scores

    def get_embeddings(self, words):
        if len(words) == 0:
            return np.zeros(self._model.vector_size)
        word_embeddings = np.array(
            [self._model.wv.get_vector(word) for word in words])
        mean_word_embeddings = np.mean(word_embeddings, axis=0)
        return mean_word_embeddings

    def sim(self, first_emb, second_emb):
        [[similarity]] = cosine_similarity(first_emb.reshape(
            1, -1), second_emb.reshape(1, -1))
        return similarity

    def _get_store_embeddings(self, store: DocumentStore) -> np.ndarray:
        documents = pd.Series(store.read_corpus()).str.split()
        doc_embeddings = documents.apply(lambda words: self.get_embeddings(words))
        return doc_embeddings 
