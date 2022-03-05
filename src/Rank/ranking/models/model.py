from abc import abstractmethod
from ranking.storage.document_store import DocumentStore
import pickle

class Model(dict):

    def __init__(self, store: DocumentStore) -> None:
        assert store is not None, 'Store must not be None.'
        self._store = store

    def rank(self, query, storage_ids, top_n=10):
        scores = self.score(query, storage_ids)
        ranking = sorted(scores, key=lambda score: score[1], reverse=True)[:top_n]
        return [id for id, _ in ranking]

    @abstractmethod
    def score(self, query, storage_ids):
        pass

    @staticmethod
    def load(file):
        with open(file, 'rb') as fin:
            return pickle.load(fin)

    def save(self, file):
        with open(file, 'wb') as fout:
            pickle.dump(self, fout, pickle.HIGHEST_PROTOCOL)
