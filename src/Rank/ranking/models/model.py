from abc import abstractmethod
from ranking.storage.document_store import DocumentStore

class Model:

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

    @abstractmethod
    def load():
        pass

    @abstractmethod
    def save(self, file):
        pass
