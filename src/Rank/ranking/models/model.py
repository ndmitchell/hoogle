from abc import abstractmethod
from ranking.storage.document_store import DocumentStore

class Model:

    def __init__(self, store: DocumentStore) -> None:
        assert store is not None, 'Store must not be None.'
        self._store = store

    def rank(self, query, hoogleIds):
        scores = self.score(query, hoogleIds)
        ranking = sorted(scores, key=lambda score: score[1], reverse=True)
        return ranking

    @abstractmethod
    def score(self, query, hoogleIds):
        pass

    @abstractmethod
    def load():
        pass

    @abstractmethod
    def save(self, file):
        pass
