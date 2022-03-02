import pickle
from ranking.models.model import Model
from rank_bm25 import BM25Okapi
from ranking.storage.document_store import DocumentStore

class BM25Model(Model):
    def __init__(self, store: DocumentStore):
        super().__init__(store)
        corpus = [tokenized.split() for tokenized in self._store.read_corpus()]
        self._bm25 = BM25Okapi(corpus)
    
    def score(self, query, storage_ids):
        scores = self._bm25.get_batch_scores(query, storage_ids)
        return zip(storage_ids, scores)
    
    def save_model(self, file):
        with open(file, 'wb') as fout:
            pickle.dump(self, fout, pickle.HIGHEST_PROTOCOL)
    
    @staticmethod
    def load_model(file):
        with open(file, 'rb') as fin:
            return pickle.load(fin)
    