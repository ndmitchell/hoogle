from ranking.models.model import Model
from ranking.storage.document_store import DocumentStore
from sentence_transformers import SentenceTransformer, util
import torch

class SBertModel(Model):
    def __init__(self, store: DocumentStore):
        super().__init__(store)
        self._model = SentenceTransformer('flax-sentence-embeddings/stackoverflow_mpnet-base', device='cuda')
        self._corpus_embeddings = self._get_normalized_embeddings(self._store.read_corpus())

    def score(self, query, storage_ids):
        query_embedding = self._get_normalized_embeddings([query])
        document_embeddings = self._select_embeddings(storage_ids)
        scores = util.dot_score(query_embedding, document_embeddings)[0]
        return [(id, score.item()) for id, score in zip(storage_ids, scores)]
    
    def score_docs(self, query, docs):
        scores = [(document, self.sim(query, document.split())) for document in docs]
        return scores

    def _select_embeddings(self, storage_ids):
        indices = torch.tensor(storage_ids).to('cuda')
        return torch.index_select(self._corpus_embeddings, 0, indices)

    def _get_normalized_embeddings(self, sentences):
        embeddings = self._model.encode(sentences, convert_to_tensor=True).to('cuda')
        embeddings = util.normalize_embeddings(embeddings)
        return embeddings
