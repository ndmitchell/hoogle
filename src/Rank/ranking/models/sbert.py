from ranking.models.model import Model
from ranking.storage.document_store import DocumentStore
from sentence_transformers import SentenceTransformer, util


class SBertModel(Model):
    def __init__(self, store: DocumentStore):
        super().__init__(store)
        self._model = SentenceTransformer('flax-sentence-embeddings/stackoverflow_mpnet-base', device='cuda')

    def score(self, query, storage_ids):
        documents = self._store.get_doc_contents_by_storage_ids(storage_ids)
        contents = [content for _, content in documents]
        doc_contents_embbeddings = self._model.encode(contents, convert_to_tensor=True)
        query_embedding = self._model.encode(query, convert_to_tensor=True)
        cos_scores = util.cos_sim(query_embedding, doc_contents_embbeddings)[0]
        scores = [(document[0], cos_scores[i].item()) for i, document in enumerate(documents)]
        return scores
    
    def score_docs(self, query, docs):
        scores = [(document, self.sim(query, document.split())) for document in docs]
        return scores
