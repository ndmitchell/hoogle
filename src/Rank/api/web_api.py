from ranking.models.sbert import SBertModel
from ranking.storage.document_store import DocumentStore
from ranking.util import dataset_paths as dp
import hug

model = SBertModel.load("..\\notebooks\\evaluation\\SBert\\tokenized-sbert-model.pkl")
store = DocumentStore(dp.tokenized_unique_functions_corpus)

@hug.get()
@hug.local()
def rank(query: hug.types.text, hoogle_ids: hug.types.comma_separated_list):
    hoogle_ids = map(int, hoogle_ids)
    storageIds = store.get_storage_ids_for_hoogle_ids(hoogle_ids)
    ranked_storage_ids = model.rank(query, storageIds)
    ranked_hoogle_ids = store.get_hoogle_ids_for_storage_ids(ranked_storage_ids)
    return ranked_hoogle_ids