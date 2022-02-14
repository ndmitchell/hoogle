import pickle

class BM25Model:
    def __init__(self, store = None, model = None):
        assert(store is not None)
        self.__store = store
        self.__bm25 = model
    
    def get_batch_scores(self, query, hoogle_ids):
        storage_ids = [self.__store.get_storage_ids_for_hoogle_ids(id) for id in hoogle_ids]
        scores = self.__bm25.get_batch_scores(query, storage_ids)
        return zip(hoogle_ids, scores)
    
    def save_model(self, file):
        with open(file, 'wb') as fout:
            pickle.dump(self.__bm25, fout, pickle.HIGHEST_PROTOCOL)
    
    def load_model(self, file):
        with open(file, 'rb') as fin:
            self.__bm25 = pickle.load(fin)
    