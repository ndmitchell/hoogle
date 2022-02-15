import pickle

class BM25Model:
    def __init__(self, store, model):
        assert(store is not None)
        assert(model is not None)
        self.__store = store
        self.__bm25 = model
    
    def get_batch_scores(self, query, hoogle_ids):
        storage_ids = [self.__store.get_storage_ids_for_hoogle_ids(id) for id in hoogle_ids]
        scores = self.__bm25.get_batch_scores(query, storage_ids)
        return zip(hoogle_ids, scores)
    
    def save_model(self, file):
        with open(file, 'wb') as fout:
            pickle.dump(self, fout, pickle.HIGHEST_PROTOCOL)
    
    @staticmethod
    def load_model(file):
        with open(file, 'rb') as fin:
            return pickle.load(fin)
    