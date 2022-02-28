from ranking.util import json_lines as jl
import numpy as np


class DocumentStore:
    def __init__(self, dataset_file) -> None:
        self._df = jl.read_dataset(dataset_file)

    def get_storage_ids_for_hoogle_ids(self, hoogle_ids):
        hoogle_ids = [hoogle_id for hoogle_id in hoogle_ids if hoogle_id in self._df.index]
        return self._df.loc[hoogle_ids].index.get_level_values('storageId').unique().values

    def get_storage_id_for_hoogle_id(self, hoogle_id):
        [storageId] = self.get_storage_ids_for_hoogle_ids([hoogle_id])
        return storageId

    def get_doc_contents_by_hoogle_ids(self, hoogle_id):
        return self._df.xs(hoogle_id, level='storageId')

    def read_corpus(self) -> np.ndarray:
        groups = self._df.groupby('storageId')
        corpus = groups['docContent'].first().to_numpy()
        return corpus
