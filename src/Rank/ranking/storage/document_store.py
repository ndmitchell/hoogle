import numpy as np
import pandas as pd
from ranking.util import json_lines as jl


class DocumentStore:
    def __init__(self, dataset_file) -> None:
        self._df = jl.read_dataset(dataset_file)
        self._grouped = self._df.reset_index(level='docId').groupby('storageId').first()
        self._assert_that_grouped_storage_ids_are_overly_strict_monotonic()

    def get_storage_ids_for_hoogle_ids(self, hoogle_ids):
        hoogle_ids = [
            hoogle_id for hoogle_id in hoogle_ids if hoogle_id in self._df.index]
        return self._df.loc[hoogle_ids].index.get_level_values('storageId').unique().values

    def get_hoogle_ids_for_storage_ids(self, storage_ids):
        return self._grouped.loc[storage_ids]['docId'].to_list()

    def get_storage_id_for_hoogle_id(self, hoogle_id):
        [storageId] = self.get_storage_ids_for_hoogle_ids([hoogle_id])
        return storageId

    def get_doc_contents_by_storage_ids(self, storage_ids):
        doc_contents = self._grouped.loc[storage_ids].apply(
            lambda res: (res.name, res['docContent']), axis=1).to_list()
        return doc_contents

    def read_corpus(self) -> np.ndarray:
        return self._grouped['docContent'].to_numpy()

    def _assert_that_grouped_storage_ids_are_overly_strict_monotonic(self):
        index = self._grouped.index
        assert_range = pd.Series(range(1,len(self._grouped) + 1))
        assert (assert_range - index).sum() == len(self._grouped), 'Grouped storage_ids must be a sequence of increasing natural numbers.'