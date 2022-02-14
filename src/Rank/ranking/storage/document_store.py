import json
from pydoc import doc


class DocumentStore:
    def __init__(self, store_file) -> None:
        self.__documents = self.__loadFromJsonl(store_file)
        self.__store_file = store_file

    def get_storage_ids_for_hoogle_ids(self, hoogle_id):
        return self.__documents[hoogle_id]

    def get_document(self, storage_id):
        with open(self.__store_file) as fin:
            for i, row in enumerate(fin):
                document = json.loads(row)
                if i == storage_id:
                    return document

    def read_corpus(self):
        corpus = []
        with open(self.__store_file) as fin:
            for row in fin:
                document = json.loads(row)
                corpus.append(document['content'])
        return corpus

    def __loadFromJsonl(self, file):
        hoogle_id_to_storage_id = {}
        with open(file) as fin:
            for i, row in enumerate(fin):
                document = json.loads(row)
                hoogle_id_to_storage_id [document['id']] = i
        return hoogle_id_to_storage_id
