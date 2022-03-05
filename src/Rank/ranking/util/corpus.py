class CorpusReader:
    def __init__(self, corpus_path):
        self._corpus_path = corpus_path

    def __iter__(self):
        path = self._corpus_path
        with open(path, 'r', encoding='utf-8') as fin:
            for line in fin:
                yield list(line.split())
