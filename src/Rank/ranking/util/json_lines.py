import pandas as pd
import os

DEFAULT_ENCODING = 'utf-8'

def read_dataset(source) -> pd.DataFrame:
    df = read_jsonl(source)
    df.set_index(['docId', 'storageId'], inplace=True)
    return df

def write_dataset(df: pd.DataFrame, foutput):
    if os.path.exists(foutput): raise ValueError('Output file already exists.') 
    to_jsonl(df.reset_index(), foutput)

def read_jsonl(source, encoding=DEFAULT_ENCODING) -> pd.DataFrame:
    with open(source, encoding=encoding) as fin:
        df = pd.read_json(fin, lines=True)
    return df

def to_jsonl(df: pd.DataFrame, foutput):
    if os.path.exists(foutput): raise ValueError('Output file already exists.') 
    df.to_json(foutput, orient='records', lines=True, force_ascii=False)