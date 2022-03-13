from nltk import pos_tag
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
from nltk.stem.snowball import SnowballStemmer
from nltk.tokenize import word_tokenize
from lxml.html import fromstring, etree

# call nltk.help.upenn_tagset() to view all pos-tags


def translate_pos_tags(tag: str):
    if tag.startswith('NN'):
        return 'n'
    elif tag.startswith('VB'):
        return 'v'
    elif tag.startswith('JJ'):
        return 'a'
    elif tag.startswith('RB'):
        return 'r'
    else:
        return 'n'


def get_pos_tags(tokens):
    return [(word, translate_pos_tags(tag)) for word, tag in pos_tag(tokens)]


def tokenize(text, language='english'):
    stop = set(stopwords.words(language))
    return [token for token in word_tokenize(text) if token not in stop and token.isalnum()]


def stem(tokens, language='english'):
    stemmer = SnowballStemmer(language)
    return [stemmer.stem(token) for token in tokens]

def lemmatize(tokens):
    lemmatizer = WordNetLemmatizer()
    return [lemmatizer.lemmatize(token, tag) for token, tag in get_pos_tags(tokens)]

def get_wn_stopwords():
    stop = set(stopwords.words('english'))
    return list(stop)

def normalize(text, stem=lemmatize, stop = []):
    tokens = word_tokenize(text)
    tokens = [token for token in tokens if token.isalnum()]
    tokens = stem(tokens)
    tokens = [token for token in tokens if token not in stop]
    return " ".join(tokens)

def clean(text: str) -> str:
    doc = fromstring(text)
    etree.strip_elements(doc, 'pre')
    cleaned_text = doc.text_content()
    cleaned_text = ' '.join(cleaned_text.splitlines())
    cleaned_text = ' '.join(cleaned_text.split()) # remove consecutive spaces
    return cleaned_text.lower()