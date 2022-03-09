import setuptools

required_packages = ['rank-bm25', 'gensim', 'pandas', 'nltk', 'scikit-learn']

setuptools.setup(
    name="ranking",
    version="0.0.1",
    author="Paul-Noel Ablöscher",
    packages=setuptools.find_packages(),
    install_requires = required_packages,
    python_requires=">=3.6",
)
