from distutils.core import set
from Cylon.Build import cythonize

setup (
    ext_modules = cythonize("w2v.pyx")
)
