# -*- coding: utf-8 -*-
"""
Created on Fri May  8 23:49:32 2015

@author: robert
"""

from pymongo import MongoClient
#import pandas as pd
import re
from nltk.corpus import stopwords
# Download the punkt tokenizer for sentence splitting
import nltk.data
#nltk.download()   
# Load the punkt tokenizer
pickle = nltk.data.load('tokenizers/punkt/english.pickle')

def connect_mongo_collection(server='localhost',port=27017):
    client = MongoClient(server,port)
    return client

def load_reviews(indb,outdb,tokenizer,skip=0,limit=0):
    sentences = []
    bulk = outdb.initialize_unordered_bulk_op()
    for rev in indb.find({},{'_id':'0','text':'1'}).limit(limit).skip(skip):
        sentences = review_to_sentences(rev['text'],tokenizer)
        for s in sentences: 
            bulk.insert({'text':s})
    return bulk.execute()
   
    
def review_to_words( review, remove_stopwords=False ):
    # Function to convert a document to a sequence of words,
    # optionally removing stop words.  Returns a list of words.
     
    # 1. Remove non-letters
    review_text = re.sub("[^a-zA-Z]"," ", review)
    #
    # 2. Convert words to lower case and split them
    words = review_text.lower().split()
    #
    # 3. Optionally remove stop words (false by default)
    if remove_stopwords:
        stops = set(stopwords.words("english"))
        words = [w for w in words if not w in stops]
    #
    # 4. Return a list of words
    return(words)
    
def review_to_sentences( review, tokenizer, remove_stopwords=False ):
    # Function to split a review into parsed sentences. Returns a 
    # list of sentences, where each sentence is a list of words
    #
    # 1. Use the NLTK tokenizer to split the paragraph into sentences
    raw_sentences = tokenizer.tokenize(review.strip())
    #
    # 2. Loop over each sentence
    sentences = []
    for raw_sentence in raw_sentences:
        # If a sentence is empty, skip it
        if len(raw_sentence) > 0:
            # Otherwise, call review_to_wordlist to get a list of words
            sentences.append( review_to_words( raw_sentence, \
              remove_stopwords ))
    #
    # Return the list of sentences (each sentence is a list of words,
    # so this returns a list of lists
    return sentences

# Import the built-in logging module and configure it so that Word2Vec 
# creates nice output messages
import logging
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s',\
    level=logging.INFO)
    
db = connect_mongo_collection()
#db.yelp.create_collection("sentence")
batchsize=10000
totalsize=db.yelp.tip.count()
nbatches = totalsize/batchsize
finalsize=totalsize%batchsize
print "Loading %d Reviews in %d batches of %d plus a final batch of %d..." % (totalsize,nbatches,batchsize,finalsize)
count=1
total=0
for n in range(0,totalsize,batchsize):
    print "%.2f%% (%d/%d) Read %d reviews and tokenized ..." % (100*n/totalsize,count,nbatches,batchsize),
    count+=1    
    result=load_reviews(db.yelp.tip,db.yelp.sentence,pickle,skip=n,limit=batchsize)
    #print (result)
    print "%d sentences inserted..." % result['nInserted']
    total+=result['nInserted']
print "Read final %d reviews and tokenized ..." % finalsize,
result = load_reviews(db.yelp.tip,db.yelp.sentence,pickle,skip=totalsize-finalsize)
print "%d sentences inserted..." % result['nInserted']
print "Done. %d total sentences." % total
#raw_input("Press Enter to continue...")


"""
# Set values for various parameters
num_features = 300    # Word vector dimensionality                      
min_word_count = 40   # Minimum word count                        
num_workers = 4       # Number of threads to run in parallel
context = 10          # Context window size                                                                                    
downsampling = 1e-3   # Downsample setting for frequent words

# Initialize and train the model (this will take some time)
from gensim.models import word2vec
print "Training model..."
model = word2vec.Word2Vec(sentences, workers=num_workers, \
            size=num_features, min_count = min_word_count, \
            window = context, sample = downsampling)

# If you don't plan to train the model any further, calling 
# init_sims will make the model much more memory-efficient.
model.init_sims(replace=True)

# It can be helpful to create a meaningful model name and 
# save the model for later use. You can load it later using Word2Vec.load()
model_name = "yelpreviewtest"
model.save(model_name)
"""
