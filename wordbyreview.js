map = function() {
  var review = this.review_id;
  var words = this.text
	.toLowerCase()
	.replace(/[,?!;()"`@%^_+=~*]/g, " ")
	.replace(/[.:$'-]/g," ")
	.replace(/\s+/g, " ")
	.split(/(?: |\/|&|and|or|:)+/);
  words.forEach(function (word) {
	emit(review,{count:1});
  });
}
reduce = function(key, values) {
  var count = 0;
  values.forEach(function(values) {
  	count += typeof(v)=="number"&&v;
  });
  return {count:count};
}

db.review.mapReduce(map,reduce, {out: "wordcounts"});
