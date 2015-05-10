
map = function() {
var stemmer=function(){function h(){}function i(){console.log(Array.prototype.slice.call(arguments).join(" "))}var j={ational:"ate",tional:"tion",enci:"ence",anci:"ance",izer:"ize",bli:"ble",alli:"al",entli:"ent",eli:"e",ousli:"ous",ization:"ize",ation:"ate",ator:"ate",alism:"al",iveness:"ive",fulness:"ful",ousness:"ous",aliti:"al",iviti:"ive",biliti:"ble",logi:"log"},k={icate:"ic",ative:"",alize:"al",iciti:"ic",ical:"ic",ful:"",ness:""};return function(a,l){var d,b,g,c,f,e;e=l?i:h;if(3>a.length)return a;
g=a.substr(0,1);"y"==g&&(a=g.toUpperCase()+a.substr(1));c=/^(.+?)(ss|i)es$/;b=/^(.+?)([^s])s$/;c.test(a)?(a=a.replace(c,"$1$2"),e("1a",c,a)):b.test(a)&&(a=a.replace(b,"$1$2"),e("1a",b,a));c=/^(.+?)eed$/;b=/^(.+?)(ed|ing)$/;c.test(a)?(b=c.exec(a),c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,c.test(b[1])&&(c=/.$/,a=a.replace(c,""),e("1b",c,a))):b.test(a)&&(b=b.exec(a),d=b[1],b=/^([^aeiou][^aeiouy]*)?[aeiouy]/,b.test(d)&&(a=d,e("1b",b,a),b=/(at|bl|iz)$/,f=/([^aeiouylsz])\1$/,d=/^[^aeiou][^aeiouy]*[aeiouy][^aeiouwxy]$/,
b.test(a)?(a+="e",e("1b",b,a)):f.test(a)?(c=/.$/,a=a.replace(c,""),e("1b",f,a)):d.test(a)&&(a+="e",e("1b",d,a))));c=/^(.*[aeiouy].*)y$/;c.test(a)&&(b=c.exec(a),d=b[1],a=d+"i",e("1c",c,a));c=/^(.+?)(ational|tional|enci|anci|izer|bli|alli|entli|eli|ousli|ization|ation|ator|alism|iveness|fulness|ousness|aliti|iviti|biliti|logi)$/;c.test(a)&&(b=c.exec(a),d=b[1],b=b[2],c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,c.test(d)&&(a=d+j[b],e("2",c,a)));c=/^(.+?)(icate|ative|alize|iciti|ical|ful|ness)$/;
c.test(a)&&(b=c.exec(a),d=b[1],b=b[2],c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,c.test(d)&&(a=d+k[b],e("3",c,a)));c=/^(.+?)(al|ance|ence|er|ic|able|ible|ant|ement|ment|ent|ou|ism|ate|iti|ous|ive|ize)$/;b=/^(.+?)(s|t)(ion)$/;c.test(a)?(b=c.exec(a),d=b[1],c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,c.test(d)&&(a=d,e("4",c,a))):b.test(a)&&(b=b.exec(a),d=b[1]+b[2],b=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,
b.test(d)&&(a=d,e("4",b,a)));c=/^(.+?)e$/;if(c.test(a)&&(b=c.exec(a),d=b[1],c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,b=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*([aeiouy][aeiou]*)?$/,f=/^[^aeiou][^aeiouy]*[aeiouy][^aeiouwxy]$/,c.test(d)||b.test(d)&&!f.test(d)))a=d,e("5",c,b,f,a);c=/ll$/;b=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*[aeiouy][aeiou]*[^aeiou][^aeiouy]*/;c.test(a)&&b.test(a)&&(c=/.$/,a=a.replace(c,""),e("5",
c,b,a));"y"==g&&(a=g.toLowerCase()+a.substr(1));return a}}();

    words = this.text
       .toLowerCase()
       .replace(/[,?!;()"`@%^_+=~*]/g, " ")
       .replace(/[#]\w+/g," %hashtag ")
       .replace(/((zero|zed|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|teen|twenty|thirty|forty|fifty|sixty|seventy|eigty|ninety|hundred|and)(-|\s)*)+/g,"11")
       .replace(/[0-9]+\s*[°|degrees|degree|deg]*\s*(celcius|fahrenheit|f|c|degrees|degree|deg)/g," %temp ")
       .replace(/(0[1-9]|1[012])[/| |-|.](0[1-9]|[12][0-9]|3[01])[/| |-|.](19|20)[0-9][0-9]/g," %date" )
       .replace(/(([0-2]?[0-9]((.|:|h|\s)?[0-9][0-9])?\s*(am|pm|a|p|o'clock|oclock|ish)+)|([0-2]?[0-9](:|h|\s)([0-9][0-9])))/g," %time ")
       .replace(/([$]+\s*([0-9]+[.][0-9]+)|([$]+\s*[0-9]+))|([$]*\s*[0-9]+\s*(euro|eur|each|ea|e|dollar|bux|buck|cent|extra|gbp)[s]*)/g," %price ")
       .replace(/((18|19|20)[0-9][0-9])|([0-9][0])('s|s)/g," %years ")
       .replace(/[0-9]+\s*(minute|min|hour|hr|day|week|month|year)[s]*/g," %wait ")
       .replace(/[0-9]+\s*(feet|foot|sf|ft|inches|inch|in|meter|mile|yard|mm|cm)[s]*/g," %dist ")
       .replace(/([0-9]+([,][0-9][0-9][0-9])*([.][0-9]+)*\s*(st|nd|rd|th)*)/g, " %number ")
       .replace(/[.:$'-]/g," ")
       .replace(/\s+/g, " ")
       .split(/(?: |\/|&|and|or|:)+/);
    words.forEach(function (word) {
       emit(stemmer(word), 1);   
    });
}

reduce = function(key, values) {
     var count = 0;
     values.forEach(function(v) {
          count += typeof(v)=="number"&&v;
     });
     return {count: count};
}

db.review.mapReduce(map, reduce, {out: "wordstems"});
