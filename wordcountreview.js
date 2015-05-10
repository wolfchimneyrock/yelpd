
var map = function() {
var stemmer=function(){function h(){}function i(){console.log(Array.prototype.slice.call(arguments).join(" "))}var j={ational:"ate",tional:"tion",enci:"ence",anci:"ance",izer:"ize",bli:"ble",alli:"al",entli:"ent",eli:"e",ousli:"ous",ization:"ize",ation:"ate",ator:"ate",alism:"al",iveness:"ive",fulness:"ful",ousness:"ous",aliti:"al",iviti:"ive",biliti:"ble",logi:"log"},k={icate:"ic",ative:"",alize:"al",iciti:"ic",ical:"ic",ful:"",ness:""};return function(a,l){var d,b,g,c,f,e;e=l?i:h;if(3>a.length)return a;
g=a.substr(0,1);"y"==g&&(a=g.toUpperCase()+a.substr(1));c=/^(.+?)(ss|i)es$/;b=/^(.+?)([^s])s$/;c.test(a)?(a=a.replace(c,"$1$2"),e("1a",c,a)):b.test(a)&&(a=a.replace(b,"$1$2"),e("1a",b,a));c=/^(.+?)eed$/;b=/^(.+?)(ed|ing)$/;c.test(a)?(b=c.exec(a),c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,c.test(b[1])&&(c=/.$/,a=a.replace(c,""),e("1b",c,a))):b.test(a)&&(b=b.exec(a),d=b[1],b=/^([^aeiou][^aeiouy]*)?[aeiouy]/,b.test(d)&&(a=d,e("1b",b,a),b=/(at|bl|iz)$/,f=/([^aeiouylsz])\1$/,d=/^[^aeiou][^aeiouy]*[aeiouy][^aeiouwxy]$/,
b.test(a)?(a+="e",e("1b",b,a)):f.test(a)?(c=/.$/,a=a.replace(c,""),e("1b",f,a)):d.test(a)&&(a+="e",e("1b",d,a))));c=/^(.*[aeiouy].*)y$/;c.test(a)&&(b=c.exec(a),d=b[1],a=d+"i",e("1c",c,a));c=/^(.+?)(ational|tional|enci|anci|izer|bli|alli|entli|eli|ousli|ization|ation|ator|alism|iveness|fulness|ousness|aliti|iviti|biliti|logi)$/;c.test(a)&&(b=c.exec(a),d=b[1],b=b[2],c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,c.test(d)&&(a=d+j[b],e("2",c,a)));c=/^(.+?)(icate|ative|alize|iciti|ical|ful|ness)$/;
c.test(a)&&(b=c.exec(a),d=b[1],b=b[2],c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,c.test(d)&&(a=d+k[b],e("3",c,a)));c=/^(.+?)(al|ance|ence|er|ic|able|ible|ant|ement|ment|ent|ou|ism|ate|iti|ous|ive|ize)$/;b=/^(.+?)(s|t)(ion)$/;c.test(a)?(b=c.exec(a),d=b[1],c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,c.test(d)&&(a=d,e("4",c,a))):b.test(a)&&(b=b.exec(a),d=b[1]+b[2],b=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,
b.test(d)&&(a=d,e("4",b,a)));c=/^(.+?)e$/;if(c.test(a)&&(b=c.exec(a),d=b[1],c=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*[aeiouy][aeiou]*[^aeiou][^aeiouy]*/,b=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*([aeiouy][aeiou]*)?$/,f=/^[^aeiou][^aeiouy]*[aeiouy][^aeiouwxy]$/,c.test(d)||b.test(d)&&!f.test(d)))a=d,e("5",c,b,f,a);c=/ll$/;b=/^([^aeiou][^aeiouy]*)?[aeiouy][aeiou]*[^aeiou][^aeiouy]*[aeiouy][aeiou]*[^aeiou][^aeiouy]*/;c.test(a)&&b.test(a)&&(c=/.$/,a=a.replace(c,""),e("5",
c,b,a));"y"==g&&(a=g.toLowerCase()+a.substr(1));return a}}();
//    var review = this.review_id;
    var user = this.user_id;
    business = this.business_id;
    words = this.text
       .toLowerCase()
       .replace(/[,?!;(){}"`@%^_+=~*]/g, " ")
       .replace(/\bi'll\b/g," ")
       .replace(/[']/g,"")
//       .replace(/\b(all|least|most|many|anything|any|everything|every|each|nothing|some|none|one)\b/g," %quant ")
       .replace(/\b(perfectly|perfect|satisfied|satisfying|awesome|grandiose|grand|great|good|better|best|superb|amazing|excellent|favourite|fave|nice|delicious|delish|friendly|enjoyable|stellar|enjoyed|enjoy|legitimate|legit|fantastic|lovely|love)\b/g," %good ")
       .replace(/\b(horrible|terribly|disappointing|terrible|poor|worse|worst|grossest|grosser|grossly|gross|awfully|awful|bad|ugly|rudest|rude|illegitimate|stinks|stinky|stink|stank|hate)\b/g," %bad ")
       .replace(/\b(all|everything|insanely|ridiculously|really|always|quite|very|extra|amazingly|extremely|mostly|more|must|definitely|likely|absolutely)\b/g," %more ")
       .replace(/\b(none|nothing|unlikely|rarely|barely|sometimes|few|hardly|least|few|fewest|less|lesser|measly)\b/g," %less ")
       .replace(/\b(fastest|faster|fast|quickest|quicker|quickly|quick|speedy|instantly|sooner|soonest|soon)\b/g," %fast ")
       .replace(/\b(slowest|slowly|slower|slow|forever|waited|waiting|wait)\b/g," %slow ")
       .replace(/\b(nearest|nearby|near|neighborhood|far|closer|close|within|distance|area)\b/g," %dist ")
       .replace(/\b(coldest|colder|cold|warmest|warmer|warm|hottest|hotter|hot|burning|freezing|frozen|froze)\b/g," %temp ")
       .replace(/\b(doesnt|youll|youd|youre|your|you|who|what|when|where|why|theyre|they|this|theirs|their|there|them|place|than|arent|are|am|and|any|for|from|to|at|as|because|became|came|come|been|before|be|but|by|were|weve|we|cant|can|cannot|tried|trying|try|some|bite|bit|think|that|those|these|now|then|later|after|soon|so|thought|havent|have|hadnt|had|wasnt|was|will|asked|ask|while|through|most|wont|didnt|did|still|around|the|of|people|person|done|dont|do|itll|it|until|say|said|read|want|wanted|wanting|eating|eat|ate|some|both|which|thatll|that|though|food|here|into|in|with|see|about|have|also|well|just|going|gone|getting|get|gotten|got|go|too|to|much|met|meeting|meet|tell|told|know|else|nothing|none|no|give|gave|let|me|my|mine|ours|our|on|or|an|you|us|take|out|look|shouldnt|should|couldnt|could|wouldnt|would|find|found|like|him|he|his|hers|her|she|how|off|if|isnt|aint|is|im|ive|i|a)\b/g," ")
       .replace(/[#]\w+/g," %hashtag ")
       .replace(/((zero|zed|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|teen|twenty|thirty|forty|fifty|sixty|seventy|eigty|ninety|hundred|and)(-|\s)*)+/g,"11")
       .replace(/[0-9]+\s*[°|degrees|degree|deg]*\s*(celcius|fahrenheit|f|c|degrees|degree|deg)/g," %temp ")
       .replace(/(0[1-9]|1[012])[/| |-|.](0[1-9]|[12][0-9]|3[01])[/| |-|.](19|20)[0-9][0-9]/g," %date" )
       .replace(/(midnight|noonish|noon)|(([0-2]?[0-9]((.|:|h|\s)?[0-9][0-9])?\s*(am|pm|a|p|o\s*clock|ish)+)|([0-2]?[0-9](:|h|\s)([0-9][0-9])))/g," %time ")
       .replace(/([#][0-9]+)|([0-9]+([)]|(.|:)))/g," %item ")
       .replace(/([$]+\s*([0-9]+[.][0-9]+)|([$]+\s*[0-9]+))|([$]*\s*[0-9]+\s*(euro|eur|each|ea|e|dollar|bux|buck|cent|extra|gbp)[s]*)/g," %price ")
       .replace(/((18|19|20)[0-9][0-9])|([0-9][0])('s|s)/g," %years ")
       .replace(/[0-9]+\s*(minute|min|hour|hr|day|week|month|year)[s]*/g," %wait ")
       .replace(/[0-9]+\s*(feet|foot|sf|ft|inches|inch|in|meter|mile|yard|mm|cm)[s]*/g," %dist ")
       .replace(/([0-9]+([,][0-9][0-9][0-9])*([.][0-9]+)*\s*(st|nd|rd|th)*)/g, " %number ")
       .replace(/[.:$'-]/g," ")
       .replace(/\s+/g, " ")
       .split(/(?: |#|[|]|\/|&|:)+/);
    var wordz = new Array();
//    var w = new Object();
    words.forEach(function(wd) {
       var w = new Object();
       w['word']=stemmer(wd);
       w['count']=1;
       wd!=""&&wordz.push(w);
    });
    var z = new Object();
    z['words']=wordz;
    emit(business,z);
}

var reduce = function(key, values) {
     var wd = new Object();
     var count = new Array();
     for (var x in values) {
       values[x].words.forEach(function(ve) {
         if(typeof(wd[ve.word])!="number") wd[ve.word]=0; 
         wd[ve.word] += ve.count;
       });
     }
//     var d = Object();
     for (n in wd) {
       var d = new Object();
       d['word'] = n;
       d['count'] = wd[n];
       count.push(d);
     }
     var z = new Object();
     z['words'] = count;
     return z;
}

var finalize = function(key,values) {
    var wd = new Object();
     var count = new Array();
     values.words.forEach(function(ve) {
         if(typeof(wd[ve.word])!="number") wd[ve.word]=0;
         wd[ve.word] += ve.count;
     });
     for (n in wd) {
       var d = new Object();
       d['word'] = n;
       d['count'] = wd[n];
       count.push(d);
     }
     var z = new Object();
     z['words'] = count;
     return z;
}
db.review.mapReduce(map, reduce, {scope: { decimalPlaces:0 },finalize: finalize, sort: {business_id:1}, out: "reviewWordsByBusiness"});
