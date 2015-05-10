db.userHistory.drop();

var map = function() {
       var val = this;
       var values = {
                name: val.name,
                street: val.street,
                city: val.city,
                state: val.state,
                loc: val.loc,
                lat: val.loc.coordinate[0],
                long: val.loc.coordinate[1],
                date: null
       };
       var reviews = val.reviews;
       var tips = val.tips;   
       reviews&&reviews.forEach(function(v) {
         values['date'] = new ISODate(v.date);
         emit(v.user_id, values);
       });
       tips&&tips.forEach(function(v) {
         values['date'] = new ISODate(v.date);
         emit(v.user_id, values);
       });
       // if(!(reviews||tips)) {emit(this.street,values);}
};

var reduce = function(k, values) {
    var result = new Object();
    values.forEach(function(v) {
        var field;
        if("date" in v) {
            if(!("history" in result)) {
                result.history = new Array();
            };
            result.history.push(v);
        };
        if("history" in v) {
            if(!("history" in result)) {
                result.history = new Array();
            };
            v.history.forEach(function(h) {
                result.history.push(h);
            });
        };
//        for (field in value) {
//            if (value.hasOwnProperty(field)) {
//                result[field] = value[field];
//            }
//        }
    });
    return result;
};
var finalize = function(k, values) {
    var result = new Object();
    result.history = new Array();
    if (!("history" in values)) {
          result.history.push(values);
    } else {
       values.history.forEach(function(v) {
          result.history.push(v);
       }); 
    };
    result.count = result.history.length;
    return result;
}
  
db.reviewByBusiness.mapReduce(map, reduce, {finalize:finalize, out:"userHistory"});

db.userHistory.find().forEach(function(item) {
    db.userHistory.update({_id: item._id}, item.value);
});
