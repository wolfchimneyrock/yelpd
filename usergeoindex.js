// this map reduce mongo shell script combines data from the business, reviews, and tips 
// collections to generate a collection by business, each document containing
// location info and a list of pertinent user_id and date for each review and tip.

// drop the collection if it already exists
db.reviewByBusiness.drop();

// map the business data first, using the business_id as the collection _id.
var mapBusiness = function() {
    var values = {
        // reshape the location data into geoJSON so we can run geospatial queries
        loc: {
             type: "Point",
             coordinate: [this.latitude, this.longitude]
             },
        street: this.full_address,
        city: this.city,
        state: this.state,
        name: this.name,
        categories: this.categories
    };
    emit(this.business_id, values);
};

// map reviews.  all we keep is the user_id, review_id, and date
var mapReview = function() {
    var values = {
        user_id: this.user_id,
        review_id: this.review_id,
        date: new ISODate(this.date)
    };
    emit(this.business_id, values);
};

// map tips.  all we keep are the user_id, tip_id, and date
var mapTip = function() {
    var values = {
        user_id: this.user_id,
        tip_id: this._id,
        date: new ISODate(this.date)
    };
    emit(this.business_id, values);
};

// the reduce function.
// combine data from the three collections above
var reduce = function(k, values) {
    var result = new Object(),

    // we generate this to use set membership test to filter out just adding fields 
    // that should be pushed to the list
        reviewFields = {
          "user_id": '',
          "review_id": '',
          "tip_id": '',
          "date": ''
        };
    values.forEach(function(value) {
        var field;
        // only reviews have "review_id", we push the data to the 'reviews' array
        if ("review_id" in value) {
            if (!("reviews" in result)) {
                result.reviews = new Array();
            }
            result.reviews.push(value);
        } 
        // only tips have "tip_id", we push the data to the 'tips' array
        if ("tip_id" in value) {
           if (!("tips" in result)) {
              result.tips = new Array();
           }
           result.tips.push(value);
        }
        // if the 'reviews' array already exists in the input -
        // e.g. this is a 2nd+ go around of reduce - we pass that straight
        // through to the output object array
        if ("reviews" in value) {
            if (!("reviews" in result)) {
                result.reviews = new Array();
            }
            value.reviews.forEach(function(v) {
              result.reviews.push(v);
            });        
        }
        // if the 'tips' array already exists in the input - 
        // e.g. this is a 2nd+ go around of reduce - we pass that straight
        // through to the output object array 
        if ("tips" in value) {
            if (!("tips" in result)) {
               result.tips = new Array();
            }
            value.reviews.forEach(function(v) {
              result.tips.push(v);
            });
        }
        // add all of the 'business' location fields back in to the output object
        for (field in value) {
            if (value.hasOwnProperty(field) && !(field in reviewFields)) {
                result[field] = value[field];
            }
        }
    });
    return result;
};

db.business.mapReduce(mapBusiness, reduce, {"sort":{"business_id":1}, "out": {"reduce": "reviewByBusiness"}});
db.review.mapReduce(mapReview, reduce, {"sort":{"business_id":1,"date":1}, "out": {"reduce":"reviewByBusiness"}});
db.tip.mapReduce(mapTip, reduce, {"sort":{"business_id":1,"date":1}, "out": {"reduce":"reviewByBusiness"}});

// reshape the collection to remove the 'key' and 'value' metaobject
db.reviewByBusiness.find().forEach(function(item) {
    db.reviewByBusiness.update({_id: item._id}, item.value);
});

