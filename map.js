var mapBusiness = function () {
	var output={	r_id	: null,
			u_id	: null,
			b_id	: this.business_id,
			b_stars	: this.stars,
			r_stars	: null,
			r_funny	: null,
			r_useful: null,
			r_cool	: null,
			u_fans	: null,
			u_comp	: null,
			u_revs	: null,
			b_revs	: null,
			r_date	: null,
			u_date	: null,
		}
 		emit(this.business_id, output);
	};

var mapUser = function () {
        var output={    r_id    : null,
                        u_id    : this.user_id,
                        b_id    : null,
                        b_stars : null,
                        r_stars : null,
                        r_funny : null,
                        r_useful: null,
                        r_cool  : null,
                        u_fans  : null,
                        u_comp  : this.compliments,
                        u_revs  : this.review_count,
                        b_revs  : null,
                        r_date  : null,
                        u_date  : this.yelping_since,
                }
                emit(this.user_id, output);
        };
var mapReview = function () {
        var output={    r_id    : this.review_id,
                        u_id    : null,
                        b_id    : null,
                        b_stars : null,
                        r_stars : this.stars,
                        r_funny : this.votes.funny,
                        r_useful: this.votes.useful,
                        r_cool  : this.votes.cool,
                        u_fans  : null,
                        u_comp  : null,
                        u_revs  : null,
                        b_revs  : null,
                        r_date  : this.date,
                        u_date  : null,
                }
                emit(this.review_id, output);
        };

var reduceF = function(key, values) {
    var outs={		r_id    : null,
                        u_id    : null,
                        b_id    : null,
                        b_stars : null,
                        r_stars : null,
                        r_funny : null,
                        r_useful: null,
                        r_cool  : null,
                        u_fans  : null,
                        u_comp  : null,
                        u_revs  : null,
                        b_revs  : null,
                        r_date  : null,
                        u_date  : null,
                }
 
    values.forEach(function(v) {
 
		if(outs.r_id ==null) { outs.r_id = v.r_id }
		if(outs.u_id ==null) { outs.u_id = v.u_id }
		if(outs.b_id ==null) { outs.b_id = v.b_id }
		if(outs.b_stars ==null) { outs.b_stars = v.b_stars }
		if(outs.r_stars ==null) { outs.r_stars = v.r_stars }
		if(outs.r_funny ==null) { outs.r_funny = v.r_funny }
		if(outs.r_useful ==null) { outs.r_useful = v.r_useful }
		if(outs.r_cool ==null) {outs.r_cool = v.r_cool }
		if(outs.u_fans ==null) {outs.u_fans = v.u_fans }
                if(outs.u_comp ==null) {outs.u_comp = v.u_comp }
		if(outs.u_revs ==null) {outs.u_revs = v.u_revs }
		if(outs.b_revs ==null) {outs.b_revs = v.b_revs }
		if(outs.r_date ==null) {outs.r_date = v.r_date }
		if(outs.u_date ==null) {outs.u_date = v.u_date }     
     });
    return outs;
};

result = db.user.mapReduce(mapUser, reduceF, {out: {reduce: 'reviewdata'}})
result = db.business.mapReduce(mapBusiness, reduceF, {out: {reduce: 'reviewdata'}})
result = db.review.mapReduce(mapReview, reduceF, {out: {reduce: 'reviewdata'}}) 

