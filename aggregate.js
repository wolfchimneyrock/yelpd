db.userHistory.aggregate([
    {$unwind: "$history"},
    {$project: {date:"$history.date", lat:"$history.lat",long:"$history.long",city:"$history.city",state:"$history.state"}},
    {$sort: {date:1}},
    {$group: {  _id:   "$_id", 
                count: {$sum: 1}, 
                first: {$min: "$date"}, 
                last:  {$max: "$date"}, 
                dates: {$push: "$date"}, 
                lat:   {$push: "$lat"},
                long:  {$push: "$long"},
                city:  {$push: "$city"},
                state: {$push: "$state"}
             }
    },
    {$out: "userLocation"}
], {allowDiskUse: true})
db.userLocation.createIndex({count:-1})
