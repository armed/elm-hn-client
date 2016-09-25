require('./styles/main.scss');

var config = {
  databaseURL: 'https://hacker-news.firebaseio.com'
};

var fireApp = firebase.initializeApp(config);
var Elm = require('../Main');
var app = Elm.Main.fullscreen();

app.ports.getStoryIds.subscribe(function (filter) {
  fireApp
    .database()
    .ref('v0/' + filter)
    .limitToFirst(100)
    .once('value', function (snapshot) {
      app.ports.storyIds.send(snapshot.val());
    });
});

function fireItemRequest(itemId, port) {
  fireApp
    .database()
    .ref('v0/item/' + itemId)
    .once('value', function (snapshot) {
      port.send([itemId, snapshot.val()]);
    });
}

app.ports.getStoryData.subscribe(function (storyId) {
  fireItemRequest(storyId, app.ports.storyData)
});

app.ports.getCommentData.subscribe(function (commentId) {
  fireItemRequest(commentId, app.ports.commentData)
});
