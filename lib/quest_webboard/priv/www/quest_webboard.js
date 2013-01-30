function init() {
    test();
    socket = new io.Socket(location.hostname);
    socket.on('connect', function(){
            console.log("Connected! (TODO: clear 'connecting' message.)");
            socket.send({msg: "Hi there!"});

        });

    socket.on('message', function(data){
            console.log("Got message: "+data);
        });
    socket.connect();
}

//========== Player Model ========================================
var players = {}
var quests = {}

/** Return the row */
function createRowForPlayer(name) {
    var newRow = $("div#templates tr#player_row").clone();
    $("#name_field", newRow).text(name);
    $("#score_field", newRow).text(0);
    $("table#player_table").append(newRow);
    newRow.hide().fadeIn(400);
}

// Test:
function test() {
  var f = function(f,i) {
    createRowForPlayer("Player "+i);
    if (i<10) setTimeout(function() {f(f,i+1)}, 500)
  }
  f(f,2);
}
