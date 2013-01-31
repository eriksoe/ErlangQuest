function init() {
    //test();
    socket = new io.Socket(location.hostname);
    socket.on('connect', function(){
            console.log("Connected.");
            socket.send({type: "replay", from:0});
            console.log("Sent replay request.");
        });

    socket.on('message', function(data){
            console.log("Got message: "+JSON.stringify(data));
            if (data.type == "achieved") {
                var p = getPlayerByName(data.user);
                p.score += data.points;
                updatePoints(p);
            }
        });
    socket.connect();
}

//========== Player Model ========================================
var players = {}
var quests = {}

function getPlayerByName(name) {
    var p = players[name];
    if (p) return p;
    return players[name] = {
        name: name,
        score: 0,
        ach_row: createAchRowForPlayer(name)
    }
}

/** Return the row */
function createAchRowForPlayer(name) {
    var newRow = $("div#templates tr#player_row").clone();
    $("#name_field", newRow).text(name);
    $("#score_field", newRow).text(0);
    $("table#player_table").append(newRow);
    newRow.hide().fadeIn(400);
    return newRow;
}

function updatePoints(player) {
    var row = player.ach_row;
    $('#score_field', row).text(player.score);

    // Maintain sorting by score:
    var prevRow;
    while ((prevRow = $(row).prev()) &&
           parseInt($('#score_field',prevRow).text()) < player.score)
    {
        $(row).after(prevRow.remove());
    }
}

// Test:
function test() {
  var f = function(f,i) {
    createRowForPlayer("Player "+i);
    if (i<10) setTimeout(function() {f(f,i+1)}, 500)
  }
  f(f,2);
}
