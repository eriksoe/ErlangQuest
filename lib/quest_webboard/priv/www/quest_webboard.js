function init() {
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