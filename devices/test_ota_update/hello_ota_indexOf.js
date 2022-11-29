console.log("Hello there streaming tcp packets into Storage yay");

function flash() {
    digitalWrite(D2,1);
    setTimeout(() => digitalWrite(D2,0), 500);
    setTimeout(() => digitalWrite(D2,1), 700);
    setTimeout(() => digitalWrite(D2,0), 1000);
}

var ssid = require('Storage').read('wifi-ssid');
var password = require('Storage').read('wifi-password');

var client;

var wifi = require('Wifi');
wifi.connect(ssid, {password: password}, function() {
    console.log('Connected to Wifi.  IP address is:', wifi.getIP().ip);
    var server_ip = require('Storage').read('server-ip');
    var server_port = parseInt(require('Storage').read('server-port'));
    console.log('Connecting to erlang at ', server_ip, server_port);
    require("net").connect({host: server_ip, port: server_port}, function(client) {
        console.log('client connected');
        client.write("Hello");
        client.on('data', streamParser());
        client.on('end', function() {
            console.log('client disconnected');
        });
    });
});

function streamParser() {
    let messageChunks = [];
    let offset = 0;
    let updateLength = null;
    let state = "waiting";
    function handleMessage() {
        if (state == "updating") {
            console.log("Finished updating :)");
            state = "waiting";
        } else if (state == "updatingWaitingForLength") {
            updateLength = parseInt(messageChunks.join(''));
            state = "updating";
            console.log("'.boot0' file has length", require('Storage').read('.boot0').length);
        } else if (state == "waiting") {
            let message = messageChunks.join('');
            console.log("Handling message of length ", message.length);
            if (message == "update") {
                state = "updatingWaitingForLength";
            } else if (message == "flash") {
                flash();
            } else if (message == "reboot") {
                if (client) {
                    client.end()
                }
                E.reboot();
            }
        } else {
            console.log("ERROR: unknown state:", state);
        }
    }
    function handleChunk(chunk) {
        if (state == "updating") {
            if (offset == 0) {
              console.log("Initializing empty '.boot0' file with length ", updateLength);
              console.log("Free flash memory: ", require("Storage").getFree());
              require('Storage').write(".boot0", chunk, 0, updateLength);
            } else {
                console.log("Writing chunk of size " + chunk.length + " to '.boot0' file at offset " + (offset+1));
                require('Storage').write(".boot0", chunk, offset);
            }
        } else if (state == "updatingWaitingForLength") {
            messageChunks.push(chunk);
            console.log("Received chunk of size " + chunk.length);
        } else if (state == "waiting") {
            messageChunks.push(chunk);
            console.log("Received chunk of size " + chunk.length);
        }
    }
    return function parseChunk(data) {
        zeroIndex = data.indexOf('\0');
        if (zeroIndex == -1) {
            handleChunk(data);
            offset += data.length;
        } else {
            handleChunk(data.slice(0, zeroIndex));
            handleMessage();
            messageChunks = [];
            offset = 0;
            if (zeroIndex < data.length - 1) {
                parseChunk(data.slice(zeroIndex + 1, data.length + 1));
            }
        }
    };
}

flash();
