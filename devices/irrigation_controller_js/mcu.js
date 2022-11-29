console.log("Hello");

function flash() {
    digitalWrite(D2,1);
    setTimeout(() => digitalWrite(D2,0), 500);
    setTimeout(() => digitalWrite(D2,1), 700);
    setTimeout(() => digitalWrite(D2,0), 1000);
}

var wifi_ssid = require('Storage').read('wifi-ssid');
var wifi_password = require('Storage').read('wifi-password');

var server_ip = require('Storage').read('server-ip');
var server_port = parseInt(require('Storage').read('server-port'));

var global_client;

require('Wifi').on('connected', function (details) {
    console.log('Connected to Wifi.  IP address is:', details.ip);
    console.log('Connecting to erlang at ', server_ip, server_port);
    connect_autofarm();
});
require('Wifi').on('disconnected', function() {
    console.log("Unable to connect to wifi ssid", wifi_ssid);
    E.reboot();
});
require('Wifi').connect(wifi_ssid, {password: wifi_password});

function connect_autofarm() {
    var timeout = setTimeout(function() {
        console.log("Couldn't connect to autofarm within timeout");
        E.reboot();
    }, 10000);
    require("net").connect({host: server_ip, port: server_port}, function(socket_client) {
        clearTimeout(timeout);
        global_client = socket_client;
        console.log('client connected');
        global_client.write("Hello");
        global_client.on('data', streamParser());
        global_client.on('end', function() {
            console.log('client disconnected');
            setTimeout(() => connect_autofarm(), 5000);
        });
    });
}

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
            if (message.length < 500) {
                console.log("Message:");
                console.log(message);
            }
            if (message == "update") {
                state = "updatingWaitingForLength";
            } else if (message == "flash") {
                flash();
            } else if (message == "reboot") {
                if (global_client) {
                    global_client.end();
                }
                E.reboot();
            } else if (message.slice(0, 17) == "activate_solenoid") {
                solenoid = parseInt(message.slice(17, 20));
                duration = parseInt(message.slice(20, 25));
                console.log("Activating solenoid", solenoid, "for", duration, "seconds");
                // TODO don't just flash
                digitalWrite(D2,1);
                setTimeout(() => digitalWrite(D2,0), duration * 1000);
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
