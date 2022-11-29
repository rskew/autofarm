import net from 'net';

export var dummySocket = {};

var messages = [];

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
            //} else if (message == "flash") {
            //    digitalWrite(D2,1);
            //    setTimeout(function(){digitalWrite(D2,0), 500)};
            //    setTimeout(function(){digitalWrite(D2,1), 700)};
            //    setTimeout(function(){digitalWrite(D2,0), 1000)};
            } else if (message == "reboot") {
                if (client) {
                    client.end()
                }
                E.reboot();
            } else {
                messages.push(message);
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
        let zeroIndex = data.indexOf('\0');
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

export function socketConnectImpl(just) {
    return function(nothing){
        return function({host, port}) {
            return function(continuation) {
                const client = net.connect({host: host, port: port});
                client.on('connect', () => client.write("Hello"));
                //client.on('data', streamParser());
                setTimeout(function(){client.on('data', streamParser())}, 10000);
                client.on('end', () => console.log('client disconnected'));
                continuation(just(client));
            }
        }
    }
}

export function socketClose(socket) {
  return function() {
    socket.end();
  }
}

export function socketWrite(socket) {
  return function(message) {
    return function(continuation) {
      socket.write(message, 'utf8', continuation);
    }
  }
}

export function socketReceiveImpl(just) {
    return function(nothing) {
    return function(storeFileMessage) {
    return function(rebootMessage) {
    return function(readSensorMessage) {
    return function(readBatteryVoltageMessage) {
    return function(flashLightsMessage) {
    return function(doThingAMessage) {
    return function(doThingBMessage) {
        return function(socket) {
            return function(continuation) {
                if (messages.length > 0) {
                    let message = messages[0];
                    messages = messages.slice(1, messages.length);
                    continuation(just(flashLightsMessage));
                } else {
                    continuation(nothing);
                }
            }
    }
    }}}}}}}}
}
