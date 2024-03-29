import net from 'net';

export var dummySocket = {};

var socketMessages = [];
var systemMessages = [];

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
                socketMessages.push(message);
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

export function socketConnectImplJs(just) {
    return function(nothing) {
    return function(systemMessageSocketClosed) {
    return function(netConfig) {
        return function(continuation) {
            const socket = net.connect({host: netConfig.host, port: netConfig.port});
            socket.on('error', (err) => {console.log('Error connecting to socket:', err); continuation(nothing)});
            socket.on('connect', function() {
                console.log("Connected");
                socket.on('data', streamParser());
                //setTimeout(function(){socket.on('data', streamParser())}, 1000);
                socket.on('end', function() {
                    console.log('client disconnected');
                    systemMessages.push(systemMessageSocketClosed);
                });
                socket.write(JSON.stringify(["connect", {"type": "device_monitor", "id": 0}]) + "\0");
                continuation(just(socket));
            });
        }
    }}}
}
export var socketConnectImplJs2 = socketConnectImplJs;

export function socketCloseImpl(socket) {
  return function() {
    socket.end();
  }
}

export function socketWriteMessageImpl(socket) {
  return function(message) {
    return function(continuation) {
        console.log("hi socketWriteMessageImpl");
        socket.write(message + "\0");
        continuation();
    };
  }
}

export function socketReceiveImplJs(just) {
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
                if (socketMessages.length > 0) {
                    let message = JSON.parse(socketMessages[0]);
                    socketMessages = socketMessages.slice(1, socketMessages.length);
                    let command = message[0];
                    let params = message[1];
                    console.log("Received command", command, "with params", params);
                    if (command == "flash") {
                        continuation(just(flashLightsMessage));
                    } else if (command == "reboot") {
                        continuation(just(rebootMessage));
                    } else {
                        console.log("Unhandled command", command, " with params", params);
                        continuation(nothing);
                    }
                } else {
                    continuation(nothing);
                }
            }
    }
    }}}}}}}}
}

export function systemReceiveImplJs(just) {
    return function(nothing) {
        return function(continuation) {
            if (systemMessages.length > 0) {
                let message = systemMessages[0];
                systemMessages = systemMessages.slice(1, systemMessages.length);
                continuation(just(message));
            } else {
                continuation(nothing);
            }
        }
    }
}
export var systemReceiveImplJs2 = systemReceiveImplJs;

export function deepSleepImplJs(millis) {
    return function(continuation) {
        setTimeout(function(){
            socketMessages = [];
            systemMessages = [];
            continuation();
        }, millis);
    }
}

export function flashLightsImpl(continuation) {
    digitalWrite(D2,1);
    setTimeout(() => digitalWrite(D2,0), 500);
    setTimeout(() => digitalWrite(D2,1), 700);
    setTimeout(() => digitalWrite(D2,0), 1000);
    continuation();
}
