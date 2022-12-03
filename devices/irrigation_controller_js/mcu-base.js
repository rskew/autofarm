if ('netConfig' in global && 'deviceConfig' in global) {
    require('Wifi').on('connected', function (details) {
        console.log('Connected to Wifi.  IP address is:', netConfig.serverIP);
        console.log('Connecting to erlang at ', netConfig.serverIP, netConfig.serverPort);
        connect_autofarm();
    });
    require('Wifi').on('disconnected', function() {
        console.log("Unable to connect to wifi ssid", netConfig.wifiSSID);
        E.reboot();
    });
    require('Wifi').connect(netConfig.wifiSSID, {password: netConfig.wifiPassword});
} else {
    console.log("Create netConfig and deviceConfig values in .boot0 file");
}

var global_client;

function connect_autofarm() {
    var timeout = setTimeout(function() {
        console.log("Couldn't connect to autofarm within timeout");
        E.reboot();
    }, 10000);
    require("net").connect({host: netConfig.serverIP, port: netConfig.serverPort}, function(socket_client) {
        clearTimeout(timeout);
        global_client = socket_client;
        console.log('client connected');
        writeMessage("connect", {"type": deviceConfig.type, "id": deviceConfig.id});
        global_client.on('data', streamParser());
        global_client.on('end', function() {
            console.log('client disconnected');
            setTimeout(() => connect_autofarm(), 5000);
            global_client = undefined;
        });
    });
}

function writeMessage(command, paramsObj) {
    global_client.write(JSON.stringify([command, paramsObj]) + '\0');
}

function streamParser() {
    let messageChunks = [];
    let offset = 0;
    let fileLength = null;
    let fileName = "";
    let state = "idle";
    function handleSocketMessage(message) {
        if (state == "writingFile") {
            console.log("Finished updating :)");
            let digest = dodgyDigest(fileName);
            writeMessage("update_state", {"file_name": fileName, "exists": true, "file_checksum": digest, "timestamp_millis": Date.now()});
            state = "idle";
        } else if (state == "idle") {
            try {
                var parsedMessage = JSON.parse(message);
                var command = parsedMessage[0];
                var params = parsedMessage[1];
                if (command == "writeFile") {
                    fileName = params.fileName;
                    fileLength = params.fileLength;
                    offset = 0;
                    state = "writingFile";
                } else if (command == "copyFile") {
                    console.log("Copying file to", params.copyToFileName);
                    require('Storage').write(params.copyToFileName, require('Storage').read(params.copyFromFileName));
                    let digest = dodgyDigest(params.copyToFileName);
                    writeMessage("update_state", {"file_name": params.copyToFileName, "exists": true, "file_checksum": digest, "timestamp_millis": Date.now()});
                } else if (command == "deleteFile") {
                    console.log("Deleting file", params.fileName);
                    require('Storage').erase(params.fileName);
                    writeMessage("update_state", {"file_name": params.deleteFileName, "exists": false, "timestamp_millis": Date.now()});
                } else if (command == "reboot") {
                    if (global_client) {
                        global_client.end();
                    }
                    E.reboot();
                } else {
                    if ('handleCommand' in global) {
                        handleCommand(command, params);
                    } else {
                        console.log("Unhandled command '", command, "' with params", params)
                    }
                }
            } catch (error) {
                console.log("Error parsing or handling message:", error);
            }
        } else {
            console.log("ERROR: unknown state:", state);
        }
    }
    function handleSocketChunk(chunk) {
        if (state == "writingFile") {
            if (offset == 0) {
                console.log("Initializing empty file", fileName, "with length ", fileLength);
                console.log("Free flash memory: ", require("Storage").getFree());
                require('Storage').write(fileName, chunk, 0, fileLength);
            } else {
                console.log("Writing chunk of size", chunk.length, "to", fileName, "at offset", offset);
                require('Storage').write(fileName, chunk, offset);
            }
        } else {
            messageChunks.push(chunk);
            console.log("Received chunk of size " + chunk.length);
        }
    }
    return function handleSocketPacket(data) {
        zeroIndex = data.indexOf('\0');
        if (zeroIndex == -1) {
            handleSocketChunk(data);
            offset += data.length;
        } else {
            handleSocketChunk(data.slice(0, zeroIndex));
            var message = messageChunks.join('');
            console.log("Handling message of length", message.length);
            if (message.length < 500) {
                console.log("Message:");
                console.log(message);
            }
            handleSocketMessage(message);
            messageChunks = [];
            if (zeroIndex < data.length - 1) {
                handleSocketPacket(data.slice(zeroIndex + 1, data.length + 1));
            }
        }
    };
}

function dodgyDigest(filename) {
    let chunkSize = 1000;
    if (require('Storage').list(filename).length > 0) {
        let digest = 0;
        for(let offset = 0; true; offset += chunkSize) {
            let content = require('Storage').read(filename, offset, chunkSize);
            if (content == undefined || content == "") {
                break;
            }
            digest += E.sum(content);
        }
        return digest;
    } else {
        console.log("Could not compute dodgyDigest, file not found:", filename);
        return -1;
    }
}
