// TODO:
// - use data.indexOf to find \0 and \1 in messages

console.log("HIHIHIHI");

function flash() {
    digitalWrite(D2,1);
    setTimeout(() => digitalWrite(D2,0), 200);
    setTimeout(() => digitalWrite(D2,1), 400);
    setTimeout(() => digitalWrite(D2,0), 600);
    setTimeout(() => digitalWrite(D2,1), 800);
    setTimeout(() => digitalWrite(D2,0), 1000);
}
var ssid = require('Storage').read('wifi-ssid');
var password = require('Storage').read('wifi-password');

var wifi = require('Wifi');
wifi.connect(ssid, {password: password}, function() {
    console.log('Connected to Wifi.  IP address is:', wifi.getIP().ip);
    var server_ip = require('Storage').read('server-ip');
    var server_port = parseInt(require('Storage').read('server-port'));
    console.log('Connecting to erlang at ', server_ip, server_port);
    var client = require("net").connect({host: server_ip, port: server_port}, function() {
      console.log('client connected');
      client.write("Hello");
      var commandPieces = [];
      var messagePieces = [];
      var readingCommand = true;
      client.on('data', function(data) {
        console.log(">"+JSON.stringify(data));

        var wordStart = 0;

        for (var i = 0; i < data.length; i++) {
            if (data[i] == '\0') {
                if (readingCommand) {
                    console.log("");
                    console.log("");
                    console.log("ERROR: received \\0 while reading command!");
                    console.log("");
                    console.log("");
                    commandPieces = [];
                } else {
                    messagePieces.push(data.slice(wordStart, i));
                    actionCommand(commandPieces.join(''), messagePieces.join(''));
                    commandPieces = [];
                    messagePieces = [];
                    readingCommand = !readingCommand;
                }
                wordStart = i + 1;
            } else if (data[i] == '\1') {
                if (readingCommand) {
                    commandPieces.push(data.slice(wordStart, i));
                    console.log("Read command: '" + commandPieces.join('') + "'");
                    readingCommand = !readingCommand;
                } else {
                    console.log("");
                    console.log("");
                    console.log("ERROR: received \\1 while reading message!");
                    console.log("");
                    console.log("");
                    messagePieces = [];
                }
                wordStart = i + 1;
            } else if (i == data.length - 1) {
                if (readingCommand) {
                    commandPieces.push(data.slice(wordStart, i + 1));
                } else {
                    messagePieces.push(data.slice(wordStart, i + 1));
                }
            }
        }
      });
      client.on('end', function() {
        console.log('client disconnected');
      });
    });
});
function actionCommand(command, message) {
    console.log("Actioning command: '" + command + "' message: '" + message + "'");
    if (command == "update") {
        require('Storage').write('.boot0', message);
        flash();
    } else if (command == "flash") {
        flash();
    } else {
        console.log("Unhanded command: '" + command + "'");
    }
}
flash();
