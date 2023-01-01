//ESP32.enableBLE(false);

function flash() {
    digitalWrite(D2,1);
    setTimeout(() => digitalWrite(D2,0), 500);
    setTimeout(() => digitalWrite(D2,1), 700);
    setTimeout(() => digitalWrite(D2,0), 1000);
}

function handleCommand(command, params) {
    if (command == "flash") {
        flash();
    } else if (command == "sayHello") {
        if ('globalClient' in global && globalClient !== undefined) {
            writeMessage("hello", {"timestamp_millis": Date.now()});
        }
    }
}

flash();

console.log("Hello");
