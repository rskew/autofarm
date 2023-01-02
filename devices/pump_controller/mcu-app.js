// Turn pump off on boot
digitalWrite(D13, 1);

// State variables
let pumpOnTimeout;

function flash() {
    digitalWrite(D2,1);
    setTimeout(() => digitalWrite(D2,0), 500);
    setTimeout(() => digitalWrite(D2,1), 700);
    setTimeout(() => digitalWrite(D2,0), 1000);
}

function handleCommand(command, params) {
    if (command == "flash") {
        flash();
    } else if (command == "pumpOn") {
        if (pumpOnTimeout) {
            clearTimeout(pumpOnTimeout);
        }
        pumpOnTimeout = setTimeout(() => digitalWrite(D13, 1),
                                   params.durationSeconds * 1000);
        digitalWrite(D13, 0);
    } else if (command == "pumpOff") {
        digitalWrite(D13, 1);
        if (pumpOnTimeout) {
            clearTimeout(pumpOnTimeout);
        }
    }
}

flash();

console.log("Hello");
