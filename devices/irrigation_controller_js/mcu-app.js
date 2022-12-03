console.log("Hello");

function flash() {
    digitalWrite(D2,1);
    setTimeout(() => digitalWrite(D2,0), 500);
    setTimeout(() => digitalWrite(D2,1), 700);
    setTimeout(() => digitalWrite(D2,0), 1000);
}

function handleCommand(command, params) {
    if (command == "flash") {
        flash();
    } else if (command == "activateSolenoid") {
        console.log("Activating solenoid", params.solenoidID, "for", params.durationSeconds, "seconds");
        // TODO don't just flash
        digitalWrite(D2,1);
        setTimeout(function() {
                digitalWrite(D2,0);
                writeMessage("update_state", {"solenoid_id": params.solenoidID, "active": false, "timestamp_millis": Date.now()});
            }, params.durationSeconds * 1000);
        writeMessage("update_state", {"solenoid_id": params.solenoidID, "active": true, "timestamp_millis": Date.now()});
    }
}

//setInterval(function() {
//    if ('global_client' in global && global_client !== undefined) {
//        writeMessage("just_saying_hi", {"random_number": Math.random()});
//    }
//}, 10000);

flash();
