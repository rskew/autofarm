//ESP32.enableBLE(false);

// State variables
let batteryReading = analogRead(D35);

// Constants
const batteryReadingSmoothingCoefficient = 0.998;
const batteryReadingIntervalMilliseconds = 100;
const batteryReadingReportingIntervalSeconds = 60;

// Battery voltage read via a 1/11 voltage divider
// Calibration:
// - reading of 0.285 when multimeter says 11.88V
function readBatteryVoltage() {
    let reading = analogRead(D35);
    batteryReading = batteryReading * batteryReadingSmoothingCoefficient + reading * (1 - batteryReadingSmoothingCoefficient);
}

solenoids = {
    1: {"pin": D23, "on": 1, "off": 0, "timeout": null},
    2: {"pin": D22, "on": 1, "off": 0, "timeout": null},
    //3: {"pin": D17, "on": 1, "off": 0, "timeout": null}, // Relay 3 is borked :(
    4: {"pin": D19, "on": 1, "off": 0, "timeout": null}, // Labelled "5" in the box :(
    5: {"pin": D18, "on": 0, "off": 1, "timeout": null}, // Labelled "3" in th box
    6: {"pin": D5,  "on": 0, "off": 1, "timeout": null},
};

// Turn off all solenoids on boot
for (var solenoidID in solenoids) {
    digitalWrite(solenoids[solenoidID].pin, solenoids[solenoidID].off);
}

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
        if (params.solenoidID in solenoids) {
            let solenoid = solenoids[params.solenoidID];
            clearTimeout(solenoid.timeout);
            digitalWrite(solenoid.pin, solenoid.on);
            solenoid.timeout = setTimeout(function() {
                    digitalWrite(solenoid.pin, solenoid.off);
                    writeMessage("update_state", {"solenoid_id": params.solenoidID, "active": false, "timestamp_millis": Date.now()});
                }, params.durationSeconds * 1000);
            writeMessage("update_state", {"solenoid_id": params.solenoidID, "active": true, "timestamp_millis": Date.now()});
        }
    }
}

// Read battery voltage, smoothing readings
setInterval(function() {
    readBatteryVoltage();
}, batteryReadingIntervalMilliseconds);

// Send battery voltage to server
setInterval(function() {
    if ('global_client' in global && global_client !== undefined) {
        writeMessage("update_state", {"battery_voltage": batteryReading, "timestamp_millis": Date.now()});
    }
}, batteryReadingReportingIntervalSeconds * 1000);

flash();
