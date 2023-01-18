// Set mode for float switch pins
D26.mode("input_pullup");
D27.mode("input_pullup");
digitalWrite(D4, 0);
digitalWrite(D5, 0);

// Battery voltage read via a 10:1 voltage divider
// Calibration (after voltage divider):
// - battery 10V, reading 2.570 / 11
// - battery 12V, reading 3.175 / 11
// - battery 13V, reading 3.480 / 11
// - battery 15V, reading 4.085 / 11
const applyBatteryVoltageCalibration = function(rawReading) {
    return rawReading * 36.3 + 1.52;
};

// State variables
let batteryReading = applyBatteryVoltageCalibration(analogRead(D35));
let tankLevelDistanceReadingCentimeters = 0.0;
let lastTankLevelDistanceReadingRaw;
let unsentTankLevelReading = false;

// Constants

const batteryReadingSmoothingCoefficient = 0.998;
const batteryReadingIntervalMilliseconds = 50;
const batteryReadingReportingIntervalSeconds = 10;
const tankLevelDistanceReadingSmoothingCoefficient = 0.990;
const tankLevelDistanceReadingIntervalMilliseconds = 500;
const tankLevelReportingIntervalSeconds = 10;
const tankLevelSensorMountHeightCentimeters = 202;

const tankLevelSensor = require("HC-SR04").connect(D25, D34, function(distanceCentimeters) {
    lastTankLevelDistanceReadingRaw = distanceCentimeters;
    if (distanceCentimeters < 1000) {
        unsentTankLevelReading = true;
        if (tankLevelDistanceReadingCentimeters == 0.0) {
            tankLevelDistanceReadingCentimeters = distanceCentimeters;
        } else {
            tankLevelDistanceReadingCentimeters = tankLevelDistanceReadingCentimeters * tankLevelDistanceReadingSmoothingCoefficient
                                                + distanceCentimeters * (1 - tankLevelDistanceReadingSmoothingCoefficient);
        }
    }
});

function readTankLevel() {
    tankLevelSensor.trigger();
}

function reportTankLevel() {
    if ('globalClient' in global && globalClient !== undefined) {
        let message = {"tank_float_switch_upper": digitalRead(D26) ? "up" : "down",
                       "tank_float_switch_lower": digitalRead(D27) ? "down" : "up", // Lower float sensor is wired the wrong way, requires pull up
                       "timestamp_millis": Date.now()};
        if (unsentTankLevelReading) {
            let tankLevelReadingMeters = (tankLevelSensorMountHeightCentimeters - tankLevelDistanceReadingCentimeters) * 0.01;
            message.last_tank_level_distance_reading_centimeters = lastTankLevelDistanceReadingRaw;
            message.tank_level_meters = tankLevelReadingMeters;
        }
        writeMessage("update_state", message);
        unsentTankLevelReading = false;
    }
}

function readBatteryVoltage() {
    let reading = analogRead(D35);
    let calibratedReading = applyBatteryVoltageCalibration(reading);
    batteryReading = batteryReading * batteryReadingSmoothingCoefficient + calibratedReading * (1 - batteryReadingSmoothingCoefficient);
}

function flash() {
    digitalWrite(D2,1);
    setTimeout(() => digitalWrite(D2,0), 500);
    setTimeout(() => digitalWrite(D2,1), 700);
    setTimeout(() => digitalWrite(D2,0), 1000);
}

function handleCommand(command, params) {
    if (command == "flash") {
        flash();
    } else if (command == "readTankLevelSensor") {
        reportTankLevel();
    }
}

// Report immediately when upper float switch goes up (tank full)
setWatch(reportTankLevel, D26, { repeat: true, edge: 'rising', debounce: 25 });
// Report immediately when lower float switch goes down (tank empty)
setWatch(reportTankLevel, D27, { repeat: true, edge: 'rising', debounce: 25 });

// Read battery voltage, smoothing readings
setInterval(function() {
    readBatteryVoltage();
}, batteryReadingIntervalMilliseconds);

// Send battery voltage to server
setInterval(function() {
    if ('globalClient' in global && globalClient !== undefined) {
        writeMessage("update_state", {"battery_voltage": batteryReading, "timestamp_millis": Date.now()});
    }
}, batteryReadingReportingIntervalSeconds * 1000);

// Read tank level, smoothing readings
setInterval(function() {
    readTankLevel();
}, tankLevelDistanceReadingIntervalMilliseconds);

// Send tank level to server
setInterval(function() {
    reportTankLevel();
}, tankLevelReportingIntervalSeconds * 1000);

flash();

console.log("Hello");
