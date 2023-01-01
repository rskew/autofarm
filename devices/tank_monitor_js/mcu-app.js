//ESP32.enableBLE(false);

// Set mode for float switch pins
D26.mode("input_pullup");
D27.mode("input_pullup");
digitalWrite(D5, 0);

// Battery voltage read via a 1/1 voltage divider
// Calibration (after voltage divider):
// - 1.85 -> 0.541
// - 2.25 -> 0.666
// - 1.5 -> 0.432
// Apply calibration then multiply by 2 for voltage divider
const batteryCalibrationScaleFactor = 3.4 * 2;

// State variables
let batteryReading = analogRead(D35) * batteryCalibrationScaleFactor;
let tankLevelDistanceReadingCentimeters = 0.0;
let lastTankLevelDistanceReadingRaw;

// Constants

const deepSleepDurationSeconds = 300;
const wakeDurationSeconds = 31;
const batteryReadingSmoothingCoefficient = 0.998;
const batteryReadingIntervalMilliseconds = 50;
const batteryReadingReportingIntervalSeconds = 10;
const tankLevelDistanceReadingSmoothingCoefficient = 0.990;
const tankLevelDistanceReadingIntervalMilliseconds = 500;
const tankLevelReportingIntervalSeconds = 10;
const tankLevelSensorMountHeightCentimeters = 200

const tankLevelSensor = require("HC-SR04").connect(D25, D34, function(distanceCentimeters) {
    lastTankLevelDistanceReadingRaw = distanceCentimeters;
    if (distanceCentimeters < 1000) {
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
        let tankLevelReadingMeters = (tankLevelSensorMountHeightCentimeters - tankLevelDistanceReadingCentimeters) * 0.01;
        writeMessage("update_state", {"tank_level_meters": tankLevelReadingMeters,
                                      "tank_float_switch_upper": digitalRead(D26) ? "up" : "down",
                                      "tank_float_switch_lower": digitalRead(D27) ? "up" : "down",
                                      "last_tank_level_distance_reading_raw": lastTankLevelDistanceReadingRaw,
                                      "timestamp_millis": Date.now()});
    }
}

function readBatteryVoltage() {
    let reading = analogRead(D35);
    let calibratedReading = reading * batteryCalibrationScaleFactor
    batteryReading = batteryReading * batteryReadingSmoothingCoefficient + calibratedReading * (1 - batteryReadingSmoothingCoefficient);
}

function flash() {
    digitalWrite(D2,1);
    setTimeout(() => digitalWrite(D2,0), 500);
    setTimeout(() => digitalWrite(D2,1), 700);
    setTimeout(() => digitalWrite(D2,0), 1000);
}

// Deep sleep after timeout
// NOTE: This may overide the reboot on wifi non-connection
console.log("Staying awake for", wakeDurationSeconds, "seconds before going back to deep sleep for", deepSleepDurationSeconds, "seconds");
let deepSleepTimeout = setTimeout(function (){
    console.log("Entering deep sleep");
    if ('globalClient' in global && globalClient !== undefined) {
        writeMessage("enter_deep_sleep", {"duration_seconds": deepSleepDurationSeconds, "timestamp_millis": Date.now()});
    }
    setTimeout(function(){
        if ('globalClient' in global && globalClient !== undefined) {
            globalClient.end();
        }
        ESP32.deepSleep(deepSleepDurationSeconds * 1000000);
    }, 100);
}, wakeDurationSeconds * 1000);

function handleCommand(command, params) {
    if (command == "flash") {
        flash();
    } else if (command == "readTankLevelSensor") {
        reportTankLevel();
    } else if (command == "stayAwake") {
        clearTimeout(deepSleepTimeout);
        if ('globalClient' in global && globalClient !== undefined) {
            writeMessage("staying_awake", {"timestamp_millis": Date.now()});
        }
    }
}

// NOTE: These do **not** wake the mcu from deep sleep, so reporting of empty/full tank
// level will not always be immediate
// Report immediately when upper float switch goes up (tank full)
setWatch(reportTankLevel, D26, { repeat: true, edge: 'rising', debounce: 25 });
// Report immediately when lower float switch goes down (tank empty)
setWatch(reportTankLevel, D27, { repeat: true, edge: 'falling', debounce: 25 });

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
