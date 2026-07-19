// References
// - https://raw.githubusercontent.com/RuiSantosdotme/Random-Nerd-Tutorials/master/Projects/ESP32/LoRaSender.ino
// - https://raw.githubusercontent.com/RuiSantosdotme/Random-Nerd-Tutorials/master/Projects/ESP32/LoRaReceiver.ino
// - https://github.com/LowPowerLab/RFM69/blob/master/Examples/Node/Node.ino
// - https://www.airspayce.com/mikem/arduino/RadioHead/rf95_reliable_datagram_server_8ino-example.html
#include <SPI.h>
#include <LoRa.h>
#include <esp_system.h>
#include <esp_task_wdt.h>

#include <Hamming.h>
HammingCode hamming;

// Pins used by the transceiver module
#define ss 5
#define rst 14
#define dio0 2

int NODE_NUMBER = 2;
const int NUM_SOLENOIDS = 1;
int solenoidPins[NUM_SOLENOIDS] = {27};
unsigned long offTimes[NUM_SOLENOIDS] = {0};

struct Action {
    int nodeNumber;
    int solenoidNumber;
    bool onOrOff;
    int secondsOn;
};

String receivedMessage;
Action receivedAction = {};

void setup() {
  Serial.begin(115200);
  // No while(!Serial); the node runs headless in the field.

  // Let supplies settle after a brown-out recovery before touching anything.
  delay(200);

#if defined(ESP_ARDUINO_VERSION_MAJOR) && ESP_ARDUINO_VERSION_MAJOR >= 3
  esp_task_wdt_config_t wdt_config = {
    .timeout_ms = 15000,
    .idle_core_mask = (1 << 0),
    .trigger_panic = true,
  };
  esp_task_wdt_deinit();
  esp_task_wdt_init(&wdt_config);
#else
  esp_task_wdt_init(15, true);
#endif
  esp_task_wdt_add(NULL);

  esp_reset_reason_t reason = esp_reset_reason();
  Serial.print("Node " + String(NODE_NUMBER)
               + " boot, reset reason: " + String(reason) + "\r\n");

  // If the last reset was brown-out, deep-sleep to let the solar panel
  // build some charge instead of immediately retrying and burning it.
  if (reason == ESP_RST_BROWNOUT) {
    Serial.print("Brown-out detected, sleeping 30s to recover\r\n");
    delay(50);
    esp_deep_sleep(30ULL * 1000000ULL);
  }

  for (int solenoid_number = 1; solenoid_number <= NUM_SOLENOIDS; solenoid_number++) {
    int index = solenoid_number - 1;
    pinMode(solenoidPins[index], OUTPUT);
    digitalWrite(solenoidPins[index], LOW);
  }

  // Hold LoRa in reset long enough to clear any partial-power state.
  pinMode(rst, OUTPUT);
  digitalWrite(rst, LOW);
  delay(20);
  digitalWrite(rst, HIGH);
  delay(50);

  LoRa.setPins(ss, rst, dio0);

  const int maxLoraAttempts = 20; // ~10s at 500ms
  int loraAttempts = 0;
  while (!LoRa.begin(915E6)) {
    Serial.print(".");
    delay(500);
    if (++loraAttempts >= maxLoraAttempts) {
      Serial.print("LoRa init failed, restarting\r\n");
      delay(50);
      ESP.restart();
    }
  }
  Serial.print("LoRa Initializing OK!\r\n");

  // Only broadcast startup state on clean boots so brown-out loops
  // don't spam the gateway with OFF messages.
  bool cleanBoot = (reason == ESP_RST_POWERON
                 || reason == ESP_RST_EXT
                 || reason == ESP_RST_SW);
  if (cleanBoot) {
    for (int solenoid_number = 1; solenoid_number <= NUM_SOLENOIDS; solenoid_number++) {
      loRaSendMessage("n" + String(NODE_NUMBER) + "s" + String(solenoid_number) + "off");
    }
  }
}

void loop() {
  esp_task_wdt_reset();

  // Handle messages from gateway
  String receivedMessage = "";
  int rssi;
  int errorsCorrected;
  for (int solenoid_number = 1; solenoid_number <= NUM_SOLENOIDS; solenoid_number++) {
    int index = solenoid_number - 1;
    if (offTimes[index] != 0 && millis() > offTimes[index]) {
      digitalWrite(solenoidPins[index], LOW);
      loRaSendMessage("n" + String(NODE_NUMBER) + "s" + String(solenoid_number) + "off");
      offTimes[index] = 0;
      Serial.print("Watering duration complete: Turning off " + String(solenoid_number) + "\r\n");
    };
  };
  if (loRaReceiveMessage(receivedMessage, rssi, errorsCorrected)) {
    Serial.print("Node '" + String(NODE_NUMBER) + "': "
      + "Received LoRa packet: '" + receivedMessage + "' "
      + "with RSSI " + String(rssi) + " "
      + "and " + String(errorsCorrected) + " errors corrected\r\n");

    if (!parseMessage(receivedMessage, receivedAction)) {
      String messageToSend = "Node '" + String(NODE_NUMBER) + "': "
        + "Error parsing message: '" + receivedMessage + "'";
      Serial.print(messageToSend + "\r\n");
    } else {
      int index = receivedAction.solenoidNumber - 1;
      int pin = solenoidPins[index];
      if (receivedAction.onOrOff) {
        digitalWrite(pin, HIGH);
        String messageToSend = "n" + String(NODE_NUMBER) + "s" + String(receivedAction.solenoidNumber) + "on";
        Serial.print(messageToSend + "\r\n");
        if (!loRaSendMessage(messageToSend)) {
          Serial.print("Failed to send message: '" + messageToSend + "'\r\n");
        }
        offTimes[index] = millis() + receivedAction.secondsOn * 1000UL;
        Serial.print("millis(): " + String(millis()) + ", setting offTimes[" + String(index) + " to : " + String(offTimes[index]) + "\r\n");
      } else {
        digitalWrite(pin, LOW);
        String messageToSend = "n" + String(NODE_NUMBER) + "s" + String(receivedAction.solenoidNumber) + "off";
        Serial.print(messageToSend + "\r\n");
        if (!loRaSendMessage(messageToSend)) {
          Serial.print("Failed to send message: '" + messageToSend + "'\r\n");
        };
        offTimes[index] = 0;
      };
    };
  };
}

bool parseMessage(String message, Action& result) {
  // 012345678
  // n2s3on 20
  // n2s3off

  // check message validity
  if (message.substring(0, 1) != "n"
    || message.substring(1, 2).toInt() == 0
    || message.substring(2, 3) != "s"
    || message.substring(3, 4).toInt() == 0
    || message.substring(4, 5) != "o"
    || (message.substring(5, 6) != "n" && message.substring(5, 6) != "f")) {
    return false;
  };

  result.nodeNumber = message.substring(1, 2).toInt();
  result.solenoidNumber = message.substring(3, 4).toInt();
  String onOrOffStr = message.substring(5, 6);
  if (onOrOffStr == "n") {
    result.onOrOff = true;
  } else if (onOrOffStr == "f") {
    result.onOrOff = false;
  } else {
    return false;
  };
  result.secondsOn = message.substring(7).toInt();
  return true;
}

// Hamming-encode the contents of the message
bool encode(byte message[], int length, byte outBuffer[]) {
  for (int i = 0; i < length; i++) {
    hamming.fastEncode(message[i], &outBuffer[i*2]);
  };
  return true;
}

byte messageBuffer[100];
byte encodedBuffer[200];

bool loRaSendMessage(String message) {
  message.getBytes(messageBuffer, 100);
  if (!encode(messageBuffer, message.length(), encodedBuffer)) {
    return false;
  }
  if (!LoRa.beginPacket()) {
    return false;
  };
  for (int i = 0; i < message.length(); i++) {
    LoRa.write(encodedBuffer[i*2]);
    LoRa.write(encodedBuffer[i*2 + 1]);
  };
  if (!LoRa.endPacket()) {
    return false;
  };
  return true;
}

// Hamming-decode the contents of the buffer
bool decode(byte buffer[], int bufferLength, byte result[], int& errorsCorrected) {
  errorsCorrected = 0;
  byte decoded;
  for (int i = 0; i < bufferLength/2; i++) {
    if (hamming.decode(&buffer[i*2], &decoded)) {
      if (hamming.errorCorrected) {
        errorsCorrected++;
      };
      result[i] = decoded;
    } else {
      return false;
    }
  };
  result[bufferLength/2] = 0;
  return true;
}

byte receiveBuffer[200];
byte decodedBuffer[100];

bool loRaReceiveMessage(String& message, int& rssi, int& errorsCorrected) {
  int packetSize = LoRa.parsePacket();
  if (packetSize) {
    int index = 0;
    while (LoRa.available()) {
      receiveBuffer[index] = LoRa.read();
      index++;
    };
    if (!decode(receiveBuffer, index, decodedBuffer, errorsCorrected)) {
      return false;
    };
    rssi = LoRa.packetRssi();
    message = String((char*)decodedBuffer);
  } else {
      return false;
  };
  return true;
}
