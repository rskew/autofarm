// References
// - https://raw.githubusercontent.com/RuiSantosdotme/Random-Nerd-Tutorials/master/Projects/ESP32/LoRaSender.ino
// - https://raw.githubusercontent.com/RuiSantosdotme/Random-Nerd-Tutorials/master/Projects/ESP32/LoRaReceiver.ino
// - https://github.com/LowPowerLab/RFM69/blob/master/Examples/Gateway/Gateway.ino
// - https://www.airspayce.com/mikem/arduino/RadioHead/rf95_reliable_datagram_client_8ino-example.html
// - https://www.airspayce.com/mikem/arduino/RadioHead/rf95_reliable_datagram_server_8ino-example.html
#include <SPI.h>
#include <LoRa.h>

#include <Hamming.h>
HammingCode hamming;

// Pins used by the transceiver module
#define ss 5
#define rst 14
#define dio0 2


struct Action {
    String nodeName;
    String onOff;
    int minutesOn;
};

void setup() {
  Serial.begin(115200);
  while (!Serial);
  Serial.print("LoRa Gateway\r\n");

  LoRa.setPins(ss, rst, dio0);
  
  while (!LoRa.begin(915E6)) {
    Serial.print(".");
    delay(500);
  };
  Serial.print("Gateway: LoRa Initializing OK!\r\n");
}

void loop() {
  // Handle messages from nodes
  String loRaMessage = "";
  int rssi;
  int errorsCorrected;
  if (loRaReceiveMessage(loRaMessage, rssi, errorsCorrected)) {
    Serial.print("Gateway: Received LoRa packet '" + loRaMessage + "' "
      + "with RSSI " + rssi + " "
      + "and " + errorsCorrected + " errors corrected\r\n");
  };

  // Handle messages from server
  if (Serial.available() > 0)
  {
    String serverMessage = Serial.readStringUntil('\r');
    if (Serial.peek() == '\n') {
      Serial.read();
    }
    Action receivedAction = {};
    if (!parseMessage(serverMessage, receivedAction)) {
      Serial.print("Gateway: Error parsing server message: '" + serverMessage + "'\r\n");
    } else {
      Serial.print("Gateway: Yay parsed server message and sent to node: '" + serverMessage + "'\r\n");
      if (!loRaSendMessage(serverMessage)) {
        Serial.print("Gateway: Error sending message '" + serverMessage + "'\r\n");
      }
    };
  }
}

bool parseMessage(String message, Action& result) {
  int firstSpace = message.indexOf(' ');
  if (firstSpace == -1) {
    return false;
  };
  String nodeName = message.substring(0, firstSpace);
  String remainingData = message.substring(firstSpace + 1);

  int secondSpace = remainingData.indexOf(' ');
  String onOff = "";
  if (secondSpace == -1) {
    onOff = remainingData;
  } else {
    onOff = remainingData.substring(0, secondSpace);
  };
  result.nodeName = nodeName;
  result.onOff = onOff;
  if (onOff == "on") {
    if (secondSpace == -1) {
      return false;
    };
    String minutesOnStr = remainingData.substring(secondSpace + 1);
    int minutesOn = minutesOnStr.toInt();
    if (minutesOn == 0) {
      return false;
    }
    result.minutesOn = minutesOn;
  } else if (onOff == "off") {
  };
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
      return "fail";
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
