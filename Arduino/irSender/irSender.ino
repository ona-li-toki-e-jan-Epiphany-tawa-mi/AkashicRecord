#include <IRremote.h>

IRsend irsend; // Use 3.3v or limit to 20mA

void setup() {
  pinMode(6, INPUT_PULLUP);
}

bool prevButnState = HIGH;

void loop() {
  if (digitalRead(6) == LOW && prevButnState == HIGH) {
    irsend.sendNEC(0x574332CD, 32); 
    delay(1000);
  }

  prevButnState = digitalRead(6);
}
