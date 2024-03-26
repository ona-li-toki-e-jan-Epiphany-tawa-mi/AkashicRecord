// Bitmask to convert decimal time to binary 
const byte hourTime[] = {0b10000, 0b01000, 0b00100, 0b00010, 0b00001};
const byte minuteTime[] = {0b100000, 0b010000, 0b001000, 0b000100, 0b000010, 0b000001};

// LEDs
const unsigned int lightRows[] = {2, 3};
const unsigned int lightColumns[] = {4, 5, 6, 7, 8};
const unsigned int extraLight = 9;

// Need RTC to finish
int timeHrs = NULL; // TODO
int timeMinutes = NULL; // TODO
// Delay between row cycles (milliseconds)
int timeDelay_ms = 10;

void setup() {
  // Sets all LED pins to OUTPUT mode and LOW
  for (byte i = 0; i < sizeof(lightRows) / sizeof(lightRows[0]); i++) {
    pinMode(lightRows[i], OUTPUT);
    digitalWrite(lightRows[i], LOW);
  }
  for (byte i = 0; i < sizeof(lightColumns) / sizeof(lightColumns[0]); i++) {
    pinMode(lightColumns[i], OUTPUT);
    digitalWrite(lightColumns[i], LOW);
  }
  pinMode(extraLight, OUTPUT);
  digitalWrite(extraLight, LOW);
}

void loop() {
  // Top row LEDs activation
  digitalWrite(lightRows[0], HIGH);
  digitalWrite(lightRows[1], LOW);

  for (byte i = 0; i < sizeof(lightColumns) / sizeof(lightColumns[0]); i++) {
    if (timeHrs & hourTime[i] > 0) {
      digitalWrite(lightColumns[i], HIGH);
    }
  }

  delay(timeDelay_ms);

  // Top row LEDs reset
  for (byte i = 0; i < sizeof(lightColumns) / sizeof(lightColumns[0]); i++) {
    digitalWrite(lightColumns[i], LOW);
  }

  
  // Bottom row LED activation
  digitalWrite(lightRows[0], LOW);
  digitalWrite(lightRows[1], HIGH);
  
  for (byte i = 0; i < sizeof(lightRows) / sizeof(lightRows[0]); i++) {
    if (timeMinutes & minuteTime[i] > 0) {
      digitalWrite(lightRows[i], HIGH);
    }
  } 
  if (timeMinutes & minuteTime[6] > 0) {
    digitalWrite(extraLight, HIGH);
  }

  delay(timeDelay_ms);

  // Bottom row LED reset
  for (byte i = 0; i < sizeof(lightRows) / sizeof(lightRows[0]); i++) {
    digitalWrite(lightRows[i], LOW);
  }
  digitalWrite(extraLight, LOW);
}
