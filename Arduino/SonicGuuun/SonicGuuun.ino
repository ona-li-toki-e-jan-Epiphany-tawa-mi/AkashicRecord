int button = 7;
int speakers = 2;
boolean tonePlaying = false;
int frequency = 73;

boolean isPresssed(int pin);

void setup() {
  pinMode(button, INPUT_PULLUP);
  digitalWrite(button, HIGH);

  pinMode(speakers, OUTPUT);
}

void loop() {
  if (isPresssed(button)) {
    if (!tonePlaying) {
      tone(speakers, frequency);
      tonePlaying = true;
    } else {
      noTone(speakers);
      tonePlaying = false;
    }
  }
}

boolean isPresssed(int pin) {
  return digitalRead(pin) == LOW;
}
