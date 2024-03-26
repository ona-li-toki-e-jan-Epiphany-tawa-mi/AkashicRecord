const int one = 2; // Sets first binary place value as digital 2
const int two = 3; // Sets second binary place value as digital 3
const int four = 4; // Sets third binary place value as digital 4
const int eight = 5; // Sets fourth binary place value as digital 5 
const int sixteen = 6; // Sets fith binary place value as digital 6
const int soundpin = A3; // Sets the busted sound sensor input on analog 3

int binary; // Used to sum up binary input
int dab; // Used to measure how much air is passing over the busted sound sensor

void setup() {
  Serial.begin(9600); // Needed for communication?

  // Block used to desginate inputs for sound and pressed buttons
  pinMode(soundpin,INPUT);
  pinMode(one,INPUT);
  pinMode(two,INPUT);
  pinMode(four,INPUT);
  pinMode(eight,INPUT);
  pinMode(sixteen,INPUT);

  // Button inputs are set to HIGH by default, if power is allowed to flow to ground it becomes LOW
  digitalWrite(one, HIGH);
  digitalWrite(two, HIGH);
  digitalWrite(four, HIGH);
  digitalWrite(eight, HIGH);
  digitalWrite(sixteen, HIGH);
}

void loop() {
  // Resets sum every loop ran
  binary = 0; 
  
  if (digitalRead(one) == LOW) {
    binary++; // Adds the value of the first binary button
  }
  if (digitalRead(two) == LOW) {
    binary += 2; // Adds the value of the second binary button
  }
  if (digitalRead(four) == LOW) {
    binary += 4; // Adds the value of the third binary button
  }
  if (digitalRead(eight) == LOW) {
    binary += 8; // Adds the value of the fourth binary button
  }
  if (digitalRead(sixteen) == LOW) {
    binary += 16; // Adds the value of the fith binary button
  }
  
  dab = analogRead(soundpin); // Grabs input from busted microphone
  if (dab > 75) { // Threshold for air flow
    tone(13,4978 + (-78.5238 * binary),dab/2); // Oscillates 13th port, finds frequency from binary sum, finds length from rate of air flow
  }
  
  delay(10); // Spaces out notes so they are not too choppy or spread apart
}
