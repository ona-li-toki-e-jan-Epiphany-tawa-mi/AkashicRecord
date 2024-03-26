#include <Arduino.h>

unsigned int speakerOut = 3;

float notes[] = {
    50, 0.25,
    100, 0.25,
    200, 0.75,
    0, 0.5,
    100, 0.25,
    150, 0.25,
    250, 0.75,
    0, 0.5,
    100, 0.125,
    50, 0.25,
    0, 0.5
};
unsigned int notePointer = 0;
unsigned int musicSize = sizeof(notes) / sizeof(notes[0]);

void setup() {
  pinMode(speakerOut, OUTPUT); 

  for (unsigned int i = 1; i < musicSize; i += 2) 
    notes[i] *= 1000;
}

void loop() {
  noTone(speakerOut); 

  if (notes[notePointer] != 0)
    tone(speakerOut, (unsigned int) notes[notePointer]);

  delay((unsigned long) notes[notePointer + 1]);

  notePointer += 2;
  if (notePointer >= musicSize) 
    notePointer = 0;
}
