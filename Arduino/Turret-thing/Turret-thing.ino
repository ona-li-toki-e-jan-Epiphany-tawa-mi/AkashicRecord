#include <Servo.h>

int xWay = 0; // Stores scaled joystick reading.
int yWay = 0; // Stores scaled joystick reading.

Servo xServ; // Servo object for x plane.
Servo yServ; // Servo object for y plane.

void setup (){
  xServ.attach(9); // Enables Arduino to use pin 9 to interact with a servo.
  yServ.attach(10); // Enables Arduino to use pin 9 to interact with a servo.
}

void loop(){

  xWay = ceil(analogRead(A0)/5.7); // Reads input from joystick and scales it from 0-1023 to 0-180 for the servos.
  yWay = ceil(analogRead(A1)/5.7); // Reads input from joystick and scales it from 0-1023 to 0-180 for the servos.
  
  xServ.write(xWay); // Sends position data to the servos.
  yServ.write(yWay); // Sends position data to the servos.
  
  delay(30); // Arduino gets tired too, breaks are needed sometimes.
}
