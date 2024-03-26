// Used for handling incoming instructions
char inputString[200];       
char safeCpyString[sizeof(inputString) / sizeof(inputString[0])];     
bool stringComplete = false;

double droneHorizontalSpeed = 0;
double droneVerticalSpeed = 0;
float droneFacingAngle = 0; //TODO change to a gyroscope? or get from controller
float droneDesiredFacingAngle = droneFacingAngle;

float stof(char* stringNum);
double stod(char* stringNum);

void setup() {
  Serial.begin(9600);
}

void loop() {
  delay(1);
  
  if (stringComplete) {
    stringComplete = false;
    
    Serial.println(inputString);

    strcpy(safeCpyString, inputString);
    
    Serial.println(inputString);
    char *token;
    char *tmpStore;
    
    token = strtok_r(safeCpyString, ' ', &tmpStore);
    if (strcmp("s!⸮t!⸮p!⸮", token) == 0) { // stop, can use "all" as argument to halt the drone, usage: stp <method> 
      token = strtok_r(NULL, ' ', &tmpStore);

      if (token == NULL) {
        strcat(inputString, " <--HERE");
        printInputError("not enough arguements (stp uses 1)", inputString);
      }
    } else if (strcmp("stsph", token) == 0) { // set speed horizontal, usage: stsph <%speed>
      token = strtok_r(NULL, ' ', &tmpStore);

      if (token == NULL) {
        strcat(inputString, " <--HERE");
        printInputError("not enough arguements (stsph uses 1)", inputString);
      }
    } else if (strcmp("stspv", token) == 0) { // set speed vertical, usage: stspv <%speed>
      token = strtok_r(NULL, ' ', &tmpStore);
      
      if (token == NULL) {
        strcat(inputString, " <--HERE");
        printInputError("not enough arguements (stspv uses 1)", inputString);
      }
    } else if (strcmp("stda", token) == 0) { // set desired angle, usage: stda <angle>
      token = strtok_r(NULL, ' ', &tmpStore);
      
      if (token == NULL) {
        strcat(inputString, " <--HERE");
        printInputError("not enough arguements (stda uses 1)", inputString);
      }
    } else if (strcmp("chaby", token) == 0) { // changle angle by, usage: chaby <angleDelta>
      token = strtok_r(NULL, ' ', &tmpStore);
      
      if (token == NULL) {
        strcat(inputString, " <--HERE");
        printInputError("not enough arguements (chaby uses 1)", inputString);
      }
    } else {
      printInputError("unknown method call", inputString);
    }
    
    Serial.println(inputString);
    memset(safeCpyString, 0, sizeof(safeCpyString));
    memset(inputString, 0, sizeof(inputString));
  }
}

void serialEvent() {
  char inChar[1];
  
  while (Serial.available()) {
    inChar[0] = (char)Serial.read();
    
    if (inChar[0] == '\n') {
      stringComplete = true;
    } else {
      strcat(inputString, inChar);
    }
  }
}

float stof(char* stringNum) {
  // TODO have it parse strings as floats
}

double stod(char* stringNum) {
  // TODO have it parse strings as doubles
}

void printInputError(char errMsg[], char errdCode[]) {
  Serial.print("err : ");
  Serial.print(errMsg);
  Serial.print(" : ");
  Serial.println(errdCode);
}
