#include <EEPROMex.h>
#include <Keypad.h>
#include <IRremote.h>

const char keys[][4] = {
  {'1', '2', '3', 'A'},
  {'4', '5', '6', 'B'},
  {'7', '8', '9', 'C'},
  {'*', '0', '#', 'D'} 
};
const byte rows[] = {13, 12, 11, 10};
const byte colums[] = {9, 8, 7, 6};
Keypad kkeypdd = Keypad(makeKeymap(keys), rows, colums, 4, 4);

IRsend irsend; // Use 3.3v or limit to 20mA
IRrecv irrecv(5); // Use 3.3v
decode_results results;

int baseAddres(char kyyyy);
byte encoding2Byte(decode_type_t encodingType);
decode_type_t byte2encoding(byte intRep);

void setup() {
 Serial.begin(9600);
 irrecv.enableIRIn();  
}

bool buttonPressed = false;

void loop() {
  char key = kkeypdd.getKey();
  
  if (key != 0) {
    if (key == '*' && buttonPressed == false) {while (true){
      key = kkeypdd.getKey();
      
      if (irrecv.decode(&results)) {
        if (!results.overflow && results.decode_type != UNKNOWN && results.decode_type != UNUSED) {while (true){
          key = kkeypdd.getKey();

          if (key != 0 && key != '*' && key != 'A' && key != 'B' && key != 'D' && key != '#') {
            if (key != 'C') {
              saveIR2Mem(key, results);}

            delay(1000);
            goto irFound;   
          }
        }} else {
          goto irFound;}
      } else if (key == 'C') {
        goto irFound;}
    }} else if ((key == '1' || key == '2' || key == '3' || key == '4' || key == '5' || key == '6' || key == '7' || key == '8' || key == '9' || key == '0')
                && buttonPressed == false) {
       loadIRFrMem(key);
    }

    irFound:
    buttonPressed = true;
  } else {
    buttonPressed = false;}
    
  irrecv.resume();  
}

void saveIR2Mem(char keyP, decode_results rezults) {
  int adress = baseAddres(keyP);

  EEPROM.isReady();

  EEPROM.updateByte(adress, encoding2Byte(rezults.decode_type));
  adress += sizeof(byte);
  
  EEPROM.updateInt(adress, rezults.bits);
  adress += sizeof(int);
  
  if (rezults.decode_type == PANASONIC) {
    EEPROM.updateInt(adress, rezults.address);}
  adress += sizeof(int);  
  
  EEPROM.updateLong(adress, rezults.value);
}

void loadIRFrMem(char keyP) {
  int adress = baseAddres(keyP);
  
  EEPROM.isReady();

  if (isnan(EEPROM.readByte(adress))) {
    return;}
  decode_type_t irType = byte2encoding(EEPROM.readByte(adress));
  adress += sizeof(byte);
  
  if (isnan(EEPROM.readInt(adress))) {
    return;}
  int bitz = EEPROM.readInt(adress);
  adress += sizeof(int);  

  int addrez = -1;
  if (irType == PANASONIC) {
    if (isnan(EEPROM.readInt(adress))) {
      return;}
    addrez = EEPROM.readInt(adress);
  }
  adress += sizeof(int);  

  if (isnan(EEPROM.readLong(adress))) {
    return;}
  long valur = EEPROM.readLong(adress);  

  if (irType == UNKNOWN || irType == UNUSED) {
    return;
  } else if (irType == RC5) {
    irsend.sendRC5(valur, bitz);
  } else if (irType == RC6) {
    irsend.sendRC6(valur, bitz);
  } else if (irType == NEC) {
    irsend.sendNEC(valur, bitz);
  } else if (irType == SONY) {
    irsend.sendSony(valur, bitz);
  } else if (irType == PANASONIC) {
    irsend.sendPanasonic(addrez, valur);
  } else if (irType == JVC) {
    irsend.sendJVC(valur, bitz, false);
  } else if (irType == SAMSUNG) {
    irsend.sendSAMSUNG(valur, bitz);
  } else if (irType == WHYNTER) {
    irsend.sendWhynter(valur, bitz);
  } else if (irType == AIWA_RC_T501) {
    irsend.sendAiwaRCT501(valur);
  } else if (irType == LG) {
    irsend.sendLG(valur, bitz);
  } else if (irType == DENON) {
    irsend.sendDenon(valur, bitz);
  }

  delay(100);
}

int baseAddres(char kyyyy) { // 1 byte, 2 ints, & a long
  int bse = 0;

  if (kyyyy == '2') {
    bse = sizeof(byte) + (sizeof(int) * 2) + sizeof(long);
  } else if (kyyyy == '3') {
    bse = (sizeof(byte) + (sizeof(int) * 2) + sizeof(long)) * 2;
  } else if (kyyyy == '4') {
    bse = (sizeof(byte) + (sizeof(int) * 2) + sizeof(long)) * 3;
  } else if (kyyyy == '5') {
    bse = (sizeof(byte) + (sizeof(int) * 2) + sizeof(long)) * 4;
  } else if (kyyyy == '6') {
    bse = (sizeof(byte) + (sizeof(int) * 2) + sizeof(long)) * 5;
  } else if (kyyyy == '7') {
    bse = (sizeof(byte) + (sizeof(int) * 2) + sizeof(long)) * 6;
  } else if (kyyyy == '8') {
    bse = (sizeof(byte) + (sizeof(int) * 2) + sizeof(long)) * 7;
  } else if (kyyyy == '9') {
    bse = (sizeof(byte) + (sizeof(int) * 2) + sizeof(long)) * 8;
  } else if (kyyyy == '0') {
    bse = (sizeof(byte) + (sizeof(int) * 2) + sizeof(long)) * 9;}

  return bse;  
}

byte encoding2Byte(decode_type_t encodingType) {
  byte intVer;
  
  switch (encodingType) {
    case UNKNOWN: 
      intVer = 99;
      break;
    case RC5: 
      intVer = 1;
      break;
    case RC6: 
      intVer = 2;
      break;
    case NEC:
      intVer = 3;
      break;
    case SONY: 
      intVer = 4;
      break;
    case PANASONIC: 
      intVer = 5;
      break;
    case JVC: 
      intVer = 6;
      break;  
    case SAMSUNG: 
      intVer = 7;
      break;
    case WHYNTER: 
      intVer = 8;
      break;
    case AIWA_RC_T501: 
      intVer = 9;
      break;
    case LG: 
      intVer = 10;
      break;
    case DENON: 
      intVer = 11;
      break;
    default: 
      intVer = 0;    
  }

  return intVer;
}

decode_type_t byte2encoding(byte intRep) {
  decode_type_t encoding;
  
  switch (intRep) {
    case 99: 
      encoding = UNKNOWN;
      break;
    case 1: 
      encoding = RC5;
      break;
    case 2: 
      encoding = RC6;
      break;
    case 3:
      encoding = NEC;
      break;
    case 4: 
      encoding = SONY;
      break;
    case 5: 
      encoding = PANASONIC;
      break;
    case 6: 
      encoding = JVC;
      break;  
    case 7: 
      encoding = SAMSUNG;
      break;
    case 8: 
      encoding = WHYNTER;
      break;
    case 9: 
      encoding = AIWA_RC_T501;
      break;
    case 10: 
      encoding = LG;
      break;
    case 11: 
      encoding = DENON;
      break;
    default: 
      encoding = UNUSED;    
  }

  return encoding;
}
