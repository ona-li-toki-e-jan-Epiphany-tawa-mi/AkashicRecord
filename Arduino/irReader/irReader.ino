#include <IRremote.h>

IRrecv irrecv(5); // Use 3.3v

decode_results results;

void setup() {
 Serial.begin(9600);
 
 irrecv.enableIRIn(); 
 
}

void loop() {
  if (irrecv.decode(&results)) {
    Serial.println(results.value, HEX);
    switch (results.decode_type) {
    default:
      case UNKNOWN:      Serial.print("UNKNOWN");       break ;
      case NEC:          Serial.print("NEC");           break ;
      case SONY:         Serial.print("SONY");          break ;
      case RC5:          Serial.print("RC5");           break ;
      case RC6:          Serial.print("RC6");           break ;
      case DISH:         Serial.print("DISH");          break ;
      case SHARP:        Serial.print("SHARP");         break ;
      case JVC:          Serial.print("JVC");           break ;
      case SANYO:        Serial.print("SANYO");         break ;
      case MITSUBISHI:   Serial.print("MITSUBISHI");    break ;
      case SAMSUNG:      Serial.print("SAMSUNG");       break ;
      case LG:           Serial.print("LG");            break ;
      case WHYNTER:      Serial.print("WHYNTER");       break ;
      case AIWA_RC_T501: Serial.print("AIWA_RC_T501");  break ;
      case PANASONIC:    Serial.print("PANASONIC");     break ;
      case DENON:        Serial.print("Denon");         break ;
    }
    Serial.println(results.bits, DEC);
    Serial.println(results.address, DEC);
    Serial.println("");
    irrecv.resume();
  }
}
