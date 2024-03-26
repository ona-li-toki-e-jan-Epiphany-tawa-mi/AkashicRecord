/**
 * Я создал эту цепь и программу, чтобы сделать пранк на мою школу (при создании ещё не происходил.)
 * При пранке поставлю 9 из этих на нескольких местах по школой, где слишком трудно выискать.
 * 
 * Цепь - это просто Arduino Nano с зуммером на D3 и GND и разъёмом к 9-вольтным батареями на Vin и GND.
 * Подключивайте батарею к разъёму, и всё будет отлично)))
 */

#define VYVOD_ZUMMYERA 3

void setup() {
  pinMode(VYVOD_ZUMMYERA, OUTPUT);
  
  // Короткая песня, чтобы знать, что действо включено.
  tone(VYVOD_ZUMMYERA, (unsigned) 1000);
  delay(100);
  tone(VYVOD_ZUMMYERA, (unsigned) 1500);
  delay(150);
  tone(VYVOD_ZUMMYERA, (unsigned) 2000);
  delay(100);
  tone(VYVOD_ZUMMYERA, (unsigned) 1500);
  delay(150);
  tone(VYVOD_ZUMMYERA, (unsigned) 200, 300);
  delay(300);
}


/**
 * Ждёт по данное время.
 * Внимание: время делится на 256, поэтому может быть небольшие значения - неверные.
 * 
 * @param vryemyaOzhidaniya Время, по которое ждать.
 */
void zhdat(long vryemyaOzhidaniya) {
  // Готовит часы, и потом их замедляет в 256 раз.
  // Подробнее: http://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-7810-Automotive-Microcontrollers-ATmega328P_Datasheet.pdf - 8.11 & 8.12.
  CLKPR = 0b10000000;
  CLKPR = 0b00001000;

  delay(vryemyaOzhidaniya / 256);

  // Готовит часы, и потом сбрасывает их скорость.
  CLKPR = 0b10000000;
  CLKPR = 0b00000000;
}

void loop() {
  // Ждёт где-нибудь от 20 минут до 1,2 часов.
  zhdat(random(1200000, 4800000));
  
  unsigned long vryemyaZvuka = random(20, 500);
  tone(VYVOD_ZUMMYERA, random(2000, 5000), vryemyaZvuka);
  delay(vryemyaZvuka);
}
