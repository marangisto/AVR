#include "WProgram.h"

static uint8_t LED = 13;

extern "C"
void setup()
{
    pinMode(LED, OUTPUT);
}

extern "C"
void loop()
{
    digitalWriteFast(LED, HIGH);
    delay(50);
    digitalWriteFast(LED, LOW);
    delay(500);
}

