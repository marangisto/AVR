#ifndef TRELLIS_QUAD_H
#define TRELLIS_QUAD_H

class TrellisQuad
{
public:
    static void begin();
    static void setLed(uint8_t r, uint8_t c, bool b = true);
    static void clrLed(uint8_t r, uint8_t c);
    static void drawChar(uint8_t r0, uint8_t c0, char x);
    static void drawNum(uint8_t r0, uint8_t c0, int8_t i);
    static void writeDisplay();
};

#endif

