#include <Arduino.h>
#include "LCDKeyPadShield.h"

#define BOARD_VERSION 10

LCDKeyPad::LCDKeyPad(): m_lcd(8, 9, 4, 5, 6, 7)
{
   m_lcd.begin(16, 2);
   m_lcd.setCursor(0,0);
   m_lcd.print("Hello World!");
}

Key LCDKeyPad::read()
{
    int x = analogRead(0);       // read the value from the sensor 

	if (x > 1000) return KeyNone; 
#if BOARD_VERSION < 11				// Version 1.0
	if (x < 50)   return KeyRight;  
	if (x < 195)  return KeyUp;
	if (x < 380)  return KeyDown; 
	if (x < 555)  return KeyLeft; 
	if (x < 790)  return KeySelect;   
#else								// Version 1.1
	if (x < 50)   return KeyRight;  
    if (x < 250)  return KeyUp; 
    if (x < 450)  return KeyDown; 
    if (x < 650)  return KeyLeft; 
    if (x < 850)  return KeySelect;  
#endif
    return KeyNone;
}

