#ifndef LCD_KEYPAD_SHIELD_H
#define LCD_KEYPAD_SHIELD_H

#include <LiquidCrystal.h>

enum Key { KeyNone, KeyLeft, KeyRight, KeyUp, KeyDown, KeySelect };

class LCDKeyPad
{
public:
	LCDKeyPad();
	LiquidCrystal& lcd() { return m_lcd; }
	static Key read();

private:
	LiquidCrystal	m_lcd;
};

#endif // LCD_KEYPAD_SHIELD_H

