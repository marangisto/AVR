#ifndef LCD_KEYPAD_SHIELD_H
#define LCD_KEYPAD_SHIELD_H

#include <LiquidCrystal.h>

enum Key { KeyNone, KeyLeft, KeyRight, KeyUp, KeyDown, KeySelect };

class LCDKeyPad
{
public:
	LCDKeyPad();
	LiquidCrystal& lcd() { return m_lcd; }
	Key read(unsigned long now);

private:
	static Key read();	// raw unbounced

	LiquidCrystal	m_lcd;
	unsigned long	m_bounce;
	Key				m_last;
	Key				m_key;
};

#endif // LCD_KEYPAD_SHIELD_H

