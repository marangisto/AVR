#ifndef USER_INTERFACE_H
#define USER_INTERFACE_H

#include "LCDKeyPadShield.h"

class UserInterface
{
public:
	UserInterface();

	void setup(LCDKeyPad& lcdkp);
	void processInput(LCDKeyPad& lcdkp, unsigned long now);

private:
	void refresh(LiquidCrystal& lcd);

	enum Item { Frames, Shutter, Intensity };

	static const uint8_t max_frames = 6;
	static const uint16_t max_shutter = 32768;
	static const uint16_t max_intensity = 4096;

	Key			m_last;
	Item		m_item;
	uint8_t		m_frames;
	uint16_t	m_shutter;
	uint16_t	m_intensity;
};

#endif // USER_INTERFACE_H

