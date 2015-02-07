#ifndef USER_INTERFACE_H
#define USER_INTERFACE_H

#include "LCDKeyPadShield.h"

enum Format { F18X24, F24X24, F24X36, F6X45, F6X6, F6X7, F6X8, F6X9, LastFormat };

class UserInterface
{
public:
	UserInterface();

	void setup(LCDKeyPad& lcdkp);
	void processInput(LCDKeyPad& lcdkp, unsigned long now);

private:
	void refresh(LiquidCrystal& lcd);

	enum Item { Film, Frames, Shutter, Intensity, Scan, Batch, LastItem };

	static const uint8_t max_frames = 6;
	static const uint16_t max_shutter = 32768;
	static const uint16_t max_intensity = 4096;

	Key			m_last;
	Item		m_item;
	Format		m_format;
	uint8_t		m_frames;
	uint16_t	m_shutter;
	uint16_t	m_intensity;
	bool		m_light;
	uint8_t		m_frame;
};

#endif // USER_INTERFACE_H

