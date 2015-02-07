#include <Arduino.h>
#include "UserInterface.h"

UserInterface::UserInterface()
	: m_last(KeyNone)
	, m_item(Frames)
	, m_frames(6)
	, m_shutter(128)
	, m_intensity(1024)
{
}

void UserInterface::setup(LCDKeyPad& lcdkp)
{
	LiquidCrystal& lcd(lcdkp.lcd());

	delay(1000);
	lcd.clear();
	lcd.print("Marangisto Scan");
	lcd.setCursor(0, 1);
	lcd.print("Firmware V0.1");
	delay(1000);
	lcd.clear();
	refresh(lcd);
}

void UserInterface::processInput(LCDKeyPad& lcdkp)
{
	Key key = lcdkp.read();

	if (key == m_last)
		return;

	switch (key)
	{
	case KeyLeft:
		switch (m_item)
		{
			case Frames: m_item = Intensity; break;
			case Shutter: m_item = Frames; break;
			case Intensity: m_item = Shutter; break;
		}
		break;
	case KeyRight:
		switch (m_item)
		{
			case Frames: m_item = Shutter; break;
			case Shutter: m_item = Intensity; break;
			case Intensity: m_item = Frames; break;
		}
		break;
	case KeyUp:
		switch (m_item)
		{
			case Frames:
				m_frames = m_frames < max_frames ? m_frames + 1 : m_frames;
				break;
			case Shutter:
				m_shutter = m_shutter < max_shutter ? m_shutter * 2 : m_shutter;
				break;
			case Intensity:
				m_intensity = m_intensity < max_intensity ? m_intensity * 2 : m_intensity;
				break;
		}
		break;
	case KeyDown:
		switch (m_item)
		{
			case Frames:
				m_frames = m_frames > 1 ? m_frames - 1 : m_frames;
				break;
			case Shutter:
				m_shutter = m_shutter > 1 ? m_shutter / 2 : m_shutter;
				break;
			case Intensity:
				m_intensity = m_intensity > 1 ? m_intensity / 2 : m_intensity;
				break;
		}
		break;
	default:;
	}

	m_last = key;
	refresh(lcdkp.lcd());
}

void UserInterface::refresh(LiquidCrystal& lcd)
{
	lcd.setCursor(0,0);
	switch (m_item)
	{
		case Frames:	lcd.print("Frames   "); break;
		case Shutter:	lcd.print("Shutter  "); break;
		case Intensity:	lcd.print("Intensity"); break;
	}
   	lcd.setCursor(0,1);
	lcd.print(" =         ");
   	lcd.setCursor(3,1);
	switch (m_item)
	{
		case Frames:	lcd.print(m_frames); break;
		case Shutter:	lcd.print(m_shutter); break;
		case Intensity:	lcd.print(m_intensity); break;
	}
}

