#include <Arduino.h>
#include "UserInterface.h"

UserInterface::UserInterface()
	: m_last(KeyNone)
	, m_item(Film)
	, m_format(F24X36)
	, m_frames(6)
	, m_shutter(128)
	, m_intensity(1024)
	, m_light(false)
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

void UserInterface::processInput(LCDKeyPad& lcdkp, unsigned long now)
{
	Key key = lcdkp.read(now);

	if (key == m_last)
		return;

	switch (key)
	{
	case KeyLeft:
		m_item = static_cast<Item>((static_cast<int>(m_item) + 1) % static_cast<int>(LastItem));
		break;
	case KeyRight:
		m_light = !m_light;
		break;
	case KeyUp:
		switch (m_item)
		{
			case Film:
				m_format = static_cast<Format>(min(static_cast<int>(m_format) + 1, static_cast<int>(LastFormat) - 1));
				break;
			case Frames:
				m_frames = m_frames < max_frames ? m_frames + 1 : m_frames;
				break;
			case Shutter:
				m_shutter = m_shutter < max_shutter ? m_shutter * 2 : m_shutter;
				break;
			case Intensity:
				m_intensity = m_intensity < max_intensity ? m_intensity * 2 : m_intensity;
				break;
			default: ;
		}
		break;
	case KeyDown:
		switch (m_item)
		{
			case Film:
				m_format = static_cast<Format>(max(static_cast<int>(m_format) - 1, 0));
				break;
			case Frames:
				m_frames = m_frames > 1 ? m_frames - 1 : m_frames;
				break;
			case Shutter:
				m_shutter = m_shutter > 1 ? m_shutter / 2 : m_shutter;
				break;
			case Intensity:
				m_intensity = m_intensity > 1 ? m_intensity / 2 : m_intensity;
				break;
			default: ;
		}
		break;
	default:;
	}

	m_last = key;
	refresh(lcdkp.lcd());
}

static const char *toString(Format f)
{
	switch (f)
	{
		case F18X24: return "18 X 24";
		case F24X24: return "24 X 24";
		case F24X36: return "24 X 36";
		case F6X45: return "6 X 4.5";
		case F6X6: return "6 X 6";
		case F6X7: return "6 X 7";
		case F6X8: return "6 X 8";
		case F6X9: return "6 X 9";
		default: return "?";
	}
}

void UserInterface::refresh(LiquidCrystal& lcd)
{
	lcd.setCursor(0,0);
	switch (m_item)
	{
		case Film:		lcd.print("Format   "); break;
		case Frames:	lcd.print("Frames   "); break;
		case Shutter:	lcd.print("Shutter  "); break;
		case Intensity:	lcd.print("Intensity"); break;
		default:		lcd.print("?"); break;
	}
   	lcd.setCursor(0,1);
	lcd.print(" =         ");
   	lcd.setCursor(3,1);
	switch (m_item)
	{
		case Film:		lcd.print(toString(m_format)); break;
		case Frames:	lcd.print(m_frames); break;
		case Shutter:	lcd.print(m_shutter); break;
		case Intensity:	lcd.print(m_intensity); break;
		default:		lcd.print("?"); break;
	}
}

