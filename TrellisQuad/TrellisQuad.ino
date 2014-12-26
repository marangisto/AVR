#include <Wire.h>
#include "Adafruit_Trellis.h"

class TrellisQuad
{
public:
	TrellisQuad(): m_trellis(&m_matrix[0], &m_matrix[1], &m_matrix[2], &m_matrix[3])
	{
	}

	void begin ()
	{
		m_trellis.begin(0x70, 0x71, 0x72, 0x73);
		for (uint8_t i=0; i<64; i++)
			m_trellis.setLED(i);
		m_trellis.writeDisplay();
		for (uint8_t i=0; i<64; i++)
			m_trellis.clrLED(i);
		m_trellis.writeDisplay();
	}

	void setLed(uint8_t r, uint8_t c, bool b = true)
	{
		if (b)
			m_trellis.setLED(index(r, c));
		else
			m_trellis.clrLED(index(r, c));
	}

	void clrLed(uint8_t r, uint8_t c)
	{
		m_trellis.clrLED(index(r, c));
	}

	void writeDisplay()
	{
		m_trellis.writeDisplay();
	}

private:
	TrellisQuad(const TrellisQuad&);

	uint8_t index(uint8_t r, uint8_t c) const
	{
		if (r < 8 && c < 8)
		{
			return m_remap[r * 8 + c];
		} else
			return 0;
	}

	Adafruit_Trellis		m_matrix[4];
	Adafruit_TrellisSet		m_trellis;
	static const uint8_t	m_remap[64];
};

const uint8_t TrellisQuad::m_remap[] =
	{  0,  1,  2,  3
	, 16, 17, 18, 19
	,  4,  5,  6,  7
	, 20, 21, 22, 23
	,  8,  9, 10, 11
	, 24, 25, 26, 27
	, 12, 13, 14, 15
	, 28, 29, 30, 31
	, 32, 33, 34, 35
	, 48, 49, 50, 51
	, 36, 37, 38, 39
	, 52, 53, 54, 55
	, 40, 41, 42, 43
	, 56, 57, 58, 59
	, 44, 45, 46, 47
	, 60, 61, 62, 63
	};

static TrellisQuad g_trellis;

void setup()
{
	Serial.begin(9600);
	Serial.println("Trellis Demo");
	g_trellis.begin();
}

void loop() {
	static int r = 0, c = 0;
	static bool b = true;

	g_trellis.setLed(r, c, b);

	if (++c > 7)
	{
		if (++r > 7)
		{
			r = 0;
			b = !b;
		}
		c = 0;
	}
	
	g_trellis.writeDisplay();
}

