#include <WProgram.h>
#include "TrellisQuad.h"
#include <Adafruit_Trellis.h>

struct Internal
{
	Internal(): m_trellis(&m_matrix[0], &m_matrix[1], &m_matrix[2], &m_matrix[3]) {}
	Internal(const Internal&);

	void begin()
	{
		m_trellis.begin(0x70, 0x71, 0x72, 0x73);
		for (uint8_t i=0; i<64; i++)
			m_trellis.setLED(i);
		m_trellis.writeDisplay();
		for (uint8_t i=0; i<64; i++)
			m_trellis.clrLED(i);
		m_trellis.writeDisplay();
	}

	Adafruit_Trellis		m_matrix[4];
	Adafruit_TrellisSet		m_trellis;
};

static Internal g_internal;

void TrellisQuad::begin()
{
	g_internal.begin();
}

static const uint8_t g_remap[] =
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

static uint8_t index(uint8_t r, uint8_t c)
{
	if (r < 8 && c < 8)
	{
		return g_remap[r * 8 + c];
	} else
		return 0;
}

void TrellisQuad::setLed(uint8_t r, uint8_t c, bool b)
{
	if (b)
		g_internal.m_trellis.setLED(index(r, c));
	else
		g_internal.m_trellis.clrLED(index(r, c));
}

void TrellisQuad::clrLed(uint8_t r, uint8_t c)
{
	g_internal.m_trellis.clrLED(index(r, c));
}

void TrellisQuad::writeDisplay()
{
	g_internal.m_trellis.writeDisplay();
}

static bool charSpace[20] =
	{ false, false, false, false
	, false, false, false, false
	, false, false, false, false
	, false, false, false, false
	, false, false, false, false
	};

static bool char0[20] =
	{ false,  true,  true,  true
	, false,  true, false,  true
	, false,  true, false,  true
	, false,  true, false,  true
	, false,  true,  true,  true
	};

static bool char1[20] =
	{ false, false,  true, false
	, false, false,  true, false
	, false, false,  true, false
	, false, false,  true, false
	, false, false,  true, false
	};

static bool char2[20] =
	{ false,  true,  true,  true
	, false, false, false,  true
	, false,  true,  true,  true
	, false,  true, false, false
	, false,  true,  true,  true
	};

static bool char3[20] =
	{ false,  true,  true,  true
	, false, false, false,  true
	, false, false,  true,  true
	, false, false, false,  true
	, false,  true,  true,  true
	};

static bool char4[20] =
	{ false,  true, false,  true
	, false,  true, false,  true
	, false,  true,  true,  true
	, false, false, false,  true
	, false, false, false,  true
	};

static bool char5[20] =
	{ false,  true,  true,  true
	, false,  true, false, false
	, false,  true,  true,  true
	, false, false, false,  true
	, false,  true,  true,  true
	};

static bool char6[20] =
	{ false,  true,  true,  true
	, false,  true, false, false
	, false,  true,  true,  true
	, false,  true, false,  true
	, false,  true,  true,  true
	};

static bool char7[20] =
	{ false,  true,  true,  true
	, false, false, false,  true
	, false, false, false,  true
	, false, false, false,  true
	, false, false, false,  true
	};

static bool char8[20] =
	{ false,  true,  true,  true
	, false,  true, false,  true
	, false,  true,  true,  true
	, false,  true, false,  true
	, false,  true,  true,  true
	};

static bool char9[20] =
	{ false,  true,  true,  true
	, false,  true, false,  true
	, false,  true,  true,  true
	, false, false, false,  true
	, false,  true,  true,  true
	};

void TrellisQuad::drawChar(uint8_t r0, uint8_t c0, char x)
{
	const bool *ch;

	switch (x)
	{
		case '0': ch = char0; break;
		case '1': ch = char1; break;
		case '2': ch = char2; break;
		case '3': ch = char3; break;
		case '4': ch = char4; break;
		case '5': ch = char5; break;
		case '6': ch = char6; break;
		case '7': ch = char7; break;
		case '8': ch = char8; break;
		case '9': ch = char9; break;
		default: ch = charSpace;
	}

	for (uint8_t r = 0; r < 5; ++r)
		for (uint8_t c = 0; c < 4; ++c)
			setLed(r0 + r, c0 + c, ch[r * 4 + c]);
}

void TrellisQuad::drawNum(uint8_t r0, uint8_t c0, int8_t i)
{
	drawChar(r0, c0 + 4, '0' + i % 10);
	drawChar(r0, c0, '0' + i / 10);
}

