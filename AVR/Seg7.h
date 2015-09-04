#pragma once

#include "Bits.h"
#include "Timer.h"
#include <string.h>
#include <stdlib.h>

class font_t
{
public:
	static uint8_t seg7(char c)
	{
		return s_glyphs[(c > 'Z' ? c - 0x20 : c) - ' '];
	}

private:
	static const uint8_t s_glyphs[];
};

const uint8_t font_t::s_glyphs[] =
	{ 0b0000000	// space
	, 0b0000000	// !
	, 0b0000000	// "
	, 0b0000000	// #
	, 0b0000000	// $
	, 0b0000000	// %
	, 0b0000000	// &
	, 0b0000000	// '
	, 0b0000000	// (
	, 0b0000000	// )
	, 0b0000000	// *
	, 0b0000000	// +
	, 0b0000000	// ,
	, 0b1000000	// -
	, 0b0000000	// .
	, 0b0000000	// /
	, 0b0111111	// 0
	, 0b0000110	// 1
	, 0b1011011	// 2
	, 0b1001111	// 3
	, 0b1100110	// 4
	, 0b1101101	// 5
	, 0b1111101	// 6
	, 0b0000111	// 7
	, 0b1111111	// 8
	, 0b1101111	// 9
	, 0b0000000	// :
	, 0b0000000	// ;
	, 0b0000000	// <
	, 0b0000000	// =
	, 0b0000000	// >
	, 0b0000000	// ?
	, 0b0000000	// @
	, 0b1110111	// A
	, 0b1111100	// B
	, 0b1011000	// C
	, 0b1011110	// D
	, 0b1111001	// E
	, 0b1110001	// F
	, 0b0111101	// G
	, 0b1110100	// H
	, 0b0000100	// I
	, 0b0011110	// J
	, 0b1110101	// K
	, 0b0111000	// L
	, 0b0110111	// M
	, 0b1010100	// N
	, 0b1011100	// O
	, 0b1110011	// P
	, 0b1100111	// Q
	, 0b1010000	// R
	, 0b1101100	// S
	, 0b1111000	// T
	, 0b0011100	// U
	, 0b0111110	// V
	, 0b1111110	// W
	, 0b1110110	// X
	, 0b1101110	// Y
	, 0b0011011	// Z
	};

template<class DIGITS, class SEGMENTS>
class seg7_t
{
public:
	static void setup()
	{
		setup_bits<DIGITS>();
		write_bits<DIGITS>(~0);
		setup_bits<SEGMENTS>();
		write_bits<SEGMENTS>(~0);
	}

	static void write(const char *s)
	{
		uint8_t l = strlen(s), c = 0;

		while (l + c < nchars)
			s_enc[c++] = ~0;

		while (c < nchars)
			s_enc[c++] = ~font_t::seg7(*s++);
	}

	static void write(int x, int radix = 10)
	{
		write(itoa(x, s_buf, radix));
	}

	static void write(unsigned x, int radix = 10)
	{
		write(utoa(x, s_buf, radix));
	}

	static void write(long x, int radix = 10)
	{
		write(ltoa(x, s_buf, radix));
	}

	static void write(unsigned long x, int radix = 10)
	{
		write(ultoa(x, s_buf, radix));
	}

	static void refresh()
	{
		static uint8_t k = 0;

		write_bits<DIGITS>(~0);
		write_bits<SEGMENTS>(s_enc[k]);
		write_bits<DIGITS>(~(1 << k));

		if (++k >= nchars)
			k = 0;
	}

	static void auto_refresh()
	{
		typedef timer_t<0> timer;

		timer::prescale(timer::prescale_256);
		timer::isr(refresh);
		timer::enable();
		sei();
	}

private:
	static const uint8_t nchars = 4;	// FIXME: depend on DIGITS!
	static uint8_t s_enc[nchars];		// encoded characters
	static char s_buf[33];				// radix 2 on a 32-bit number plus terminator
};

template<class DIGITS, class SEGMENTS>
char seg7_t<DIGITS, SEGMENTS>::s_buf[33];

template<class DIGITS, class SEGMENTS>
uint8_t seg7_t<DIGITS, SEGMENTS>::s_enc[seg7_t<DIGITS, SEGMENTS>::nchars];

